"""Regression tests for resumable, paired evaluation; no public DNS queries."""
import contextlib
import io
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

import run_queries


class EvaluationTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.queries = self.root / 'queries.txt'
        self.queries.write_text('one.test. A\ntwo.test. AAAA\nthree.test. NS\n')
        self.tool = self.root / 'tool'
        self.dig = self.root / 'dig'
        self.tool.mkdir()
        self.dig.mkdir()
        self.quiet = contextlib.redirect_stdout(io.StringIO())
        self.quiet.__enter__()
        self.addCleanup(self.quiet.__exit__, None, None, None)

    def evaluate(self, **kwargs):
        return run_queries.evaluate(self.queries, self.tool, self.dig, 30, 30, **kwargs)

    @patch('run_queries.random.shuffle')
    @patch('run_queries.run_and_save')
    def test_random_order_and_completed_pairs_are_skipped(self, save, shuffle):
        shuffle.side_effect = lambda entries: entries.reverse()
        (self.tool / '3.txt').write_text('existing tool')
        (self.dig / '3.txt').write_text('existing dig')
        save.side_effect = lambda command, path, timeout: path.write_text('new')
        self.assertEqual(self.evaluate(limit=1), 1)
        self.assertEqual([call.args[1].name for call in save.call_args_list], ['2.txt', '2.txt'])
        self.assertEqual((self.tool / '3.txt').read_text(), 'existing tool')
        self.assertFalse((self.tool / '1.txt').exists())
        shuffle.assert_called_once()

    @patch('run_queries.random.shuffle')
    @patch('run_queries.run_and_save')
    def test_either_missing_result_reruns_both(self, save, shuffle):
        (self.tool / '1.txt').write_text('stale tool')
        (self.dig / '2.txt').write_text('stale dig')
        (self.tool / '3.txt').write_text('complete')
        (self.dig / '3.txt').write_text('complete')

        def write(command, path, timeout):
            if path.parent == self.tool:
                self.assertFalse(path.exists())
                self.assertFalse((self.dig / path.name).exists())
            path.write_text('fresh')

        save.side_effect = write
        self.assertEqual(self.evaluate(), 2)
        self.assertEqual(save.call_count, 4)
        for number in [1, 2]:
            self.assertEqual((self.tool / f'{number}.txt').read_text(), 'fresh')
            self.assertEqual((self.dig / f'{number}.txt').read_text(), 'fresh')

    @patch('run_queries.random.shuffle')
    @patch('run_queries.run_and_save')
    def test_interrupted_pair_is_rerun_on_next_invocation(self, save, shuffle):
        def interrupted(command, path, timeout):
            if path.parent == self.dig:
                raise KeyboardInterrupt
            path.write_text('first attempt')

        save.side_effect = interrupted
        with self.assertRaises(KeyboardInterrupt):
            self.evaluate(limit=1)
        self.assertFalse((self.dig / '1.txt').exists())
        save.reset_mock()
        save.side_effect = lambda command, path, timeout: path.write_text('second attempt')
        self.evaluate(limit=1)
        self.assertEqual(save.call_count, 2)
        self.assertEqual((self.tool / '1.txt').read_text(), 'second attempt')

    @patch('run_queries.run_and_save')
    def test_zero_limit_does_not_run_commands(self, save):
        self.assertEqual(self.evaluate(limit=0), 0)
        save.assert_not_called()

    def test_command_failure_is_logged_atomically(self):
        output = self.tool / '1.txt'
        run_queries.run_and_save([sys.executable, '-c', "print('diagnostic'); raise SystemExit(7)"], output, 2)
        self.assertIn('diagnostic', output.read_text())
        self.assertIn('exited with status 7', output.read_text())
        self.assertFalse(output.with_suffix('.tmp').exists())

    def test_timeout_is_a_logged_result(self):
        output = self.dig / '1.txt'
        run_queries.run_and_save([sys.executable, '-u', '-c',
                                 "import time; print('partial'); time.sleep(30)"], output, 0.1)
        self.assertIn('partial', output.read_text())
        self.assertIn('timed out', output.read_text())
        self.assertFalse(output.with_suffix('.tmp').exists())


if __name__ == '__main__':
    unittest.main()
