This example models the denic outage of May 2026 (see https://blog.denic.de/en/final-report-dns-outage-of-5-may-2026/).
In that outage, DENIC intended to update their KSKs, but did not include two out of the three keys at the zone apex.
All three keys had the same key tag.
This misconfiguration lead to roughly 2/3 of the zone being non-resolvable to validating resolvers.

Running the following command naturally shows that there are regressions during the rollout of the update:
```
% stack run -- regressions old/dsset-de. new/dsset-de. de. old/a.de old/b.de old/c.de old/de :: new/a.de new/b.de new/c.de new/de
resolvable('a', ['de', 'a', 'ns'], top()).
resolvable('a', ['de', 'b', 'ns'], top()).
resolvable('a', ['de', 'x'], top()).
resolvable('a', ['de', 'y'], top()).
resolvable('txt', ['de', 'a'], top()).
resolvable('txt', ['de', 'b'], top()).
```

In a KSK-rollover, one wouldn't expect *any* changes in resolvability, yet, there are some, but only for validating resolvers.
