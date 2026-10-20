This example models the denic outage of May 2026 (see https://blog.denic.de/en/final-report-dns-outage-of-5-may-2026/).
In that outage, DENIC intended to update their KSKs, but did not include two out of the three keys at the zone apex.
All three keys had the same key tag.
This misconfiguration lead to roughly 2/3 of the zone being non-resolvable to validating resolvers.

Running the following command naturally shows that there are regressions during the rollout of the update:
```
% stack run -- regressions old/dsset-de. new/dsset-de. de. old/a.de old/b.de old/c.de old/de :: new/a.de new/b.de new/c.de new/de
resolvable(['de'], 'a', ['de', 'a', 'ns'], top(), ad(0)).
resolvable(['de'], 'a', ['de', 'b', 'ns'], top(), ad(0)).
resolvable(['de'], 'a', ['de', 'x'], top(), ad(1)).
resolvable(['de'], 'a', ['de', 'y'], top(), ad(1)).
resolvable(['de'], 'txt', ['de', 'a'], top(), ad(0)).
resolvable(['de'], 'txt', ['de', 'b'], top(), ad(0)).
```

In a KSK-rollover, one wouldn't expect *any* changes in resolvability, yet, there are some, but only for validating resolvers.
