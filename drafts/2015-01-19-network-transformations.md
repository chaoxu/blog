---
title: Network Transformations
---

There are so many variations of what minimum cost flow means. Sometimes an algorithm might state it only works if the costs are all positive.

Here I consider a table, and which variations are strong enough to the general version.

The general problem, minimum cost flow:

Each vertex has a balance constraint.
Each edge has lower bound(demand) and upper bound(capacity).

Symmetric:
For every arc, there is an antiparallel arc with the same lower bound/upper bound.