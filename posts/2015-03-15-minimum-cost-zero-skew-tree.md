---
title: Minimum cost zero skew tree
---

{Problem}
    Let $T$ be a rooted tree with costs $c(e)$ and length lower bounds $l(e)$ on each edge $e$. We are interested in compute a function $f$, such that $f(e)\geq l(e)$, for any root to leaf path $P$, $\sum_{e\in P} f(e)=t$ and $\sum_{e\in E} c(e)f(e)$ is minimized. 

This problem can be solved in $O(n\log n)$ time by reducing it to minimum cost flow on 2-terminal series parallel graph. [@Booth1993416]

$C(v)$ denote the set of all the children of $v$. 

Let $P(vu)$ for a edge $vu\in E(T)$ to be the parallel connection of one single edge with cost $c(uv)$, lower bound $l(uv)$, infinite upper bound and series-parallel graph $S(u)$.
Let $S(v)$ to be the series connection of $P(vu)$ for all $u\in C(v)$.

Let $G=S(r)$, where $r$ is the root of $T$. 
It is easy to see that there is a bijection between the edges in $T$ and the edges in $G$. 

Finding the minimum cost flow of value $t$ in $G$ gives us the desired solution by going back to the original edge using the bijection.

{Remark}
    It's not hard to see for any two terminal series-parallel graph, one can find a rooted tree $(T,r)$ such that $G=S(r)$. So the two problems are actually equivalent.

# References