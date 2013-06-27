---
title: Problems
---
## Swap balls so bins have same color balls

http://cs.stackexchange.com/questions/12560/use-minimum-number-of-swaps-so-each-bin-contains-balls-of-the-same-color

## Minimum moves to fill up the bins

http://cs.stackexchange.com/questions/12441/is-it-np-hard-to-fill-up-bins-with-minimum-moves

## Radar Charts

1. Find the condition a existance of a convex radar charts.
2. Optimize for the max area under the condition.
3. Given two set of data, find the radar chart that maximizes or minimizes $|a-b|$, $a/b$.
 
## k-wise intersections and (k+1)-wise intersections

Let $S$ a finite set of sets. Define $S_k$ and $D_k$.
\[
S_k = \{ \bigcap T : |T|=k, T \subset S\}
\]
\[
D_k = \{x \backslash \bigcup S_{k+1} : x \in S_k\}
\]
Trivially, $x\in S_{k+1}$, $y\in S_k$, then $x\subset y$.

{Problem}
    Prove that $D_k$ is a set of disjoint sets.

{Proof}
    If $x,y\in D_k$ and $x\cap y$ is nonempty and $x\neq y$. Since $x=\bigcap_{i=1}^k x_i$ and $y = \bigcap_{i=1}^k y_i$, $x\cap y = \bigcap_{i=1}^k x_i \cap y_i$, and there are at least some $i,j$, such that $x_i\neq y_j$. This shows $x\cap y \subset \bigcap \mathcal{S}_{k+1}$.

## Maximum number of edges in a Hasse Diagram on a poset of $n$ elements
What's the maximum number of edges in a Hasse Diagram on a poset of $n$ elements?

Equivalently, find an upper bound of the number of edges in the transitive reduction of any graph $G$.

Answer: For even $n$, it's $\frac{n^2}{4}$, for odd $n$, $\frac{n^2-2n}{4}$. Solution comes from the maximal triangle free graph is bipartite, and transitive reductions are triangle free.

## Compute associative operations faster
**Input:** Let $M$ be some set, such that all elements in $M$ takes $O(1)$ space to store. 

1. List $a_1,\ldots, a_n$, where $a_i\in M$. This is stored as an array and cost no extra memory.
2. Sequence of pairs $(s_1, t_1),\ldots, (s_m,t_m)$ such that $s_i,t_i\in \{1,\ldots,n\}$ and $s_i\leq t_i$. They represent "intervals".
3. $\oplus$, such that $\oplus$ is an associative operation on $M$. Compute $a \oplus b$ takes $O(1)$ time for all elements $a,b\in M$.

**Output:**
Let $f(i,j) = a_i\oplus a_{i+1} \oplus \ldots \oplus a_j$. It is a sum over the interval. We want to compute $f(s_1,t_1), \ldots, f(s_m,t_m)$. 

**Goals:**
Let 
\[
k = \left| \bigcup_{1 \leq i\leq m} \{s_i,s_i+1,\ldots, t_i -1, t_i\} \right|
\].

1. The number of times the algorithm uses $\oplus$ is minimized, or if it's too hard, a constant factor away from it.
2. The algorithm uses little extra memory. A lower bound is $\Omega(m+k)$.
3. The algorithm uses little time. A lower bound is $\Omega(m+k)$.

I do not know if there exist an algorithm that satisfies all the goals at the same time.

http://cs.stackexchange.com/questions/11091/find-interval-sums-with-minimum-number-of-operation

## Distance from closest permutation with one cycle
Let $[n]=\{1,\ldots,n\}$. $f:[n]\to[n]$. Find a permutation $\pi:[n]\to[n]$, such that $\pi$ has only one cycle, and the hamming distance between $f$ and $\pi$ is minimized.  (By Matt Dipple)

## Saving on multiple binary searchs

We have two sorted arrays $A$ with $n$ elements and $B$ with $m$ elements. such that all the elements are unique and every element in $B$ appears in $A$.

Compute $C$, such that $C[i]$ contains the positions where $B[i]$ is equal to $A[j]$. In other words, compute $C[i]$, such that $B[i]=A[C[i]]$.

If $m=\Theta(n)$, then we use a variation of a merge algorithm and get $O(n)$ time.

If $m = O(\frac{n}{\log n})$. We can do $m$ binary searches, and find all the positions in $O(m \log n)$ time.  Can we do better in this case?

What about when $m = \Omega(\frac{n}{\log n})$?

Answer: It can be done in $O(m \log \frac{n}{m})$ time \href{http://cstheory.stackexchange.com/questions/10407/merging-two-binary-search-trees}{according to this}.