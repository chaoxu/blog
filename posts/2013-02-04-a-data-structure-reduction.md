---
title: A Data Structure Reduction
tags: data structure
---

Consider a data structure that maintain a sequence of element from $A$.
$m:A\to M$ is a function that maps $A$ to a monoid $(M,+)$.

We have the following data structure: 

1. The data structure stores the sequence $a_1,\ldots,a_n$, where $\{a_1,\ldots,a_n\}\subset A$.
2. $split(p)$, $p:M\to \{0,1\}$. $p$ is a monotonic predicate on $M$, i.e. $p(a+b)\geq p(a)$. It split the sequence at position $t$, such that $p(\sum_{i=0}^j m(a_i)) = 0$ for all $j < t$, and $p(\sum_{i=0}^j m(a_i)) = 1$ for all $i\geq t$. This is $O(T \log \min(n,n-t))$ time.
3. $concat(a,b)$, concat sequence $a$ and $b$ of length $n$ and $m$, respectively, in $O(T \log \min(n,m))$ time.
4. $sum(a)$, returns $\sum_{i=0}^n a_i$ in $O(1)$ time.

Such data structure exist, say a finger tree. $T$ is the time required for an monoid operation.

Can we use this data structure as a black box, such that we can solve the following problem in $O(\log n)$ time per operation?

We maintain a collection of intervals, there are 3 operations.

1. insert(a,b), insert interval $(a,b)$ into the collection.
2. delete(a,b). Delete a interval $(a,b)$ if it exists in the collection.
3. measure(). Find the measure of the union of all intervals in the collection.