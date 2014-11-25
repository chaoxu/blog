---
title: Applications of finger trees
---

# Finger trees

Consider a data structure that maintains a sequence of monoid elements from $(M,\cdot)$. The data structure $S$ stores a sequence of elements from $M$, say $a_1,\ldots,a_n$. It has four operations. All time are amortized and not worst case.

1. $split(S,p)$, $p:M\to \{0,1\}$ is a monotonic predicate on $M$, i.e. $p(ab)\geq p(a)$. It split the sequence at position $t$, such that $p(\prod_{i=1}^j a_i) = 0$ for all $j < t$, and $p(\prod_{i=1}^j a_i) = 1$ for all $j\geq t$. This takes $O(\log \min(n,n-t))$ time.

2. $concat(S,T)$, concatenate sequence represented by $S$ and the sequence represented by $T$. If the length are $n$ and $m$, respectively, then in $O(\log \min(n,m))$ time.

3. $product(S)$, returns $\prod_{i=1}^n a_i$ in $O(1)$ time.

4. $endomorphism(S,f)$, apply $f$ to everything in the sequence. So $S$ would represent $f(a_1),\ldots,f(a_n)$. In $O(1)$ time.

We can produce a empty sequence using $Empty()$.

We have to first make a few assumptions: All monoid operations take $O(1)$ time. All endomorphisms we consider can be evaluated in $O(1)$ time AND we can compute $h = f\circ g$ in $O(1)$ time. Note this means we compute (a representation) of the function $h$ it self, and we can use this representation to compute $h(x)$ in $O(1)$ time.

Otherwise, all "$O(T)$ time" should be replaced with "$O(T)$ monoid operation, endomorphism evaluation, endomorphism composition and constant time operations.". 

Such data structure exists. The first $3$ operation are supported by finger tree [@Hinze2006]. It is not hard to add the last operation, the idea is to tag nodes with an endomorphism $f$(in the beginning $f$ is just the identity), and it will mean "apply $f$ to everything below!". It propagate only when the lower level nodes need to be accessed, therefore the cost would be charged into the other operations. Many other binary search trees probably can implement these operations too. For example, if the dynamic optimality conjecture for splay tree is true, it be best to use splay tree for implementation. However, that is not the point. The point is to have an abstract data structure over a monoid sequence.

# Extend its power

Finger tree can be extended to query its index: take the Cartesian product of the monoid and $(\N,+)$, and we get a new monoid. So we can just assume our data structure has the following extensions:

1. $splitAt(S,i)$, split the sequence to two sequence $A$ and $B$ at the $i$th index in $O(\log \min(n,n-i))$ time.

2. $insertAt(S,i, x)$, insert element to the $i$th position.

3. $delete(S,i)$, delete the element at $i$th position.

# Application

There are some common application of finger trees.

1. Stack, queue, dequeue with product operation, all amortized constant time. As a special case, it would solve the [min stack problem](http://www.geeksforgeeks.org/design-and-implement-special-stack-data-structure/). 

2. Random access sequence, which is related to the [rope](http://en.wikipedia.org/wiki/Rope_%28data_structure%29) data structure.

3. Ordered sequence(sorted list).

4. Priority queues.

5. Interval trees, segment trees.

Most of the above has been described in [@Hinze2006], but we will talk about two specifics that usually not mentioned by others. 

## Merge sorted lists in optimal time bound

{Theorem}

    If $S$ and $T$ are two ordered sequences of length $n$ and $m$, respectively. $n\leq m$. Both ordered sequences are represented by finger trees. Compute the finger tree representation of $S\cup T$ takes $O(n\log \frac{n}{m})$ time.

Split $T$ into $n$ pieces one by one by split along the $i$th element of $S$ to the second part of the $i$th produced piece for all $i$, then concatenate all of them. The splitting takes $\sum_{i=1}^n \log t_i$ time, where $t_i$ is the size of the $i$th piece. By concavity, we have the time for split is $\sum_{i=1}^n \log t_i \leq n \log \frac{m}{n}$, and concatenation time is similar.

## Solve the Klee's measure problem

This section shows how the endomorphism operation is quite crucial because we can make "range update" operations.

The motivation came from the following question. Can finger tree substitute for the [popular competitive programming data structure segment tree](http://letuskode.blogspot.com/2013/01/segtrees.html), a special case of the real [segment tree](http://en.wikipedia.org/wiki/Segment_tree)? This is not possible, and it's not because the large hidden constants. The abstract definition in this article goes cannot do the following operation on an sequence of integers: increment the $i$th number by $i$. This operation make little sense if we allow insert and deletions, but many use of segment tree do not consider insert and deletes.

Fortunately, [Klee's measure problem](
http://en.wikipedia.org/wiki/Klee%27s_measure_problem), the problem that caused the invention of (real) segment tree can be solved with a finger tree. We show how to solve it in 1D. It's well known how to [use it for solving the $n$D version](http://cstheory.stackexchange.com/questions/17252/number-of-maximum-overlap-in-n-dimensions/17374).

We produce another data structure, such that each operation takes $O(\log n)$ amortized time. Where $n$ is the number of times we called insert.

We maintain a collection of intervals with data structure $D$, there are 3 operations.

1. $insert(D,a,b)$, insert interval $(a,b)$ into the collection.
2. $delete(D,a,b)$. Delete a interval $(a,b)$ from the collection, we assume it's always a interval inserted before.
3. $measure(D)$. Find the measure of the union of all intervals in the collection. 

{Remark}
    It can be sharpened such that $n$ is the number of intervals inside the data structure. We just do a global rebuild if the number of interval doubled or reduced to half since last global update. Also, measure() can be done in constant amortized time.

The finger tree stores a sequence of elementary intervals that partitions the space, and how many copies of that interval exists. Thus we can represent it as $((l,r),c)$, where $(l,r)$ is the interval with left and right boundary, and $c$ is the number of copies.

To insert a interval, we check if it creates new elementary intervals by split at positions where the boundary points should be in the elementary interval, remove it, and replace it with new elementary intervals. This takes $O(\log n)$ time, and we increase the number of elementary intervals by at most $2$.

$f$ is an automorphism defined as $f(((a,b),c))=((a,b),c+1)$. For each insertion of interval $(a,b)$, $f$ gets applied to all elementary interval in the range. For deletion, we apply $f^{-1}$ instead. 

The monoid product is simply $\prod_{i=1}^n ( (l_i, r_i),c) = \sum_{i=1}^n \min(c,1)(r_i-l_i)$, and this is exactly what $measure(D)$ should output.

# Reference