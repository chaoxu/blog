---
title: Open Problems
---

Here are some problems that are either open to the best of my knowledge, or asked by me but unable to answer. If you know a solution, or can offer me more reference on the material, please comment. I appreciate it greatly. :)

# Problems I currently have interest 
These problems ordered by my current interest.

{Problem}
    Find the [Fréchet distance](http://en.wikipedia.org/wiki/Fr%C3%A9chet_distance) of two polygonal chain of length $n$ and $m$ respectively in $1$D in $o(nm)$ time.

For $2$D, there is a $\Omega(nm)$ lower bound assuming SETH. If this is solved, one can generalize it so the underlying topology is a tree. There might be a negative result due to how similar this problem is to edit distance. 

{Problem}
    Let $T$ be a tree with vertex weights. Find $k$ disjoint subtrees, such that the sum of the weights of the vertices covered is maximized. ($k$ is part of the input). 

I have some preliminary results. Also see the [path version of this problem](http://www.chaoxuprime.com/posts/2014-10-13-maximum-sum-k-disjoint-subarrays.html). I mainly want to see if generalize those techniques to tree if possible. Also, what happens if we consider a cycle? cactus graph? outerplanar graph? This problem for planar graph is NP-hard.

{Problem}
    How fast can we [improve the pseudo-polytime algorithm for subset sum](http://cstheory.stackexchange.com/questions/21533/faster-pseudo-polynomial-time-algorithm-for-subset-sum)? 

Interesting to think about but probably can't be improved easily.

# Other problems

{Problem}

    There are $n$ people sitting on a circle of $n$ seats. Out of the $n$ people, there are $k$ couples($2k$ people). You can swap any two people. Find an algorithm that swap the least amount of times such that all the couples are sitting with his/her partner.

{Problem}

    Given a graph $G$ with max degree $3$ and it's hamiltonian cycle $C$, there is a matching $M$ of size $k$. Find a matching $N$ of $C$, such that each component in $M\cup N$ have same number of edges from $M$ and $N$, and the number of cycles in $M\cup N$ is maximized. (we consider a component of a single edge a cycle).

Seems NP-hard and related to the previous problem, I could explain the relations. 

{Problem}
    Classify all kind of matrices such that the quadratic assignment problems can be solved with a fixed kind of permutations.

A vague question, but see [this paper](http://link.springer.com/article/10.1007/BF01585868). This is a hard problem, probably not for undergrads. 

## Combinatorics

{Problem}
    What's the length of the shortest supersequence of all permutations on $n$ alphabets? 
[It's featured on open problem garden](http://garden.irmacs.sfu.ca/?q=op/smallest_universal_supersequence).

{Problem}
    What's the length of the shortest superstring of all permutations on $n$ alphabets? 

[OEIS:A180632](http://oeis.org/A180632)

These are some problems released as [project for CSE 549 Fall 2010 in Stony Brook](http://www.cs.sunysb.edu/~skiena/549/projects.pdf) by [Steven Skiena](http://www.cs.sunysb.edu/~skiena/).

{Problem}(Shortest Subset Subsequence)
    As discussed in algorithm reading group, what is the string on an alphabet of size n such that every one of the $2^n$ subsets is represented by a substring of minimum size, ie. equal to the cardinality.

{Problem}(Finding the Most Frequent Subsequence)
    Given a string S, which string S' occurs most often as a scattered subsequence of S? Note that the number of candidates and possible occurrences are exponential. Can you prove this is NP-complete, and maybe give an approximation algorithm?

{Problem}
    Fix string $x$ with length $|x|$, let $f(y,x)$ be the number of different ways $y$ can be a subsequence of $x$. $m(x) = \max_{y} f(y,x)$, where $y$ range through subsequences of $x$. Let $\phi(x)$ be the number of distinct subsequences of $x$. 

    What are the relations between these parameters? Trivially $m(x)\geq \frac{|x|}{\phi(x)}$.

## Discrete Geometry
{Problem}
    There are a set $S$ of points on the plane, such that the distance between any two is at least $d$. What is the smallest $d$, such that no matter how large $|S|$ is, one can always cover $S$ with $|S|$ disjoint unit disks? 

## Algorithms

{Problem}
    Given $a_1,\ldots,a_n$ and $b_1,\ldots,b_n$. Does there exist a permutation such that
    \[ 
    \sum_{i=1}^n a_{\pi(i)}a_{\pi(i+1)} - b_{\pi(i)}b_{\pi(i+1)} < 0
    \], where we define $\pi(n+1)=\pi(1)$.
    Is this problem NP-hard?

Note this is basically traveling salesman problem such that the distance between two vertices is the difference of two symmetric product matrix, where a matrix $M$ is a product matrix of vector $x$ and $y$ if $M_{ij}=x_iy_j$.

{Problem}
    Let $T$ be a rooted, ordered, labeled tree. For each vertex, find the maximum common subtree in all the subtree rooted at it's children. Anything better than the trivial algorithm here?

{Problem}
    Given a simple unit edge capacity undirected graph $G=(V,E)$ with a subset of vertices $T$ with unit capacity, and remaining vertices with infinite capacity. Can one find a $st$-maximum flow in deterministic $O(n^{2.5})$ time?

{Problem}
    Let $G$ be a directed graph with a source vertex $s$, how fast can one find $\min_{t\in T} \lambda(s,t)$? 

{Problem}
    How hard(bit complexity) is it to sample from the distribution where $i$th event has probability $p_i/q_i$ to happen?

{Problem}
    Is there a sparsifier for a (simple) mixed graph with element connectivity? Namely, $k$ spanning forests over the undirected edges, such that the element connectivity between any two vertices is preserved up to $k$?

{Problem}
    [Fill up bins with minimum moves](http://cs.stackexchange.com/questions/12441/is-it-np-hard-to-fill-up-bins-with-minimum-moves)

{Problem}
    [Maximum local edge connectivity](http://cstheory.stackexchange.com/questions/25531/maximum-local-edge-connectivity)

{Problem}
    For every constant $k$, there exist a Laminar family $X$ over ground set $V$ of $n$ vertices, such that for any graph $G=(V,E)$ such that $G[A]$ has diameter at most $k$ for all $A\in X$, then $|E|=\Omega(n\lambda_k(n))$. Find the best $\lambda_k(n)$ possible. See [this](http://www.chaoxuprime.com/posts/2014-09-21-augment-induced-subtree-constant-diameter.html).

{Problem}
    Given $a_1,\ldots,a_n$ with weight $w(a_i)$ and label $l(a_i)$, and a integer $k$. You want to partition the list into $k$ lists, say $L_1,\ldots,L_k$. Now for each list $L_i$, you have a label $l(L_i)$. The weight of the list $L$ is $\sum_{e\in L,l(e)\neq l(L)} w(e)$. Find a partition with minimum total weight. 

Note this is called the homogeneous string segmentation problem.  http://link.springer.com/article/10.1007%2Fs00453-008-9225-8 We can also consider this problem on trees. 
