---
title: Open Problems
---

Here are some problems that are either open to the best of my knowledge, or asked by me but unable to answer. If you know a solution, or can offer me more reference on the material, please comment. I appreciate it greatly. :)

{Problem}

    There are $n$ people sitting on a circle of $n$ seats. Out of the $n$ people, there are $k$ couples($2k$ people). You can swap any two people. Find an algorithm that swap the least amount of times such that all the couples are sitting with his/her partner.

{Problem}

    Given a graph $G$ with max degree $3$ and it's hamiltonian cycle $C$, there is a matching $M$ of size $k$. Find a matching $N$ of $C$, such that each component in $M\cup N$ have same number of edges from $M$ and $N$, and the number of cycles in $M\cup N$ is maximized. (we consider a component of a single edge a cycle).

## Combinatorics and Algorithms

{Problem}
    Are there any sorting algorithms such that every element is compared $O(\log n)$ time, and does not depend on the AKS sorting network?

[Related link on TCS.SE](http://cstheory.stackexchange.com/questions/7131/sorting-algorithm-such-that-each-element-is-compared-o-log-n-times-and-does).

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
    Can you use dynamic programming to count the number of occurences of a candidate S'?

{Problem}
    Fix string $x$ with length $|x|$, let $f(y,x)$ be the number of different ways $y$ can be a subsequence of $x$. $m(x) = \max_{y} f(y,s)$, where $y$ range through all strings. Let $\phi(x)$ be the number of distinct subsequences of $x$. 

    What are the relations between these parameters? Trivially $m(x)\geq \frac{|x|}{\phi(x)}$.

### Algebra

{Problem}
    How fast can one solve the word problem on the following presentation $G = \langle x_1,x_2,\ldots,x_n \mid  x_iw = wx_i, R=1 \rangle$?
    Where $R$ is a permutation of the word $x_1\ldots x_nx_1^{-1}\ldots x_n^{-1}$, and $w\in G$.

{Problem}
    Characterize automatic group with star free language.

{Problem}
    Can the word problem for $B_4$ be done in subquadratic time? $B_4$ is the braid group on 4 strands. 

### Discrete Geometry
{Problem}
    There are a set $S$ of points on the plane, such that the distance between any two is at least $d$. What is the smallest $d$, such that no matter how large $|S|$ is, one can always cover $S$ with $|S|$ disjoint unit disks? 

### Topology
{Problem}
    Let the set $C$ be a set of homotopy classes of simple closed curves on an orientable surface $S$ of genus $g$, such that if $a,b\in C$, then their minimal intersection number is at most 1. Let $X_g$ be the set of all the sets with the above property. Find bounds on $f(g) = \sup_{C\in X_g}|C|$.

[Collections of Simple Closed Curves Intersecting at Most Once](http://www.math.uchicago.edu/~may/VIGRE/VIGRE2007/REUPapers/FINALFULL/Constantin.pdf) The paper by Sarah Constantin proves an exponential upper bound and a quadratic lower bound. 
[Arcs intersecting at most once](http://arxiv.org/abs/1402.1570). This preprint by Piotr Przytycki gives a cubic upper bound, and in fact, a polynomial upper bound for any fixed number of intersections.

### Others
http://cstheory.stackexchange.com/questions/16682/scheduling-with-rsvp-deadline-and-possible-cancelations

{Problem}
    Find the minimum length regular expression that describes a regular language.

{Problem}
    Given $a_1,\ldots,a_n$, $b_1,\ldots,b_n$ and $\lambda$ in $\R^+$. Does there exist a permutation such that
    \[ 
    \sum_{i=1}^n a_{\pi(i)}a_{\pi(i+1)} - \lambda b_{\pi(i)}b_{\pi(i+1)} < 0
    \], where we define $\pi(n+1)=\pi(1)$.

{Problem}
    Can finger tree replace [segment trees](letuskode.blogspot.com/2013/01/segtrees.html)?

{Problem}
    How fast can we [improve the pseudo-polytime algorithm for subset sum](http://cstheory.stackexchange.com/questions/21533/faster-pseudo-polynomial-time-algorithm-for-subset-sum)? 

{Problem}
    Let $T$ be a rooted, ordered, labeled tree. For each vertex, find the maximum common subtree in all the subtree rooted at it's children. 