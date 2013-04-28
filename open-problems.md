---
title: Open Problems
---

Here are some problems that are either open to the best of my knowledge, or asked by me but unable to answer. If you know a solution, or can offer me more reference on the material, please comment. I appreciate it greatly. :)

## Combinatorics and Algorithms

### Number of functions for smallest position that maximizes the sum of $k$ items

{Problem}
    If $a$ is a finite sequence of $0$ and $1$. Define the function $f_a(k)=m$ if $m$ is the smallest number that maximizes $\sum_{i=0}^{k-1} a_{m+i}$. Let $A_n$ be the set of all finite sequences of $0$ and $1$ with length $n$. Find $g(n)$, where $g(n) = |\{f_a | a\in A_n\}|$.

Why is this interesting? 
One can think of a database that store the number of times some event happened every minute, and ask for the first time when the event occurred more than 6 times in a hour. This kind of queries can be answered of course by precompute the function $f$. If $g(n)$ is small, then one can store $f$ very efficiently(ok, a little bit more efficient than $n$ integers, so really there is no real world application).

{Problem}
    Same as above problem, but now one asks the question on finite sequences containing integer from $0$ to $N$.

Data for g(n)

- N=2: 1, 2, 4, 9, 19, 40, 83, 171, 347, 706, 1428, 2887, 5814, 11721, 
23545, 47345, 94960, 190572, 381632, 764686, 1529258, 3059716, 6111843
- N=3: 1, 2, 4, 12, 35, 118, 372, 1184, 3643, 11222, 33775, 101795, 302927, 901721
- N=4: 1, 2, 4, 12, 36, 144, 532, 2133, 8300, 32965, 126963, 493064, 1871752, 7110566
- N=5: 1, 2, 4, 12, 36, 148, 570, 2558, 11097, 50606, 223481, 1016800
- N=6: 1, 2, 4, 12, 36, 148, 583, 2718, 12455, 61338
- N=7: 1, 2, 4, 12, 36, 148, 586, 2770, 13031, 66992
- N=8: 1, 2, 4, 12, 36, 148, 586, 2785, 13228, 69575

Clearly a trivial upper-bound for $g(n)$ is $\min(n!,(N+1)^n)$. 

{Problem}
    Same as above problem, but now one asks the question on finite sequences containing non-negative integers. 

Conjectured, the first few numbers for $g(n)$ should be
1, 2, 4, 12, 36, 148, 586

{Problem}
    Are there any sorting algorithms such that every element is compared $O(\log n)$ time, and does not depend on the AKS sorting network?

[Related link on TCS.SE](http://cstheory.stackexchange.com/questions/7131/sorting-algorithm-such-that-each-element-is-compared-o-log-n-times-and-does).

{Problem}
    [How hard is unshuffling a string?](http://cstheory.stackexchange.com/questions/34/how-hard-is-unshuffling-a-string)

{Problem}
    What's the length of the shortest supersequence of all permutations on $n$ alphabets? 
[It's featured on open problem garden](http://garden.irmacs.sfu.ca/?q=op/smallest_universal_supersequence).

{Problem}
    What's the length of the shortest superstring of all permutations on $n$ alphabets? 

[OEIS:A180632](http://oeis.org/A180632)

These are some problems released as [project for CSE 549 Fall 2010 in Stony Brook](http://www.cs.sunysb.edu/~skiena/549/projects.pdf) by [Steven Skiena](http://www.cs.sunysb.edu/~skiena/).

{Problem}(Shortest Subset Subsequence)
    As discussed in algorithm reading group, what is the string on an alphabet of size n such that every one of the $2^n$ subsets is represented by a substring of minimum size, ie. equal to the cardinality.


{Problem}(Path Consistant Walk in a String)
Can you find a low-period string defining a path between two vertices in a character-labeled path, i.e. a string? The problem is hard for trees and dags. 
A related problem: Given two strings $S_1$ and $S_2$, can you find a string $S$ such that both $S_1$ and $S_2$ can be realized by a back and forth walk on $S$?

{Problem}
    http://mathoverflow.net/questions/37782/how-much-must-deleting-a-spanning-tree-reduce-edge-connectivity

{Problem}(Finding the Most Frequent Subsequence)
    Given a string S, which string S' occurs most often as a scattered subsequence of S? Note that the number of candidates and possible occurrences are exponential. Can you prove this is NP-complete, and maybe give an approximation algorithm?
    Can you use dynamic programming to count the number of occurences of a candidate S'?


{Problem}
    Fix string $x$ with length $|x|$, let $f(y,x)$ be the number of different ways $y$ can be a subsequence of $x$. $m(x) = \max_{y} f(y,s)$, where $y$ range through all strings. Let $\phi(x)$ be the number of distinct subsequences of $x$. 

    What are the relations between these parameters? Trivially $m(x)\geq \frac{|x|}{\phi(x)}$.


#### A problem similar to union-find algorithm analysis

We have a colored graph $G(V,E,C)$ with $V=\{v_0,v_1,\ldots,v_n\}$, edges $E$ and a
coloring function $C$, color vertices red and blue. The graph will always be a
tree. There are 2 operations $R$ and $B$.

R: $R(G(V,E,C),v_i)=G'(V,E',C')$ and cost $k$

Where $C'(v)$ = $C(v)$ if $v_i\neq v$. $C'(v_i) = red$.

Let $S = \{(v_a,v_b)|a\leq i,i\leq b, (v_a,v_b)\in E\}$, and
\[E'' = E\backslash S \cup \{(v_i,v_b)|(v_a,v_b)\in S\}\]
Let $v_j$ be the closest red vertex to $v_i$ that is closer to $v_0$.
If $v_i,v_{a_1},\ldots,v_{a_k},v_j$ be the unique path from $v_i$ to $v_j$

\[ E' = E''\backslash\{(v_i,v_{a_1}),(v_{a_1},v_{a_2}),\ldots,(v_{a_k},v_j)\}\cup \{(v_i,v_j),(v_{a_1},v_j),(v_{a_2},v_j),\ldots,(v_{a_k},v_j)\}\]
which is the same as path compression in union-find.


B: $B(G(V,E,C),v_i) = G'(V,E,C')$ and cost $0$

$C'(v) = C(v)$ if $v_i\neq v$. $C'(v_i)=blue$.

Claim: If we apply operation $R$ and $B$ $m$ times to $G(V,E,C)$, where
$E=\{(v_i,v_0)|i>0\}$, $C(v) = red$ for all $v\in V$. Then the sum of cost is
$O(m\alpha(n))$.

### Algebra

{Problem}
    How fast can one solve the word problem on the following presentation $G = \langle x_1,x_2,\ldots,x_n \mid  x_iw = wx_i, R=1 \rangle$?
    Where $R$ is a permutation of the word $x_1\ldots x_nx_1^{-1}\ldots x_n^{-1}$, and $w\in G$.

{Problem}
    Characterize automatic group with star free language.

{Problem}
    If one defines that the automorphism 
    \[
    \sigma_i^*(t_j) =
    \begin{cases}
     t_j & \text{if } j\neq i, i+1 \\
     t_{i+1} & \text{if } j=i \\
     t_{i+1}t_it^{-1}_{i+1} & \text{if } j=i+1\\
    \end{cases}
    \]
    What can we say about the length of $(w^*)^n(t_i)$, where $w^* = \sigma_{a_1}\circ\ldots\circ\sigma_{a_n}$

{Problem}
    Can the word problem for $B_4$ be done in subquadratic time? $B_4$ is the braid group on 4 strands. 

### Discrete Geometry
{Problem}
    There are a set $S$ of points on the plane, such that the distance between any two is at least $d$. What is the smallest $d$, such that no matter how large $|S|$ is, one can always cover $S$ with $|S|$ disjoint unit disks? 

### Topology
{Problem}
    Let the set $C$ be a set of homotopy classes of simple closed curves on an orientable surface $S$ of genus $g$, such that if $a,b\in C$, then their minimal intersection number is at most 1. Let $X_g$ be the set of all the sets with the above property. Find bounds on $f(g) = \sup_{C\in X_g}|C|$.

[Collections of Simple Closed Curves Intersecting at Most Once](http://www.math.uchicago.edu/~may/VIGRE/VIGRE2007/REUPapers/FINALFULL/Constantin.pdf) The paper by Sarah Constantin proves an exponential upper bound and a quadratic lower bound. 

### Data Structure
We want to store a list such that the following operations can be done fast:

1. $O(\log d)$ worst time split(predicate) First position where the predicate is true, predicate follows monoid
2. $O(1)$ worst time concat 2 trees
3. $O(\log n)$ time find  $a_i+...+a_j$

### Others
http://cstheory.stackexchange.com/questions/16682/scheduling-with-rsvp-deadline-and-possible-cancelations

{Problem}
    Find the minimum length regular expression that describes a regular language.