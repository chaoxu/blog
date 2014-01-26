---
title: Compute equivalent classes
tags: algorithm
---
I met the following practical problem.
$a, b \in X$ are in a equivalent class if $f(a,y) = f(b,y)$ for all $y \in Y$. Find the set of all equivalent classes. 

I coded a naive (and easy to code) solution so I could get things done. 
The naive solution:
Compute all $f(x,y)$ and store in a matrix. Then find rows in the matrix that has the same value. Suppose $|X|=|Y|=n$, then worst case is $O(n^3)$ and $O(n^2)$ space, where $m$ is the complexity of $f(x,y)$.

After that, I start to think of a better solution so I can practically use it.
$O(n^3)$ can be improved to $O(n^2)$. We can define equivalent class $E_k$, such that $a,b \in X$ are in the same equivalent class if $f(a,y) = f(b,y)$ for the $k$ smallest elements in $Y$.(given some strict order). One can see given $E_n$, one can compute $E_{n+1}$ in $O(n)$ time, given the matrix.

It's also easy to see, there is never the need to store the entire matrix. We can reduce the space to $O(n)$ by computing part of the matrix on the fly. 

$O(n^2)$ time and $O(n)$ space.

Lastly, we only need to compute $E_{n+1}$ for equivalent classes with more than one element. Another practical advantage if $f$ is a computationally intensive process.
