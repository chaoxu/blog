---
title: Faster Pseudo-polynomial Time Algorithm for Subset Sums
---

# Introduction

A [multiset](http://en.wikipedia.org/wiki/Multiset) $(X,\chi_X)$ is a set $X$ associated with a function $\chi_X$, where $\chi_X:X\to \N$, and $\chi_X(i)$ is the number of times $i$ appears in $(X,\chi_X)$. For simplicity, we will abuse the notation and use $X$ in place of $(X,\chi_X)$. For more definitions, read the Wikipedia page.

{Problem}(Multisubset Target Sum)
    
    Given a multiset $X$ of positive integers and a target sum $k$. Find if there is a multisubset of $Y\subset X$ such that $\sum_{y\in Y} = k$ in $O(n+f(k))$ time. 

We want $f(k)$ to be as small as possible. This was a paraphrase of the 7th problem on [UIUC theory qualify exam 2003 Spring](http://sarielhp.org/research/algorithms/quals/03/03_spring.pdf). After hours of literature search, I can't find anything better than $f(k)=k^2$, so I'm writing up a solution where $f(k) = (k\log k)^{\frac{3}{2}}$. Notice there is a related result. Pisinger has an algorithm that runs in $O(nc)$ time where the input multiset contain numbers of size at most $c$.[@Pisinger19991] This is strictly better when $k$ is much worse than $c$, but our algorithm would still be faster for the lot of other cases. 

Through out the article, we assume $k$ is fixed. 

{Definition}
    
    $X$ is a multiset/set, $S(X) = \{ \sum_{y\in Y} y| Y\subset X \} \cap \{1,\ldots,k\}$. $S(X)$ is defined to be a set.

Check if $k\in S(X)$ solves [Problem 1].

# Reduce the **Multisubset Target Sum** to **Subset Sums**

{Problem}(Subset Sums)
    
    Given a set $X\subset \{1,\ldots,k\}$, find $S(X)$.

There exist a $O(n+k\log k)$ time algorithm to reduce the **Multisubset Target Sum** problem to two calls of **Subset Sums** problem. The algorithm starts by reading the elements in $X$ and build a new multiset $Y$, it is defined so $\chi_Y(t) = \min(\chi_X(t),\lfloor k/t\rfloor)$. The new multiset has size $O(k\log k)$. Clearly $S(Y)=S(X)$. We will further decrease the size of $Y$ by applying the following theorem.

{Theorem}

    If $\chi_X(t)\geq 3$, and $X'$ is a multiset such that

    1. $\chi_{X'}(x) = \chi_{X}(x)$ for all $x\not \in \{t,2t\}$.
    2. $\chi_{X'}(t) = \chi_{X}(t)-2$.
    3. $\chi_{X'}(2t) = \chi_{X}(2t)+1$.

    then $S(X) = S(X')$.

{Proof}
    We can express $ut$ where $u\leq \chi_X(t)-2$ in $X'$ trivially as the sum of $u$ copies of $t$. We can express $\chi_X(t)t=(\chi_X(t)-2)t + (2t)$ and $(\chi_X(t)-1)t = (\chi_X(t)-3)t + (2t)$. Note both are possible because $\chi_X(t)-3\geq 0$. The other direction is similar.

Apply the the process of pick any element $t$ where $\chi_Y(t)\geq 3$, and create $Y'$. Notice the size of $Y$ is greater than the size of $Y'$, therefore the size of $|Y|$ is an upper bound on the number of time we repeat the process. Each operation can be done in $O(1)$ time, and we have a $O(k\log k)$ reduction to a multiset $X$(yes, I'm reusing variables) with $\chi_X \leq 2$.

It's easy to partition the resulting multiset $X$ into $A,B$ such that $\chi_A\leq \chi_B\leq 1$. In other words, $A$ and $B$ are actual sets. An operation $*$ that satisfies $S(A)*S(B) = S(A\cup B)$ reduces the computation to one application of $*$ and two subset sums computation. In fact, $*$ can be taken as the convolution over the characteristic function, an $O(k\log k)$ operation [^1]. 

[^1]: This folklore algorithm comes from solving Exercise 3 in [Jeff's notes on Fast Fourier Transforms](http://www.cs.uiuc.edu/~jeffe/teaching/algorithms/notes/02-fft.pdf).

# $O(k^2)$ dynamic programming algorithms for subset sum

## The naive dynamic programming algorithm

In most algorithm class, there are discussion of a [simple algorithm to solve subset sum](http://www.cs.cmu.edu/~ckingsf/class/02713-s13/lectures/lec15-subsetsum.pdf).

{Theorem}

    There exist an algorithm that finds $S(X)$ in $O(nk)$ time, where $n$ is the number of elements in $X$.

{Proof}
    
    See [wikipedia](http://en.wikipedia.org/wiki/Subset_sum_problem#Pseudo-polynomial_time_dynamic_programming_solution). 

When $|X|=\Omega(k)$, the naive dynamic programming algorithm is an $O(k^2)$ time algorithm. Notice this algorithm can be improved to account for the size of $S(X)$.

## An output sensitive algorithm for computing $S(X)$

Let's consider what is exactly happening at each step of the dynamic programming solution. Once we move on to the next element in the set, we can either add into existing set or don't do anything. $S(A\cup \{i\}) = S(A) \cup \{t+i |t\in S(A)\}$. If we represent $S(A)$ as a sorted linked list, finding $S(A\cup \{i\})$ can be done easily through merging two sorted lists(while removing duplicates) in linear time. Thus we can compute $S(A\cup \{i\})$ in time $O(|S(A)|)$.

Since $|S(A)| \leq |S(X)|$ for $A\subset X$, we can bound the number of operations to compute $S(X)$ by $|X||S(X)|$. 

Simple Haskell code that demonstrates this fact:

```haskell
import Data.List
import Data.List.Utils

allSubsetSum n = foldl' addNext [0]
  where addNext l x = takeWhile (<=n) $ (map head . group) $ merge l (map (+x) l)
```

{Theorem}
    There exist an algorithm that finds $S(X)$ in time $O(|X||S(X)|)$.

In the worst case, $S(X)$ is dense($\Omega(k)$). This is still a $O(k^2)$ time algorithm. We will have an additional term of $k$ if we want to turn $S(X)$ into an bit-array representation, which is required for FFT.

# The Algorithm that Exploits the Sparsity

Consider a sequence of numbers $1=a_0,a_1,\ldots,a_m=k$.

1. Partition $X$ into sets $X_1,\ldots,X_m$, where $X_i = X\cap \{a_{i-1},\ldots, a_i-1\}$.

2. Compute $S(X_i)$ for each $i$ in $O(\sum_{i=1}^{m} |X_i||S(X_i)| + k)$ time.

3. Combine all $S(X_i)$'s to form $S(X)$ in $O(m k\log k)$ time.

$|S(X_i)|$ will be bounded by some crude combinatorial estimates. If $t > \frac{k}{a_{i-1}}$, then no subset of size $t$ contained in $X_i$ contributes to the size of $|S(X_i)|$, because the sum would be at least $ta_{i-1}> \frac{k}{a_{i-1}} k = k$. 
The sums of a size $t$ subset lies in the interval $[a_{i-1}t, a_{i-1}t + (a_i-a_{i-1})t]$, thus sums of subsets of size $t$ contributes at most $(a_i-a_{i-1})t$ points to $S(X_i)$. Also, there is no subset with size greater than $a_i-a_{i-1}$. The punchline:

\[
|S(X_i)| \leq (a_i-a_{i-1})t^2
\]

where $t=\min(\frac{k}{a_{i-1}},a_i-a_{i-1})$. This gives us the choice to mix and match which one of the two we going to use.

## Uniformly distributed partitions

Assume $a_i$'s are evenly distributed, namely $a_i=i\frac{k}{m}$, and $|S(X_i)|\leq  \frac{k}{m} t^2\leq \left(\frac{k}{m} \right)^2 \frac{k}{i\frac{k}{m}}$.

\begin{align*}
m k\log k + (\sum_{i=1}^{m} |X_i||S(X_i)| + k) 
&= m k\log k + \sum_{i=1}^{m} \frac{k}{m} \left(\frac{k}{m}\right)^2\frac{k}{i\frac{k}{m}}\\
&= m k\log k + \sum_{i=1}^{m} \left (\frac{k}{m} \right)^2\frac{k}{i} \\
&= m k\log k + \frac{k^3}{m^2} \sum_{i=1}^{m} \frac{1}{i} \\
&= m k\log k + \frac{k^3}{m^2} \log m \\
\end{align*}

Solve $m k\log k = \frac{k^3}{m^2} \log m$ for $m$ and we get $m=k^\frac{2}{3}$, so we find a $O(k^\frac{5}{3} \log k)$ algorithm.

## Geometrically distributed partitions

Let's return to the analysis of the running time for the sparse subset sum tables.

\begin{align*}
\sum_{i=1}^{m} |X_i||S(X_i)|
&\leq \sum_{i=1}^{m} (a_i-a_{i-1}) (a_i-a_{i-1})(\frac{k}{a_{i-1}})^2 \\
&= k^2\sum_{i=1}^{m} (\frac{a_i}{a_{i-1}}-1)^2 \\
\end{align*}

This suggest to minimize the running time, we want $a_i =a_0 r^i$ for some constant $r$. Since $a_0=1,a_m=k$, $r=k^\frac{1}{m}$.

\[
m k\log k + k^2\sum_{i=0}^{m} (\frac{a_i}{a_{i-1}}-1)^2 
=m k\log k + m k^2 (k^\frac{1}{m}-1)^2 
\]

Solve for $m$ so both terms are equal

\begin{align*}
m k \log k&= m k^2 (k^\frac{1}{m}-1)^2\\
\sqrt{\frac{\log k}{k}} + 1&= k^\frac{1}{m}\\
\log (\sqrt{\frac{\log k}{k}} + 1)&= \frac{1}{m} \log k\\
\frac{\log k}{\log (\sqrt{\frac{\log k}{k}} + 1)}&= m\\
\Theta(\sqrt{k\log k}) &=m\\
\end{align*}
The last step comes from $\lim_{x\to 0} \frac{x}{\log(x+1)} = 1$.

This concludes the algorithm takes $O\left((k\log k)^{\frac{3}{2}}\right)$ time.

# Can we do better?

Note in our general framework, the size of the set $X$ was not considered. It make no sense to use this algorithm when $n=O(\sqrt{k})$, a $O(nk)$ algorithm is much better.

Take the last geometric distribution, but consider a tighter analysis. It's not hard to show we can improve the algorithm to $O(k^{3/2}\log k \sqrt{\log n})$. Are there any possibility to beat $O(nk)$ for $n=o(\sqrt{k})$? 

Not much is possible with this approach. Our approach saves time by having a bound on $\frac{k}{a_i}$ and $|X_i|$. Now, let's consider $n=\Theta(\sqrt{k})$ and every element in $X$ is smaller than $\sqrt{k}$ and distributed randomly. This bound is not useful, as almost every interval before $\sqrt{k}$ is going to be full, and no sum can jump outside the range we care about. Any partition will sum up to a $\Omega(k^\frac{3}{2})$ bound.

# References