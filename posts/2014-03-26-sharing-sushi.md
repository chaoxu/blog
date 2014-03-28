---
title: Sushi sharing problem
---

The problem was first brought up by [Sam McCauley](http://www.cs.sunysb.edu/~smccauley/) when I was still in Stony Brook.

Two friends are sharing sushi. There are two kind of sushi, $0$ and $1$. The sushi are lied out on a $n\times m$ matrix. There are $a$ sushi of type $0$ and $b$ sushi of type $1$, both are even numbers and $a+b = nm$. One can pick up a sushi with a chopstick by picking it up horizontally, or vertically. It will touch the adjacent sushis horizontally or vertically, respectively.

Each person want to eat exactly $a/2$ sushi of type $0$ and $b/2$ sushi of type $1$, and no one want to eat a sushi touched by someone else. Is this always possible? Yes it is!

First, it's easy to see each person can pick up a entire row or column, and collapse the remaining matrix. All the remaining sushi in the matrix are clean. We will assume each person pick up a entire row/column.

We can prove by induction by considering a few cases. Let $f(i)$ be the number of zeros in the $i$th column.

- $n>m$, rotate the matrix.

- $n< m+1$, by pigeonhole principle, there exist 2 columns with same number of $0$'s and $1$'s, each person can take 1 of those two columns.

- $n=m+1$ and $n\geq 3$, then if there is no $2$ columns with the same number of $0$'s and $1$'s, then there are columns where $f$ takes on values $0,1,n, n-1$. So one person pick columns $f^{-1}(\{0,n\})$ and the other person take columns $f^{-1}(\{1,n-1\})$.

- $n=m$ and $n\geq 4$. $n$ has to be even. If there is no $2$ columns with the same number of $0$'s and $1$'s, then $f([1..n])=\{0,\ldots,n\}\backslash \{k\}$. $k$ must be even. Let $f([1..n])=\{a_1,\ldots,a_n\}$, $a_i<a_{i+1}$. One can check a few cases to find that we can let one person take columns $f^{-1}(\{a_1,a_n\})$ and the other person take columns $f^{-1}(\{a_2,a_{n-1}\})$.

So the above show the base case are when $n=1,m=2$ and $n=m=2$. Just check they work by case work.