---
title: Minimum of submodular function over family of subsets
tags: submodular
---

{Theorem}
	Let $f:2^V\to \R$ be a submodular function and $P:2^T\to 2^{2^V}$ is a function with the property that $P(A)\cup P(B)\subset P(A\cup B)$ and $P(A)\cap P(B)\subset P(A\cap B)$. $f_P:2^T\to \R$ defined as 
	\[
		f_P(X) = \min_{Y\in P(X)} f(Y)\\
	\]
	is submodular. 

{Proof}
	Let $X^* = \argmin_{Y\in P(X)} f(Y)$,
	note since $X^*\in P(X)$ and $Y^*\in P(Y)$, we have $X^*\cup Y^* \in P(X\cup Y)$ and $X^*\cap Y^* \in P(X\cap Y)$.
	\begin{align*}
	f_P(X) + f_P(Y) &= f(X^*) + f(Y^*)\\
	                &\geq f(X^* \cup Y^*) + f(X^*\cap Y^*)\\
	                &\geq f((X\cup Y)^*) + f((X\cap Y)^*)\\
	                &= f_P(X\cup Y) + f_P(X\cap Y)
	\end{align*}

This is quite useful, for starters, it proves that we can create a monotone submodular function from any submodular function.

{Theorem}
	
	Let $f:2^V\to \R$ be a submodular function, then $f_*,f^*:2^V\to \R$ defined as 
	\[
		f_*(X) = \min \{f(Y)|Y\subset X\}\\
		f^*(X) = \min \{f(Y)|X\subset Y\}
	\] 
	are monotone and submodular.

A practical application is to generalize the cut function. Consider for a directed graph graph $G=(V,E)$. We would define $\delta^+(A)$ to be the set of out going edges from $A$ to $V\setminus A$. $f=|\delta^+|$ is a submodular function. An alternate definition for $f$ is the minimum number of edges to be removed so there is no path from $A$ to $V\setminus A$.

A simple generalization is when we only care about $T\subset V$. We can define $f_T(A)$ to be the minimum number of edges to be removed so there is no path from $A$ to $T\setminus A$. Amazingly(or not not surprisingly, depending on your intuition), $f_T$ is also a submodular function by invoking the next theorem, which is a direct corollary of our first theorem.

{Theorem}
	Let $f:2^V\to \R$ be a submodular function, then $f_T:2^T\to \R$ defined as 
	\[
		f_T(X) = \min \{f(Y)|Y\subset X, T\setminus X\subset V\setminus Y\}\\
	\] 
	is submodular.