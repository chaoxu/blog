---
title: Countably infinite groups such that every element has order 2 are isomorphic
tags: group theory, puzzle
---
I once saw the following puzzle: 

{Problem}
    Given a list of $2n-1$ non-negative integers. Every number except one appeared twice. The memory that contain the integers are read only. Can you use $O(1)$ additional space to find the integer that only appeared once?
 
The solution was the xor function.
 
If $a_j$ is the number that didn't appear twice,
\[
\bigoplus_{i=1}^{2n-1} a_i = a_j
\]

The reason was because xor have the following property.
$a \oplus b = b \oplus a$, $a \oplus a = 0$ and $0 \oplus a = a$ for all $a,b\geq 0$.
One can see $(\mathbb{N}_0, \oplus)$ is a abelian group. 

Is this the unique function to solve this problem?

In some way, yes. 
Here is a theorem.

{Theorem}
    The countably infinite group $G$ such that $g^2 = 1$ for all $g\in G$ is $(\mathbb{N}_0, \oplus)$ up to isomorphism.

{Proof}
    
    Note that abelian is unnecessary, since $g^2 = 1$ implies abelian. 
    
    The presentation of $G$ is $\langle S \mid W\cup R\rangle$, where $S=\{a_1,a_2,\ldots\}$ is the infinite generating set with no redundant generator(i.e. $a_i$ can't be generated from $S-\{a_i\}$). $W=\{w^2=1|w\in G\}$, $R$ is any other identity relation that can't be derived from $w^2=1$.
    The presentation of $(\mathbb{N}_0, \oplus)$ is $\langle S' \mid W'\rangle$, where $S' = \{2^n|n\in \mathbb{N}\}$ and $W' = \{n\oplus n=1| n\in \mathbb{N}_0\}$. 
    It's easy to see that the presentation of $(\mathbb{N}_0, \oplus)$ is structurally the same as $\langle S \mid W \rangle$.
    $w^2=1$ implies any identity relation in $R$ can't involve more than one $a_i$, and all $a_i^{-1} = a_i$. All the possible identities in $R$ are of the form $a_{i_1} a_{i_2} \ldots a_{i_n} = 1$ where $i_j\neq i_k$ when $j\neq k$. Such relation can't exist, as it implies $a_{i_1} = a_{i_2} \ldots a_{i_n}$. This shows $a_{i_1}$ can be generated from $a_{i_2},\ldots,a_{i_n}$, and $a_{i_1}$ is a redundant generator. A contradiction. $R$ must be empty.