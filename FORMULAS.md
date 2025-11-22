# Formulas
- Elementary symmetric polynomial of degree $k$ in $m$ variables:  
  $e_k(x_1,\dots,x_m) = \displaystyle\sum_{1\le i_1<\cdots<i_k\le m} x_{i_1}\cdots x_{i_k}$.
- Lehmer code of a permutation $w=w_1\cdots w_n$:  
  $L(w)_i = \big|\{\,j>i : w_j < w_i\,\}\big|$, and $\ell(w)=\sum_i L(w)_i$ is the inversion number.
- Divided-difference operator on a polynomial $f$:  
  $\partial_i f = \dfrac{f - s_i f}{x_i - x_{i+1}}$, where $s_i$ swaps $x_i$ and $x_{i+1}$.
- Schubert polynomial for $w\in S_n$:  
  $\mathfrak S_w = \partial_{w^{-1}w_0}\!\left(x_1^{\,n-1} x_2^{\,n-2} \cdots x_{n-1}\right)$ with $w_0 = n\, (n-1)\,\dots 1$.
- When $w$ avoids $1432,1423,4132,3142$ and $L(w)_{i+1}-L(w)_i\le 1$, the bottom pipe dream columns do not interact. A column of height $h$ whose bottommost cross sits in row $r$ contributes the factor $e_h(x_1,\dots,x_r)$, so  
  $\mathfrak S_w = \prod_{\text{columns }c} e_{h_c}(x_1,\dots,x_{r_c})$.
- Diagonal separation property (Lemma 2.3): for columns $c<c'$ in the bottom pipe dream with top row $t_c$ and bottom row $b_{c'}$, the northeast diagonals satisfy $t_c + c > b_{c'} + c'$, ensuring columns slide independently.
- Rectangular obstruction (Section 3): a Lehmer code of the form $(\underbrace{0,\dots,0}_{A},\underbrace{N,\dots,N}_{B},\underbrace{0,\dots,0}_{\ast})$ cannot yield such a product of elementary symmetric polynomials.
