---
title: 数論的関数の用語と例
date: 2018-07-09 22:50:00
tags: math
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

数論的関数の用語やその関連について整理したかったので書くことにした.

## 数論的関数

数論的関数は, 定義域が正整数 $\mathbb{Z}^{+}$ である複素数を値にもつ関数である.
すなわち

<div class="m-def">
<header class="m-def-title"><p>数論的関数</p></header>
<div class="m-def-content">
  \\(\mathbb{Z}^{+}\\) から複素数 \\(\mathbb{C}\\) への関数
  \\(\mathbb{Z}^{+}\to\mathbb{C}\\)
  </div>
</div>

をいう.

### 加法的関数

加法的関数は以下で定義される.

<div class="m-def">
<header class="m-def-title"><p>加法的関数</p></header>
<div class="m-def-content">
  \\(m\in\mathbb{Z}^{+}, n\in\mathbb{Z}^{+}, \gcd(m,n)=1\\) について
  \\(f(mn)=f(m)+f(n)\\) を満たす数論的関数 \\(f(x)\\)
  </div>
</div>

<!--more-->

e.g.:

* $n$ の異なる素因数の総数 \\(:= \omega (n)\\). \\(\omega(4)=1, \omega(20)=\omega(2^2\cdot 5)=2, \omega(2018)=\omega(2\cdot 1009)=2\\)
* $n$ の異なる素因数の和 \\(:= \text{sopf}(n)\\). \\(\text{sopf}(1)=0,\text{sopf}(4)=2,\text{sopf}(20)=2+5=7,\text{sopf}(2018)=1011\\)

また, 完全加法的関数は以下で定義される.

<div class="m-def">
<header class="m-def-title"><p>完全加法的関数</p></header>
<div class="m-def-content">
  \\(^\forall m\in\mathbb{Z}^{+}, ^\forall n\in\mathbb{Z}^{+}\\) について
  \\(f(mn)=f(m)+f(n)\\) を満たす加法的関数 \\(f(x)\\)
  </div>
</div>

e.g.:

* $n$ の重複も含めた素因数の総数 \\(:= \Omega(n)\\). \\(\Omega(1)=0, \Omega(20)=\Omega(2\cdot 2\cdot 5)=3, \Omega(2018)=\Omega(2\cdot 1009)=2\\)
* $n$ の重複も含めた素因数の和 \\(:= \text{sopfr}(n)\\). \\(\text{sopfr}(4)=2+2=4,\text{sopfr}(20)=\text{sopfr}(2^2\cdot 5)=2+2+5=9,\\
\text{sopfr}(2018)=\text{sopfr}(2\cdot 1009)=2+1009=1011\\)

### 乗法的関数

乗法的関数は以下で定義される.

<div class="m-def">
<header class="m-def-title"><p>乗法的関数</p></header>
<div class="m-def-content">
  \\(m\in\mathbb{Z}^{+}, n\in\mathbb{Z}^{+}, \gcd(m,n)=1\\) について
  \\(f(mn)=f(m)f(n)\\) を満たす数論的関数 \\(f(x)\\)
  </div>
</div>

乗法的関数は, 任意の加法的関数 $f(n)$ を用いて簡単に構成することができる.
たとえば, 乗法的関数 $g(n)$ を指数法則より $g(n)=2^{f(n)}$ とおくことができる.
また, $2$ つの乗法的関数 $f(n)$ と $g(n)$ をつかって, $h(n)=f(n)g(n)$
という乗法的関数をおくことができる. より一般化すると,


<div class="m-prop">
<header class="m-prop-title"><p>命題 1</p></header>
<div class="m-prop-content">
\\(f(n)\\) が乗法的関数, 和 \\(\displaystyle\sum_{d\mid n}\\) で \\(d\\) が \\(n\\) のすべての約数にわたるとき, \\(\displaystyle g(n)=\sum_{d\mid n}f(d)\\) は乗法的関数である. 
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>命題 1</p></header>
<div class="m-proof-content">
\\(n=n_1n_2,\ \gcd(n_1,n_2)=1\\) とすると,
$n$ の約数 $d$ は \\(n_1\\) の約数 \\(d_1\\) と, \\(n_2\\) の約数 \\(d_2\\)
との積で尽くされる.
すなわち \\(\gcd(d_1, d_2)=1\\) だから
\\[
\begin{aligned}
g(n)&=\sum_{d\mid n}f(d)
\\&=\sum_{d_1\mid n_1,\ d_2\mid n_2}f(d_1,d_2)
\\&=\sum_{d_1\mid n_1,\ d_2\mid n_2}f(d_1)f(d_2)
\\&=\sum_{d_1\mid n_1}f(d_1)\sum_{d_2\mid n_2}f(d_2)
\\&=g(n_1)g(n_2)
\end{aligned}
\\]
</div>
</div>

e.g.:

* $f(n):=\gcd(n,k)$. $k=2$ としたとき, $f(15)=f(3)f(5)=1,\ f(24)=f(3)f(8)=2$
* 指数法則: $k\in\mathbb{Z}$ に対する $n^k$
* メビウス関数 \\(:= \mu(n)\\). \\(\mu(18)=\mu(2\cdot 3^2)=0,\ \mu(6)=\mu(2\cdot 3)=1, \mu(7)=-1\\).
* [オイラーのトーシェント関数](/roki.log/2018/07/13/elgamalEncryption/#totientf) \\(:= \phi(n)\\). \\(\phi(6)=\phi(3)\cdot\phi(2)=2,\ \phi(28)=\phi(4)\cdot\phi(7)=12\\).

また, 完全乗法的関数は以下で定義される[^1].

<div class="m-def">
<header class="m-def-title"><p>完全乗法的関数</p></header>
<div class="m-def-content">
  \\(^\forall m\in\mathbb{Z}^{+}, ^\forall n\in\mathbb{Z}^{+}\\) について
  \\(f(mn)=f(m)f(n)\\) を満たす乗法的関数 \\(f(x)\\)
  </div>
</div>

e.g.:

* ディリクレ級数 $a(n)$ におけるディリクレの L 関数: \\(\displaystyle L(s,a)=\sum_{n=1}^{\infty}\dfrac{a(n)}{n^s}=\prod_{p}(1-\dfrac{a(p)}{p^s})^{-1},\\). 自然数全体の総和が素数全体の積に等しい.

## 参考文献

* [Completely multiplicative function](https://en.wikipedia.org/w/index.php?title=Completely_multiplicative_function&oldid=825869650) 2018 年 7 月 9 日アクセス.

[^1]: 代数学的な定義でいえば, モノイド($\mathbb{Z}^{+},\cdot$)から他のモノイドまでの準同型写像である.
