---
title: オイラーの定理とカーマイケルの定理
date: 2018-07-25 00:32:00
tags: Elementary number theory, Group theory, math
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

以前の記事, [エルガマル暗号](/roki.log/2018/07/13/elgamalEncryption/)では,
エルガマル暗号に関する諸々の前提の説明と, その実装について示した. 
同エントリ内で, フェルマーの小定理[^1]については取り扱ったものの, 
その一般形であるオイラーの定理およびカーマイケルの定理について[特に触れなかった](/roki.log/2018/07/13/elgamalEncryption/#fn-5)ため,
本エントリでそれらに関してまとめる. しばしば値の確認には, 簡単のため Haskell を使う.

<!--more-->

## オイラーの定理

いま, [フェルマーテスト](/roki.log/2018/07/13/elgamalEncryption/#fermattest)を定義したとき, 
\\(FT_n(a)\\) をパスするには (すなわち, フェルマーの小定理が示す合同式が成り立つには), 要件として,
既約剰余類郡 \\(\mathbb{Z}^{\ast}_n\\) の各要素と
\\(^\exists a\ \in\mathbb{Z}\\) の積が全て異なり, 
$\bmod n$ の既約代表系のすべての積と合同でなければならない.
たとえば, 法 $n=8$ による合同関係で構成する剰余類の完全代表系[^2]は
\\(0,1,2,3,4,5,6,7\\) であるが,
\\(a=2\\) としてしまうと, 既約剰余類郡が構成できていないので, 次のようにしても完全代表系が得られない(積をわざわざ示していないが, 非合同でないことは, 各要素の積が全て異なっていない時点で明白である).

```Haskell
Prelude> [x * 2 `rem` 8 | x <- [0..7]]
[0,2,4,6,0,2,4,6]
```

そこで, 先に述べた剰余類の既約代表系を考える. これは $\phi(8)=4$ 個[^3]で, $1,3,5,7$ である.
これを同じように, \\(\left\{1\cdot a,\ 3\cdot a,\ 5\cdot a,\ 7\cdot a\right\}\\) とし, 
先の要件を確認すると, 
[この補題2](/roki.log/2018/07/13/elgamalEncryption/#lem2)より, 
$\bmod 8$ で全体として \\(\left\{1,3,5,7\right\}\\) と一致していて,
\\[1\cdot 3\cdot 5\cdot 7\equiv 1\cdot 3\cdot 5\cdot 7\cdot a^4\pmod{8}\tag{1}\\]
$\gcd(1\cdot 3\cdot 5\cdot 7,8)=1$ だから, $1\cdot 3\cdot 5\cdot 7$ を約して,
\\[a^4\equiv 1\pmod{8}\tag{2}\\] 
これは, $\gcd(a,n)=1$ ということの他に, $a$ および $n$ の値に依存した論ではない. すなわち,

<div class="m-thm">
<header class="m-thm-title"><p><span id="eulerstheorem">オイラーの定理</span></p></header>
<div class="m-thm-content">
\\[a^{\phi(n)}\equiv 1\pmod{n}\ (2\leq n\in\mathbb{Z}^{+},\ \gcd(a,n)=1)\\]
  </div>
</div>

がいえる[^4].

```Haskell
*Main> euler'sTheorem n = [a `modExp` (totient n) $ n | a <- [2..], gcd a n == 1]
*Main> all (==1) $ take 100 $ euler'sTheorem 8
True
```

$n$ が素数 $p$ であるとき, $\phi(p)=p-1$ で, フェルマーの小定理[^1]となる[^5].

## ラグランジュの定理

いま述べた[オイラーの定理](#eulerstheorem)は, 
ラグランジュの定理を使っても証明できる. ラグランジュの定理は,

<div class="m-def">
<header class="m-def-title"><p><span id="lagrangestheorem">ラグランジュの定理</span></p></header>
<div class="m-def-content">
  有限郡 \\(G\\) の部分郡 \\(H\\) の位数 \\(\mid H\mid\\) は, \\(G\\) の位数 \\(\mid G\mid\\) 
  の約数となる.
  \\[\mid G\mid\ =\ \mid G:H\mid \mid H\mid\\]
  </div>
</div>

である. 

<div class="m-proof">
<header class="m-proof-title"><p><span id="euclidean">ラグランジュの定理</span></p></header>
<div class="m-proof-content">
有限郡 \\(G\\) の部分郡 \\(H\\) による類別が \\(\displaystyle G=\bigcup_i^r a_iH\\) であるとき, \\(\mid G\mid=r\mid H\mid\\) といえる. この \\(r\\) は \\(r=\mid G:H\mid\\) 
そのものなので, \\(\mid G\mid\ =\ \mid G:H\mid\mid H\mid\\). 
</div>
</div>

ごく直感的な定理である. これを使えば, [オイラーの定理](#eulerstheorem)は次のように証明できる.

<div class="m-lem">
<header class="m-lem-title"><p>補題 1</p></header>
<div class="m-lem-content">
有限郡 $G$ とその元 $^\forall g\in G$ に対し, $g^{\mid G\mid}=e$. $e\in G$ は単位元. 
</div>
</div>


<div class="m-proof">
<header class="m-proof-title"><p><span id="euclidean">補題 1</span></p></header>
<div class="m-proof-content">

巡回部分郡 $H=\lt g\gt$ の元 $g$ の位数 $\mid H\mid$ は,
巡回して $g^i=e$ となる最小の $i\in\mathbb{N}$ であるといえる. すなわち
\\[g^{\mid H\mid}=e\\]
ここで, 商集合の位数を両辺に次のように与える.
\\[(g^{\mid H\mid})^{\mid G:H\mid}=(e)^{\mid G:H\mid}\\]
左辺は指数法則により, また右辺は単位元の繰り返しだから, これを次のようにかける.
\\[g^{\mid H\mid\mid G:H\mid}=e\\]
ラグランジュの定理より
\\[g^{\mid G\mid}=e\\]
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p><span id="euclidean">オイラーの定理</span></p></header>
<div class="m-proof-content">
[オイラーの定理](#eulerstheorem)を仮定したとき, 
脚注[^2]より剰余類 \\(\overline{a}\\) は法 \\(n\\) 
に関する既約剰余類郡 \\(\mathbb{Z}^{\ast}_{n}\\) に含まれる.  
\\(\mid\mathbb{Z}^{\ast}_{n}\mid=\phi(n)\\) だから補題 1 より 
\\(\overline{a}^{\phi(n)}=\overline{a^{\phi(n)}}=\overline{1}\\).
</div>
</div>

## カーマイケルの定理

[オイラーの定理](#eulerstheorem)で用いる $\phi$ 関数は, 
$a^{m}\equiv 1\pmod{n}\ (m\in{N}, \gcd(a,n)=1$ を成立させる最小の整数 $m$ を持ち得ない.
たとえば, $n=8$ では, 先の通り確かに $m=\phi(8)=4$ で合同式が満足できたが, 
$m=2$ としても, これを満足できる.

```Haskell
*Main> all (==1) $ take 100 $ [a `modExp` 2 $ 8 | a <- [2..], gcd a 8 == 1]
True
```

カーマイケルの \\(\lambda\\) 関数は, 与えられた整数 $n$ に対して同合同式を満足する最小の
$m$ を定義より自明に与える.

<div class="m-def">
<header class="m-def-title"><p>カーマイケルの \\(\lambda\\) 関数</p></header>
<div class="m-def-content">
  扱う文字を全て整数とし, \\(\lambda(n)\\) は
  \\[
  \begin{array}{lcl}
  \lambda(1)&:=&1\\
  \lambda(2)&:=&1\\
  \lambda(4)&:=&4\\
  \lambda(2^k)&:=&\phi(2^k)\ (0\leq k\leq 2)\\
  \lambda(2^k)&:=&2^{k-2}=\dfrac{\phi(2^k)}{2}\ (e\geq 3)\\
  \lambda(p^h)&:=&\phi(p^h)=(p-1)\cdot p^{h-1}\ (p\ is\ an\ odd\ prime, h\geq 1)\\
  \lambda(2^kp_1^{h_1}p_2^{h_2}p_3^{h_3}\cdots p_t^{h_t})&:=&\mathrm{lcm}(\lambda(2^k),\lambda(p_1^{h_1}),\lambda(p_2^{h_2}),\lambda(p_3^{h_3}),\cdots,\lambda(p_t^{h_t}))\ (p_n\ is\ an\ odd\ prime, k\geq 0, h_n\geq 1)
  \end{array}
  \\]
  と定義する.
  </div>
</div>

<div class="m-thm">
<header class="m-thm-title"><p><span id="carmichelstheorem">カーマイケルの定理</span></p></header>
<div class="m-thm-content">
  \\[a^{\lambda(n)}\equiv 1\pmod{n}\ (2\leq n\in\mathbb{Z}^{+},\ \gcd(a,n)=1)\\]
</div>
</div>

実装して確かめてみる.

```Haskell
*Main> let carmichael'sLambda n = head [k | k <- [1..], and [(m `modExp` k $ n) < 2 | m <- [1..n] gcd m n < 2]]
*Main> let carmichael'sTheorem n = [a `modExp` (carmichael'sLambda n) $ n | a <- [2..], gcd a n == 1]
*Main> all (==1) $ take 100 $ carmichael'sTheorem 8
True
```

[^1]: 証明: [フェルマーの小定理](/roki.log/2018/07/13/elgamalEncryption/#fermatstheorem)
[^2]: 補足. 郡 $G$ とその部分郡 $H$ があるとき, $H$ は郡であるから単位元 $e\in H$ を含む. よって, $^\exists a\in G$ の剰余類を $aH=\left\{ah\mid h\in H\right\}$ としたとき(簡単のため, 左剰余類として式をおいたが, これに深い意味はない.), $a=ae\in aH$ より $a\in aH$ である. この $a$ を剰余類 $aH$ の代表という. また郡 $G$ は, 異なる \\(a_i\\) を代表とした剰余類 \\(a_iH\\) によって類別できる(\\(\displaystyle G=\bigcup_i a_iH\\)). この \\(\mid G:H\mid\\) 個の類別に対して, 各剰余類から代表の元を取り, 構成した集合を, $G$ の $H$ に対する代表系という.
[^3]: ここで, $\phi$ は, [オイラーのトーシェント関数](/roki.log/2018/07/13/elgamalEncryption/#totientf).
[^4]: コード内の`totient`と`modExp`は, それぞれ以前の投稿のうち, [オイラーのトーシェント関数の実装部分](/roki.log/2018/07/13/elgamalEncryption/#totientf)と, [カーマイケル数を得るための実装](/roki.log/2018/07/13/elgamalEncryption/#modexpref)を利用.
[^5]: この補足は冗長的かもしれないが, $n$ が素数 $p$ である場合, \\(\mathbb{Z}_p^{\ast}\\) が構成されるから, これから取った代表は素数 $p$ と互いに素であることから, 既約代表である. この事実も, 一般に \\(\\) から \\(\\) へのような式変形が実行できることとの整合を示す.
