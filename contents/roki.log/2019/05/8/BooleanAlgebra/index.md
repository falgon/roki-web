---
title: ブール代数
date: 2019-05-29 00:00:00
tags: math
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

ブール代数は古典論理における命題論理と密接に関連している.
結論からいえば, 両者の違いは歴史的な背景ぐらいであり, 殆どの場合は同等の理論であるということができる[^1].
ブール代数はその応用として論理回路の構築に直接役立つことから, 計算機科学, とくにハードウェアの分野において重宝される代数系の 1 つである.


## ブール代数の公理とその定理

次に示すのはブール代数の公理系である. 公理系に関する詳細は証明理論 (TODO) の冒頭を参照のこと.

<div class="m-def">
<header class="m-def-title"><p><a class="disabled" id="boolean_algebra">ブール代数</a></p></header>
<div class="m-def-content">
半順序集合 \\((B,\lor,\land,',0,1)\\) 
が可補分配[束](/roki.log/2019/03/15/Relation/#lattice_def)ならば, 
\\((B,\lor,\land,',0,1)\\) はブール代数である.
すなわち, \\(x,y,z\in B\\) に対して, 次のすべての公理を満たした \\((B,\lor,\land,',0,1)\\) はブール代数である.

1. \\(\htmlId{boolean_algebra1}{\rm 可換律}\\): \\(x\land y=y\land x,x\lor y=y\lor x\\)
2. \\(\htmlId{boolean_algebra2}{\rm 分配律}\\): 
\\((x\lor y)\land z=(x\land z)\lor(y\land z),(x\land y)\lor z=(x\lor z)\land(y\lor z)\\)
3. \\(\htmlId{boolean_algebra3}{\rm 同一律}\\): \\(^\forall x\in L\\) に対して \\(x\land 1=x,x\lor 0=x\\). 
ここで \\(1\\) は最大限, 単位元である. \\(0\\) は最小限, 零元である.
4. \\(\htmlId{boolean_algebra4}{\rm 補元律}\\): 
\\(^\exists x'\in L\ {\rm s.t.}\ ^\forall x\in L, x\lor x'=1, x\land x'=0\\)

</div>
</div>


また, この \\(1,0\\) からのみ成る集合をブール領域, ブール代数の下に書かれた式をブール式,
\\(n\in\mathbb{N}\\) 個のブール領域の引数をとり, 1 個のブール領域の値となる関数 \\(f:B^n\to B\\) をブール関数という.
例えば, 2 変数ブール関数 \\(f(x_1,x_2)\\) では \\(x_1,x_2\\) がそれぞれ \\(1,0\\) のいずれかとなるので, 
\\(2^4=16\\) 通りの 2 変数ブール式が存在することとなる.
以下, 演算の優先順序は左結合性で \\(',\land,\lor\\) の順とする. ただし, 括弧内の演算はより優先される.

さてブール代数の公理における乗法 \\(\land\\) と加法 \\(\lor \\), および \\(1, 0\\) をそれぞれ入れ替えると, 
再びブール代数の公理である. 
これは[双対](#dual_def)の原理という公理である.

<div class="m-def">
<header class="m-def-title"><p><span id="dual_def">双対の原理</span></p></header>
<div class="m-def-content">
ブール代数で成立する文/式は, そこに現れるすべての \\(\lor ,\land,0,1\\) をそれぞれ \\(\land,\lor ,1,0\\) 
で置き換えても成立する.
</div>
</div>

これらの公理から[補元の一意性](#complement_unique), [べき等律](#idempotence), 
[有界律](#bounded), [吸収律](#absorption), [結合律](#associative), [対合律](#involution), 
[ド・モルガンの法則](#de_morgan), [シャノンの展開定理](#chanon_theorem)が導出可能である. 
以下 \\(x,y,z\in B\\) とする.

<!--more-->

<div class="m-thm">
<header class="m-thm-title"><p>補元の一意性</p></header>
<div class="m-thm-content">
\\(x\\) に対する補元は一意である.
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>補元の一意性</p></header>
<div class="m-proof-content">
2 つの \\(x\\) の補元 \\(x'_1,x'_2\\) を仮定する.
\\[\begin{aligned}
x'_1&=&x'_1\land1&(\because {\rm \href{#boolean_algebra3}{公理 3}:同一律})\\
&=&x'_1\land(x\lor x'_2)&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&(x'_1\land x)\lor (x'_1\land x'_2)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&0\lor (x'_1\land x'_2)&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&(x\land x'_2)\lor (x'_1\land x'_2)&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&x'_2\land(x\lor x'_1)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&x'_2\land1&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&x'_2&(\because {\rm \href{#boolean_algebra3}{公理 3}:同一律})
\end{aligned}
\\]
より \\(x\\) の補元が一意であることは明らか.
</div>
</div>

<div class="m-thm">
<header class="m-thm-title"><p><a id="idempotence" class="disabled">べき等律</a></p></header>
<div class="m-thm-content">
以下が成り立つ.

1. \\(x\land x\Leftrightarrow x\\)
2. \\(x\lor x\Leftrightarrow x\\)
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>べき等律</p></header>
<div class="m-proof-content">
[束の定理](/roki.log/2019/03/15/Relation/#proof1)による.
</div>
</div>

<div class="m-thm">
<header class="m-thm-title"><p><a id="bounded" class="disabled">有界律</a></p></header>
<div class="m-thm-content">
以下が成り立つ.

1. \\(x\lor 1=1\\)
2. \\(x\land 0=0\\)
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>有界律</p></header>
<div class="m-proof-content">
\\[\begin{aligned}
x\lor 1&=&(x\lor 1)\land1&(\because {\rm \href{#boolean_algebra3}{公理 3}:同一律})\\
&=&(x\lor 1)(x\lor x')&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&x\lor (1\land x')&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&x\lor x'&(\because {\rm \href{#boolean_algebra1}{公理1}, \href{#boolean_algebra3}{公理3}:可換律, 同一律})\\
&=&1\\
x\land0&=&x\land x\land x'&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&x\land x'&(\because {\rm 定理:\href{#idempotence}{べき等律}})\\
&=&0&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})
\end{aligned}
\\]
</div>
</div>

<div class="m-thm">
<header class="m-thm-title"><p><a id="absorption" class="disabled">吸収律</a></p></header>
<div class="m-thm-content">
以下が成り立つ.

\\[x\lor x\land y=x\land(x\lor y)=x\\]
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>吸収律</p></header>
<div class="m-proof-content">
\\[\begin{aligned}
x\lor x\land y&=&(x\land1)\lor (x\land y)&(\because {\rm \href{#boolean_algebra3}{公理3}: 同一律})\\
&=&x\land(1\lor y)&(\because {\rm \href{#boolean_algebra2}{公理2}:分配律})\\
&=&x\land1&(\because {\rm \href{#boolean_algebra1}{公理 1}: 可換律, 定理: \href{#bounded}{有界律}} )\\
&=&x&(\because {\rm\href{#boolean_algebra3}{公理 3}: 同一律})\\
x\land(x\lor y)&=&(x\lor 0)(x\lor y)&(\because {\rm \href{#boolean_algebra3}{公理3}: 同一律})\\
&=&(0\land y)\lor x&(\because {\rm \href{#boolean_algebra2}{公理2}: 分配律})\\
&=&x\lor 0&(\because \rm{\href{#boolean_algebra1}{公理1}: 可換律,定理: \href{#bounded}{有界律}})\\
&=&x&(\because \rm{\href{#boolean_algebra3}{公理3}: 同一律})
\end{aligned}\\]
</div>
</div>

<div class="m-thm">
<header class="m-thm-title"><p><a id="associative" class="disabled">結合律</a></p></header>
<div class="m-thm-content">
以下が成り立つ.

\\[x\lor(y\lor z)=(x\lor y)\lor z\\]
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>結合律</p></header>
<div class="m-proof-content">
\\(A=x\lor (y\lor z), B=(x\lor y)\lor z\\) とおく. このとき,

\\[\begin{aligned}
x\land A&=&x\land(x\lor (y\lor z))\\
&=&x&(\because {\rm 定理: \href{#absorption}{吸収律}})\\
x\land B&=&x\land((x\lor y)\lor z)\\
&=&x\land(x\lor y)\lor x\land z&(\because {\rm \href{#boolean_algebra2}{公理2}: 分配律})\\
&=&x\lor x\land z&(\because {\rm 定理: \href{#absorption}{吸収律}})\\
&=&x&(\because {\rm 定理: \href{#absorption}{吸収律}})
\end{aligned}\\]

ゆえに \\[x\land A=x\land B=x\tag{L1}\\]
また,

\\[\begin{aligned}
x'\land A&=&x'\land(x\lor (y\lor z))\\
&=&x'\land x\lor x'\land(y\lor z)&(\because  {\rm \href{#boolean_algebra2}{公理2}: 分配律})\\
&=&x'\land(y\lor z)\lor 0&(\because {\rm \href{#boolean_algebra1}{公理1}, \href{#boolean_algebra4}{公理 4}: 可換律, 補元律})\\
&=&x'\land(y\lor z)&(\because {\rm \href{#boolean_algebra3}{公理3}: 同一律})\\
x'\land B&=&x'\land((x\lor y)\lor z)\\
&=&x'\land(x\lor y)\lor x'\land z&(\because {\rm \href{#boolean_algebra2}{公理2}: 分配律})\\
&=&(x'\land x\lor x'\land y)\lor x'\land z&(\because {\rm \href{#boolean_algebra2}{公理2}: 分配律})\\
&=&(0\lor x'\land y)\lor x'\land z&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&x'\land y\lor x'\land z&(\because {\rm \href{#boolean_algebra3}{公理3}: 同一律})\\
&=&x'\land(y\lor z)&(\because {\rm \href{#boolean_algebra2}{公理2}: 分配律})
\end{aligned}\\]

ゆえに \\[x'\land A=x'\land B=x'\land(y\lor z)\tag{L2}\\]
従って,

\\[\begin{aligned}
A&=&A\land1&(\because {\rm \href{#boolean_algebra3}{公理3}: 同一律})\\
&=&A\land(x\lor x')&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&A\land x\lor A\land x'&(\because {\rm \href{#boolean_algebra2}{公理2}: 分配律})\\
&=&x\land A\lor x'\land A&(\because {\rm \href{#boolean_algebra1}{公理1}: 可換律})\\
&=&x\land B\lor x'\land B&(\because,\ {\rm より})\\
&=&B(x\lor x')&(\because {\rm \href{#boolean_algebra2}{公理2}: 分配律})\\
&=&B\land1&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&B&(\because {\rm \href{#boolean_algebra3}{公理3}: 同一律})
\end{aligned}\\]

また, 双対の原理より \\(x(y\land z)=(x\land y)\land z\\).
</div>
</div>

<div class="m-thm">
<header class="m-thm-title"><p><a id="involution" class="disabled">対合律</a></p></header>
<div class="m-thm-content">
以下が成り立つ.

\\[(x')'=x\\]
</div>
</div>


<div class="m-proof">
<header class="m-proof-title"><p>対合律</p></header>
<div class="m-proof-content">

\\[\begin{aligned}
(x')'&=&(x')'\lor 0&(\because \rm{\href{#boolean_algebra3}{公理3}: 同一律})\\
&=&(x')'\lor x\land x'&(\because {\href{#boolean_algebra4}{公理 4}:補元律})\\
&=&((x')'\lor x)((x')'\lor x')&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&(x\lor (x')')\land1&(\because {\rm \href{#boolean_algebra1}{公理1}, \href{#boolean_algebra4}{公理 4}:可換律, 補元律})\\
&=&(x\lor (x')')(x\lor x')&(\because {\rm \href{#boolean_algebra4}{公理 4}: 補元律})\\
&=&x\lor ((x')'\land x')&(\because {\rm \href{#boolean_algebra2}{公理2}:分配律})\\
&=&x\lor 0&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&x&(\because {\rm \href{#boolean_algebra3}{公理3}: 同一律})
\end{aligned}\\]

</div>
</div>

<div class="m-thm">
<header class="m-thm-title"><p><a id="de_morgan" class="disabled">ド・モルガンの法則</a></p></header>
<div class="m-thm-content">
以下が成り立つ.
\\[(x\lor y)'=x'\land y'\\]
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>ド・モルガンの法則</p></header>
<div class="m-proof-content">
\\(x'\land y'\\) が \\((x\lor y)\\) の補元でなければならない.
すなわち, [公理4](#boolean_algebra4): 補元律より \\((x\lor y)\lor (x'\land y')=1\\) および 
\\((x\lor y)\land(x'\land y')=0\\) 
が同時に成り立つことを示せばよい.

\\[\begin{aligned}
(x\lor y)\lor (x'\land y')&=&((x\lor y)\lor x')((x\lor y)\lor y')&(\because {\rm \href{#boolean_algebra2}{公理2}:分配律})\\
&=&(y\lor (x\lor x'))(x\lor (y\lor y'))&(\because {\rm \href{#boolean_algebra1}{公理1}: 可換律, 定理: \href{#associative}{結合律}})\\
&=&(y\lor 1)(x\lor 1)&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&1\land1&(\because {\rm 定理: \href{#bounded}{有界律}} )\\
&=&1&(\because {\rm 定理:\href{#idempotence}{べき等律}})\\
(x\lor y)\land(x'\land y')&=&(x'\land y')\land(x\lor y)&(\because {\rm \href{#boolean_algebra1}{公理1}: 可換律})\\
&=&((x'\land y')\land x)\lor ((x'\land y')\land y)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&(y'\land(x\land x'))\lor (x'\land(y\land y'))&(\because {\rm \href{#boolean_algebra1}{公理1}: 可換律, 定理: \href{#associative}{結合律}})\\
&=&(y'\lor 0)\lor (x'\land1)&(\because {\rm \href{#boolean_algebra4}{公理 4}: 補元律})\\
&=&0\lor 0&(\because {\rm 定理: \href{#bounded}{有界律}} )\\
&=&0&(\because {\rm 定理:\href{#idempotence}{べき等律}})
\end{aligned}\\]

また, [双対の原理](#dual_def)より \\((x\land y)'=x'\lor y'\\).
</div>
</div>

<div class="m-thm">
<header class="m-thm-title"><p><a id="chanon_theorem" class="disabled">シャノン展開</a></p></header>
<div class="m-thm-content">

任意の \\(n\\) 変数ブール関数 \\(f(x_1,x_2,\cdots,x_n)\\) を \\(x_1\\) について, 次のように展開できる.

\\[\begin{aligned}
f(x_1,x_2,\cdots,x_n)&=&(x'_1\lor x_1)\land f(x_1,x_2,\cdots,x_n)&(\because {\rm \href{#boolean_algebra4}{公理 4}, \href{#boolean_algebra3}{公理 3} :補元律, 同一律})\\
&=&x'_1\land f(x_1,x_2,\cdots,x_n)\lor x_1\land f(x_1,x_2,\cdots,x_n)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&x'_1\land f(0,x_2,\cdots,x_n)\lor x_1\land f(1,x_2,\cdots,x_n)&(\because \href{#chanon_theorem_proof}{以下に証明})
\end{aligned}\\]
</div>
</div>


<div class="m-proof">
<header class="m-proof-title"><p><a id="chanon_theorem" class="disabled">シャノン展開</a></p></header>
<div class="m-proof-content">
\\(x_1=0\\) のとき, 
\\[f(0,x_2,\cdots,x_n)=1\land f(0,x_2,\cdots,x_n)\lor 0\land f(1,x_2,\cdots,x_n)=f(0,x_2,\cdots,x_n)\\]
\\(x_1=1\\) のとき, 
\\[f(1,x_2,\cdots,x_n)=0\land f(0,x_2,\cdots,x_n)\lor 1\land f(1,x_2,\cdots,x_n)=f(1,x_2,\cdots,x_n)\\]
</div>
</div>

例として, \\(f(x_1,x_2,x_3)=x_1\land x_2\lor x_2\land x_3\lor x_1\land x_3\\) を \\(x_1\\) について展開すると,

\\[\begin{aligned}
f(x_1,x_2,x_3)&=&x_1\land x_2\lor x_2\land x_3\lor x_1\land x_3\\
&=&(x'_1\lor x_1)\land f(x_1,x_2,x_3)&(\because {\rm \href{#boolean_algebra4}{公理 4}, \href{#boolean_algebra3}{公理 3} :補元律, 同一律})\\
&=&x'_1\land f(x_1,x_2,x_3)\lor x_1\land f(x_1,x_2,x_3)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&x'_1\land f(0,x_2,x_3)\lor x_1\land f(1,x_2,x_3)&(\because {\rm 定理: \href{#chanon_theorem}{シャノンの展開定理}})\\
&=&x'_1\land x_2\land x_3\lor x_1\land x_2\lor x_2\land x_3\lor x_3&(\because {\rm \href{#boolean_algebra3}{公理 3}:同一律})\\
&=&x'_1\land x_2\land x_3\lor x_1\land x_2\lor x_3&(\because {\rm \href{#boolean_algebra1}{公理1}: 可換律, 定理: \href{#absorption}{吸収律}})\\
\end{aligned}\\]

となる. また[双対の原理](dual_def)より, 

\\[\begin{aligned}
f(x_1,x_2,\cdots,x_n)&=&(x_1\land x'_1)\lor f(x_1,x_2,\cdots,x_n)&(\because {\rm \href{#boolean_algebra4}{公理 4}, \href{#boolean_algebra3}{公理 3} :補元律, 同一律})\\
&=&(x_1\lor f(x_1,x_2,\cdots,x_n))\land(x'_1\lor f(x_1,x_2,\cdots,x_n))&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&(x_1\lor f(0,x_2,\cdots,x_n))\land(x'_1\lor f(1,x_2,\cdots,x_n))&(\because {\rm \href{#chanon_theorem_proof}{上記証明}の\href{#dual_def}{双対}})
\end{aligned}\\]

この展開をシャノン双対展開という.

## 標準形

異なる表現のなされたブール式が同値であるかを即座に断定することは, 一般的に困難であることが多い[^2].
ここで, ブール式を一意に表す方法が決まっていれば, 即座に同値であるか判断がしやすく, 便利である. 
ブール代数では主に 2 つの形式が決められており, その形式への変形を標準化, また正規化という.
以下, \\(n\\) 変数ブール関数 \\(f(x_1,x_2,\cdots,x_n)\\) において, \\(x_1,x_2,\cdots,x_n\\) を入力変数, 
また入力変数およびその否定をリテラルという.
さらに \\(n\\) 個の入力変数に対し, \\(k\\) 番目のリテラル \\(x_k^{e_k}\ (1\leq k\leq n)\\) を次のように表す.

\\[\begin{aligned}
x^{e_k}_k=\begin{cases}
x_k&(=x^1_k)&e_k=1{\rm\ のとき}\\
x'_k&(=x^0_k)&e_k=0{\rm\ のとき}
\end{cases}
\end{aligned}\\]

### 加法標準形, 主加法標準形

\\(f(x_1,x_2,\cdots,x_n)\\) を \\(x_1,x_2\\) について[シャノン展開](#chanon_theorem)すると,

\\[\begin{aligned}
f(x_1,x_2,\cdots,x_n)&=&x'_1\land x'_2\land f(0,0,\cdots,x_n)\\
&&\lor x'_1\land x_2\land f(0,1,\cdots,x_n)\\
&&\lor x_1\land x'_2\land f(1,0,\cdots,x_n)\\
&&\lor x_1\land x_2\land f(1,1,\cdots,x_n)
\end{aligned}\\]

となる. 従って, 全入力変数 \\(x_1,x_2,\cdots,x_n\\) について[シャノン展開](#chanon_theorem)すると,

\\[\begin{aligned}
f(x_1,x_2,\cdots,x_n)&=&x'_1\land x'_2\land\cdots\land x'_n\land f(0,0,\cdots,0)\\
&&\lor x'_1\land x'_2\land\cdots\land x_n\land f(0,0,\cdots, 1)\\
&&\lor\cdots\\ 
&&\lor x_1\land x_2\land\cdots\land x'_n\land f(1,1,\cdots,0)\\
&&\lor x_1\land x_2\land\cdots\land x_n\land f(1,1,\cdots,1)
\end{aligned}\\]

となる ([シャノンの定理](#chanon_theorem)より数学的帰納法により証明できるが, 省略). 
これを以下のように定義する.

<div class="m-def">
<header class="m-def-title"><p><a id="minimum_term_expansion" class="disabled">最小項展開</a></p></header>
<div class="m-def-content">
\\(n\\) 変数ブール関数 \\(f(x_1,x_2,\cdots,x_n)\\) のすべての入力変数 \\(x_1,x_2,\cdots,x_n\\) について, 
[シャノン展開](#chanon_theorem)した形式
\\[f(x_1,x_2,\cdots,x_n)=
\bigvee_{(e_1,e_2,\cdots,e_n)\in B^n}f(e_1,e_2,\cdots,e_n)\land\bigwedge_{i=1}^{n}x_i^{e_i}\tag{1}\\]
は \\(f(x_1,x_2,\cdots,x_n)\\) の最小項展開である. 
なお, このときの項数は \\(2^n\\) となる.
</div>
</div>

これを踏まえ, 加法標準形および主加法標準形を導入する.

<div class="m-def">
<header class="m-def-title"><p><a id="DNF_PDNF" class="disabled">加法標準形, 主加法標準形</a></p></header>
<div class="m-def-content">
* 加法標準形 (disjunctive normal form: DNF) は, ブール式のリテラル, または 2 つ以上のリテラルの積の和のことをいう.
ここで, 2 つ以上のリテラルの積で同じ入力変数を 2 度以上含まない論理式を基本積, また標準項という. 
基本積のうち, すべての入力変数を含むブール式を最小項という. 
すなわち, 上記[最小項展開](#minimum_term_expansion)の \\(\displaystyle\bigwedge_{i=1}^nx^{e_i}_i\\) は最小項である.
* 主加法標準形 (principal disjunctive normal form: PDNF) は, 
ブール関数を[最小項展開](#minimum_term_expansion)した形式, すなわち[最小項展開](#minimum_term_expansion)の形式である.
</div>
</div>

例えば, 否定論理積 \\(\mid\\) を PDNF で表すとしよう.
否定論理和は 2 項演算子なので, その PDNF は 2 変数ブール関数を最小項展開した形となる.

\\[f(x_1,x_2)=f(0,0)\land x'_1\land x'_2\lor f(0,1)x'_1\land x_2\lor f(1,0)\land x_1\land x'_2\lor f(1,1)\land x_1\land x_2\\]

便宜上, 2 項演算子 \\(\mid\\) を最小項展開した形を \\(f_\mid(x_1,x_2)\\) で表すこととする.
あとは[真理値表 2](#truthtable2) の \\(\mid\\) の列のとおりに \\(f_\mid(x_1,x_2)\\) の値を決めてやればよいので

\\[
\begin{aligned}
f_\mid(x_1,x_2)&=&1\land x'_1\land x'_2\lor 1\land x'_1\land x_2\lor 1\land x_1\land x'_2\lor 0\land x_1\land x_2\\
&=&x'_1\land x'_2\lor x'_1\land x_2\lor x_1\land x'_2
\end{aligned}
\\]

従って, 否定論理積の PDNF は \\(x'_1\land x'_2\lor x'_1\land x_2\lor x_1\land x'_2\\) となる.
この操作を振り返ると, 真理値表から PDNF を書くためには, 結果が \\(1\\) となっている入力変数の全パターンに対して, 
元の入力変数の値が \\(1\\) ならそのまま, \\(0\\) ならその補元をとり,
それらすべての和を取ればよいことがわかる. 
何故ならば, 結果が \\(0\\) となる部分は, 
[シャノンの展開定理の証明](#chanon_theorem_proof)でも示したように消えてしまうからである.
同じようにして, 否定論理和, 排他的論理和も[真理値表 2](#truthtable2) の \\(\downarrow,\oplus\\) の列をみると, 
\\(1\\) となる入力のパターンから

\\[
\begin{aligned}
f_{\downarrow}(x_1,x_2)&=&x'_1\land x'_2\\
f_{\oplus}(x_1,x_2)&=&x_1\land x'_2\lor x'_1\land x_2\\
\end{aligned} 
\\]

となる.
すなわち, 真理値表で表現できるブール式は PDNF で表せるということである[^5].

次に, 任意の論理式から PDNF に変換することを考える.
結論からいうと, 次の手順に従えば PDNF へ機械的に変換できることが知られている.

1. ブール式全体を基本項による積の和の形にする (分配律等を利用)
2. 最小項でない基本項に対し, その基本項に含まれないすべてのリテラル \\(x_i\\) について \\((x_i\land x'_i)\\) を乗ずる
3. 分配律等に従い展開して, 冗長な項を除去する

以下, PDNF で表されたブール式を \\(f(x_1,x_2,\cdots,x_n)_{\rm P D N F}\\) と書くこととする.
例えば, ブール式 \\(f(x_1,x_2)=x_1\land x_2\land x_1\lor x_2\\) を PDNF で表すとすると,

\\[
\begin{aligned}
f(x_1,x_2)&=&x_1\land x_2\land x_1\lor x_2\\
&=&x_1\land x_1\land x_2\lor x_2&(\because {\rm\href{#boolean_algebra1}{公理1}: 可換律})\\
&=&x_1\land x_2\lor x_2&(\because {\rm 定理:\href{#idempotence}{べき等律}})\\
&=&x_1\land x_2\lor 1\land x_2&(\because {\rm \href{#boolean_algebra3}{公理 3}:同一律})\\
&=&x_1\land x_2\lor (x_1\lor x_1')\land x_2&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&x_1\land x_2\lor x_1\land x_2\lor x_1'\land x_2&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\ 
&=&x_1\land x_2\lor x_1'\land x_2&(\because {\rm 定理:\href{#idempotence}{べき等律}})
\end{aligned}
\\]

従って \\[f(x_1,x_2)_{\rm P D N F}=x_1\land x_2\lor x_1'\land x_2\\] となる.
例としてもう 1 つ, \\(f(x_1,x_2,x_3)=x_1\land x_2'\land x_3\lor x_1\land x'_3\lor x_2\land x'_3\\) としたときの PDNF は

\\[
\begin{aligned}
f(x_1,x_2,x_3)&=&x_1\land x'_2\land x_3\lor x\land x'_3\lor x_2\land x'_3\\
&=&x_1\land x'_2\land x_3\lor x_1\land(x_2\lor x'_2)\land x'_3\lor (x_1\lor x_1')\land x_2\land x'_3&(\because {\rm \href{#boolean_algebra3}{公理 3}, \href{#boolean_algebra4}{公理 4}: 同一律, 補元律} )\\
&=&x_1\land x'_2\land x_3\lor x_1\land x_2\land x'_3\lor x_1\land x'_2\land x'_3\lor x_1\land x_2\land x'_3\lor x'_1\land x_2\land x'_3&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&x_1\land x'_2\land x_3\lor x_1\land x_2\land x'_3\lor x_1\land x'_2\land x'_3\lor x'_1\land x_2\land x'_3&(\because {\rm 定理: \href{#idempotence}{べき等律}})
\end{aligned}
\\]

従って \\[f(x_1,x_2,x_3)_{\rm P D N F}=x_1\land x'_2\land x_3\lor x_1\land x_2\land x'_3\lor x_1\land x'_2\land x'_3\lor x'_1\land x_2\land x'_3\\] となる.

### 乗法標準形, 主乗法標準形

\\(f(x_1,x_2,\cdots,x_n)\\) を \\(x_1,x_2\\) について[シャノン双対展開](#chanon_theorem)すると,

\\[
\begin{aligned}
f(x_1,x_2,\cdots,x_n)&=&(x_1\lor x_2\lor f(0,0,\cdots,x_n))\\
&&\land (x_1\lor x'_2\lor f(0,1,\cdots,x_n))\\
&&\land (x'_1\lor x_2\lor f(1,0,\cdots,x_n))\\
&&\land (x'_1\lor x'_2\lor f(1,1,\cdots,x_n))
\end{aligned}
\\]

となる. 従って, 全入力変数 \\(x_1,x_2,\cdots,x_n\\) について[シャノン双対展開](#chanon_theorem)すると,

\\[
\begin{aligned}
f(x_1,x_2,\cdots,x_n)&=&(x_1\lor x_2\lor\cdots\lor x_n\lor f(0,0,\cdots,0))\\
&&\land(x_1\lor x_2\lor\cdots\lor x'_n\lor f(1,0,\cdots,0))\\
&&\land\cdots\\
&&\land(x'_1\lor x'_2\lor\cdots x_n\lor f(1,1,\cdots,0))\\
&&\land(x'_1\lor x'_2\lor\cdots x'_n\lor f(1,1,\cdots,1))
\end{aligned}
\\]

となる ([シャノンの定理](#chanon_theorem)より数学的帰納法により証明できるが, 省略).
これを以下のように定義する.

<div class="m-def">
<header class="m-def-title"><p><a id="maximum_term_expansion" class="disabled">最大項展開</a></p></header>
<div class="m-def-content">
\\(n\\) 変数ブール関数 \\(f(x_1,x_2,\cdots,x_n)\\) のすべての入力変数 \\(x_1,x_2,\cdots,x_n\\) について, 
[シャノン双対展開](#chanon_theorem)した形式
\\[f(x_1,x_2,\cdots,x_n)=
\bigwedge_{(e_1,e_2,\cdots,e_n)\in B^n}(f(e_1,e_2,\cdots,e_n)\lor\bigvee_{i=1}^{n}x_i^{e'_i})\tag{2}\\]
は \\(f(x_1,x_2,\cdots,x_n)\\) の最大項展開である.  なお, このときの項数は \\(2^n\\) となる.
</div>
</div>

これを踏まえ, 乗法標準形, 主乗法標準形を導入する.

<div class="m-def">
<header class="m-def-title"><p><a id="CNF_PCNF" class="disabled">乗法標準形, 主乗法標準形</a></p></header>
<div class="m-def-content">
* 乗法標準形 (CNF: conjunctive normal form) は, ブール式のリテラル, または 2 つ以上のリテラルの和の積のことをいう.
ここで, 2 つ以上のリテラルの和で同じ入力変数を 2 度以上含まないブール式を基本和, また標準項という.
基本和のうち, すべての入力変数を含むブール式を最大項という. 
すなわち, 上記[最大項展開](#maximum_term_expansion)の \\(\displaystyle\bigvee_{i=1}^{n}x_i^{e'_i}\\) は最大項である.
* 主乗法標準形 (principal conjunctive normal form: PCNF) は, 
ブール関数を[最大項展開](#maximum_term_expansion)した形式, すなわち[最大項展開](#maximum_term_expansion)の形式である.
</div>
</div>

例えば, 否定論理積 \\(\mid\\) を PCNF で表すとしよう. 
否定論理積は 2 項演算子なので, その PCNF は 2 変数ブール関数を最大項展開した形となる.

\\[
f(x_1,x_2)=
(x_1\lor x_2\lor f(0,0))\land (x_1\lor x'_2\lor f(0,1))\land(x'_1\lor x_2\lor f(1,0))\land(x'_1\lor x'_2\lor f(1,1))\\]

2 項演算子 \\(\mid\\) を最大項展開した式 \\(f_{\mid}(x_1,x_2)\\) は,
[真理値表 2](#truthtable2) の \\(\mid\\) の列のとおりに \\(f_{\mid}(x_1,x_2)\\) の値を決めてやればよいので

\\[
\begin{aligned}
f_{\mid}(x_1,x_2)&=&(x_1\lor x_2\lor 1)\land (x_1\lor x'_2\lor 1)\land(x'_1\lor x_2\lor 1)\land(x'_1\lor x'_2\lor 0)\\
&=&x'_1\lor x'_2
\end{aligned}
\\]

従って, 否定論理積の PCNF は \\(x'_1\lor x'_2\\) となる. この操作を振り返ると, 真理値表から PCNF を書くためには,
結果が \\(0\\) となっている入力変数の全パターンに対して, 元の入力変数の値が \\(1\\) なら補元をとり, \\(0\\) ならそのままで和を取り, それらすべての積を取ればよいことがわかる.
何故ならば, 結果が \\(1\\) となる部分は, 和の性質, すなわち[公理 2: 同一律](#boolean_algebra3)より消えてしまうからである.
同じようにして, 否定論理和, 排他的論理和も[真理値表 2](#truthtable2) の \\(\downarrow,\oplus\\) の列をみると, \\(0\\) となる入力のパターンから

\\[
\begin{aligned}
f_{\downarrow}(x_1,x_2)&=&(x'_1\lor x'_2)\land(x'_1\lor x_2)\land(x_1\lor x'_2)\\
f_{\oplus}(x_1,x_2)&=&(x'_1\lor x'_2)\land(x_1\lor y_1)
\end{aligned}
\\]

となる. すなわち, 真理値表で表現できるブール式は, PCNF で表せるということである<sup><a class="footnote-ref" href="#fn-5">5</a></sup>.
次に, 任意の論理式から PCNF に変換することを考える. 結論からいうと, 次の手順に従えば PCNF へ機械的に変換できることが知られている.

1. ブール式全体を基本項による和の積の形にする (分配律等を利用)
2. 最大項でない基本項に対し, その基本項に含まれないすべてのリテラル \\(x_i\\) について \\(x_i\land x'_i\\) を乗ずる
3. 分配律等に従い展開して, 冗長な項を除去する

以下, PCNF で表されたブール式を \\(f(x_1,x_2,\cdots,x_n)_{\rm P C N F}\\) と書くこととする. 例えば, ブール式
\\(f(x_1,x_2,x_3)=x_1\land(x'_2\land x_3)'\\) を PCNF で表すとすると,

\\[
\begin{aligned}
f(x_1,x_2,x_3)&=&x_1\land(x'_2\land x_3)'\\
&=&x_1\land(x_2\lor x'_3)&(\because {\rm 定理: \href{#de_morgan}{ド・モルガンの法則}})\\
&=&(x_1\lor x_2\land x'_2)\land(x_2\lor x'_3)&(\because {\rm \href{#boolean_algebra3}{公理 3}:同一律})\\
&=&(x_1\lor x_2)\land(x_1\lor x'_2)\land(x_2\lor x'_3)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&(x_1\lor x_2\lor x_3\land x'_3)\land(x_1\lor x'_2\lor x_3\land x'_3)\land(x_1\land x'_1\lor x_2\land x'_3)&(\because {\rm \href{#boolean_algebra3}{公理 3}:同一律})\\
&=&(x_1\lor x_2\lor x_3)\land(x_1\lor x_2\lor x'_3)\land(x_1\lor x'_2\lor x_3)\\
&&\land(x_1\lor x'_2\lor x'_3)\land(x_1\lor x_2\lor x'_3)\land(x'_1\lor x_2\lor x'_3)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&(x_1\lor x_2\lor x_3)\land(x_1\lor x_2\lor x'_3)\land(x_1\lor x'_2\lor x_3)\\
&&\land(x_1\lor x'_2\lor x'_3)\land(x'_1\lor x_2\lor x'_3)&(\because {\rm 定理:\href{#idempotence}{べき等律}})
\end{aligned}
\\]

従って \\[f(x_1,x_2,x_3)_{\rm P C N F}=(x_1\lor x_2\lor x_3)\land(x_1\lor x_2\lor x'_3)\land(x_1\lor x'_2\lor x_3)\land(x_1\lor x'_2\lor x'_3)\land(x'_1\lor x_2\lor x'_3)\\]

となる. 例としてもう 1 つ, \\(f(x_1,x_2,x_3)=x_1\land x'_2\lor x_2\land x_3\\) としたときの PCNF は

\\[
\begin{aligned}
f(x_1,x_2,x_3)&=&x_1\land x'_2\lor x_2\land x_3\\
&=&(x_1\land x'_2\lor x_2)\land(x_1\land x'_2\lor x_3)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})\\
&=&(x_1\lor x_2)\land(x_1\lor x_3)\land(x'_2\lor x_3)&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&(x_1\lor x_2\lor x_3\land x'_3)\land(x_1\lor x_2\land x'_2\lor x_3)\land(x_1\land x'_1\lor x'_2\lor x_3)&(\because {\rm \href{#boolean_algebra4}{公理 4}:補元律})\\
&=&(x_1\lor x_2\lor x_3)\land(x_1\lor x_2\lor x'_3)\land(x_1\lor x'_2\lor x_3)\land(x'_1\lor x'_2\lor x_3)&(\because {\rm \href{#boolean_algebra2}{公理 2}:分配律})
\end{aligned}
\\]

従って
\\[f(x_1,x_2,x_3)_{\rm P C N F}=(x_1\lor x_2\lor x_3)\land(x_1\lor x_2\lor x'_3)\land(x_1\lor x'_2\lor x_3)\land(x'_1\lor x'_2\lor x_3)\\]
となる.

## 簡単化

ブール式を簡単化する方法について見ていく.

### カルノー図

例えばブール関数が \\[f(x_1,x_2)=x_1\land x'_2\lor x'_1\land x'_2\lor x_1\land x'_2\lor x_1\land x_2 \tag{3}\\] と与えられたとき,
式変形をしていくと簡単化できることがわかる.
事実, \\((3)=(x_1\lor x'_1)\land x'_2\lor x_1\land(x'_2\lor x_2)=x'_2\lor x_1\\) である.
このような,  [公理4: 補元律](#boolean_algebra4)を用いた式変形は, カルノー図という図式を用いることで視覚的に捉えることができる.
カルノー図は, 真理値表を 2 次元的に並べる形をしており,
例えば式 \\((3)\\) をカルノー図で表すと次のようになる.

| \\(x_1\backslash x_2\\) | \\(0\\) | \\(1\\) |
| :--: | :--: | :--: |
| \\(0\\) | \\(1\\) | \\(0\\) |
| \\(1\\) | \\(1\\) | \\(1\\) |

Table: カルノー図 1

緑色の表示となっているセルの部分それぞれが最小項を表しており, 
元の論理関数に含まれる最小項のセルには \\(1\\), 含まれないセルには \\(0\\) を記述する[^6]ことで[上図](#karnaugh1)のようになる.
このとき, 緑色の表示となっているセルの部分の個数は \\(2^n\\) である. これらをまとめることが, 冗長な変数の削除に対応するのである. 
カルノー図をみて \\(1\\) となるセルの和を書き下すことは, PDNF を書くことと同値である式 (\\((3)\\) は元から PDNF である.
[カルノー図 1](#karnaugh1) から結果として, これが導出できる).

カルノー図の隣接するセルの間では, 全ての変数のうちのどれか 1 つの \\(1\\) か \\(0\\) が変わった結果となっているので,
[公理4: 補元律](#boolean_algebra4)を利用してまとめることができるのである.
従って, 隣接する \\(1\\) のセルをグループ化し, [公理4: 補元律](#boolean_algebra4)を利用することで簡単化が実現できる.

以下, \\(i\\) 行目 \\(j\\) 列目のセルを \\(i:j\\) と示すこととする.
例えば, [カルノー図 1](#karnaugh1) で \\(1\\) となるセルは \\(1:1,2:1,2:2\\) で, これらはそれぞれ \\(x'_1\land x'_2,x_1\land x'_2,x_1\land x_2\\) である.
ここで, 任意の隣接するセル \\(c_i\\) をグループ化したことを \\({c_1,c_2,\cdots,c_n}\\) と書くとき, 
\\(g_1={1:1,2:1}, g_2={2:1,2:2}\\) とグループ化できることがわかる.
従って, それぞれを論理式で基本積の形に表すと, 

\\[
\begin{aligned}
g_1&=&x'_1\land x'_2\lor x_1\land x'_2&=&(x'_1\lor x_1)\land x'_2&=&x'_2\\
g_2&=&x_1\land x'_2\lor x_1\land x_2&=&x_1\land(x'_2\lor x_2)&=&x_1
\end{aligned}
\\]

よって, \\(=x'_2\lor x_1\\) とわかる. 別のカルノー図についてもやってみよう.

| \\(x_1x_2\backslash x_3x_4\\) | \\(00\\) | \\(01\\) | \\(11\\) | \\(10\\) |
| :--: | :--: | :--: | :--: | :--: |
| \\(00\\) | \\(0\\) | \\(1\\) | \\(1\\) | \\(0\\) |
| \\(01\\) | \\(0\\) | \\(1\\) | \\(0\\) | \\(0\\) |
| \\(11\\) | \\(0\\) | \\(0\\) | \\(0\\) | \\(0\\) |
| \\(10\\) | \\(1\\) | \\(0\\) | \\(1\\) | \\(1\\) |

Table: カルノー図 2

これは, 4 つの引数をもったブール関数のカルノー図である.
カルノー図では, 最上, 最下また最左, 最右のセルは隣接していると考える. 
従って, [カルノー図 2](#karnaugh2) におけるグループ化の一つの例として
\\(g_1={1:2,2:2},g_2={1:3,4:3},g_3={4:1,4:4}\\) と構成できる.
よって,

\\[\begin{aligned}
g_1&=&x'_1\land x'_2\land x'_3\land x_4\lor x'_1\land x_2\land x'_3\land x_4&=&x'_1\land x'_3\land x_4\\
g_2&=&x'_1\land x'_2\land x_3\land x_4\lor x_1\land x'_2\land x_3\land x_4&=&x'_2\land x_3\land x_4\\
g_3&=&x_1\land x'_2\land x'_3\land x'_4\lor x_1\land x'_2\land x_3\land x'_4&=&x_1\land x'_2\land x'_4
\end{aligned}\\]

ゆえに[カルノー図 2](#karnaugh2) で示される論理関数の簡単な表現は

\\[x'_1\land x'_3\land x_4\lor x'_2\land x_3\land x_4\lor x_1\land x'_2\land x'_4\\]

となる. このグループ化という操作が一意ではないことからわかるように, 簡単化したブール式も一意でないことがわかる.
例えば, \\(g_1={1:2,2:2},g_2={1:2,1:3},g_3={4:1,4:4},g_4={4:3,4:4}\\) とグループ化すると,

\\[\begin{aligned}
g_1&=&x'_1\land x'_2\land x'_3\land x_4\lor x'_1\land x_2\land x'_3\land x_4&=&x'_1\land x'_3\land x_4\\
g_2&=&x'_1\land x'_2\land x'_3\land x_4\lor x'_1\land x'_2\land x_3\land x_4&=&x'_1\land x'_2\land x_4\\
g_3&=&x_1\land x'_2\land x'_3\land x'_4\lor x_1\land x'_2\land x_3\land x'_4&=&x_1\land x'_2\land x'_4\\
g_4&=&x_1\land x'_2\land x_3\land x_4\lor x_1\land x'_2\land x_3\land x'_4&=&x_1\land x'_2\land x_3
\end{aligned}\\]

ゆえに,

\\[x'_1\land x'_3\land x_4\lor x'_1\land x'_2\land x_4\lor x_1\land x'_2\land x'_4\lor x_1\land x'_2\land x_3\\]

となる. 1 つの例を見ただけなので, 厳密に言えたことではないが, 一般的にグループの数が少なく, かつグループ内のセルの数を(\\(2n\\) 個で)なるべく多く取る方がより簡単な論理式を構成できることがわかる.

さて, ある論理変数の組み合わせが予め起こりえないことがわかっていたとき, その場合もカルノー図を用いて簡単化を進めることができる.
このような組み合わせに対する最小項を禁止項, または don't care 項という.
例えば, 
\\[\begin{aligned}
f(x_1,x_2,x_3,x_4)&=&x'_1\land x'_2\land x'_3\land x_4 \\ 
&&\lor x'_1\land x_2\land x'_3\land x_4 \\ 
&&\lor x'_1\land x_2\land x_3\land x_4\\
&&\lor x'_1\land x_2\land x_3\land x'_4\\
&&\lor x_1\land x'_2\land x_3\land x_4\\
&&\lor x_1\land x'_2\land x_3\land x'_4\tag{\htmlId{formula4}{4}}
\end{aligned}\\]

というブール関数を簡単化することを考える. このとき, \\(x_1\land x_2\\), また \\(x_1\land x'_2\land x'_3\land x'_4\\) は禁止項とする.
禁止行の対応するセルには \\(\phi\\) を記述する. すると, 式 \\((4)\\) のカルノー図は次のとおりとなる.

| \\(x_1x_2\backslash x_3x_4\\) | \\(00\\) | \\(01\\) | \\(11\\) | \\(10\\) |
| :--: | :--: | :--: | :--: | :--: |
| \\(00\\) | \\(0\\) | \\(1\\) | \\(0\\) | \\(0\\) |
| \\(01\\) | \\(0\\) | \\(1\\) | \\(1\\) | \\(1\\) |
| \\(11\\) | \\(\phi\\) | \\(\phi\\) | \\(\phi\\) | \\(\phi\\) |
| \\(10\\) | \\(\phi\\) | \\(0\\) | \\(1\\) | \\(1\\) |

Table: カルノー図 3

禁止項は, \\(1\\) でも \\(0\\) でもよいということになるので,
グループを構成するにあたって自分で都合よく \\(1\\) か \\(0\\) に解釈してしまって良い.
できる限り多くのセルと少ないグループの数で構成するために, いま \\(3:3,3:4\\) を \\(1\\) と解釈すれば, 
\\(g_1={1:2,2:2},g_2={2:3,2:4,3:3,3:4},g_3={3:3,3:4,4:3,4:4}\\) とグループ化できる.
従って,

\\[
\begin{aligned}
g_1&=&x'_1\land x'_2\land x'_3\land x_4\lor x'_1\land x_2\land x'_3\land x_4&=&x'_1\land x'_3\land x_4\\
g_2&=&x'_1\land x_2\land x_3\land x_4\lor x'_1\land x_2\land x_3\land x'_4\lor x_1\land x_2 \land x_3\land x_4\lor x_1\land x_2\land x_3\land x'_4&=&x_2\land x_3\\
g_3&=&x_1\land x_2\land x_3\land x_4\lor x_1\land x_2\land x_3\land x'_4\lor x_1\land x'_2\land x_3\land x_4\lor x_1\land x'_2\land x_3\land x'_4&=&x_1\land x_3
\end{aligned}
\\]

ゆえに, 式 \\(\href{#formula4}{(4)}\\) は

\\[f(x_1,x_2,x_3,x_4)=x'_1\land x'_3\land x_4\lor x_2\land x_3\lor x_1\land x_3\\]

と簡単化できた.
カルノー図は, 1 次元につき 2 つまでの引数を扱えると考えると, 人間の次元認識能力の見地から実質 6 つの引数にまで対応できることとなるわけだが,
実際は平面的に考えることが多いので, 大抵, 最大 4 個の引数までしか扱うことができない.

### クワイン・マクラスキー法

主にクワイン・マクラスキー法は \\[x\land y\lor x\land y'=x\land(y\lor y')=x\tag{\htmlId{formula5}{5}}\\] を繰り返し利用し, ブール関数を機械的に簡単化していく方法であり,
その手順は次のとおりである.

1. ブール式を PDNF にする
2. 式 \\(\href{#formula5}{(5)}\\) を利用して圧縮し, 主項を求める
    1. ブール式を 2 進値に割り当てる[^7]
    2. ハミング距離 1 のビット列同士を可能な限り繰り返し組み合わせる
3. 求めた主項からただ 1 つの最小項を包含する主項(必須項)を求める.
4. ブール関数を作成する
    1. 必須項の和をとる
    2. 必須項により包含されていない最小項があるとき, 最も簡単な主項を選択し和を取る.

いま, 式 \\(\href{#formula4}{(4)}\\) を簡単化することを考えるとしよう.
このとき, まず式を PDNF にする. \\((4)\\) はすでに PDNF の形式となっているので, 今回は必要ない.
次に, [カルノー図 3](#karnaugh3) の各セル \\(i:j\\) を \\(m_0=1:1,m_1=2:1,m_2=3:1,m_3=4:1,m_4=1:2,\cdots,m_{15}=4:4\\)
とおき, PDNF を構成する各最小項について次のようにビット列と対応させる.

\begin{aligned}
m_4&=&x'_1\land x'_2\land x'_3\land x_4=0001\\
m_5&=&x'_1\land x_2\land x'_3\land x_4=0101\\
m_9&=&x'_1\land x_2\land x_3\land x_4=0111\\
m_{11}&=&x_1\land x'_2\land x_3\land x_4=1011\\
m_{13}&=&x'_1\land x_2\land x_3\land x'_4=0110\\
m_{15}&=&x_1\land x'_2\land x_3\land x'_4=1010
\end{aligned}

ここで, 先と同様, \\(x_1\land x_2\\) と \\(x_1\land x'_2\land x'_3\land x'_4\\) を禁止項としたときは, 
それについてもビット列と対応させておく.

\begin{aligned}
m_2&=&x_1\land x_2\land x'_3\land x'_4=1100\\
m_3&=&x_1\land x'_2\land x'_3\land x'_4=1000\\
m_6&=&x_1\land x_2\land x'_3\land x_4=1101\\
m_{10}&=&x_1\land x_2\land x_3\land x_4=1111\\
m_{14}&=&x_1\land x_2\land x_3\land x'_4=1110
\end{aligned}

これは \\(\displaystyle\bigvee {\rm m}(4,5,9,11,13,15)+{\rm dc}(2,3,6,10,14)=(4)\\) と書かれる.
このとき, 例えば \\(m_4\lor m_5\\) は式 \\((5)\\) を利用して簡単化できることがわかる.
事実, \\[x'_1\land x'_2\land x'_3\land x_4\lor x'_1\land x_2\land x'_3\land x_4=x'_1\land x'_3\land x_4\land(x'_2\lor x_2)=x'_1\land x'_3\land x_4\\] である.
このような簡単化をすべての可能な組み合わせについて繰り返し実行する. この作業を圧縮ということとする. 
先にブール式をビット列と対応させたので, 圧縮とはハミング距離 1 のビット列同士を繰り返し組み合わせることと同値である.
次の表に, 圧縮を 1 度行った結果を示す.
組み合わせられたビット部分は \\(-\\), それ以上圧縮できないものを主項といい, \\(\ast\\) で示すものとする[^8]. これを圧縮表ということとする.

<div class="content">
<table class="table"><thead><th>1 の数</th><th>最小項</th><th>ビット列表現</th></thead>
<caption id="compress_table1" style="caption-side: bottom">圧縮表 (1 回目)</caption>
<tbody>
<tr><td rowspan="3">\\(1\\)</td><td>\\(m_{4,5}\\)</td><td>\\(0-01\ast\\)</td></tr>
<tr><td>\\(m_{3,15}\\)</td><td>\\(10-0\\)</td></tr>
<tr><td>\\(m_{2,3}\\)</td><td>\\(1-00\\)</td></tr> 
<tr><td rowspan="8">\\(2\\)</td><td>\\(m_{5,9}\\)</td><td>\\(01-1\\)</td></tr>
<tr><td>\\(m_{5,6}\\)</td><td>\\(-101\\)</td></tr>
<tr><td>\\(m_{9,13}\\)</td><td>\\(011-\\)</td></tr>
<tr><td>\\(m_{13,14}\\)</td><td>\\(-110\\)</td></tr>
<tr><td>\\(m_{14,15}\\)</td><td>\\(1-10\\)</td></tr>
<tr><td>\\(m_{11,15}\\)</td><td>\\(101-\\)</td></tr>
<tr><td>\\(m_{2,6}\\)</td><td>\\(110-\\)</td></tr>
<tr><td>\\(m_{2,14}\\)</td><td>\\(11-0\\)</td></tr>
<tr><td rowspan="5">\\(3\\)</td><td>\\(m_{9,10}\\)</td><td>\\(-111\\)</td></tr>
<tr><td>\\(m_{10,11}\\)</td><td>\\(1-11\\)</td></tr>
<tr><td>\\(m_{6,10}\\)</td><td>\\(11-1\\)</td></tr>
<tr><td>\\(m_{10,14}\\)</td><td>\\(111-\\)</td></tr>
</tbody>
</table>
</div> 

残りのすべての最小項について \\(\ast\\) がつくまで繰り返す.

<div class="table-responsive">
<table class="table table-hover"><thead><th>1 の数</th><th>最小項</th><th>ビット列表現</th></thead>
<caption id="compress_table2" style="caption-side: bottom">圧縮表 (2 回目)</caption>
<tbody>
<tr><td>\\(1\\)</td><td>\\(m_{2,3,14,15}\\)</td><td>\\(1--0\ast\\)</td></tr>
<tr><td rowspan="4">\\(2\\)</td><td>\\(m_{5,6,9,10}\\)</td><td>\\(-1-1\ast\\)</td></tr>
<tr><td>\\(m_{9,10,13,14}\\)</td><td>\\(-11-\ast\\)</td></tr>
<tr><td>\\(m_{10,11,14,15}\\)</td><td>\\(1-1-\ast\\)</td></tr>
<tr><td>\\(m_{2,6,10,14}\\)</td><td>\\(11--\ast\\)</td></tr>
</tbody>
</table>
</div> 

従って, 主項は \\(m_{4,5},m_{2,3,14,15},m_{5,6,9,10},m_{9,10,13,14},m_{10,11,14,15},m_{2,6,10,14}\\) であるから, 
[圧縮表 1](#compress_table1) および [2](#compress_table2) より, 式 \\(\\) は次のように表現できることがわかる.

\\[\underbrace{x'_1\land x'_3\land x_4}_{m_{4,5}}\lor
\underbrace{x_1\land x'_4}_{m_{2,3,14,15}}\lor\underbrace{x_2\land x_4}_{m_{5,6,9,10}}\lor\underbrace{x_2\land x_3}_{m_{9,10,13,14}}\lor\underbrace{x_1\land x_3}_{m_{10,11,14,15}}\lor
\underbrace{x_1\land x_2}_{m_{2,6,10,14}}\\]

しかしこれはまだ冗長である. この主項から必須項を調べる. 縦軸に主項, 横軸に最小項を並べ, 最小項を包含する主項のセルに印 \\(\bigcirc\\) を, 
包含する最小項が 1 つしかない主項のセルに印 \\(\circledcirc\\) をつける. これを主項表という.

<div class="table-responsive">
| 主項 \\(\backslash\\) 最小項 | \\(m_4\\) | \\(m_5\\) | \\(m_9\\) | \\(m_{13}\\) | \\(m_{11}\\) | \\(m_{15}\\) | ビット表現 |
| :--: | :--: | :--: | :--: | :--: | :--: | :--: | :--: |
| \\(m_{4,5}\\) | \\(\color{blue}{\circledcirc}\\) | \\(\color{blue}{\bigcirc}\\) | | | | | \\(0-01\\) |
| \\(m_{5,6,9,10}\\) | | \\(\bigcirc\\) | \\(\bigcirc\\) | | | | \\(-1-1\\) |
| \\(m_{9,10,13,14}\\) | | | \\(\color{blue}{\bigcirc}\\) | \\(\color{blue}{\circledcirc}\\) | | | \\(-11-\\) |
| \\(m_{10,11,14,15}\\) | | | | | \\(\color{blue}{\circledcirc}\\) | \\(\color{blue}{\bigcirc}\\) | \\(1-1-\\) |
| \\(m_{2,3,14,15}\\) | | | | | | \\(\bigcirc\\) | \\(1--0\\) |
| \\(m_{2,6,10,14}\\) | | | | | | | \\(11--\\) |

Table: 主項表 1
</div>

従って, 必須項は \\(\circledcirc\\) のつく \\(m_{4,5},m_{9,10,13,14},m_{10,11,14,15}\\) である. 
あとはそれらを書き出し, 残りの主項で最小項を全て含む最も簡単な組み合わせを探すこととなる(ここで現れる必須項が全ての最小項を包含するとは限らない).
ここで, もし \\(\circledcirc\\) が 1 つも含まれない最小項があれば, すなわち必須項にすべての最小項が含まれていなければ,
最も簡単となりかつ, 全ての最小項を含むブール式となるよう適当な主項を選択する.
今回の場合では, 必須項のみで全ての最小項を包含することができている(印を青の表示としておいた)から
\\(m_{4,5},m_{9,10,13,14},m_{10,11,14,15}\\) の和, すなわち
\\[x'_1\land x'_3\land x_4\lor x_2\land x_3\lor x_1\land x_3\\] が式 \\((4)\\) の最簡形である.
先に示したカルノー図による簡単化で得られたブール式と同等の結果が得られたことがわかる.
なお, クワイン・マクラスキー法は NP 完全である[^9]ため, 使用範囲が限られる.


### ペトリック法

先のクワイン・マクラスキー法の最後では,
「最も簡単となりかつ, 全ての最小項を含むブール式となるよう適当な主項を選択」することによって最簡形を得るとのことであったが,
この部分をペトリック法で置き換えることにより, 機械的に最簡形のブール式を決定することができる.
ここでも, \\((4)\\) を例に方法を示すこととする.
クワイン・マクラスキー法の手順のうち 3 まで実行したものとし, [主項表 1](#prime_implicant_table1) が得られたとしよう.
まず[主項表 1](#prime_implicant_table1) の列を見て, 印のある主項らで和を取り, それらの積をとった次の式を得る.

\\[m_{4,5}\land (m_{4,5}\lor m_{5,6,9,10})\land(m_{5,6,9,10}\lor m_{9,10,13,14})\land m_{9,10,13,14}\land m_{10,11,14,15}\land(m_{10,11,14,15}\lor m_{2,3,14,15})\tag{6}\\]

式 \\((6)\\) を[公理2](#boolean_algebra2): 分配律および[吸収律](#absorption)を用いて変形していくと,

\\[
\begin{aligned}
(6)&=&(m_{4,5}\lor m_{4,5}\land m_{5,6,9,10})\land(m_{5,6,9,10}\land m_{9,10,13,14}\lor m_{9,10,13,14})\land(m_{10,11,14,15}\lor m_{10,11,14,15}\land m_{2,3,14,15})\\
&=&m_{4,5}\land m_{9,10,13,14}\land m_{10,11,14,15}
\end{aligned}
\\]

この主項の積となっている部分を主項の和とすることで, ブール関数の最簡形が求まる.
従って, 先と同様の結果が機械的に得られたことがわかる.

#### クワイン・マクラスキー法, ペトリック法の実装

これらは一度プログラムで実装することが割と学習の定番となっているので, Haskell で実装した.
次のリポジトリにて管理している.

<div class="has-text-centered mb-3 mt-3">
<i class="fab fa-github fa-fw"></i>
<a href="https://github.com/falgon/bsimplified">falgon/bsimplified - The simple and pure implementation of Quine-McCluskey method, Petrick's method and parsing of Boolean formula</a>
</div>

まず, いま解いた簡単化を再度実行してみる. 
よくある実装法だと思うが, クワイン・マクラスキー法の圧縮過程は二分木のデータ構造として表現する.
従って, まずはじめに各最小項に対応するノードを作成することで PDNF を表現することとした.

```haskell
Prelude> :m +BSimplified.QMM Data.Maybe
Prelude BSimplified.QMM Data.Maybe> let tr = fromJust $ pdnfForest (fromJust $ strBitsList ["0001","0101","0111","1011","0110","1010","1100","1000","1101","1111","1110"]) (replicate 6 False ++ replicate 5 True)
Prelude BSimplified.QMM Data.Maybe> tr
[CTNode {row = TruthTableRow { var = 1 (binary), dontcare = False }, mergedFlag = -1, identifier = 18370878410602274422, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 101 (binary), dontcare = False }, mergedFlag = -1, identifier = 17795521015237778886, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = -1, identifier = 9505003458451781531, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 1011 (binary), dontcare = False }, mergedFlag = -1, identifier = 5344459161969259783, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 110 (binary), dontcare = False }, mergedFlag = -1, identifier = 2509103263232437805, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 1010 (binary), dontcare = False }, mergedFlag = -1, identifier = 9465362842462816922, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 1100 (binary), dontcare = True }, mergedFlag = -1, identifier = 5080631804786515567, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 1000 (binary), dontcare = True }, mergedFlag = -1, identifier = 8905849755465195471, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 1101 (binary), dontcare = True }, mergedFlag = -1, identifier = 7582384624143865392, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 1111 (binary), dontcare = True }, mergedFlag = -1, identifier = 4819471800686604738, prevLeft = CTEmpty, prevRight = CTEmpty},CTNode {row = TruthTableRow { var = 1110 (binary), dontcare = True }, mergedFlag = -1, identifier = 8582731269452337816, prevLeft = CTEmpty, prevRight = CTEmpty}]
```

`pdnfForest` には `QMyBits` のインスタンス (`DigitListBits`, `StrBits`, `RawBits`) 
のリストと Don't care か否かを制御するフラグのリストを渡す.
このとき, 2 つのリストの要素数は同一でなければならない (もしそうでなければ `Nothing` が返る).
ここで, `DigitListBits`, `StrBits`, `RawBits` はそれぞれビット列の表現を包括的に捉えるための型である.

```haskell
Prelude BSimplified.QMM Data.Maybe> :m +BSimplified.Bits
Prelude BSimplified.QMM Data.Maybe BSimplified.Bits> toRawBits (StrBits "1010") == toRawBits (RawBits 10)
True
Prelude BSimplified.QMM Data.Maybe BSimplified.Bits> toRawBits (StrBits "1010") == toRawBits (DigitListBits [1,0,1,0])
True
```

クワイン・マクラスキー法によって圧縮を実行する.

```haskell
Prelude BSimplified.QMM Data.Maybe BSimplified.Bits> quineMcCluskey tr
PrimeImplicants [CTNode {row = TruthTableRow { var = 1 (binary), dontcare = False }, mergedFlag = 2, identifier = 577617992350188464, prevLeft = CTNode {row = TruthTableRow { var = 1 (binary), dontcare = False }, mergedFlag = -1, identifier = 18370878410602274422, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 101 (binary), dontcare = False }, mergedFlag = -1, identifier = 17795521015237778886, prevLeft = CTEmpty, prevRight = CTEmpty}},CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 2, identifier = 6829379978376433071, prevLeft = CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 1, identifier = 8439440437090953821, prevLeft = CTNode {row = TruthTableRow { var = 101 (binary), dontcare = False }, mergedFlag = -1, identifier = 17795521015237778886, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = -1, identifier = 9505003458451781531, prevLeft = CTEmpty, prevRight = CTEmpty}}, prevRight = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = 1, identifier = 3159344086055741426, prevLeft = CTNode {row = TruthTableRow { var = 1101 (binary), dontcare = True }, mergedFlag = -1, identifier = 7582384624143865392, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1111 (binary), dontcare = True }, mergedFlag = -1, identifier = 4819471800686604738, prevLeft = CTEmpty, prevRight = CTEmpty}}},CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 2, identifier = 10719524625750522604, prevLeft = CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 0, identifier = 11617789877111658934, prevLeft = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = -1, identifier = 9505003458451781531, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 110 (binary), dontcare = False }, mergedFlag = -1, identifier = 2509103263232437805, prevLeft = CTEmpty, prevRight = CTEmpty}}, prevRight = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = 0, identifier = 3889360259696288602, prevLeft = CTNode {row = TruthTableRow { var = 1111 (binary), dontcare = True }, mergedFlag = -1, identifier = 4819471800686604738, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1110 (binary), dontcare = True }, mergedFlag = -1, identifier = 8582731269452337816, prevLeft = CTEmpty, prevRight = CTEmpty}}},CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 1, identifier = 18197116026168548551, prevLeft = CTNode {row = TruthTableRow { var = 101 (binary), dontcare = False }, mergedFlag = 0, identifier = 14515361159708671901, prevLeft = CTNode {row = TruthTableRow { var = 1011 (binary), dontcare = False }, mergedFlag = -1, identifier = 5344459161969259783, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1010 (binary), dontcare = False }, mergedFlag = -1, identifier = 9465362842462816922, prevLeft = CTEmpty, prevRight = CTEmpty}}, prevRight = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = 0, identifier = 3889360259696288602, prevLeft = CTNode {row = TruthTableRow { var = 1111 (binary), dontcare = True }, mergedFlag = -1, identifier = 4819471800686604738, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1110 (binary), dontcare = True }, mergedFlag = -1, identifier = 8582731269452337816, prevLeft = CTEmpty, prevRight = CTEmpty}}},CTNode {row = TruthTableRow { var = 10 (binary), dontcare = False }, mergedFlag = 1, identifier = 14507692373987760546, prevLeft = CTNode {row = TruthTableRow { var = 100 (binary), dontcare = False }, mergedFlag = 1, identifier = 17927799263722280277, prevLeft = CTNode {row = TruthTableRow { var = 1010 (binary), dontcare = False }, mergedFlag = -1, identifier = 9465362842462816922, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1000 (binary), dontcare = True }, mergedFlag = -1, identifier = 8905849755465195471, prevLeft = CTEmpty, prevRight = CTEmpty}}, prevRight = CTNode {row = TruthTableRow { var = 110 (binary), dontcare = False }, mergedFlag = 1, identifier = 3574163930668469495, prevLeft = CTNode {row = TruthTableRow { var = 1100 (binary), dontcare = True }, mergedFlag = -1, identifier = 5080631804786515567, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1110 (binary), dontcare = True }, mergedFlag = -1, identifier = 8582731269452337816, prevLeft = CTEmpty, prevRight = CTEmpty}}},CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 0, identifier = 1892010599057419013, prevLeft = CTNode {row = TruthTableRow { var = 110 (binary), dontcare = False }, mergedFlag = 0, identifier = 3438512683213686879, prevLeft = CTNode {row = TruthTableRow { var = 1100 (binary), dontcare = True }, mergedFlag = -1, identifier = 5080631804786515567, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1101 (binary), dontcare = True }, mergedFlag = -1, identifier = 7582384624143865392, prevLeft = CTEmpty, prevRight = CTEmpty}}, prevRight = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = 0, identifier = 3889360259696288602, prevLeft = CTNode {row = TruthTableRow { var = 1111 (binary), dontcare = True }, mergedFlag = -1, identifier = 4819471800686604738, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1110 (binary), dontcare = True }, mergedFlag = -1, identifier = 8582731269452337816, prevLeft = CTEmpty, prevRight = CTEmpty}}}]
```

実際の計算ではこれで十分なのだが, 一応人間にとってより分かりやすい `traceQuineMcCluskey` を用意してある.
引数には, `pdnfForest` で作成したノードのリストと, それに対応する変数名のリストを渡す.
両者の要素数は同一でなければならない (もしそうでない場合 `Nothing` が返る).
ここでは, 先に扱った例題に対応した変数名を渡しているので,
得られた主項が先と同一であることが確認できる.

```haskell
Prelude BSimplified.QMM Data.Maybe BSimplified.Bits> :m +Control.Monad
Prelude BSimplified.QMM Data.Maybe BSimplified.Bits Control.Monad> void $ traceQuineMcCluskey tr ["m_4","m_5","m_9","m_11","m_13","m_15","m_2","m_3","m_6","m_10","m_14"]
The state of compression #1: ["m_4 m_5","m_5 m_9","m_5 m_6","m_9 m_13","m_9 m_10","m_11 m_15","m_11 m_10","m_13 m_14","m_15 m_3","m_15 m_14","m_2 m_3","m_2 m_6","m_2 m_14","m_6 m_10","m_10 m_14"]
Found prime implicants: ["m_4 m_5"]
The state of compression #2: ["m_5 m_9 m_6 m_10","m_9 m_13 m_10 m_14","m_11 m_15 m_10 m_14","m_15 m_3 m_2 m_14","m_2 m_6 m_10 m_14"]
Found prime implicants: ["m_5 m_9 m_6 m_10","m_9 m_13 m_10 m_14","m_11 m_15 m_10 m_14","m_15 m_3 m_2 m_14","m_2 m_6 m_10 m_14"]
```

簡単化された最低限の項を得るには `minTerms` 等を用いる.
これも, 変数名と対応した結果を得ることのできる `minTermsStr` を用意してある.

```haskell
Prelude BSimplified.QMM Data.Maybe BSimplified.Bits Control.Monad> minTerms tr
[CTNode {row = TruthTableRow { var = 1 (binary), dontcare = False }, mergedFlag = 2, identifier = 577617992350188464, prevLeft = CTNode {row = TruthTableRow { var = 1 (binary), dontcare = False }, mergedFlag = -1, identifier = 18370878410602274422, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 101 (binary), dontcare = False }, mergedFlag = -1, identifier = 17795521015237778886, prevLeft = CTEmpty, prevRight = CTEmpty}},CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 2, identifier = 10719524625750522604, prevLeft = CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 0, identifier = 11617789877111658934, prevLeft = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = -1, identifier = 9505003458451781531, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 110 (binary), dontcare = False }, mergedFlag = -1, identifier = 2509103263232437805, prevLeft = CTEmpty, prevRight = CTEmpty}}, prevRight = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = 0, identifier = 3889360259696288602, prevLeft = CTNode {row = TruthTableRow { var = 1111 (binary), dontcare = True }, mergedFlag = -1, identifier = 4819471800686604738, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1110 (binary), dontcare = True }, mergedFlag = -1, identifier = 8582731269452337816, prevLeft = CTEmpty, prevRight = CTEmpty}}},CTNode {row = TruthTableRow { var = 11 (binary), dontcare = False }, mergedFlag = 1, identifier = 18197116026168548551, prevLeft = CTNode {row = TruthTableRow { var = 101 (binary), dontcare = False }, mergedFlag = 0, identifier = 14515361159708671901, prevLeft = CTNode {row = TruthTableRow { var = 1011 (binary), dontcare = False }, mergedFlag = -1, identifier = 5344459161969259783, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1010 (binary), dontcare = False }, mergedFlag = -1, identifier = 9465362842462816922, prevLeft = CTEmpty, prevRight = CTEmpty}}, prevRight = CTNode {row = TruthTableRow { var = 111 (binary), dontcare = False }, mergedFlag = 0, identifier = 3889360259696288602, prevLeft = CTNode {row = TruthTableRow { var = 1111 (binary), dontcare = True }, mergedFlag = -1, identifier = 4819471800686604738, prevLeft = CTEmpty, prevRight = CTEmpty}, prevRight = CTNode {row = TruthTableRow { var = 1110 (binary), dontcare = True }, mergedFlag = -1, identifier = 8582731269452337816, prevLeft = CTEmpty, prevRight = CTEmpty}}}]
Prelude BSimplified.QMM Data.Maybe BSimplified.Bits Control.Monad> minTermsStr tr ["m_4","m_5","m_9","m_11","m_13","m_15","m_2","m_3","m
_6","m_10","m_14"]
Just [["m_4","m_5"],["m_9","m_13","m_10","m_14"],["m_11","m_15","m_10","m_14"]]
```

また, ブール式を入力して, 最も簡単な式となる項を見つけられるようにもしてある.
ブール式の表記としては, 後述の記号 (否定: `~`, 積: `*`, 和: `+`, 括弧: `(`,`)`) を用いることができる.
アルファベット 1 文字は変数名として捉える.
ここでは, 例として式 \\((3)\\) を入力として与える.

```bash
$ stack build
$ stack exec bsimplified -- "A*~B+~A*~B+A*~B+A*B"
Minterms (Truth patterns):
        m_0 = { A = True, B = True }
        m_1 = { A = True, B = False }
        m_3 = { A = False, B = False }
The state of compression #1: ["m_0 m_1","m_1 m_3"]
Found prime implicants: ["m_0 m_1","m_1 m_3"]
Simplified terms: (m_0 m_1), (m_1 m_3)
```

\\(m_{0,1},m_{1,3}\\) が結果として得られた. 
それぞれ, ビット列上の圧縮された部分を \\(-\\) で表記すると, \\(m_{0,1}=1-, m_{1,3}=-0\\) である.
これはつまり \\(A B'\\) なので, 従って最簡形は \\(A\lor B'\\) である.
当然ながら, 先に求めた解と同じ結果が得られたことが確認できる.
実装については普通に字句解析, 再起下降で計算, 真理値表を構成して PDNF をつくっている.
ところで, この最簡形を得るという問題は充足可能性問題であり NP 困難[^10]なので, 変数の多いブール関数に対する最簡形を得ることは難しい.
その場合, 現実的な時間で比較的良質な解が得られるヒューリスティックを含む方法で求めることとなる.

## ブール代数の例

ここではブール代数の一例として, 計算機科学で一般的に用いられるブール代数を挙げる.
集合 \\(L={0,1}\\) に対して \\(\land\\) を積 \\(\cdot:L\times L\to L\\), 
\\(\lor\\) を和 \\(+:L\times L\to L\\), 補元 \\(x'\\) を否定 \\(\overline{x}\\) とおく. 
各演算子は, 次の[真理値表 1](#truthtable1) に従う (\\(\overline{y}\\) は \\(\overline{x}\\) と同様なので省略).

| \\(x\\) | \\(y\\) | \\(\overline{x}\\) | \\(x\cdot y\\) | \\(x+y\\) |
| :--: | :--: | :--: | :--: | :--: |
| \\(1\\) | \\(1\\) | \\(0\\) | \\(1\\) | \\(1\\) |
| \\(1\\) | \\(0\\) | \\(0\\) | \\(0\\) | \\(1\\) |
| \\(0\\) | \\(1\\) | \\(1\\) | \\(0\\) | \\(1\\) |
| \\(0\\) | \\(0\\) | \\(1\\) | \\(0\\) | \\(0\\) |

Table: \\(\htmlId{truthtable1}{\rm 真理値表 1}\\)

これは紛れもなくブール代数である.
この形式の下で書かれる式は一般に論理式, 演算子は論理記号といわれる.
計算機科学の分野において,
ブール代数は排他的論理和: \\(\oplus\\), 否定論理積: \\(\mid\\), 否定論理和: \\(\downarrow\\) といった記号らをも含めて論理記号として扱うことが多い.
これらの真理値表は \\(x,y\in L\\) に対して次の通りである.

| \\(x\\) | \\(y\\) | \\(x\mid y\\) | \\(x\downarrow y\\) | \\(x\oplus y\\) |
| :--: | :--: | :--: | :--: | :--: |
| \\(1\\) | \\(1\\) | \\(0\\) | \\(0\\) | \\(0\\) | 
| \\(1\\) | \\(0\\) | \\(1\\) | \\(0\\) | \\(1\\) |
| \\(0\\) | \\(1\\) | \\(1\\) | \\(0\\) | \\(1\\) | 
| \\(0\\) | \\(0\\) | \\(1\\) | \\(1\\) | \\(0\\) |

Table: \\(\htmlId{truthtable2}{\rm 真理値表 2}\\)

とくに, 否定論理積 \\(\mid\\) はそれ一つで積, 和, 否定が定義できるため, 他の論理記号よりも特別視されるような場合のある結合子である. 
これは, シェファーの棒記号といわれ, 真理値表をみるとわかるように \\(\overline{(x\land y)}\\), また \\(\overline{x}\lor \overline{y}\\) と同値である. 
この否定論理積を使って, \\(\overline{x}:=x\mid x\\) と否定が定義できる. 
この否定を使って, \\(x\land y:=\overline{(x\mid y)}\\) と積が定義できるし, 
\\(x\lor y:=\overline{x}\mid \overline{y}\\) と和も定義できる. 
どちらかをも定義せずとも, 否定に加えて積があれば和が定義できるし, 和があれば積が定義できる.

## 参考文献

1. "<a id="ref1" href="https://math.stackexchange.com/questions/1210458/what-is-the-difference-between-boolean-logic-and-propositional-logic">What is the difference between Boolean logic and propositional logic?</a>" 2019 年 4 月 13 日アクセス.
2. "<a id="ref2" href="https://www.quora.com/What-is-the-difference-between-Boolean-Algebra-and-propositional-logic-If-either-they-are-same-or-one-is-a-subset-of-another-why-should-we-study-those-separately">What is the difference between Boolean Algebra and propositional logic? If either they are same or one is a subset of another why should we study those separately?</a>" 2019 年 4 月 13 日アクセス.
3. J. Donald Monk (1976) "Mathematical Logic (Graduate Texts in Mathematics)" Springer; Softcover reprint of the original 1st ed. 1976版 (1976/9/7). ISBN-13: 978-1468494549
4. 赤間世紀, 長田康敬, 玉城史朗 (2006)『<a id="ref4" class="disabled">情報数学入門</a>』共立出版. ISBN-13: 978-4320018143
5. W. V. Quine (1952) "The Problem of Simplifying Truth Functions" The American Mathematical Monthly Vol. 59, No. 8 (Oct., 1952), pp. 521-531
6. W. V. Quine (1955) "A Way to Simplify Truth Functions" The American Mathematical Monthly Vol. 62, No. 9 (Nov., 1955), pp. 627-631
7. [動画] Phalanetra. H.S "[Quine McCluskey minimisation and Petrick's method for obtaining simplified Boolean expressions](https://www.youtube.com/watch?v=97KpndF8-So)", 2019 年 4 月 30 日アクセス.
8. [Lecture #10: Petrick's Method](http://www.mrc.uidaho.edu/mrc/people/jff/349/lect.10)
9. Czort, S. (1999) "<a id="ref9" class="disabled">The complexity of minimizing disjunctive normal form formulas (Master's thesis)</a>". University of Aarhus.

[^1]: イギリスの数学者ジョージ・ブール (英: George Boole) は 19 世紀半ばに人間の思考を代数計算で行うための研究を行い, ブール代数を形式化した. 命題論理はそれよりも昔にフレーゲにより構築された論理体型であるが, これは哲学的投機から派生したものである. 19 世紀後半になると, 哲学者たちは殆どブールの象徴主義を採用し, その後の 20 世紀ではこれらの学問間における明確な区別はなかったとのこと([参考文献2](#ref2) より引用: <i>The main difference is historical. George Boole was a mathematician interested in efficient practical solutions of complicated logical questions in the middle 19th century. His main innovation was symbolic logic, a system of notation for clear specification of propositions and relations among propositions. [..] Propositional logic goes back to ancient times and derives from philosophical speculation. In the late 19th century philosophers mostly adopted Boole’s symbolism. Therefore in the 20th century there’s no clear distinction between the two fields, </i>). しかしながら, ブール代数に還元できないいくつかの命題論理が残っているとのこと([参考文献2](#ref2) より引用: <i>although there remains some propositional logic that cannot be reduced to Boolean symbols. </i>). これが事実ならば, ブール代数は命題論理のサブセット的な論理であることがいえるが, [参考文献3](#ref3) p.158 では, "<i>the correspondence between Boolean algebras and sentential logics [...] We shall see that there is a full correspondence between these two kinds of mathematical objects.</i> とあり, さらに同著書 p.160 で "<i>the following theorem, which is another kind of completeness theorem for Boolean algebras. [...] Hence we may say that the theories of Boolean algebras and of sentential logics are equivalent, in some sense.</i>" とも言われていることから, 大まかに言い切ってしまえば, 殆ど差はないということであろう.
[^2]: ブール式は 1 つのブール関数を定めるが, ブール関数はブール式を一意には定めない.
[^3]: $\displaystyle\bigwedge^{n}_{i=1} A_i = A_1\land\cdots\land A_n, \bigvee^{n}_{i=1} A_i=A_1\lor\cdots\lor A_n$
[^4]: 真理値表とは, 簡単にいえばここでは \\(x,y\in B\\) に対してそれぞれ \\(1\\) または \\(0\\) を実際に代入したときに取りうるすべての値を書き下したものである. より厳密な取り扱いについては命題論理のエントリ(TODO)を参照.
[^5]: このあたりは自明とする.
[^6]: すなわち, ブール関数の値を記述する. \\(0\\) は記述せずに省略される場合もある.
[^7]: 2 進値を用いず, 変数を用いて圧縮表を作成する方法はクワイン法といわれる. ウィラード・ヴァン・オーマン・クワイン (英: Willard van Orman Quine) によって提案されたクワイン法がエドワード・J・マクラスキーによって発展されたため, このように言われている.
[^8]: この表記の仕方は [Wikipedia の記事](https://ja.wikipedia.org/wiki/%E3%82%AF%E3%83%AF%E3%82%A4%E3%83%B3%E3%83%BB%E3%83%9E%E3%82%AF%E3%83%A9%E3%82%B9%E3%82%AD%E3%83%BC%E6%B3%95)を参考とした.
[^9]: [参考文献 9](#ref9) より
[^10]: これについては, 命題論理のエントリ(TODO)内のトートロジー判定器のセクションにおいて取り上げている.
