---
title: LU 分解
date: 2019-01-01 00:00:00
tags: math, Haskell
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

LU 分解に関して初歩的な内容から網羅的にまとめた.

<!--toc-->

<!--more-->

## ガウスの消去法 

ガウスの消去法は, 前進消去による上三角行列の形成と後退代入の組み合わせのことをいい, 
その本質は行列の行基本変形, すなわち中学校で習う連立方程式の解法そのものである.
例えば, 何の芸もないが, 次の連立方程式

\\[
\begin{aligned}
\begin{cases}
x+2y&=&3  \\
3x+4y&=&5
\end{cases}\leftrightarrow
\left(\begin{array}{cc|c}
1 & 2 & 3 \\
3 & 4 & 5
\end{array}\right)\tag{1}
\end{aligned}
\\]

をガウスの消去法では次のようにして解くのであった.

$$
\left(\begin{array}{cc|c}
1 & 2 & 3 \\
3 & 4 & 5
\end{array}\right)\to
\underbrace{
\left(\begin{array}{cc|c}
\overbrace{1}^{\times (-3)} & \overbrace{2}^{\times (-3)} & \overbrace{3}^{\times (-3)} \\
\underline{3} & 4 & 5
\end{array}\right)
}_{\rm 前進消去}\to
\left(\begin{array}{cc|c}
1 & 2 & 3 \\
0 & -2 & -4
\end{array}\right)\to
\underbrace{
\left(\begin{array}{cc|c}
1 & \underline{2} & 3 \\
\overbrace{0}^{\times 1} & \overbrace{-2}^{\times 1} & \overbrace{-4}^{\times 1}
\end{array}\right)
}_{\rm 後退代入} \\ \to
\left(\begin{array}{cc|c}
1 & 0 & -1 \\
0 & -2 & -4
\end{array}\right) \\
\therefore (x,y)=(-1\div 1,-4\div (-2))=(-1,2)
$$

この前進消去の操作は次のように行列の積で表現できる.

$$
\left(\begin{array}{cc}
1 & 0 \\
-3 & 1
\end{array}\right)
\left(\begin{array}{cc}
1 & 2 \\
3 & 4
\end{array}\right)=
\left(\begin{array}{cc}
1 & 2 \\
0 & -2
\end{array}\right)
$$

これを理解しておくと後述する LU 分解の理解に容易くなる. ここで, この時間計算量について, 一般の \\(n\\) 次線形連立方程式
\\(X{\boldsymbol a}={\boldsymbol y}\ {\rm where}\ X\in\mathbb{R}^{n\times n}, 
{\boldsymbol a}\in\mathbb{R}^{n\times 1}, {\boldsymbol y}\in\mathbb{R}^{n\times 1}\\) 
を用いて考えるする.

$$\left(\begin{array}{ccccc}
x_{11} & x_{12} & x_{13} & \cdots & x_{1n} \\
x_{21} & x_{22} & x_{23} & \cdots & x_{2n} \\
x_{31} & x_{32} & x_{33} & \cdots & x_{3n} \\
\vdots & \vdots & \vdots & \ddots & \vdots \\
x_{n1} & x_{n2} & x_{n3} & \cdots & x_{nn}
\end{array}\right)
\left(\begin{array}{c}a_1 \\ a_2 \\ a_3 \\ \vdots \\ a_n\end{array}\right)=
\left(\begin{array}{c}y_1 \\ y_2 \\ y_3 \\ \vdots \\ y_n\end{array}\right)
$$

まず前進消去の操作について考える.
前進消去では, \\(x_{11}\\) を使って残りの \\(x_{21}, x_{31}, \cdots, x_{n1}\\)
を掃き出していくのであった. ただし \\(x_{21}, x_{31}, \cdots, x_{n1}\\) は必ず $0$ 
となるのだから, これは実質の計算量として考える必要はないだろう.
すると \\(x_{21}, x_{31}, \cdots, x_{n1}\\) を掃きだすのに伴って, 
実際の計算を要される部分というのは

$$
\begin{array}{cccc}
x_{22} & x_{23} & \cdots & x_{2n} \\
x_{32} & x_{33} & \cdots & x_{3n} \\
\vdots & \vdots & \ddots & \vdots \\
x_{n2} & x_{n3} & \cdots & x_{nn}
\end{array}
$$

の箇所であることがいえる. 
この部分の成分数は \\((n-1)^2\\)
なので, 一回目の前進消去では \\((n-1)^2\\) 回の計算を行うことがわかる.
次に, 二回目の前進消去では \\(x_{22}\\) を使って, 
それと同列のそれよりも下の行の成分 \\(x_{32},\cdots,x_{n2}\\) を先と同様に消していくわけだが,
ここでも同様, \\(x_{32},\cdots,x_{n2}\\) は必ず $0$ なので,
実際に計算を行う部分は

$$
\begin{array}{ccc}
x_{33} & \cdots & x_{3n} \\
\vdots & \ddots & \vdots \\
x_{n3} & \cdots & x_{nn}
\end{array}
$$

の箇所である. 
この部分の成分数は \\((n-2)^2\\)
なので, 二回目の全身消去では \\((n-2)^2\\) 回の計算を行うことがわかる.
これを繰り返すと, 計算を行う部分が残り一成分となるまで同様の計算回数がかかることがわかるから

\\[(n-1)^2+(n-2)^2+\cdots+2^2+1^2
=\sum^{n}_{i=1}(i-1)^2\\]

ここで, 高校数学の教科書にも載っている定理(証明略)
\\(\sum^n_{j=1}j^2=\frac{n(n+1)(2n+1)}{6}\\)
を思い出せば, 先の式は \\(\sum^n_{i=1}(i-1)^2=\frac{n^3}{3}+n^2\\) 
と表せることがわかる.

続いて, 後退代入について考える.
後退代入は, 上三角行列となっている係数行列に対して代入を繰り返し, 
対角行列を形成していく操作であった.

$$\left(\begin{array}{ccccc}
x_{11} & x_{12} & x_{13} & \cdots & x_{1n} \\
& x_{22} & x_{23} & \cdots & x_{2n} \\
& & x_{33} & \cdots & x_{3n} \\
& & & \ddots & \vdots \\
&&&& x_{nn}
\end{array}\right)
\left(\begin{array}{c}a_1 \\ a_2 \\ a_3 \\ \vdots \\ a_n\end{array}\right)=
\left(\begin{array}{c}y_1 \\ y_2 \\ y_3 \\ \vdots \\ y_n\end{array}\right)
$$

この場合, まず \\(x_{nn}\\) を使って, 
\\(x_{1n},x_{2n},x_{3n},\cdots,x_{(n-1)n}\\) を消していくわけだが,
この際, 係数行列に対する操作というのはただ $0$ にしていくということだけである. 
これを \\(x_{22}\\) まで繰り返し行うわけだが, その間の係数行列に関する操作はただ $0$
にしていくということだけなので, これを計算量に含む必要はない.
実際に計算が発生するのは, 右辺ベクトルの部分である.
まず \\(y_n\\) に対する計算は, 後退代入の操作を考えれば, 当然なにもする必要はない.
次に \\(y_{n-1}\\) に対する計算は
\\((y_{n-1})'=y_{n-1}-\frac{x_{(n-1)n}}{x_{nn}}y_n\\) であり,
減算, 除算, 積算が各一回ずつ行われることがいえる.
次の \\(y_{n-2}\\) に対する計算も同様
\\(y_{n-2}-\frac{x_{(n-2)n}}{x_{(n-1)n}}(y_{n-1})'\\)
であり, 計算の形式は先と全く同じであるが, その前の後退代入の結果 \\((y_{n-1})'\\) 
を用いている点で計算量は異なる. 
つまり, いま計算したい右辺ベクトルの成分を計算するのには, 
その一つ下の右辺ベクトルの成分に対する計算結果が必要となる(後退代入の操作そのもの)ことから,
これが \\(y_1\\) にまで及ぶことを考えると, 後退代入の総回数は

\\[1+2+\cdots+(n-1)=\frac{n(n-1)}{2}\\]

とかける(右辺への式変形の証明は高校数学の教科書で扱われているので略).
ガウスの消去法は前進消去と後退代入の組み合わせであるので, その時間計算量は,

\\[O(\frac{n^3}{3}+\frac{n(n-1)}{2})\tag{2}\\]

ただし各項の中で最高次の係数だけを考えればよいので, ガウスの消去法の時間計算量は

\\[\frac{1}{3}O(n^3)\\]

である.

## ガウス・ジョルダン法

ガウスの名がつく行列を用いた線形方程式の直接解法には, 
今述べたガウスの消去法のほかにガウス・ジョルダン法というものがある.
これらは明確に区別されたところであまり意味がないような気もするが,
ガウスの消去法が上記のように係数行列となる部分を単位行列でない対角行列へと変形していったのに対し,
ガウス・ジョルダン法はそれを直接単位行列となるように変形していく点で異なる.
ガウス・ジョルダン法で同様にして計算していくと,

$$
\left(\begin{array}{cc|c}
1 & 2 & 3 \\
3 & 4 & 5
\end{array}\right)\to
\left(\begin{array}{cc|c}
\overbrace{1}^{\times (-3)} & \overbrace{2}^{\times (-3)} & \overbrace{3}^{\times (-3)} \\
\underline{3} & 4 & 5
\end{array}\right)\to
\left(\begin{array}{cc|c}
1 & 2 & 3 \\
0 & -2 & -4
\end{array}\right)\to
\left(\begin{array}{cc|c}
1 & 2 & 3 \\
\overbrace{0}^{\times (-\frac{1}{2})} & \overbrace{-2}^{\times (-\frac{1}{2})} & \overbrace{-4}^{\times (-\frac{1}{2})}
\end{array}\right) \\ \to
\left(\begin{array}{cc|c}
1 & 2 & 3 \\
0 & 1 & 2
\end{array}\right)\to
\left(\begin{array}{cc|c}
1 & \underline{2} & 3 \\
\overbrace{0}^{\times (-2)} & \overbrace{1}^{\times (-2)} & \overbrace{2}^{\times (-2)}
\end{array}\right)\to
\left(\begin{array}{cc|c}
1 & 0 & -1 \\
0 & 1 & 2
\end{array}\right) \\
\therefore (x,y)=(-1,2)
$$

これは掃き出し法とも言われることがある. 
ガウス・ジョルダン法の計算量は \\(\frac{1}{2}O(n^3)\\) なので(導出は省略),
ガウス・ジョルダン法をわざわざ用いるシーンはあまりない.


## ピボッティング

ところで, いま示したガウスの消去法, ガウスジョルダン法の手順は, このままでは困る場合がある. 
例えば, 次の線形方程式をガウスの消去法で解いてみると

$$
\left(\begin{array}{cccc|c}
1 & 2 & 7 & 6 & 6 \\
2 & 4 & 4 & 2 & 2 \\
1 & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
\overbrace{1}^{\times (-2)} & \overbrace{2}^{\times (-2)} & \overbrace{7}^{\times (-2)} & \overbrace{6}^{\times (-2)} & \overbrace{6}^{\times (-2)} \\
\underline{2} & 4 & 4 & 2 & 2 \\
1 & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
1 & 2 & 7 & 6 & 6 \\
0 & 0 & -10 & -10 & -10 \\
1 & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right) \\ \to
\left(\begin{array}{cccc|c}
\overbrace{1}^{\times (-1)} & \overbrace{2}^{\times (-1)} & \overbrace{7}^{\times (-1)} & \overbrace{6}^{\times (-1)} & \overbrace{6}^{\times (-1)} \\
0 & 0 & -10 & -10 & -10 \\
\underline{1} & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
1 & 2 & 7 & 6 & 6 \\
0 & 0 & -10 & -10 & -10 \\
0 & 6 & -2 & -4 & 6 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
\overbrace{1}^{\times (-2)} & \overbrace{2}^{\times (-2)} & \overbrace{7}^{\times (-2)} & \overbrace{6}^{\times (-2)} & \overbrace{6}^{\times (-2)} \\
0 & 0 & -10 & -10 & -10 \\
0 & 6 & -2 & -4 & 6 \\
\underline{2} & 4 & 3 & 3 & 5
\end{array}\right) \\ \to
\left(\begin{array}{cccc|c}
1 & 2 & 7 & 6 & 6 \\
0 & 0 & -10 & -10 & -10 \\
0 & 6 & -2 & -4 & 6 \\
0 & 0 & -11 & -9 & -7
\end{array}\right)\to
\left(\begin{array}{cccc|c}
1 & 2 & 7 & 6 & 6 \\
0 & \overbrace{0}^{\times (-\frac{6}{0})} & \overbrace{-10}^{\times (-\frac{6}{0})} & \overbrace{-10}^{\times (-\frac{6}{0})} & \overbrace{-10}^{\times (-\frac{6}{0})} \\
0 & \underline{6} & -2 & -4 & 6 \\
0 & 0 & -11 & -9 & -7
\end{array}\right)
$$

というように $0$ で割るということが起きてしまうのである.
これを防ぐために考えられる方法としては,
掃きだすのに利用する値がその列の中で絶対値最大となるように行を入れ替える.
これは, 部分ピボット選択付きガウスの消去法といわれる. 
ピボットとは, いま述べた掃きだすのに利用する値のことである. 
部分ピボット選択といわれる理由は, 入れ替えの操作が行に対してのみ行われるからである.
列に対する入れ替え操作をも含んだ方法は完全ピボット選択といわれるが,
当然それは行基本変形の範疇でないので,
そのまま計算を続行して単に右辺ベクトルを取り出せばよいという話ではなくなる.
完全ピボット選択は, 絶対値最大の値の選択の余地が部分ピボット選択よりも当然広がるので,
直感的に, より大きな絶対値の値をピボットとして選択できる確率が上がることは考えられるだろう.
しかしながら, このアドバンテージは当然行列に依存したものであり,
プログラムの複雑度が上がることに釣り合っていないため,
実際に用いられるようなことはあまりないと思われる.

というわけで, 
下記に部分ピボット選択付きガウスの消去法による計算過程を省略することなく書き下したが,
とくに面白みもない上に長いので隠しておいた.
<em onclick="obj=document.getElementById('open_gaussian_piv_com').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">クリックで展開.</a>
</em>
<div id="open_gaussian_piv_com" style="display:none;clear:both;">

$$
\left(\begin{array}{cccc|c}
1 & 2 & 7 & 6 & 6 \\
2 & 4 & 4 & 2 & 2 \\
1 & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 2 & 2 \\
1 & 2 & 7 & 6 & 6 \\
1 & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
\overbrace{2}^{\times(-\frac{1}{2})} & \overbrace{4}^{\times(-\frac{1}{2})} & \overbrace{4}^{\times(-\frac{1}{2})} & \overbrace{2}^{\times(-\frac{1}{2})} & \overbrace{2}^{\times(-\frac{1}{2})} \\
\underline{1} & 2 & 7 & 6 & 6 \\
1 & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\\ \to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 2 & 2 \\
0 & 0 & 5 & 5 & 5 \\
1 & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
\overbrace{2}^{\times(-\frac{1}{2})} & \overbrace{4}^{\times(-\frac{1}{2})} & \overbrace{4}^{\times(-\frac{1}{2})} & \overbrace{2}^{\times(-\frac{1}{2})} & \overbrace{2}^{\times(-\frac{1}{2})} \\
0 & 0 & 5 & 5 & 5 \\
\underline{1} & 8 & 5 & 2 & 12 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 2 & 2 \\
0 & 0 & 5 & 5 & 5 \\
0 & 6 & 3 & 1 & 11 \\
2 & 4 & 3 & 3 & 5
\end{array}\right)\\ \to
\left(\begin{array}{cccc|c}
\overbrace{2}^{\times (-1)} & \overbrace{4}^{\times (-1)} & \overbrace{4}^{\times (-1)} & \overbrace{2}^{\times (-1)} & \overbrace{2}^{\times (-1)} \\
0 & 0 & 5 & 5 & 5 \\
0 & 6 & 3 & 1 & 11 \\
\underline{2} & 4 & 3 & 3 & 5
\end{array}\right)\to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 2 & 2 \\
0 & 0 & 5 & 5 & 5 \\
0 & 6 & 3 & 1 & 11 \\
0 & 0 & -1 & 1 & 3
\end{array}\right)\to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 2 & 2 \\
0 & 6 & 3 & 1 & 11 \\
0 & 0 & 5 & 5 & 5 \\
0 & 0 & -1 & 1 & 3
\end{array}\right) \\ \to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 2 & 2 \\
0 & 6 & 3 & 1 & 11 \\
0 & 0 & 5 & 5 & 5 \\
0 & 0 & -1 & 1 & 3
\end{array}\right)\to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 2 & 2 \\
0 & 6 & 3 & 1 & 11 \\
0 & 0 & \overbrace{5}^{\times\frac{1}{5}} & \overbrace{5}^{\times\frac{1}{5}} & \overbrace{5}^{\times\frac{1}{5}} \\
0 & 0 & \underline{-1} & 1 & 3
\end{array}\right)\to
\underbrace{\left(\begin{array}{cccc|c}
2 & 4 & 4 & 2 & 2 \\
0 & 6 & 3 & 1 & 11 \\
0 & 0 & 5 & 5 & 5 \\
0 & 0 & 0 & 2 & 4
\end{array}\right)}_{上三角行列} \\ \to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & \underline{2} & 2 \\
0 & 6 & 3 & 1 & 11 \\
0 & 0 & 5 & 5 & 5 \\
0 & 0 & 0 & \overbrace{2}^{\times(-1)} & \overbrace{4}^{\times(-1)}
\end{array}\right) \to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 0 & -2 \\
0 & 6 & 3 & 1 & 11 \\
0 & 0 & 5 & 5 & 5 \\
0 & 0 & 0 & 2 & 4
\end{array}\right)\to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 0 & -2 \\
0 & 6 & 3 & \underline{1} & 11 \\
0 & 0 & 5 & 5 & 5 \\
0 & 0 & 0 & \overbrace{2}^{\times (-\frac{1}{2})} & \overbrace{4}^{\times (-\frac{1}{2})}
\end{array}\right) \\ \to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 0 & -2 \\
0 & 6 & 3 & 0 & 9 \\
0 & 0 & 5 & 5 & 5 \\
0 & 0 & 0 & 2 & 4
\end{array}\right)\to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 0 & -2 \\
0 & 6 & 3 & 0 & 9 \\
0 & 0 & 5 & \underline{5} & 5 \\
0 & 0 & 0 & \overbrace{2}^{\times (-\frac{5}{2})} & \overbrace{4}^{\times (-\frac{5}{2})}
\end{array}\right)\to
\left(\begin{array}{cccc|c}
2 & 4 & 4 & 0 & -2 \\
0 & 6 & 3 & 0 & 9 \\
0 & 0 & 5 & 0 & -5 \\
0 & 0 & 0 & 2 & 4
\end{array}\right) \\ \to
\left(\begin{array}{cccc|c}
2 & 4 & \underline{4} & 0 & -2 \\
0 & 6 & 3 & 0 & 9 \\
0 & 0 & \overbrace{5}^{\times (-\frac{4}{5})} & \overbrace{0}^{\times (-\frac{4}{5})} & \overbrace{-5}^{\times (-\frac{4}{5})} \\
0 & 0 & 0 & 2 & 4
\end{array}\right) \to
\left(\begin{array}{cccc|c}
2 & 4 & 0 & 0 & 2 \\
0 & 6 & 3 & 0 & 9 \\
0 & 0 & 5 & 0 & -5 \\
0 & 0 & 0 & 2 & 4
\end{array}\right) \to
\left(\begin{array}{cccc|c}
2 & 4 & 0 & 0 & 2 \\
0 & 6 & \underline{3} & 0 & 9 \\
0 & 0 & \overbrace{5}^{\times (-\frac{3}{5})} & \overbrace{0}^{\times (-\frac{3}{5})} & \overbrace{-5}^{\times (-\frac{3}{5})} \\
0 & 0 & 0 & 2 & 4
\end{array}\right) \\ \to
\left(\begin{array}{cccc|c}
2 & 4 & 0 & 0 & 2 \\
0 & 6 & 0 & 0 & 12 \\
0 & 0 & 5 & 0 & -5 \\
0 & 0 & 0 & 2 & 4
\end{array}\right) \to
\left(\begin{array}{cccc|c}
2 & \underline{4} & 0 & 0 & 2 \\
0 & \overbrace{6}^{\times(-\frac{2}{3})} & \overbrace{0}^{\times(-\frac{2}{3})} & \overbrace{0}^{\times(-\frac{2}{3})} & \overbrace{12}^{\times(-\frac{2}{3})} \\
0 & 0 & 5 & 0 & -5 \\
0 & 0 & 0 & 2 & 4
\end{array}\right) \to
\left(\begin{array}{cccc|c}
2 & 0 & 0 & 0 & -6 \\
0 & 6 & 0 & 0 & 12 \\
0 & 0 & 5 & 0 & -5 \\
0 & 0 & 0 & 2 & 4
\end{array}\right) \\
\therefore (-6\div 2,12\div 6, -5\div 0, 4\div 2)^T =(-3,2,-1,2)^T
$$
</div>


## LU 分解

漸く本題の LU 分解(LR 分解, 三角分解)について.
簡単のために式 \\((1)\\) を使って LU 分解の導出をする.
式 \\((1)\\) は次の式と同値である.

$$
A^{(0)}\left(\begin{array}{c}x \\ y\end{array}\right)={\boldsymbol v}
\ {\rm where}\ A^{(0)}=\left(\begin{array}{cc}1 & 2 \\ 3 & 4\end{array}\right),{\boldsymbol v}=\left(\begin{array}{c}3\\ 5\end{array}\right)
$$

\\({A^{(0)}}^{-1}{\boldsymbol v}\\) とすれば \\((x,y)^T\\) は求まるが, 逆行列の計算はガウスの消去法により \\(\frac{1}{3}O(n^3)\\) の時間計算量がかかる.
一定の条件下でそれよりも高速に求める方法を考えることとする.
いま \\(A^{(0)}\\) を徐に上三角行列にすることを考えると, ガウスの消去法の前進消去より

$$
A^{(1)}=L^{(1)}A^{(0)}=\left(\begin{array}{cc}1 & 2 \\ 0 & -2\end{array}\right)
\leftrightarrow
A^{(0)}={L^{(1)}}^{-1}A^{(1)}
\ {\rm where}\ L^{(1)}=\left(\begin{array}{cc}1 & 0 \\ -3 & 1\end{array}\right)
$$

従って

$$
{L^{(1)}}^{-1}A^{(1)}\left(\begin{array}{c}x \\ y\end{array}\right)={\boldsymbol v}
$$

ここで, \\({\boldsymbol b}=A^{(1)}(x,y)^T\\) とおくと, 上の式は \\({L^{(1)}}^{-1}{\boldsymbol b}={\boldsymbol v}\\) と同値であり,
この式を用いて \\({\boldsymbol b}\\) について解くことができる. 
まずこの時間計算量を考えるとする.
\\(L^{(1)}\\) は元々前進消去のための行列であり, それは必ず下三角行列である.
正則な下三角行列の逆行列は下三角行列であり(証明略), いま \\(L^{(1)}\\) が正則であるとする(これが特異となるような場合には後述する PLU 分解が有効)と,
その計算は前進代入(上記後退代入の下三角行列バージョンと考えればよい)を実行すればよいので, 時間計算量は \\(\frac{1}{2}O(n^2)\ \because\\) となる.

その後に \\(A^{(1)}(x,y)^T={\boldsymbol b}\\) を \\((x,y)^T\\) について解くわけであるが,
\\(A^{(1)}\\) は上三角行列であるので, その計算にはガウスの消去法の後退代入を実行すれば良く, 従ってその時間計算量は 
\\(\frac{1}{2}O(n^2)\ \because\\) である.

よって, この一連の操作における時間計算量は \\(\frac{1}{3}O(n^3)\\) であり, 部分ピボットつきガウスの消去法を実行した場合と変わらない.

しかし, \\(L U\\) を流用できる(つまり, 共通の \\(A^{(0)}\\) に対し異なる右辺ベクトル \\({\boldsymbol v}\\) から成る連立方程式を解く)とすればどうだろう.
この場合, やらなければならない計算は前進代入および後退代入のみなので,
全体の時間計算量は \\(\frac{1}{2}O(n^2)\\) となり, 先よりも高速に解を得ることができる.

いまの説明では, 式 \\(\\) において \\(A^{(0)}\\) を \\({L^{(1)}}^{-1}\\) と \\(A^{(1)}\\) に分解したが, これを LU 分解(\\(L={L^{(1)}}^{-1},U=A^{(1)}\\))といい, 
\\(L^{(i)}\\) が正則ならば, 一般の場合においても同様にしていうことができる.

<div class="m-def">
<header class="m-def-title"><p><span id="LU">LU 分解 (外積形式ガウス法)</span></p></header>
<div class="m-def-content">
すべての前進消去の行列 \\(L^{(i)}\\) が正則ならば \\(A\in\mathbb{R}^{m\times n}\\) に対する LU 分解は
\\[A=A^{(0)}=L U\ {\rm where}\ L=\prod_{i=1}^{n-1}{L^{(i)}}^{-1}, U=A^{(n-1)}\\]
</div>
</div>

補足すると, \\(A^{(0)}\\) に対し \\(n-1\\) 回の前進消去をするというのは, \\(L^{(n-1)}\cdots L^{(2)}L^{(1)}A^{(0)}\\) ということであり,
\\(\lambda=L^{(n-1)}\cdots L^{(2)}L^{(1)}\\) とおくと \\(\lambda A^{(0)}=U=A^{(n-1)}\\) だから \\(A^{(0)}=\lambda^{-1}U\\). 
ここで逆行列の公式 \\((X Y)^{-1}=Y^{-1} X^{-1}\\) より(証明略)上式となる.
一般論を得たところで, 実際に一つ LU 分解を実践してみることとする.

$$
A^{(0)}=
\left(\begin{array}{ccc}
3 & 1 & 0 \\
6 & 1 & -2 \\
-3 & 0 & 3
\end{array}\right)\to
\underbrace{\left(\begin{array}{ccc}
1 & 0 & 0 \\
-2 & 1 & 0 \\
1 & 0 & 1
\end{array}\right)}_{L^{(1)}}
\left(\begin{array}{ccc}
3 & 1 & 0 \\
6 & 1 & -2 \\
-3 & 0 & 3
\end{array}\right)=
\underbrace{
\left(\begin{array}{ccc}
3 & 1 & 0 \\
0 & -1 & -2 \\
0 & 1 & 3
\end{array}\right)
}_{A^{(1)}} \\
\to
\underbrace{
\left(\begin{array}{ccc}
1 & 0 & 0 \\
0 & 1 & 0 \\
0 & 1 & 1
\end{array}\right)
}_{L^{(2)}}
A^{(1)}=
\underbrace{
\left(\begin{array}{ccc}
3 & 1 & 0 \\
0 & -1 & -2 \\
0 & 0 & 1
\end{array}\right)}_{A^{(2)}}
$$

より $$L=(L^{(2)}L^{(1)})^{-1}=\left(\begin{array}{ccc} 1 & 0 & 0 \\ 2 & 1 & 0 \\ -1 & -1 & 1 \end{array}\right),U=A^{(2)}$$
実際には, すべてを減算で考えることで, \\((L^{(2)}L^{(1)})^{-1}\\) の計算は楽に済む(つまり
$A^{0}=({\boldsymbol a_1}^T,{\boldsymbol a_2}^T,{\boldsymbol a_3}^T)^T$ としたとき
\\(\underline{2}{\boldsymbol a_1}-{\boldsymbol a_2}, \underline{-1}{\boldsymbol a_1}-{\boldsymbol a_3}, \underline{-1}{\boldsymbol a_2}-{\boldsymbol a_3}\\) より \\(L\\) が導けるということ).

この導出過程を見ればなんとなく LU 分解が一意となることは直感的にも納得できるが, 一応証明を与えておく.

<div class="m-thm">
<header class="m-thm-title"><span id="theorem1"><p>定理 1</p></span></header>
<div class="m-thm-content">
LU 分解された \\(L,U\\) は一意に決まる
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>定理 1</p></header>
<div class="m-proof-content">
正則行列 \\(A\\) の LU 分解 \\(A= L U\\) に対して \\(A= L_{1}U_{1} = L_{2}U_{2} \leftrightarrow L_{2}^{-1}L_{1} = U_{2}U_{1}^{-1}\\) とおく.
\\(L\\) は元々下三角行列であり下三角行列の逆行列は下三角行列, また下三角行列の積は下三角行列だから \\(L_{2}^{-1}L_{1}\\) は下三角行列である.
\\(U\\) は元々上三角行列であり上三角行列の逆行列は上三角行列, また上三角行列の積は上三角行列だから \\(U_{2}U_{1}^{-1}\\) は上三角行列である.
従って, 両行列は上および下三角行列でなければならず, それを満たす唯一の行列は対角行列であり, \\(L\\) の対角成分は元々 \\(1\\) であるから
\\(L_{2}^{-1}L_{1} = I = U_{2}U_{1}^{-1}\\). 
故に \\(L_{2}=L_{1}, U_{2}=U_{1}\\). 
</div>
</div>

次に, 次のような行列に対する LU 分解を考えてみる.

\\[
\begin{aligned}
A=\left(\begin{array}{ccc}
0 & 1 & 0 \\
-8 & 8 & 1 \\
2 & -2 & 0
\end{array}\right)
\end{aligned}
\\]

見てわかるように, 前進消去の段階で \\(-8\div 0\\) となってしまい計算できない.
しかし, これも部分ピボット選択付きガウスの消去法と同様に, 絶対値最大の値がピボットとなるように行を予め入れ替えておけば,
計算が続行できる.
そのような手続きのある LU 分解は PLU 分解といわれ, 置換行列 \\(P\in\mathbb{R}^{m\times n}\\) をつかって, \\(A=P L U\\) とする.
以下 \\(A\\) をつかって導出してみることとする. \\({\boldsymbol a_1}^T\\) と \\({\boldsymbol a_2}^T\\) を入れ替えれば良いので,

\\[
\begin{aligned}
\underbrace{\left(\begin{array}{ccc}
0 & 1 & 0 \\
1 & 0 & 0 \\
0 & 0 & 1
\end{array}\right)}_{P^{(1)}}A=
\underbrace{\left(\begin{array}{ccc}
-8 & 8 & 1 \\
0 & 1 & 0 \\
2 & -2 & 0
\end{array}\right)}_{A'}\to
\underbrace{\left(\begin{array}{ccc}
1 & 0 & 0 \\
0 & 1 & 0 \\
\frac{1}{4} & 0 & 1
\end{array}\right)}_{L^{(1)}}A'=
\underbrace{\left(\begin{array}{ccc}
-8 & 8 & 1 \\
0 & 1 & 0 \\
0 & 0 & \frac{1}{4}
\end{array}\right)}_{A'^{(1)}}
\end{aligned}
\\]

より 

$$L = {L^{(1)}}^{-1}=\left(\begin{array}{ccc}1 & 0 & 0 \\ 0 & 1 & 0 \\ -\frac{1}{4} & 0 & 1\end{array}\right),U={A'}^{(1)}$$

とおくと \\(A'={L^{(1)}}^{-1}{A'}^{(1)}\\) だから

$$P^{(1)}A=L U$$

\\(P^{(1)}\\) は元々置換行列であるから正則であり, また直行行列でもある. 
すなわち \\({P=P^{(1)}}^{-1}={P^{(1)}}^T\\) とおけて

\\[
\begin{aligned}
A&=&P L U\\
\left(\begin{array}{ccc}
0 & 1 & 0 \\
-8 & 8 & 1 \\
2 & -2 & 0
\end{array}\right)&=&
\left(\begin{array}{ccc}
0 & 1 & 0 \\
1 & 0 & 0 \\
0 & 0 & 1
\end{array}\right)
\left(\begin{array}{ccc}
1 & 0 & 0 \\
0 & 1 & 0 \\
-\frac{1}{4} & 0 & 1
\end{array}\right)
\left(\begin{array}{ccc}
-8 & 8 & 1 \\
0 & 1 & 0 \\
0 & 0 & \frac{1}{4}
\end{array}\right)
\end{aligned}
\\]

ここで \\({\boldsymbol a}\in\mathbb{R}^{n\times 1},{\boldsymbol v}\in\mathbb{R}^{n\times 1}\\) に対して
\\(P L U {\boldsymbol a}={\boldsymbol v}\\) というように, PLU 分解を用いて連立方程式解くことを考えると,
\\(L\underbrace{(U{\boldsymbol a})}_{{\boldsymbol b}}=P^{-1}{\boldsymbol v}\\) だから先と同様にまず前進代入によって
\\({\boldsymbol b}\\) を求め(このときの \\(P^{-1}{\boldsymbol v}\\) は \\(P^{-1}=P^{(1)}\\) であり, また置換行列であるので, その計算は単に \\({\boldsymbol v}\\) を並び替えるだけである),
\\(U{\boldsymbol a}={\boldsymbol b}\\) を \\({\boldsymbol a}\\) について後退代入によって求めればよい.

さて, 数学的な言葉では, 以上のように書き下すことで十分であるが,
これをプログラムで組むことを考えると, 様々な工夫やアプローチが考えられる.
まず \\(L, U\\) はそれぞれ必ず上三角行列, 
下三角行列であり $0$ や $1$ の部分をそのまま持っているのは無駄である.
従って \\(L, U\\) の値は次のように一つの行列として持っておけば十分.

$$
\left(\begin{array}{ccc}
-8 & 8 & 1 \\
0 & 1 & 0 \\
-\frac{1}{4} & 0 & \frac{1}{4}
\end{array}\right)
$$

また, 置換行列 $P$ はただの並び替えでなので, これも各インデックスへの対応関係をテーブルにでもしておけば十分.
サンプル実装, 実行例を下記に示す.
Haskell で書いたわけだが, 普通の ST モナドによる実装とリストによる実装をそれぞれ行った.
<em onclick="obj=document.getElementById('open_lu').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">クリックで実装を開く.</a>
</em>
<div id="open_lu" style="display:none;clear:both;">
<div class="mb-2 mt-2" style="max-height: 400px; overflow-y: scroll;">
<script src="https://emgithub.com/embed.js?target=https%3A%2F%2Fgithub.com%2Ffalgon%2FPlayLinearAlgebra%2Fblob%2F7a387f803e1ee30d3e7e83915b88eea18d7e096b%2Fsrc%2FMath%2FMatrix%2FCore.hs%23L139-L217&style=github&showLineNumbers=on&showFileMeta=on&fetchFromJsDelivr=on"></script>
</div>
</div>
```haskell
λ> :m Data.Array Math.Matrix.LU
λ> lu [[0,1,0],[-8,8,1],[2,-2,0]] :: Maybe (PLU Array Int Double)
Just (array (0,2) [(0,1),(1,0),(2,2)],
{       -8.0    8.0     1.0     }
{       -0.0    1.0     0.0     }
{       -0.25   0.0     0.25    }
)
λ> luST' [[0,1,0],[-8,8,1],[2,-2,0]] :: Maybe (PLU Array Int Double)
Just (array (0,2) [(0,1),(1,0),(2,2)],
{       -8.0    8.0     1.0     }
{       -0.0    1.0     0.0     }
{       -0.25   0.0     0.25    }
)
```

連立方程式を解く.
<em onclick="obj=document.getElementById('open_resolveLinearEq').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">クリックで実装を開く.</a>
</em>
<div id="open_resolveLinearEq" style="display:none;clear:both;">
<div class="mb-2 mt-2" style="max-height: 400px; overflow-y: scroll;">
<script src="https://emgithub.com/embed.js?target=https%3A%2F%2Fgithub.com%2Ffalgon%2FPlayLinearAlgebra%2Fblob%2F7a387f803e1ee30d3e7e83915b88eea18d7e096b%2Fsrc%2FMath%2FMatrix%2FCore.hs%23L259-L305&style=github&showLineNumbers=on&showFileMeta=on&fetchFromJsDelivr=on"></script>
</div>
</div>
```haskell
λ> resolveLinearEq' [[1,2,7,6],[2,4,4,2],[1,8,5,2],[2,4,3,3]] [6,2,12,5] :: Maybe (Array Int Rational)
Just (array (0,3) [(0,(-3) % 1),(1,2 % 1),(2,(-1) % 1),(3,2 % 1)])
λ> plu = luST' [[1,2,7,6],[2,4,4,2],[1,8,5,2],[2,4,3,3]] :: Maybe (PLU Array Int Double)
λ> plu
Just (array (0,3) [(0,1),(1,2),(2,0),(3,3)],
{       2.0     4.0     4.0     2.0     }
{       0.5     6.0     3.0     1.0     }
{       0.5     0.0     5.0     5.0     }
{       1.0     0.0     -0.2    2.0     }
)
λ> fromJust plu `assign` (listArray (0, 3) [6,2,12,5]) :: Maybe (Array Int Double)
Just (array (0,3) [(0,-3.0),(1,2.0),(2,-1.0),(3,2.0)])
λ> fromJust plu `assign` (listArray (0, 3) [1,2,3,4]) :: Maybe (Array Int Double)
Just (array (0,3) [(0,0.6666666666666667),(1,0.6666666666666666),(2,-1.0),(3,1.0)])
λ> fromJust plu `assign` (listArray (0, 3) [5,6,7,8]) :: Maybe (Array Int Double)
Just (array (0,3) [(0,1.666666666666667),(1,0.8666666666666667),(2,-0.8),(3,1.2)])
```

ところで, LU 分解をしておくと逆行列も簡単に求めることがすぐに示せる.
逆行列とはそもそも \\(A A^{-1} =I\\) であり, 
\\(A^{-1}=({\boldsymbol a^{-1}_1},{\boldsymbol a^{-1}_2},\cdots,{\boldsymbol a^{-1}}_m), I=({\boldsymbol I_1},{\boldsymbol I_2},\cdots,{\boldsymbol I_m})\\) 
とすると行列の積の定義より \\(A {\boldsymbol a^{-1}_i}={\boldsymbol I_i}\ {\rm where\ } i\in\mathbb{Z}^{+}, 1\leq i\leq m\\) だから, 
\\(A=L U\\) と分解して \\(1\\) から \\(m\\) までのすべての \\({\boldsymbol a^{-1}}_i\\) を得てそれらをそのまま 1 つの行列とすればよい.
当然ながら, 構成される方程式のうち変わる部分は \\({\boldsymbol a^{-1}}_i\\) と \\(I_i\\) の部分だけなので, LU 分解は一度行うだけで済む.
プログラムでの実行例. 
<em onclick="obj=document.getElementById('open_inverse').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">クリックで実装を開く.</a>
</em>
<div id="open_inverse" style="display:none;clear:both;">
<div class="mb-2 mt-2" style="max-height: 400px; overflow-y: scroll;">
<script src="https://emgithub.com/embed.js?target=https%3A%2F%2Fgithub.com%2Ffalgon%2FPlayLinearAlgebra%2Fblob%2F7a387f803e1ee30d3e7e83915b88eea18d7e096b%2Fsrc%2FMath%2FMatrix%2FCore.hs%23L307-L324&style=github&showLineNumbers=on&showFileMeta=on&fetchFromJsDelivr=on"></script>
</div>
</div>

```haskell
λ> inverse' [[3,1,1],[5,1,3],[2,0,1]] :: Maybe (Matrix Array Int Rational)
Just
{       1 % 2   (-1) % 2        1 % 1   }
{       1 % 2   1 % 2   (-2) % 1        }
{       (-1) % 1        1 % 1   (-1) % 1        }
```

ただし, 逆行列の計算には今述べたようにすべての \\({\boldsymbol I^{-1}}_i\\) に関して代入操作を行わなければならないので,
\\(A{\boldsymbol x}={\boldsymbol v}\\) といった方程式を解く目的で逆行列 \\(A^{-1}\\) を求めることはただの愚行である.

また, LU 分解は行列式の計算も簡単にする. 
\\(A= L U\\) ならば積の行列式は行列式の積(証明略)なので \\(\left|A\right|=\left|L U\right|=\left|L\right|\left|U\right|\\) であるが,
上および下三角行列の行列式は対角成分の積(証明略)であるので \\(\left|L\right|=1\\) である.
よって \\(\left|A\right|=\prod_{i=1}^{n}{\boldsymbol u}_{ii}\\) である.
置換行列 \\(P\\) を考慮すれば, いま \\(S\\) を LU 分解の過程で行の入れ替えを行った回数としたとき,
\\(\left|A\right|=\left|P\right|\left|L\right|\left|U\right|=(-1)^S \prod_{i=1}^{n}{\boldsymbol u}_{ii}\\) となる.
また, 後述する Crout 法では \\(U\\) のすべての対角成分を $1$ とするので, その場合 \\(\left|A\right| = (-1)^S \prod_{i=1}^{n}{\boldsymbol l}_{ii}\\) となる.
<em onclick="obj=document.getElementById('open_det').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">クリックで実装を開く.</a>
</em>
<div id="open_det" style="display:none;clear:both;">
<div class="mb-2 mt-2" style="max-height: 400px; overflow-y: scroll;">
<script src="https://emgithub.com/embed.js?target=https%3A%2F%2Fgithub.com%2Ffalgon%2FPlayLinearAlgebra%2Fblob%2F7a387f803e1ee30d3e7e83915b88eea18d7e096b%2Fsrc%2FMath%2FMatrix%2FCore.hs%23L327-L340&style=github&showLineNumbers=on&showFileMeta=on&fetchFromJsDelivr=on"></script>
</div>
</div>
```haskell
λ> determinant $ toMat $ listArray ((0,0),(2,2)) [3,1,1,5,1,3,2,0,1]
2.0
λ> determinant $ toMat $ listArray ((0,0),(3,3)) [1,2,7,6,2,4,4,2,1,8,5,2,2,4,3,3]
120.0
```

なお, LU 分解は LDU 分解ともいわれることがある. その場合, 上記の \\(U\\) に含まれる対角成分を対角行列 \\(D\\) に分離して \\(A = L D U\\) とする.

$$
L D U=
\left(\begin{array}{cccc}
1 & 0 & \cdots & 0 \\
l_{21} & 1 & \cdots & 0 \\
\vdots & \vdots & \ddots & \vdots \\
l_{m1} & l_{m2} & \cdots & 1
\end{array}\right)
\mathrm{diag}(d_1,\cdots,d_m)
\left(\begin{array}{cccc}
1 & u_{12} & \cdots & u_{1n} \\
0 & 1 & \cdots & u_{2n} \\
\vdots & \vdots & \ddots & \vdots \\ 
0 & 0 & \cdots & 1
\end{array}\right)
$$

ところで, \\(A\in\mathbb{R}^{m\times n}\\) が対称行列ならば, この LDU 分解は \\(A=L D L^{T}\\) と計算することができる.

<div class="m-thm">
<header class="m-thm-title"><p><span id="theorem2">定理 2</span></p></header>
<div class="m-thm-content">
\\(A=A^T\ {\rm where}\ A\in\mathbb{R}^{m\times n}\\) ならば \\(A\\) の LDU 分解は \\(A=L D L^{T}\\)
</div>
</div>

<div class="m-proof">
<header class="m-proof-title"><p>定理 2</p></header>
<div class="m-proof-content">
\\(M^T = D^{-1}U\\) とし \\(A\\) の LDU 分解を \\(A= L D M^T\\) とおくと \\[A = A^T = (L D M^T)^T = M D L^T = L U\\]
ここで, 第二辺から第三辺への変形は, 積の転置は積の左右を入れ替えた転置の積なる公式を用いた.
このとき \\(M(D L^T)\\) と \\(L U\\) は LU 分解の 2 つの表現であるが, [定理 1](#theorem1) より LU 分解は一意であるから
\\(M=L\\) でなければならない(後述する Crout 法の LU 分解ならば \\(M=L D\\) でなければならない. 導かれる結論は同じ).
従って \\(L D M^T = L D L^T\\). 
</div>
</div>

ここまで述べてきた LU 分解の方法は, 外積形式ガウス法といわれるものであるが,
LU 分解の他の方法として内積形式ガウス法(以下 Doolittle 法), クラウト法がある. 
簡単のために \\(n=3\\) として, 行列 \\(X\\) を次のように下三角行列 \\(L\in\mathbb{R}^{3\times 3}\\) 
と上三角行列 \\(U\in\mathbb{R}^{3\times 3}\\) に分解することを考える.
このとき \\(X = L' D U'\\) に対して \\(L = L' D\\) とおいて \\(X = L U'\\) というように分解できることを過程して行列 \\(L,U'\\) を導出することを Doolittle 法,
また \\(U=D U'\\) とおいて \\(X = L' U\\) というように分解できることを過程して行列 \\(L' U\\) を導出することを Crout 法という.

\\[
\begin{aligned}
X= L U'\leftrightarrow 
\left(\begin{array}{ccc}
x_{11} & x_{12} & x_{13} \\
x_{21} & x_{22} & x_{23} \\
x_{31} & x_{32} & x_{33}
\end{array}\right)=\left(\begin{array}{ccc}
1 & 0 & 0 \\
l_{21} & 1 & 0 \\
l_{31} & l_{32} & 1
\end{array}\right)\left(\begin{array}{ccc}
u_{11} & u_{12} & u_{13} \\
0 & u_{22} & u_{23} \\
0 & 0 & u_{33}
\end{array}\right)&:=&{\rm Doolittle\ 法} \\
X= L' U\leftrightarrow 
\left(\begin{array}{ccc}
x_{11} & x_{12} & x_{13} \\
x_{21} & x_{22} & x_{23} \\
x_{31} & x_{32} & x_{33}
\end{array}\right)=\left(\begin{array}{ccc} 
l_{11} & 0 & 0 \\
l_{21} & l_{22} & 0 \\
l_{31} & l_{32} & l_{33} 
\end{array}\right)\left(\begin{array}{ccc}
1 & u_{12} & u_{13} \\
0 & 1 & u_{23} \\
0 & 0 & 1
\end{array}\right)&:=&{\rm Crout\ 法}
\end{aligned}
\\]

いま行列 \\(X\\) を Doolittle 法により LU 分解できたならば
\\(L U'\\) を単に計算して \\(X=L U'\\) は次のようにかけるはずである.

$$
\left(\begin{array}{ccc}
u_{11} & u_{12} & u_{13} \\
l_{21}u_{11} & l_{21}u_{12}+u_{22} & l_{21}u_{13}+u_{23} \\
l_{31}u_{11} & l_{31}u_{12}+l_{32}u_{22} & l_{31}u_{13}+l_{32}u_{23}+u_{33}
\end{array}\right)=
\left(\begin{array}{ccc}
1 & 0 & 0 \\
l_{21} & 1 & 0 \\
l_{31} & l_{32} & 1
\end{array}\right)\left(\begin{array}{ccc}
u_{11} & u_{12} & u_{13} \\
0 & u_{22} & u_{23} \\
0 & 0 & u_{33}
\end{array}\right)
$$

よって, 行列 \\(X\\) 
の成分で行列 
$L, U'$ を次のように書き換えることができる.

\\[
\begin{aligned}
\left(\begin{array}{ccc}
x_{11} & x_{12} & x_{13} \\
x_{21} & x_{22} & x_{23} \\
x_{31} & x_{32} & x_{33} 
\end{array}\right)&=&
\left(\begin{array}{ccc}
1 & 0 & 0 \\ 
\frac{x_{21}}{u_{11}} & 1 & 0 \\
\frac{x_{31}}{u_{11}} & l_{32} & 1
\end{array}\right)
\left(\begin{array}{ccc}
u_{11} & u_{12} & u_{13} \\
0 & u_{22} & u_{23} \\
0 & 0 & u_{33}
\end{array}\right)\because
\begin{array}{l}
x_{21}=l_{21}u_{11}\leftrightarrow l_{21}=\frac{x_{21}}{u_{11}}, \\
l_{31}\ {\rm についても同様}
\end{array} \\
&=&
\left(\begin{array}{ccc}
1 & 0 & 0 \\
\frac{x_{21}}{x_{11}} & 1 & 0 \\
\frac{x_{31}}{x_{11}} & l_{32} & 1
\end{array}\right)\left(\begin{array}{ccc}
x_{11} & x_{12} & x_{13} \\
0 & u_{22} & u_{23} \\
0 & 0 & u_{33}
\end{array}\right)\because\ u_{11}=x_{11},u_{12}=x_{12},u_{13}=x_{13} \\
&=& 
\left(\begin{array}{ccc}
1 & 0 & 0 \\
\frac{x_{21}}{x_{11}} & 1 & 0 \\
\frac{x_{31}}{x_{11}} & l_{32} & 1
\end{array}\right)\left(\begin{array}{ccc}
x_{11} & x_{12} & x_{13} \\
0 & x_{22}-\frac{x_{21}}{x_{11}}x_{12} & x_{23}-\frac{x_{21}}{x_{11}}x_{13} \\
0 & 0 & u_{33} 
\end{array}\right)\because
\begin{array}{l}
x_{22}=l_{21}u_{12}+u_{22} \leftrightarrow u_{22}=x_{22}-l_{21}u_{12},\\
u_{23}\ {\rm についても同様}
\end{array} \\
&=& 
\left(\begin{array}{ccc}
1 & 0 & 0 \\
\frac{x_{21}}{x_{11}} & 1 & 0 \\
\frac{x_{31}}{x_{11}} & \frac{x_{32}-\frac{x_{31}}{x_{11}}x_{12}}{x_{22}-\frac{x_{21}}{x_{11}}x_{12}} & 1
\end{array}\right)\left(\begin{array}{ccc}
x_{11} & x_{12} & x_{13} \\
0 & x_{22}-\frac{x_{21}}{x_{11}}x_{12} & x_{23}-\frac{x_{21}}{x_{11}}x_{13} \\
0 & 0 & x_{33}-\frac{x_{31}}{x_{11}}x_{13}-\frac{x_{32}-\frac{x_{31}}{x_{11}}x_{12}}{x_{22}-\frac{x_{21}}{x_{11}}x_{12}}(x_{23}-\frac{x_{21}}{x_{11}}x_{13})
\end{array}\right)\\
&\because&
\begin{array}{l}
x_{32}=l_{31}u_{12}+l_{32}u_{22}\leftrightarrow l_{32}=\frac{x_{32}-l_{31}u_{12}}{u_{22}}, \\
u_{33}\ {\rm についても同様}
\end{array}
\end{aligned}
\\]

これをみると, 一行目, 一列目, 二行目, 二列目 \\(\cdots\\) と展開していくことで,
芋づる式に $L,U'$ が決まっていくことがわかる.
この作業を一般化すると, \\(u_{ij}\\) の導出およびそれによって得られた値で \\(l_{ij}\\) を導出する部分に分けることができる.
それぞれをいま漸化式で書くと

\\[
\begin{aligned}
{\rm Doolittle 法} &:=&
\begin{cases}
\begin{cases}
u_{1k}&=&x_{1k} \\
u_{ik}&=&x_{1k}-\sum^{i-1}_{j=1}l_{ij}u_{jk},\ (i=2,3,\cdots,k)
\end{cases} \\
l_{ik}=\frac{(x_{ik}-\sum^{k-1}_{j=1}l_{ij}u_{jk})}{u_{kk}},\ (i=k+1,k+2,\cdots,n)
\end{cases}
\end{aligned}
\\]

ただし \\(u_{kk}=0\\) の場合は計算できないので,
実際にはピボッティングを要することになるわけであるが,
\\(U\\) の \\(k\\) 番目の行が \\(L\\) の対応する列の前に計算されるという Doolittle 法の性質上,
このままではどの行が \\(k\\) 番目に来るのかを処理以前に知ることができない.
この問題は \\(l_{ik}\\) の分子を次のように計算することで自明に克服できる.

\\[s_i = x_{ik}-\sum^{k-1}_{j=1}l_{ij}u_{jk},\ (i=k,\cdots,n)\\]

これにより \\(s_i\\) の最大値を求め,
対応する行を入れ替えて最大の要素を \\(k\\) 行目に入れることができる.
交換後は \\(u_{kk}=s_{k}\\) となるが,
\\(U\\) の \\(k\\) 番目の行の他の要素はそれ以前と同様に計算することができ, 
対応する \\(L\\) の要素は \\(l_{ik}=\frac{s_i}{u_{kk}}\\) と得られる.

Crout 法も同様に, \\(X=L' U\\) を次のように書けるはずなので,

\\[
\begin{aligned}
\left(\begin{array}{ccc}
l_{11} & l_{11}u_{12} & l_{11}u_{13} \\
l_{21} & l_{21}u_{12}+l_{22} & l_{21}u_{13}+l_{22}u_{23} \\
l_{31} & l_{31}u_{12}+l_{32} & l_{31}u_{13}+l_{32}u_{23}+l_{33}
\end{array}\right)=
\left(\begin{array}{ccc}
l_{11} & 0 & 0 \\
l_{21} & l_{22} & 0 \\
l_{31} & l_{32} & l_{33}
\end{array}\right)
\left(\begin{array}{ccc}
1 & u_{12} & u_{13} \\
0 & 1 & u_{23} \\
0 & 0 & 1
\end{array}\right)
\end{aligned}
\\]

従ってこの作業の一般形は結果的に

\\[
\begin{aligned}
{\rm Crout 法} &:=&
\begin{cases}
l_{ik}&=&x_{ik}-\sum^{k-1}_{j=1}l_{ij}u_{jk},\ (i=k,k+1,\cdots,n)\\
u_{kj}&=&\frac{(x_{kj}-\sum^{k-1}_{i=1}l_{ki}u_{ij})}{l_{kk}},\ (j=k,k+1,\cdots,n)
\end{cases}
\end{aligned}
\\]

\\(L\\) の \\(k\\) 番目の列の要素を計算した後に最大値を求め,
\\(L\\) の要素を含む最初の \\(k-1\\)
列に対応する行列の行を交換できるため, 
Crout 法はピボットを簡単に選択できる.

## 参考文献

* Richard Hamming (1987) "Numerical Methods for Scientists and Engineers (Dover Books on Mathematics)" Dover Publications, ISBN 9780486652412
