---
title: Haskell で C コンパイラを作ってみた
date: 2020-03-18 00:00:00
tags: Haskell, Compiler
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

本エントリ投稿の 2, 3 ヶ月前に Haskell でスクラッチから x86-64 向けの C コンパイラを作った.
本エントリは, その記録である.

<!--more-->

## 動機/背景

動機としては, 私は 2020 年度の新卒として,
とある会社に技術者として入社することとなっており,
コンパイラの自作は, 社会人になる前に,
前々から一度はやっておきたいと思っていた事柄の一つであったこと,
また関数プログラミングとの関係性について探求したかったこと,
さらに, 一部には, 関数プログラミングはコンパイラ開発を容易にする
[^1] 
という認識があるが,
数学的構造の実用化の一つとも言える関数プログラミングに関する考察においては,
圏論的な理由付けによりその有用性を言うことができるはずであろうという,
私の中での何となくの予想が本当であるのかどうか, 確認したかったことから,
実際に Haskell で C コンパイラを作るに至った.
なお, 圏論の話題は再度別のエントリとしてまとめ, その後,
さらに別のエントリにそれと関連付いた話題としてまとめようと考えているため,
本エントリでは特に立ち入らず, 
あくまでも, 
Haskell で C コンパイラを作ってみたという単なる取り組みへの記録程度に止める.

## 成果

プロジェクトは, 次のリポジトリにて管理している.

<div class="has-text-centered mb-2">
<i class="fab fa-github fa-fw"></i>
<a href="https://github.com/falgon/htcc">falgon/htcc - A tiny C language compiler (x86-64) (WIP)</a>
</div>

執筆時最新のコミット 
[2301374](https://github.com/falgon/htcc/tree/230137475bf08265db9bd31ea65e2d867b1207fc) 
におけるコンパイル可能なコードは構文は, テストコードに記されている通りである. 
より実用的な (コンパイル可能な) サンプルコードは example 配下にある
(ナップザック問題, 連結リストのマージソート, Fisher–Yates シャッフルとクイックソート,
ライフゲームシミュレータ等).

htcc は標準 C 言語[^2]の構文の他に, 一部の GNU 拡張の構文を実装している. 
例えば, [Statement Expression](https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html) はそのうちの一つである. 
近年, Rust のような多くの"現代的な"言語は, 
文の構文を式として捉えている[^3]が, Statement Expression はそれと同様の機能, 
すなわち, C の compound-statement を式として捉える機能を提供する.
また, [Conditionals with Omitted Operands](https://gcc.gnu.org/onlinedocs/gcc/Conditionals.html)
もそのうちの一つである.
条件演算子は N1570 において次のように定義されているが

\\[
\begin{array}{llllll}
\text{conditional-expression}:\\
&\text{logical-OR-expression}\\
&\text{logical-OR-expression}&?&\text{expression}&:&\text{conditional-expression}
\end{array}
\\]

この expression オペランドが省略された次の構文

\\[
\begin{array}{lll}
\text{logical-OR-expression}&?:&\text{conditional-expression}
\end{array}
\\]

をサポートする.

htcc の機能そのものの説明は, 基本的に上記リポジトリの README.md に書かれている通りであるが, 
ここにコミット 
[2301374](https://github.com/falgon/htcc/tree/230137475bf08265db9bd31ea65e2d867b1207fc) 
時点での説明を再掲することとする.


![htcc の実行イメージ](https://raw.githubusercontent.com/falgon/htcc/230137475bf08265db9bd31ea65e2d867b1207fc/assets/some_operation.gif "htcc の実行イメージ")

コマンドラインオプションは次のようになっている.

```bash
$ stack exec htcc -- -h
Usage: htcc [--visualize-ast] [--img-resolution RESOLUTION] file [-o|--out file]
            [-w|--supress-warns]

Available options:
  -h,--help                Show this help text
  --visualize-ast          Visualize an AST built from source code
  --img-resolution RESOLUTION
                           Specify the resolution of the AST graph to be
                           generated (default: 640x480)
  file                     Specify the input file name
  -o,--out file            Specify the output destination file name, supported
                           only svg (default: ./out.svg)
  -w,--supress-warns       Disable all warning messages
```

例えば, 標準出力に `hello world` を出力する C ソースコードのコンパイルは, 次のように実行できる.

```bash
$ echo 'int printf(); int main() { printf("hello world!\n"); }' | stack exec htcc -- /dev/stdin | gcc -xassembler -no-pie -o out -  
```

htcc には, 内部で構築した構文木をベクタ画像として視覚化し,
出力する機能を実装してある[^4].
次の表は, 実行されるコマンドと出力されるベクタ画像の対応を示したものである.

<div class="table-responsive">
<table class="table table-bordered table-hover">
<thead><tr><th style="text-align: center;">コマンド</th><th style="text-align: center;">出力画像</th></tr></thead>
<caption id="karnaugh1" style="caption-side: bottom">htcc の構築した構文木のベクタ画像出力例</caption>
<tbody>
<tr>
<td>
<pre>$ echo 'int main() { return 1 * 2 + 4; }' |\
    stack exec htcc -- /dev/stdin\
        --visualize-ast\
        --img-resolution 640x480\
        --out calc.svg
</pre>
</td>
<td><img width="250px" class="img-responsive" src="https://raw.githubusercontent.com/falgon/htcc/230137475bf08265db9bd31ea65e2d867b1207fc/assets/example_ast/calc.png" alt="ast_graph"></td>
</tr>
<tr>
<td>
<pre>$ echo 'int printf();
void fizzbuzz(int n) { 
    for (int i = 1; i &lt; n; ++i) { 
        if (!(i % 15)) printf("fizzbuzz\n"); 
        else if (!(i % 3)) printf("fizz\n"); 
        else if (!(i % 5)) printf("buzz\n"); 
        else printf("%d\n", i); 
    } 
} 
int main() { fizzbuzz(50); }' |\
    stack exec htcc -- /dev/stdin\
        --visualize-ast\
        --img-resolution 1280x720\
        --out fizzbuzz.svg
</pre>
</td>
<td><img width="250px" class="img-responsive" src="https://raw.githubusercontent.com/falgon/htcc/230137475bf08265db9bd31ea65e2d867b1207fc/assets/example_ast/fizzbuzz.png" alt="ast_graph"></td>
</tr>
</tbody>
</table>
</div>

## 開発様相

コンパイラの開発には, 
『[低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook)』を参考とさせて頂いており, 
この内容から習うようにして, インクリメンタルなテスト駆動開発の手段をとることとした.
今回は, 動機に示された理由により, 
とくに Haskell での実装を進めたかったため, 
セルフホストコンパイラの開発という目的には一致していなかったが, 
同文書は, 具体的な開発順序や手段の詳細に関する, 多くの知見を与えてくださった.
同書の他に, コンパイラの構成には [References](https://github.com/falgon/htcc/tree/230137475bf08265db9bd31ea65e2d867b1207fc#references)-4 を参考とした. 
言語仕様は同書同様 N1570 に従い, 
ABI 等の仕様確認には [References](https://github.com/falgon/htcc/tree/230137475bf08265db9bd31ea65e2d867b1207fc#references)-1 を用いた. また, より理論的な参考としては,

* Benjamin C. Pierce. (2002). _Types and Programming Languages_. The MIT Press
* 中田育男. (2009). _コンパイラの構成と最適化_. 朝倉書店

が挙げられる.
また, 今回は, [gitmoji](https://gitmoji.carloscuesta.me/) のガイドラインに従って, 
コミットメッセージに絵文字を含めてみた.
これに大した理由はないが, やってみた結果としては, 後にコミットを見返した際に,
視覚的な印象により, 多少はその概要をより素早く見直すことができるような気はした.

## まとめ

これは, 字句解析器や構文解析器の自動生成ツールを用いずに x86-64 アセンブラを出力する
C コンパイラを作ってみるという目的の他, 私自身が関数プログラミングと圏論の関係性を学び, 
それをコンパイラ開発という一つの用途にあてはめたときに発見できる明確な有用性について,
私自身が議論できるようになる, 
という目的で行った取り組みであったが, 
C コンパイラはそれなりに動くところまで作れ, またモナドを利用した言語内 DSL による文脈の強制は,
コンパイラ開発の場面でも強力な機能であり,
その結果として, 生成されるコードの安全性を保証するに至るということも身を以て分かり,
新たな興味や疑問も多く湧いたので, 私自身にとっては非常に有意義な取り組みであった.

今後は, 生成コードの最適化, 質の良いエラーと警告情報の提供,
アドレスサニタイザに関して深掘りしていきたい.
また, いわゆるプログラム論理として言われる分野の応用による,
マルチステージプログラミング[^5]や, 定理証明支援等の分野には非常に興味があるため,
そのような方向へ広げていきたい.

[^1]: [Why is writing a compiler in a functional language easier? - stack overflow](https://stackoverflow.com/questions/2906064/why-is-writing-a-compiler-in-a-functional-language-easier) より. なお, 同質問は [opnion-based](https://stackoverflow.com/help/closed-questions) とされているため文中ではこれを一部の認識としている. 
[^2]: 本エントリでいう C 言語とは厳密に言えば C11 の最終ドラフトである N1570 のことを指す.
[^3]: 例えば, C の `if`, `else` は文であるが, Rust では三項式である. また, C の \\(\text{compound-statement}\\) は, Rust において `;` で区切られた一連の式に対応する.
[^4]: Special thanks to [diagrams-lib](https://hackage.haskell.org/package/diagrams-lib), [diagrams-svg](https://hackage.haskell.org/package/diagrams-svg) and [diagrams-contrib](https://hackage.haskell.org/package/diagrams-contrib)
[^5]: マルチステージプログラミングに関する記事は別途記述予定. 著者の興味としてまず目を引いたものとしては, Oleg Kiselyov. (2014). “_The Design and Imple-mentation of BER MetaOCaml System Descrip-tion_”, FLOPS 2014 であった. これは, 単刀直入に言えば, C++14 でマルチステージプログラミングを可能とするための言語拡張に関する研究である. 論文にはその理論のほかに, clang (というか LLVM コンパイラインフラストラクチャ) を用いた処理系の実装までもが示されているが, この実装に対して著者は以前[ほんの軽微なコントリビュート](https://github.com/meta-cpp/clang/pull/1)をした.
