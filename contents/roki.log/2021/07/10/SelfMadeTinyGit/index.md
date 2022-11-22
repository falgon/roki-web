---
title: 技術書典 11 にて Git (一部) を自作する記事を執筆した
date: 2021-07-10 16:14:17
tags: Haskell, Git
---

[技術書典 11](https://techbookfest.org/event/tbf11) に出展の
[KLab Tech Book Vol.8](https://techbookfest.org/product/6185615265628160) 第七章にて『ミニマル Git を自作しよう』という記事を執筆した.
<br />
技術書典 11 オンラインマーケットから電子 + 物理本セットは 1000 円, 電子版のみは無料で頒布されている.
<br />
**※追記：加筆・調整版を新規にリリースしました👉<https://note.com/_roki/n/n1e17f077c2eb>**

![[KLab Tech Book Vol.8](https://techbookfest.org/product/6185615265628160)](./tbf11.png){width=250px}

この本に関する公式的な案内は『[技術書典11で同人誌を頒布します & 既刊PDFダウンロードページ](https://www.klab.com/jp/blog/tech/2021/tbf11.html)』にて掲載中である.
本記事は, その記事執筆に関するレポートである.<br>

<!--more-->

なお,
このブログは個人ブログであり,
また注意書きとして,
既にサイトトップ部分で

> All opinions expressed by Author on this blog is solely Author’s opinions and
> do not reflect the opinions of the company to which I belong. 

と示しているので言うまでもないことではあるのだが,
記事の特性上, 社名が文章内に含まれることとなるため,
ここにあらためて明示する.
**ここに示された意見や感想等は,
私個人のものであり,
所属する組織を代表するものではない.**

## 内容について

大雑把に言えば, これを読むと, Git の内部構造 (Git オブジェクトモデル,
インデックス, データ構造) を理解し,
インデックス化からコミットまでの主要コマンド 8 つ (`add`, `cat-file`, `diff`, `hash-object`,
`init`, `ls-files`, `status`, `commit`) を実装できるようになるはず (?) の内容となっている.
Git にまつわる各種概念や用語, 仕様等の紹介を通じて, 概形を把握し, その後, 詳細な実装に立ち入っていく.
実装例の説明自体には Haskell を用いているが,
それほど技巧的なことはしていないつもりであるので, 拒絶反応をせずに,
むしろ, それなりに簡素な作りで実際に動かすことができるという点に着目していただけると嬉しい.

<div class="has-text-centered mt-3 mb-3">
<i class="fab fa-github fa-fw"></i>
<a href="https://github.com/falgon/hmgit">falgon/hmgit - HMGit (Haskell Minimal Git)</a>
</div>

実行例やオプション等の説明はすべて README に載っている.
実装をもし読む場合は, その参考になればと [Haddock](https://falgon.github.io/hmgit/)
も一応生成しておいた.
コードや設計についてはとくにここで新たに述べることもないので,
上記 KLab Tech Book Vol.8 やリポジトリの中身を確認してほしい.

<blockquote class="twitter-tweet" data-align="center"><p lang="ja" dir="ltr">無駄にこんな感じで遊べます <a href="https://t.co/L0vtu8fOpy">pic.twitter.com/L0vtu8fOpy</a></p>&mdash; Roki (@530506) <a href="https://twitter.com/530506/status/1413741996783534083?ref_src=twsrc%5Etfw">July 10, 2021</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

私の書いた記事では,
出来る限り図を挿入し,
Git にまつわる各種概念やコミット, ブランチ,
リファレンスの関係を視覚的に捉えられるよう意識した.
これは (文字情報だけでも Git
について理解することはできるのだろうが),
それら概念の関係図を頭の中で描いてみようとするときに発生するコストを抑えられるはずなので,
積極的に行った.
また, 記事の執筆過程での下調べ中に知った Git に関する細かい経緯や,
実装するにあたって若干ハマるかもしれないファイルモード等に関する話題等,
所々にニッチ (?) な内容のコラムを挿入するようにした.
書かれているコマンドについては, ページの進みにあわせて実行していくことができるようになっているはずである.

## 結

このような発出をしていくことは個人的にも好きなので, またの機会があれば続けていきたい.
個人的にはぜひ一読いただき, なにかリアクション等いただけると嬉しい.
そして, サンプル実装や文章構成への改善案,
また, もし間違い等に気付いた場合についても指摘をいただけると嬉しい.<br>
最後に, このような機会を設けてくださった各所関係者の皆様や,
私の文章の校生, レビュー等に関わってくださった共著者のみなさまへ, 改めてこの場を借りて感謝の念を表したい.

## 関連

- [KLab Tech Book Vol. 8 で Git (一部) を自作する章を書きました - KLablog](https://www.klab.com/jp/blog/tech/2021/20211209-techbook8-git.html)
