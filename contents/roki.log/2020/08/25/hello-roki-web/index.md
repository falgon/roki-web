---
title: 個人ページ, ブログを移行した
date: 2020-08-25 20:45:32
tags: Haskell
---

[旧個人ページ](https://falgon.github.io/roki/ )及び,
[旧ブログ](https://falgon.github.io/roki.log/)を本ウェブサイト 
([roki.dev](/)), 
本ブログ ([roki.dev/roki.log](/roki.log), [roki.dev/roki.diary](/roki.diary)) に移行した.
以下では, 移行した経緯や技術的概要, 本サイトおよびブログの方針について (ゆるく) 紹介したい.

<!--more-->

## 移行に至った経緯

移行前の[旧ブログ](https://falgon.github.io/roki.log/)[^1]の構成では, static site generator
である [pelican](https://github.com/getpelican/pelican) を使っていた.
以下に, それを使ってそこそこの期間の運用をした上で, 実状, 感想を挙げる.

* [テンプレート](http://www.pelicanthemes.com/)や[プラグイン](https://github.com/getpelican/pelican-plugins)が充実しており, 設定も非常に少ない記述から簡潔に行えるようになっていて, 主観的な感想として pelican は使い勝手の良いツールであった. 自分の場合は, [nikhil-theme](https://github.com/gunchu/nikhil-theme) を元に[拡張して](https://github.com/falgon/nikhil-theme)利用しており,
いくつかの機能の追加実装や bug fix, 依存関係の更新作業などを行っていた
* 記事の執筆は Markdown で行い, D3.js や emscripten を導入していたので, 記事内で簡単なシミュレータや計算の視覚化などでヌルヌル動かしたり, 遊んだりできるようにしていた
* 執筆時には MathJax が意図通り描画できているか等で瞬時にプレビューを見たかったため, 記事内の更新に合わせて記事のリビルド, ブラウザの自動リロードがされる[スクリプト](https://gist.github.com/falgon/5d3fe6838e7f6cb4090823df417680e5)を作り, それを実用していたのでブログ記事執筆時のストレスもそこまではなかった
* 全体のブログ記事管理の仕組みとして, draft から release ブランチに merge & push すると, [Bitbucket Pipelines が走り](https://bitbucket.org/r0ki/roki.log/addon/pipelines/home#!/results/288)[^2], ブログのビルドと GitHub pages へのデプロイが実行されるようにしていたので, 管理コストや記事公開のための作業も少なく, その点は快適であった
* [その他の細かい作業](https://falgon.github.io/roki.log/posts/2018/%203%E6%9C%88/22/my_first_post/) (旧ブログ記事) を行うことで"色々とそれなりに"便利にしていた

...とこれまでを振り返ると, あまり不満はなかったのではないかというような気がしてくるが,
全く不満がなかったかというとやはりそうではない.

* 元テンプレートの Bootstrap のバージョンが低い
* MathJax が重い
* テンプレートが膨大
* 標準 (プラグイン) の検索機能が日本語に未対応
* リンクのバリデーション機能がない
* 無料 GitHub アカウントでもプライベートリポジトリが使えるようになり, 
Bitbucket と GitHub 間を横断させる必然性がなくなった (GitHub Actions も使えるようになったことで, 
ブログ管理のすべてを GitHub のみで一元管理できる)
* 記事や記事内で使うスクリプトを校生するもの (textlint の linter 等) と,
ブログそのもの (Jinja2 テンプレート, ビルド, デプロイ, ライブプレビュー, プラグイン管理...) 
は全くの別物なので, これらの管理を分離したい

無論, 元のブログでも修正, 更新, 機能追加でこれらをすべて満たそうとすることはできるが,
もうそこまでするならいっそのことリニューアルしてしまったほうが...:thinking:となってしまった.<br/>

ブログの他にも, 個人 (プロフィール) サイトを公開しており, それにおいては
Typescript + React を使って[構築していた](https://github.com/falgon/roki)[^3].
こちらは特に何か変わったこともしていないので, 特筆すべきトピックもないのだが,
そこまで DOM 操作をするわけでもないプロフィールページにこれらの技術を用いたのはオーバースペックだったし,
bundle.js の重さからしてもあまり理にかなっていなかったように思う.


## 新個人サイトとブログ

そのようなわけで, 
上記のようなモチベーションがあり, 
本個人サイトとブログを新設したわけだが,
今回新設したことによって, これらの不満足な点についてすべて解消ができたと考えている.
それを果たすことのできた要因や特徴について, いくつか挙げていければと思う.

### Hakyll について

今回, この新個人サイトとブログを新設するにおいては, Haskell 製の static site generator である
[Hakyll](https://jaspervdj.be/hakyll/) を使った. 
一言に static site generator と聞くと, 
テンプレートがいくつかあって, それのうちの何かを選び, 場合によってはカスタムなどして,
また config があって, そこに任意のほげほげを設定して...というイメージが湧くかもしれない.
Hakyll においても, 勿論そのような使い方が可能なのだが,
Hakyll はあくまでも static site generator 
そのものを作るためのライブラリであるという点で特徴的である (と私は感じている).
これにより, static site の細かい部分にまで手を付けることができるのである. <br /><br />

例えば, 上記で問題視していた「MathJax が重い」についてであるが,
これについてはまず \\(\KaTeX\\) へ移行することで対応を行おうと当初考えていた.
しかしよくよく考えてみると, 
[`unixFilter`](https://hackage.haskell.org/package/hakyll-4.13.4.0/docs/Hakyll-Core-UnixFilter.html#v:unixFilter) といったような, 外部のプログラムを `Compiler` として扱うための便利な関数もあるので, 
こういったものを使って \\(\KaTeX\\) から吐き出される数式のタグをサイト生成時に埋め込めれば,
わざわざ javascript から動的に書き換える必要すらないのでは...となり[^4],
今回はそのように実装することで, 静的な数式レンダリングが行えるようになった[^5]. <br /><br />

ただこれについては, ビルド時間が比較的長くなるという問題がある.
後述しているように, このウェブサイトのビルド, デプロイに関してはすべて GitHub Actions 上で行っているのもあり,
デプロイ前のビルドで時間がかかっても, (GitHub Actions 上で許される範囲内ならば) 特別問題はないのだが,
手元で記事のプレビューをすぐしたいとき等にこれはストレスになる 
(Hakyll はデフォルト 
([`hakyll`](https://hackage.haskell.org/package/hakyll-4.13.4.0/docs/Hakyll-Main.html#v:hakyll), 
[`hakyllWith`](https://hackage.haskell.org/package/hakyll-4.13.4.0/docs/Hakyll-Main.html#v:hakyllWith) 等) 
で `watch` オプションが使えるようになっている. 
これは, 記事等の更新があった場合に自動でビルドを再実行してくれるものである).
今回は `hakllWithArgs` を用いて, まずこちらでプログラムへの引数を拡張し,
それによって static site のビルドの挙動を切り替えられるようにすることでこの問題に対処した.
具体的には, 以下の `--preview` フラグをセットして実行することにより,
ビルド時に \\(\KaTeX\\) のレンダリングを行わないようにし,
生成される HTML の `head` の中に \\(\KaTeX\\) を動的にレンダリングする js ファイルを埋め込むようにした.

<script src="https://gist-it.appspot.com/github/falgon/roki-web/blob/af49165391316846dbfbd41056b0a0d14f0e2640/README.md?slice=18:40"></script>

別の似たような事例として, 不必要な js ファイルの読み込みを行わないようにするといった工夫ができる.
このブログでは [d3.js](https://d3js.org/) と [math.js](https://mathjs.org/) を使えるようにしてあるのだが,
全ての記事でこれらを利用するわけではないので, そのようなときはスクリプトファイルの読み込みを抑えたい.
ある記事でこれらのうちの何かが使われたとき, 必要にスクリプトファイルの読み込みが必要になるであろうシーンは, 
単一の記事を表示するときと, `teaser` 内にそれが表示範囲として入っていてかつ記事一覧を表示するときであり,
単一の記事を表示しているときは, 単に `metadataField` にその読み込みの記述があるかないかで判定できるが,
記事一覧を表示するときはその表示される記事一覧の `teaser` に表示範囲として含まれているか判定しなければならない.
が, `teaser` 内のコンテンツまで読んで判定するとなると,
後々なにか変わった読み込み方をしたくなったときなどに柔軟な対応ができなくなる等の懸念事項があったので,
今回は記事一覧に表示される記事の `metadataField` に読み込みの記述があれば 
(`teaser` に含まれているかとは無関係に),
その記事一覧のページにスクリプト読み込みを埋め込むようにしている.

<script src="https://gist-it.appspot.com/github/falgon/roki-web/blog/a496b4fe14d6e0b26861f47fac800fb5f1d1222e/src/Rules/Blog/Core.hs?slice=72:77"></script>

...というように, 様々なパフォーマンスに対するニーズについて柔軟に対応ができる点も, 
Hakyll の良いところであると思う.

### リッチな設定ファイル

[個人ページ](/)の Contributions の一覧は (より多く増やしていきたいという気持ちも込めて) 
HTML テンプレートに直接埋め込むのではなく, 外部ファイルから読み込むようにしている.
その外部ファイルの形式として, 今回は [Dhall](https://github.com/dhall-lang/dhall-lang) を採用した.
Dhall は簡単に言えば, json に型, 関数, インポートの機能が乗っかった設定ファイル言語である[^6].
ここでは Dhall そのものについて詳しくは説明しないが,
例えば, 以下のように各ジャンルについて Union で定義し, 文字列への射を定義することで,
誤った文字列の設定を静的に防ぐといったことができる.

<script src="https://gist-it.appspot.com/github/falgon/roki-web/blob/dca4914dad8f00c167e3eea9ba0de6221cdd4243/contents/config/contributions/Type/Genre.dhall"></script>

Haskell との親和性も高く, 例えば `Dhall.input` 等で簡単に読み込むことができる.

<script src="https://gist-it.appspot.com/github/falgon/roki-web/blob/ab69900746e7ffa9c4a7ec2a4d3c84429e52f406/src/Config/Contributions.hs?slice=16:45"></script>

### RSS/Atom Feed や Site map

このサイトは, トップの個人ページの下に二つのブログがぶら下がっている構造をしており,
よって, Site map や Atom に関してそれぞれのブログから提供する必要がある.

これは, 標準の `renderRSS` や `renderAtom` では対応できないのだが,
`renderRssWithTemplates` や `renderAtomWithTemplates` で独自の XML 
テンプレートやコンテキストを渡すことができるので, サイトの構造に合わせて柔軟な対応ができる.
Site map については標準の機能として盛り込まれていないのだが,
Hakyll のサイトからも紹介されている[このブログ記事](https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html)の通り, Feed の生成と同様,
以下のように XML 用のテンプレートファイルを読ませたり (Lucid 等の) DSL で生成したもの等を使えば良い.

<script src="https://gist-it.appspot.com/github/falgon/roki-web/blob/a496b4fe14d6e0b26861f47fac800fb5f1d1222e/src/Rules/Blog/Core.hs?slice=215:227"></script>


### バージョン情報の埋め込み

このブログ及びウェブページを生成するアプリケーションのバージョン情報には
[Cabal ファイル](https://github.com/falgon/roki-web/blob/72473891c2bb4c31534aafa0b8d76ae88cfe3683/roki-web.cabal#L10)で定義されたパッケージバージョンと Git のコミットハッシュ値を埋め込んでいる.

```bash
$ stack exec site -- --version
The static site roki.dev compiler
version: 0.1.0.0, commit hash: d4dcc402eb6ac271ec070a539e206580ad9cbe5e
```

Git のコミットハッシュ値を埋め込むのには 
[`Development.GitRev.gitHash`](http://hackage.haskell.org/package/gitrev-1.3.1/docs/Development-GitRev.html#v:gitHash) 
が非常に便利で役立った.

<script src="https://gist-it.appspot.com/github/falgon/roki-web/blob/d8a997823c03ebf8008f56c6cebf870e66140545/app/site/Main.hs?slice=95:104"></script>


### CSS フレームワーク Bulma

このブログは CSS のフレームワークとして [Bulma](https://bulma.io/) を使用している.
当初は以前のブログと同様に Bootstrap を使おうかと考えていたが, 
極力 javascript をなくし, 最小構成を軽くしたいと考えていたため, こちらを採用した.
ウェブサイトのデザインについて疎い私であっても, 
一応このような"それなりにそれっぽい"見た目を構成できたという点において, 
この CSS フレームワークには助けられたといえる.
個人的に非常に便利だったのが, Bulma の提供している [Helpers](https://bulma.io/documentation/helpers/) で,
これは単に色の定数値や `margin`, `padding` 等に関するショートカットが用意されているのだが,
「ほんのちょっと手を入れたい」というときのタイプ数がかなり抑えられるので, 
精神的負担が少なく, とても良いものであった.

また, 数学系の記事を書く際には「定義」, 「命題」, ... といったような見出しをつけたいものだが,
このスタイルの作成についても助けられた (Bulma が便利というのもあるが, 単純に SCSS の展開能力にも助けられた).

<script src="https://gist-it.appspot.com/github/falgon/roki-web/blob/f5735daf569603ba872b9ee56820dfbbbbe4423e/contents/scss/blog/_mixins.scss"></script>

これを使って, 例えば以下のように書けば

```html
<div class="m-prop">
<header class="m-prop-title"><p>ABC 予想</p></header>
<div class="m-prop-content">
\\[ a + b = c \\]
を満たす, 互いに素な自然数の組 \\(a, b, c\\) に対し, 積 \\(abc\\) の互いに異なる素因数の積を \\(d\\) とおく.
このとき, 任意の \\(\epsilon\gt 0\\) に対して,
\\[ c\gt d^{1+\epsilon} \\]
を満たす組 \\((a,b,c)\\) は高々有限個しか存在しない
</div>
</div>
```

次のような表示になる.

<div class="m-prop">
<header class="m-prop-title"><p>ABC 予想</p></header>
<div class="m-prop-content">
\\[ a + b = c \\]
を満たす, 互いに素な自然数の組 \\(a, b, c\\) に対し, 積 \\(abc\\) の互いに異なる素因数の積を \\(d\\) とおく.
このとき, 任意の \\(\epsilon\gt 0\\) に対して,
\\[ c\gt d^{1+\epsilon} \\]
を満たす組 \\((a,b,c)\\) は高々有限個しか存在しない
</div>
</div>

### CI/CD と全体の管理体系

前述したとおり, このブログ, ウェブページは GitHub Actions でビルド, 
デプロイを行っており, それ以外の CI/CD としていくつかボットを設定している.
この構成の特徴としては, 
ドラフトをリモートリポジトリに非公開の状態で保存できるようにしてあること,
またそれが Git 上で矛盾しないように構成している点が挙げられると思う.
といっても, 下図の通り非常にシンプルな構成である.

![個人ページ/ブログ管理の構造](https://user-images.githubusercontent.com/1241783/90969880-d99b8a00-e538-11ea-8f35-684365e14406.png "個人ページ/ブログ管理の構造"){ width=640px }

執筆時点現在では, 少なくとも GitHub 上において「プライベートブランチ」なる概念は存在しない.
従って, 非公開情報を扱いたいのならば, 必然的にそれをプライベートリポジトリとして扱う必要があるが,
この構成はドラフトとリリースでリポジトリの公開情報をそれぞれわけることができるということに加えて,
ブログ記事そのものに対する管理と, 
ウェブサイトを生成するアプリケーションの管理を分離することができるという利点がある.
例えば, ブログ記事の管理のほうで扱われるのは Markdown テキスト, 画像などのメディアファイル, 
記事内で使うような js ファイル等であり,
これらの linter やメディアファイルへの自動圧縮などの CI/CD を構成する際に, 
アプリケーションの CI と完全に分離できるのである.<br /><br />
なお, この構成をするのには既存の Action である 
[GitHub Actions for GitHub Pages](https://github.com/peaceiris/actions-gh-pages) が非常に役立った.
特に, 既存のファイルを残すように設定できたり, 
外部のプロジェクトへ Push できる設定項目については特筆すべき内容で,
この Action のおかげで全体の構成をスムーズに行うことが出来たといっても過言ではない.
<br /><br />
次に, パフォーマンス計測についてであるが,
一応デプロイした後に, Google PageSpeed Insights で, [デプロイ後の状態を計測させるようにしている](https://github.com/falgon/roki-web/runs/1027626194?check_suite_focus=true).
現状, PC 版は 90, モバイル版は 65 を閾値として取り敢えず合否を設定しているが,
モバイル版のスコアをもう少し上げられるようにしていきたいところである.
これには, [actions/github-script](https://github.com/actions/github-script) が非常に役立った.
javascript を直接書き込める Action で, 手軽に導入が勧められる.
以下の処理においては, GitHub pages の状態がデプロイ完了になるまでポーリングさせている.

<script src="https://gist-it.appspot.com/github/falgon/roki-web/blob/415890efb43ac2e286b721b09a2881337a0245f0/.github/workflows/build.yml?slice=96:128"></script>

さて, ここまで見てみて, もしかすると, 
どうやって roki-web と roki-web-post 側で同期を取っているのか,
普段はこの環境でどのようにブログの更新等行うのか等, 疑問に思うかもしれないので少し補足しておく.
まず, ブログ記事の更新は, すべて roki-web-post リポジトリ内で行う.
roki-web リポジトリ内では, ブログ記事については一切触らない.
これをもし触ってしまうと, GitHub Actions によって roki-web の master 
ブランチにプッシュした際にコンフリクトすることになるので, この点では注意が必要である
(が, まあもし間違えて触ってしまったとしてもそこは Git なので別になんとかなる).
反対に, ウェブサイトを生成するアプリケーションについて, roki-web-post では一切触らない.
ウェブサイトを生成するアプリケーションについて何らかの変更を加えたい場合は, roki-web のみから行うようにする.
<br />
次に roki-web で何らかの変更が加えられたときに, roki-web-post がどのようにそれを取り込めばよいかであるが,
これは単純に remote を複数登録しておいて, 場合によって切り替えて pull してくれば良い. 
もっと言えば, 下記のように設定しているので,

```
$ git remote -v
origin  git@github.com:falgon/roki-web-post.git (fetch)
origin  git@github.com:falgon/roki-web-post.git (push)
site-system git@github.com:falgon/roki-web.git (fetch)
site-system git@github.com:falgon/roki-web.git (push)
```

roki-web の変更を取り込むときは単に

```
$ git pull site-system master
```

等をすれば良いようにしている.
このとき, `--set-upstream` 等でデフォルトを `origin` に設定しておくと, 
ぼーっとしていて `site-system` 取り込んでしまった！だとか `site-system` に 
push してしまった！等という事故を防ぎやすくなる.<br /><br />

なお, roki-web へのデプロイは deployment key を使って行っているが,
これは [Action 内でデフォルトで使える `GITHUB_TOKEN` では,
あるワークフローによって別のワークフローをトリガーできないため](https://docs.github.com/en/actions/reference/events-that-trigger-workflows#triggering-new-workflows-using-a-personal-access-token)である.

> When you use the repository's GITHUB_TOKEN to perform tasks on behalf of the GitHub Actions app, events triggered by the GITHUB_TOKEN will not create a new workflow run.

この回避策に関しては, [create pull request という GitHub Actions のドキュメント](https://github.com/peter-evans/create-pull-request/blob/8f96fd02520b1086ddc0ec0625b6b5814fb0e694/docs/concepts-guidelines.md#triggering-further-workflow-runs)中に記載があるので,
必要ならば参照すると良いかもしれない.

## 総括

ここまで振り返ってみて, 割とまだまだ拘れる点はあると感じている.
例えば, テンプレートの Lucid 化だとか, 現在の実装をもう少しモナドの合成等ですっきり表せないか等である.
一応, このブログ, ウェブページを構築した際に何をやったのかまとめやすいように,
構築するのに行った大体のタスクについて[ラベルをつけておいた](https://github.com/falgon/roki-web/issues?q=is%3Aissue+label%3Ainit-build+)ので,
気になる場合は見てみても良いかもしれない.
そのようなわけで, 今後はこのブログ, ウェブサイトを使って以前と同様何かしら書いていければと思っているので,
(お手柔らかに) よろしくお願い致します.

[^1]: それよりも前は 2016 年に[はてなブログ (Roki のチラ裏)](https://roki.hateblo.jp/) で技術系の記事を書いていた.
さらにそれよりも前はアメーバブログで技術系の記事を書いていたが, 
随分前にもう消してしまっていた...特別意識しているわけではないが, 
こう見ると二年周期で移行している気がする... (もう移行はしたくないなぁという気持ち)
[^2]: ここで貼っている Pipelines のリンクは, このブログへ移行する前の旧ブログに対する最後の変更コミットのもの. 切ない.
[^3]: 元々は何か色々と DOM 操作をするつもりでいたのだが, 結局単なるプロフィールページなのでオーバースペックであった. この旧プロフィールページはすでにクローズしているが, 記録としてなんとなくキャプチャしたものを [YouTube にアップロードしておいた](https://youtu.be/31VIygOOwLw)ので, もし興味があれば.
[^4]: 同様の取り組みをされているサイトがいくつかあったため, 大いに参考とさせて頂いた.
[^5]: これにより, 例えば比較的多くの数式を使っている「[エルガマル暗号](/roki.log/2018/07/13/elgamalEncryption/index.html)」といった記事について, Google PageSpeed Insights で何度か計測した結果, [旧ブログ記事](https://falgon.github.io/roki.log/posts/2018/%207%E6%9C%88/13/elgamalEncryption/)と比較してインタラクティブになるまでの時間が PC で平均 7 倍, モバイルで平均 5 倍高速になった (てきとう調べ)
[^6]: あくまで設定のための言語であり, チューリング完全ではない. Dhall については, 別途なにか記事を書きたい...
