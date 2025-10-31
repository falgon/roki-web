---
title: 放物運動
date: 2019-03-07 00:00:00
tags: math, physics, emscripten, C++
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

放物運動に関する復習と再現.

## 等加速度運動をする物体の位置関数の導出

時刻 \\(t=0\\) における物体の位置を ${\boldsymbol x_0}$, 速度を \\({\boldsymbol v_0}\\)
とし, 加速度を考慮しない等速運動の三次元空間上の一点に対する位置関数 \\({\boldsymbol x}(t)\\) を
\\[{\boldsymbol x}(t)={\boldsymbol x_0}+{\boldsymbol v_0}t\\] とおく.
このとき, 時間 \\(t_1\\) から \\(t_2\\) への変化量 \\({\boldsymbol x}(t_2)-{\boldsymbol x}(t_1)\\) を \\(\Delta t=t_2-t_1\\) で割れば,
時間経過に対する物体の位置の対比が得られる.
これは正しく速度のことであるが, これは時間 \\(t_1\\) に位置 \\({\boldsymbol x}(t_1)\\), 時間 \\(t_2\\) に位置 \\({\boldsymbol x}(t_2)\\)
にあった物体の平均速度である. 
いま時間 \\(t\\) における物体の瞬間速度を知りたいとすると, \\(\Delta t\\) が微小量となるように極限(\\(t_2\to t_1\Leftrightarrow \Delta t\to 0\\))を取れば良い.<!--more-->
従って, 物体の瞬間速度の関数 \\({\boldsymbol v}(t)\\) を \\({\boldsymbol x}(t)\\) の導関数 

\\[{\boldsymbol v}(t)=\dot{{\boldsymbol x}}(t)=\frac{d{\boldsymbol x}(t)}{dt}\\]

でおけることがわかる.
しばしば力学においては, 上のように原始関数の上部に点を記述し, 時間の微分を表現する.
なお, いまは速度を一定としているので \\({\boldsymbol v}(t)={\boldsymbol v_0}\\) である.

ここで, 物体の運動が加速することを加味するために, 加速度を導入する.
加速度とは, 平均の速度とその所用時間の対比のことであり, 
すなわち \\(\frac{{\boldsymbol v}(t_2)-{\boldsymbol v}(t_1)}{\Delta t}\\) である.
先と同様, \\(\Delta t\to 0\\) としていけば, その瞬間の加速度を得ることができるから, 
瞬間加速度の関数 \\({\boldsymbol a}(t)\\) は

\\[{\boldsymbol a}(t)=\dot{{\boldsymbol v}}(t)=\ddot{{\boldsymbol x}}(t)=\frac{d^2{\boldsymbol x}(t)}{dt^2}\\] 

であり, 結果として 2 次導関数でおかれる.
従って, 一定の加速度 \\({\boldsymbol a}(t)={\boldsymbol a_0}\\) を受けている物体の瞬間速度関数は
\\({\boldsymbol w}(t)={\boldsymbol v_0}+{\boldsymbol a_0}t\\) となるから, 
時刻 \\(t_1\\) から \\(t_2\\) 間の移動距離は \\(d=\int^{t_2}_{t_1}{\boldsymbol w}(t)dt\\) 
であり, 経過時刻 \\(t\\) に対する物体の移動距離は同様にして 
\\[d=\int^t_0{\boldsymbol w}(t)dt={\boldsymbol v_0}t+\frac{{\boldsymbol a_0}t^2}{2}\\]
となる. よって初期位置 \\(x_0\\) を加えることで等加速度運動をする物体の位置関数 \\({\boldsymbol y}(t)\\) が
\\[{\boldsymbol y}(t)={\boldsymbol x_0}+{\boldsymbol v_0}t+\frac{{\boldsymbol a_0}t^2}{2}\tag{1}\\] と定められる.

## 重力を踏まえた運動

重力のみの影響を受ける物体の運動について考える.
三次元空間上の上方向を \\(z\\) 軸としたとき, 下向きの重力加速度ベクトルは \\({\boldsymbol g}=(0,0,-g)^T\\) である.
これは, 地球上において \\(g=9.80665{\rm m/s}^2\\) と知られているから, 地球上の質量 \\(m\\) の物体は下向きに \\(m{\boldsymbol g}\\)
の力を受けていることとなる.

いま時刻 \\(t=0\\) における初期位置を \\({\boldsymbol x_0}=(x_0,y_0,z_0)^T\\),
初速度を \\({\boldsymbol v_0}=(v_x,v_y,v_z)^T\\) としたとき, 物体 \\(P\\) の位置は \\((1)\\) より
\\[{\boldsymbol x}(t)={\boldsymbol x_0}+{\boldsymbol v_0}t+\frac{{\boldsymbol g}t^2}{2}\tag{2}\\]
である. また, 初速度 \\({\boldsymbol v_0}\\) の各成分は, \\(x\\) 方向に発射速度 \\(v\\) で発射されるとしたとき,
三角関数を思い出せば, \\({\boldsymbol v_0}=(v\cos\theta,0,v\sin\theta)^T\\) であることがいえる.

### 到達高度に達するとき, 到達高度

いま物体 \\(P\\) が到達高度に達するときと, その高度について考える.
到達高度に達するときとは, 垂直方向の速度が \\(0\\) であるときなので, 
物体 \\(P\\) の時刻 \\(t\\) における \\(z\\) 成分を \\((2)\\) より
\\[z(t)=z_0+v_zt-\frac{gt^2}{2}\tag{3}\\] とおくと,
次のように微分方程式で表現できる.

\\[\dot{z}(t)=v_z-gt=0\Leftrightarrow t=\frac{v_z}{g}\tag{4}\\]

後はこの \\(t\\) を \\(z(t)\\) に代入すれば

\\[
\begin{aligned}
h&=&z_0+v_z\frac{v_z}{g}-\frac{g}{2}(\frac{v_z}{g})^2\\
&=&z_0+\frac{v_z^2}{g}-\frac{v_z^2}{2g}\\
&=&z_0+\frac{v_z^2}{2g}
\end{aligned}
\\]

と到達高度 \\(h\\) が求まる.
また, 到達高度を \\(h\\) にするための発射角度 \\(\theta\\) は上式より \\(h=\frac{(v\sin\theta)^2}{2g}\\) だから
\\[\sin^2\theta=\frac{2gh}{v^2}\Leftrightarrow\sin\theta=\frac{\sqrt{2gh}}{v}\Leftrightarrow\theta=\sin^{-1}\frac{\sqrt{2gh}}{v}\\]
と求まる.

### 到達距離

水平面 \\(z_0=0\\) から物体 $P$ が発射されたときを考える. 
発射されてから地面につく時刻 \\(t\\) は \\((3)\\) より 
\\[v_zt-\frac{gt^2}{2}=t(v_z-\frac{gt}{2})=0\Leftrightarrow t=0, \frac{2v_z}{g}\\]
であり, 発射した時刻と地面につく時刻の解が得られた.
いま関心があるのは \\(\frac{2v_z}{g}\\) であるが, これは \\((4)\\) の $2$ 倍, 
すなわち到達高度に達する時間の $2$ 倍の時間をかければ地面につくという,
左右対称の扇型の放物線運動を思い浮かべれば, 当然と思える結果が導けた.

従って, 物体 $P$ が原点から \\(x\\) 軸方向に発射されたとすると,
到達距離 \\(l\\) は \\(x\\) 方向の移動距離に等しいから

\\[l=v_x\frac{2v_z}{g}\\]

となる.
また, 到達距離を \\(l\\) にするための発射角度 \\(\theta\\) は上式より 

\\[
\begin{aligned}
l&=&\frac{2(v\cos\theta)(v\sin\theta)}{g}\\
&=& \frac{2v^2}{g}\sin\theta\cos\theta\\
&=& \frac{v^2\sin2\theta}{g}\ (\because\ 2{\rm 倍角の公式:}\ 2\sin\theta\cos\theta=\sin2\theta)
\end{aligned}
\\]

だから \\[\theta=\frac{1}{2}\sin^{-1}\frac{lg}{v^2}\\]
ただし, \\(\sin(\pi-\theta)=\sin\theta\\) より 
\\(\sin2\theta=\sin(\pi-2\theta)=\sin2(\frac{\pi}{2}-\theta)\\) とすると,
\\(\frac{\pi}{2}-\theta\\) でも同一の到達距離 \\(l\\) となることがわかる. すなわち, 2 つの到達の解があることとなる.

### 放物運動のシミュレート

ここまで示した内容で, 重力のみが考慮された簡単な物体の放物運動について<em onclick="obj=document.getElementById('openscsim').style; obj.display=(obj.display=='none')?'block':'none';"><a style="font-style: normal; cursor:pointer;">シミュレートできる(クリックで展開).</a></em>
<div id="openscsim" style="display:none;clear:both;width:100%;text-align:center;">
<canvas style="display: inline;" id="canvas" oncontextmenu="event.preventDefault()"></canvas>
</div>
<script type='text/javascript'>
var Module = {
    canvas: (function() { return document.getElementById('canvas'); })()
};
Module['locateFile'] = function(path, prefix) {
    return "/roki.log/2019/03/7/ParabolicMotion/" + path;
}
</script>
<script src="./parabsbc.js"></script>

C++ と SDL2 で書いたものを emscripten で Web Assembly にしているので, 恐らく古いブラウザでは動かないだろう.
操作感でわかると思うが, 初速の指定はボールとのユークリッド距離を元に決めている.
このときのボールの回転角度は, arctan の定義そのものである.

一応全体のソースコードは, 次のリポジトリにて公開している.

<div class="box is-shadowless has-text-centered">
<i class="fab fa-github mr-2"></i>
<a href="https://github.com/falgon/sdl2_wasm_parabolic">falgon/sdl2_wasm_parabolic - For blog posts</a>
</div>


