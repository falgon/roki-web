/**
 * Vitestのテストセットアップファイル
 * グローバル変数の初期化を行う
 */

import * as d3Module from "d3";

// d3をグローバルスコープに登録
Object.assign(globalThis, { d3: d3Module });
