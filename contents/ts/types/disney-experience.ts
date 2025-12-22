/**
 * Disney体験記録の型定義ファイル
 * バックエンド(Haskell)で生成されるJSONデータの型定義
 */

/**
 * 日別カウントデータ
 */
export interface DailyCount {
    date: string; // ISO 8601形式の日付文字列 (例: "2024-01-01")
    count: number; // その日の体験記録数
}

/**
 * 時系列データ
 */
export interface TimeSeriesData {
    daily: DailyCount[]; // 日別カウントのリスト
}

/**
 * タグ別カウントデータ
 */
export interface TagCount {
    tag: string; // タグ名 (例: "TDL", "TDS", "DHM")
    count: number; // そのタグの体験記録数
}

/**
 * タグ統計データ
 */
export interface TagStats {
    tags: TagCount[]; // タグ別カウントのリスト
}

/**
 * 可視化データの全体構造
 */
export interface VisualizationData {
    timeSeries: TimeSeriesData; // 時系列データ
    tagStats: TagStats; // タグ統計データ
}

/**
 * ヒートマップセルのデータ
 * D3.jsでの描画に使用される内部データ構造
 */
export interface HeatmapCell {
    date: Date; // JavaScript Dateオブジェクト
    count: number; // 体験記録数
    weekday: number; // 曜日 (0-6, 日曜日が0)
    weekIndex: number; // 週のインデックス
}

/**
 * サークルパッキングノードのデータ
 * D3.jsのhierarchy構造で使用
 */
export interface CircleNode {
    name: string; // ノード名（タグ名または"root"）
    value?: number; // ノードの値（タグのカウント数）
    children?: CircleNode[]; // 子ノードのリスト
}

/**
 * D3.jsのhierarchyノード型
 * D3の内部型定義と互換性を持つ
 */
export interface D3HierarchyNode {
    data: CircleNode;
    value?: number;
    depth: number;
    height: number;
    parent: D3HierarchyNode | null;
    children?: D3HierarchyNode[];
    x?: number;
    y?: number;
    r?: number;
}
