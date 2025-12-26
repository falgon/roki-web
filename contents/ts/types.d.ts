/**
 * Disney Experience可視化のグローバル型定義
 */

declare global {
    /**
     * 可視化データの全体構造
     */
    interface VisualizationData {
        timeSeries: TimeSeriesData;
        yearlyTimeSeries?: YearlyTimeSeriesData[];
        tagStats: TagStats;
    }

    /**
     * 時系列データ
     */
    interface TimeSeriesData {
        daily: DailyCount[];
    }

    /**
     * 年度別時系列データ
     * Haskellバックエンドの出力形式に対応
     */
    interface YearlyTimeSeriesData {
        year: number;
        daily: DailyCount[];
    }

    /**
     * 日別カウントデータ
     */
    interface DailyCount {
        date: string; // ISO 8601形式 (YYYY-MM-DD)
        count: number;
    }

    /**
     * タグ統計データ
     */
    interface TagStats {
        tags: TagCount[];
    }

    /**
     * タグカウントデータ
     */
    interface TagCount {
        tag: string;
        count: number;
    }

    /**
     * ヒートマップセルデータ
     */
    interface HeatmapCell {
        date: Date;
        count: number;
        weekday: number; // 0 (日曜日) から 6 (土曜日)
        weekIndex: number; // 週のインデックス
    }

    /**
     * サークルパッキングのノードデータ
     */
    interface CircleNode {
        name: string;
        value?: number;
        children?: CircleNode[];
    }
}

// このファイルをモジュールとして扱うため、空のexportを追加
export {};
