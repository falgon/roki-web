/**
 * ホテルカードクリック時にタグフィルタリング＋タブ切り替え
 */
export function initHotelCardNavigation(): void {
    const hotelCards = document.querySelectorAll("[data-hotel-code]");

    hotelCards.forEach((card) => {
        card.addEventListener("click", () => {
            const hotelCode = card.getAttribute("data-hotel-code");
            if (!hotelCode) return;

            // 1. タグフィルターUIを表示
            const toggleBtn = document.getElementById("toggle-tag-filter");
            const filterContainer = document.querySelector(".tag-filter") as HTMLElement;
            if (filterContainer?.style.display === "none" || !filterContainer?.style.display) {
                toggleBtn?.click();
            }

            // 2. 既存の選択をクリア
            const clearBtn = document.getElementById("clear-selection");
            const selectedTags = document.querySelectorAll(".tag-filter-btn.active");
            if (selectedTags.length > 0) {
                clearBtn?.click();
            }

            // 3. 対応するタグボタンをクリック
            const tagButton = document.querySelector(
                `.tag-filter-btn[data-tag="${hotelCode}"]`,
            ) as HTMLElement;
            tagButton?.click();

            // 4. 体験録一覧タブに切り替え
            const listTab = document.querySelector('[aria-controls="panel-list"]') as HTMLElement;
            listTab?.click();
        });

        // キーボード操作対応
        card.addEventListener("keydown", (event) => {
            const keyboardEvent = event as KeyboardEvent;
            if (keyboardEvent.key === "Enter" || keyboardEvent.key === " ") {
                keyboardEvent.preventDefault();
                (card as HTMLElement).click();
            }
        });
    });
}
