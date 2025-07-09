interface TagButton extends HTMLElement {
    getAttribute(name: string): string | null;
}

interface LogEntry extends HTMLElement {
    getAttribute(name: string): string | null;
}

document.addEventListener('DOMContentLoaded', (): void => {
    const tagFilterButtons: HTMLCollectionOf<Element> = document.getElementsByClassName('tag-filter-btn');
    const logEntries: HTMLCollectionOf<Element> = document.getElementsByClassName('log-entry');
    const selectedTagsContainer: HTMLElement | null = document.getElementsByClassName('selected-tags')[0] as HTMLElement;
    const selectedTagsList: HTMLElement | null = document.getElementsByClassName('selected-tags-list')[0] as HTMLElement;
    const clearSelectionBtn: HTMLElement | null = document.getElementById('clear-selection');
    const toggleTagFilterBtn: HTMLElement | null = document.getElementById('toggle-tag-filter');
    const tagFilterContainer: HTMLElement | null = document.getElementsByClassName('tag-filter')[0] as HTMLElement;
    
    const selectedTags: Set<string> = new Set();

    // タグフィルターの表示/非表示切り替え
    if (toggleTagFilterBtn && tagFilterContainer) {
        toggleTagFilterBtn.addEventListener('click', (): void => {
            const isVisible: boolean = tagFilterContainer.style.display !== 'none';
            
            if (!isVisible) {
                // フィルターを表示
                tagFilterContainer.style.display = 'block';
                toggleTagFilterBtn.className = toggleTagFilterBtn.className.replace('is-outlined', 'is-info');
                if (!toggleTagFilterBtn.className.includes('is-info')) {
                    toggleTagFilterBtn.className += ' is-info';
                }
            } else {
                // フィルターを非表示
                tagFilterContainer.style.display = 'none';
                toggleTagFilterBtn.className = toggleTagFilterBtn.className.replace('is-info', 'is-outlined');
                if (!toggleTagFilterBtn.className.includes('is-outlined')) {
                    toggleTagFilterBtn.className += ' is-outlined';
                }
                // フィルターを閉じる際は選択をクリア
                clearSelection();
                filterLogEntries();
            }
        });
    }

    // タグフィルターボタンのクリックイベント
    Array.from(tagFilterButtons).forEach((button: Element): void => {
        button.addEventListener('click', function(this: TagButton): void {
            const clickedTag: string | null = this.getAttribute('data-tag');
            
            if (clickedTag) {
                if (this.classList.contains('active')) {
                    // タグの選択を解除
                    this.classList.remove('active');
                    this.classList.add('is-outlined');
                    selectedTags.delete(clickedTag);
                } else {
                    // タグを選択
                    this.classList.add('active');
                    this.classList.remove('is-outlined');
                    selectedTags.add(clickedTag);
                }
                
                updateSelectedTagsDisplay();
                filterLogEntries();
            }
        });
    });

    // クリアボタンのクリックイベント
    if (clearSelectionBtn) {
        clearSelectionBtn.addEventListener('click', (): void => {
            clearSelection();
            filterLogEntries();
        });
    }

    // 選択をクリアする関数
    const clearSelection = (): void => {
        selectedTags.clear();
        Array.from(tagFilterButtons).forEach((btn: Element): void => {
            btn.classList.remove('active');
            btn.classList.add('is-outlined');
        });
        
        updateSelectedTagsDisplay();
    };

    // 選択されたタグの表示を更新する関数
    const updateSelectedTagsDisplay = (): void => {
        if (!selectedTagsContainer || !selectedTagsList) return;
        
        if (selectedTags.size === 0) {
            selectedTagsContainer.style.display = 'none';
        } else {
            selectedTagsContainer.style.display = 'block';
            const tagButtons: string = Array.from(selectedTags).map((tag: string): string => {
                const button: TagButton | null = document.querySelector(`[data-tag="${tag}"]`) as TagButton;
                const color: string = button?.style.borderColor || button?.style.color || '#363636';
                return `<span class="tag is-small" style="background-color: ${color}; color: white; margin-right: 0.5rem;">${tag}</span>`;
            }).join('');
            selectedTagsList.innerHTML = tagButtons;
        }
    };

    // ログエントリのフィルタリング関数
    const filterLogEntries = (): void => {
        Array.from(logEntries).forEach((entry: Element): void => {
            const htmlEntry = entry as HTMLElement;
            if (selectedTags.size === 0) {
                // タグが選択されていない場合はすべて表示
                htmlEntry.style.display = 'block';
                entry.classList.remove('filtered-out');
            } else {
                // 選択されたタグに基づいてフィルタリング（かつ条件）
                const entryTags: string | null = entry.getAttribute('data-tags');
                if (entryTags) {
                    const entryTagArray: string[] = entryTags.split(',').map((tag: string): string => tag.trim());
                    const hasAllSelectedTags: boolean = Array.from(selectedTags).every((selectedTag: string): boolean => 
                        entryTagArray.includes(selectedTag)
                    );
                    
                    if (hasAllSelectedTags) {
                        htmlEntry.style.display = 'block';
                        entry.classList.remove('filtered-out');
                    } else {
                        htmlEntry.style.display = 'none';
                        entry.classList.add('filtered-out');
                    }
                } else {
                    htmlEntry.style.display = 'none';
                    entry.classList.add('filtered-out');
                }
            }
        });

        // フィルタリング結果のアニメーション
        animateFilteredEntries();
    };

    // フィルタリング結果のアニメーション
    const animateFilteredEntries = (): void => {
        const visibleEntries: Element[] = Array.from(logEntries).filter((entry: Element): boolean => 
            !entry.classList.contains('filtered-out')
        );
        
        visibleEntries.forEach((entry: Element, index: number): void => {
            const htmlEntry = entry as HTMLElement;
            htmlEntry.style.opacity = '0';
            htmlEntry.style.transform = 'translateY(20px)';
            
            setTimeout((): void => {
                htmlEntry.style.transition = 'opacity 0.3s ease, transform 0.3s ease';
                htmlEntry.style.opacity = '1';
                htmlEntry.style.transform = 'translateY(0)';
            }, index * 50);
        });
    };
}); 