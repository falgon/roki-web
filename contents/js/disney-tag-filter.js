document.addEventListener('DOMContentLoaded', function() {
    const tagFilterButtons = document.querySelectorAll('.tag-filter-btn');
    const logEntries = document.querySelectorAll('.log-entry');
    const selectedTagsContainer = document.querySelector('.selected-tags');
    const selectedTagsList = document.querySelector('.selected-tags-list');
    const clearSelectionBtn = document.getElementById('clear-selection');
    
    let selectedTags = new Set();

    // タグフィルターボタンのクリックイベント
    tagFilterButtons.forEach(button => {
        button.addEventListener('click', function() {
            const clickedTag = this.getAttribute('data-tag');
            
            if (clickedTag === 'all') {
                // 「すべて」ボタンがクリックされた場合
                clearSelection();
                this.classList.add('active', 'is-info');
                this.classList.remove('is-outlined');
            } else {
                // 個別のタグボタンがクリックされた場合
                const allButton = document.querySelector('[data-tag="all"]');
                allButton.classList.remove('active', 'is-info');
                allButton.classList.add('is-outlined');
                
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
            }

            // ログエントリの表示/非表示を切り替え
            filterLogEntries();
        });
    });

    // クリアボタンのクリックイベント
    clearSelectionBtn.addEventListener('click', function() {
        clearSelection();
        filterLogEntries();
    });

    // 選択をクリアする関数
    function clearSelection() {
        selectedTags.clear();
        tagFilterButtons.forEach(btn => {
            btn.classList.remove('active', 'is-info');
            btn.classList.add('is-outlined');
        });
        
        const allButton = document.querySelector('[data-tag="all"]');
        allButton.classList.add('active', 'is-info');
        allButton.classList.remove('is-outlined');
        
        updateSelectedTagsDisplay();
    }

    // 選択されたタグの表示を更新する関数
    function updateSelectedTagsDisplay() {
        if (selectedTags.size === 0) {
            selectedTagsContainer.style.display = 'none';
        } else {
            selectedTagsContainer.style.display = 'block';
            const tagButtons = Array.from(selectedTags).map(tag => {
                const button = document.querySelector(`[data-tag="${tag}"]`);
                const color = button.style.borderColor || button.style.color;
                return `<span class="tag is-small" style="background-color: ${color}; color: white; margin-right: 0.5rem;">${tag}</span>`;
            }).join('');
            selectedTagsList.innerHTML = tagButtons;
        }
    }

    // ログエントリのフィルタリング関数
    function filterLogEntries() {
        logEntries.forEach(entry => {
            if (selectedTags.size === 0) {
                // タグが選択されていない場合はすべて表示
                entry.style.display = 'block';
                entry.classList.remove('filtered-out');
            } else {
                // 選択されたタグに基づいてフィルタリング（かつ条件）
                const entryTags = entry.getAttribute('data-tags');
                if (entryTags) {
                    const entryTagArray = entryTags.split(',').map(tag => tag.trim());
                    const hasAllSelectedTags = Array.from(selectedTags).every(selectedTag => 
                        entryTagArray.includes(selectedTag)
                    );
                    
                    if (hasAllSelectedTags) {
                        entry.style.display = 'block';
                        entry.classList.remove('filtered-out');
                    } else {
                        entry.style.display = 'none';
                        entry.classList.add('filtered-out');
                    }
                } else {
                    entry.style.display = 'none';
                    entry.classList.add('filtered-out');
                }
            }
        });

        // フィルタリング結果のアニメーション
        animateFilteredEntries();
    }

    // フィルタリング結果のアニメーション
    function animateFilteredEntries() {
        const visibleEntries = document.querySelectorAll('.log-entry:not(.filtered-out)');
        visibleEntries.forEach((entry, index) => {
            entry.style.opacity = '0';
            entry.style.transform = 'translateY(20px)';
            
            setTimeout(() => {
                entry.style.transition = 'opacity 0.3s ease, transform 0.3s ease';
                entry.style.opacity = '1';
                entry.style.transform = 'translateY(0)';
            }, index * 50);
        });
    }

}); 