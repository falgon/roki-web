document.addEventListener('DOMContentLoaded', function() {
    var tagFilterButtons = document.querySelectorAll('.tag-filter-btn');
    var logEntries = document.querySelectorAll('.log-entry');
    var selectedTagsContainer = document.querySelector('.selected-tags');
    var selectedTagsList = document.querySelector('.selected-tags-list');
    var clearSelectionBtn = document.getElementById('clear-selection');
    
    var selectedTags = [];

    // タグフィルターボタンのクリックイベント
    for (var i = 0; i < tagFilterButtons.length; i++) {
        tagFilterButtons[i].addEventListener('click', function() {
            var clickedTag = this.getAttribute('data-tag');
            
            if (clickedTag === 'all') {
                // 「すべて」ボタンがクリックされた場合
                clearSelection();
                this.classList.add('active', 'is-info');
                this.classList.remove('is-outlined');
            } else {
                // 個別のタグボタンがクリックされた場合
                var allButton = document.querySelector('[data-tag="all"]');
                allButton.classList.remove('active', 'is-info');
                allButton.classList.add('is-outlined');
                
                if (this.classList.contains('active')) {
                    // タグの選択を解除
                    this.classList.remove('active');
                    this.classList.add('is-outlined');
                    removeFromArray(selectedTags, clickedTag);
                } else {
                    // タグを選択
                    this.classList.add('active');
                    this.classList.remove('is-outlined');
                    if (selectedTags.indexOf(clickedTag) === -1) {
                        selectedTags.push(clickedTag);
                    }
                }
                
                updateSelectedTagsDisplay();
            }

            // ログエントリの表示/非表示を切り替え
            filterLogEntries();
        });
    }

    // クリアボタンのクリックイベント
    clearSelectionBtn.addEventListener('click', function() {
        clearSelection();
        filterLogEntries();
    });

    // 配列から要素を削除する関数
    function removeFromArray(array, item) {
        var index = array.indexOf(item);
        if (index > -1) {
            array.splice(index, 1);
        }
    }

    // 選択をクリアする関数
    function clearSelection() {
        selectedTags = [];
        for (var i = 0; i < tagFilterButtons.length; i++) {
            tagFilterButtons[i].classList.remove('active', 'is-info');
            tagFilterButtons[i].classList.add('is-outlined');
        }
        
        var allButton = document.querySelector('[data-tag="all"]');
        allButton.classList.add('active', 'is-info');
        allButton.classList.remove('is-outlined');
        
        updateSelectedTagsDisplay();
    }

    // 選択されたタグの表示を更新する関数
    function updateSelectedTagsDisplay() {
        if (selectedTags.length === 0) {
            selectedTagsContainer.style.display = 'none';
        } else {
            selectedTagsContainer.style.display = 'block';
            var tagButtons = '';
            for (var i = 0; i < selectedTags.length; i++) {
                var tag = selectedTags[i];
                var button = document.querySelector('[data-tag="' + tag + '"]');
                var color = button.style.borderColor || button.style.color;
                tagButtons += '<span class="tag is-small" style="background-color: ' + color + '; color: white; margin-right: 0.5rem;">' + tag + '</span>';
            }
            selectedTagsList.innerHTML = tagButtons;
        }
    }

    // ログエントリのフィルタリング関数
    function filterLogEntries() {
        for (var i = 0; i < logEntries.length; i++) {
            var entry = logEntries[i];
            if (selectedTags.length === 0) {
                // タグが選択されていない場合はすべて表示
                entry.style.display = 'block';
                entry.classList.remove('filtered-out');
            } else {
                // 選択されたタグに基づいてフィルタリング（かつ条件）
                var entryTags = entry.getAttribute('data-tags');
                if (entryTags) {
                    var entryTagArray = entryTags.split(',').map(function(tag) {
                        return tag.trim();
                    });
                    var hasAllSelectedTags = true;
                    for (var j = 0; j < selectedTags.length; j++) {
                        if (entryTagArray.indexOf(selectedTags[j]) === -1) {
                            hasAllSelectedTags = false;
                            break;
                        }
                    }
                    
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
        }

        // フィルタリング結果のアニメーション
        animateFilteredEntries();
    }

    // フィルタリング結果のアニメーション
    function animateFilteredEntries() {
        var visibleEntries = document.querySelectorAll('.log-entry:not(.filtered-out)');
        for (var i = 0; i < visibleEntries.length; i++) {
            var entry = visibleEntries[i];
            entry.style.opacity = '0';
            entry.style.transform = 'translateY(20px)';
            
            (function(entry, index) {
                setTimeout(function() {
                    entry.style.transition = 'opacity 0.3s ease, transform 0.3s ease';
                    entry.style.opacity = '1';
                    entry.style.transform = 'translateY(0)';
                }, index * 50);
            })(entry, i);
        }
    }

}); 