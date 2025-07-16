// ローディング機能
const initLoadingScreen = (): void => {
    const loadingOverlay: HTMLElement | null = document.getElementById('loading-overlay');
    const mainContent: HTMLElement | null = document.getElementById('main-content');
    const progressBar: HTMLElement | null = document.getElementById('progress-bar');
    const progressText: HTMLElement | null = document.getElementById('progress-text');
    const loadingDetails: HTMLElement | null = document.getElementById('loading-details');
    
    if (!loadingOverlay || !mainContent || !progressBar || !progressText || !loadingDetails) return;
    
    let progress: number = 0;
    const progressInterval: ReturnType<typeof setInterval> = setInterval((): void => {
        progress += Math.random() * 2;
        if (progress > 85) progress = 85; // 85%で止める
        progressBar.style.width = progress + '%';
        progressText.textContent = Math.floor(progress) + '%';
    }, 50);
    
    // すべてのリソースの読み込み完了を検知
    const waitForAllResources = async (): Promise<void> => {
        const promises: Promise<void>[] = [];
        
        // フォントの読み込み完了を待つ
        promises.push(document.fonts.ready.then((): void => {
            progress = 90;
            progressBar.style.width = '90%';
            progressText.textContent = '90%';
            loadingDetails.textContent = 'Loading images...';
        }));
        
        // 画像の読み込み完了を待つ
        const images: NodeListOf<HTMLImageElement> = document.querySelectorAll('img');
        const imagePromises: Promise<void>[] = Array.from(images).map((img: HTMLImageElement): Promise<void> => {
            if (img.complete && img.naturalHeight !== 0) {
                return Promise.resolve();
            } else {
                return new Promise((resolve: () => void): void => {
                    img.onload = resolve;
                    img.onerror = resolve; // エラーでも続行
                });
            }
        });
        promises.push(Promise.all(imagePromises).then((): void => {
            progress = 95;
            progressBar.style.width = '95%';
            progressText.textContent = '95%';
            loadingDetails.textContent = 'Loading stylesheets...';
        }));
        
        // CSSの読み込み完了を待つ
        const stylesheets: NodeListOf<HTMLLinkElement> = document.querySelectorAll('link[rel="stylesheet"]');
        const cssPromises: Promise<void>[] = Array.from(stylesheets).map((link: HTMLLinkElement): Promise<void> => {
            if (link.sheet) {
                return Promise.resolve();
            } else {
                return new Promise((resolve: () => void): void => {
                    link.onload = resolve;
                    link.onerror = resolve; // エラーでも続行
                });
            }
        });
        promises.push(Promise.all(cssPromises).then((): void => {
            progress = 97;
            progressBar.style.width = '97%';
            progressText.textContent = '97%';
            loadingDetails.textContent = 'Loading JavaScript...';
        }));
        
        // JavaScriptの読み込み完了を待つ
        const scripts: NodeListOf<HTMLScriptElement> = document.querySelectorAll('script[src]');
        const scriptPromises: Promise<void>[] = Array.from(scripts).map((script: HTMLScriptElement): Promise<void> => {
            return new Promise((resolve: () => void): void => {
                // readyStateプロパティの型安全性を確保
                const scriptElement = script as HTMLScriptElement & { readyState?: string };
                if (scriptElement.readyState === 'complete' || scriptElement.readyState === 'loaded') {
                    resolve();
                } else {
                    script.onload = resolve;
                    script.onerror = resolve; // エラーでも続行
                }
            });
        });
        promises.push(Promise.all(scriptPromises).then((): void => {
            progress = 98;
            progressBar.style.width = '98%';
            progressText.textContent = '98%';
            loadingDetails.textContent = 'Initializing page...';
        }));
        
        // DOMContentLoadedイベントを待つ
        promises.push(new Promise<void>((resolve: () => void): void => {
            if (document.readyState === 'loading') {
                document.addEventListener('DOMContentLoaded', resolve);
            } else {
                resolve();
            }
        }));
        
        // loadイベントを待つ
        promises.push(new Promise<void>((resolve: () => void): void => {
            if (document.readyState === 'complete') {
                resolve();
            } else {
                window.addEventListener('load', resolve);
            }
        }));
        
        // すべてのリソースの読み込み完了を待つ
        await Promise.all(promises);
    };
    
    // リソース読み込み完了後の処理
    waitForAllResources().then((): void => {
        // プログレスを100%にする
        progress = 100;
        progressBar.style.width = '100%';
        progressText.textContent = '100%';
        loadingDetails.textContent = 'Done!';
        
        // 少し待ってからローディング画面を隠す
        setTimeout((): void => {
            loadingOverlay.classList.add('hidden');
            mainContent.classList.add('loaded');
            
            // ローディング画面を完全に削除
            setTimeout((): void => {
                loadingOverlay.remove();
            }, 500);
        }, 500);
    });
    
    // フォールバック: 15秒後に強制的にローディング完了
    setTimeout((): void => {
        if (!mainContent.classList.contains('loaded')) {
            clearInterval(progressInterval);
            progress = 100;
            progressBar.style.width = '100%';
            progressText.textContent = '100%';
            
            setTimeout((): void => {
                loadingOverlay.classList.add('hidden');
                mainContent.classList.add('loaded');
                
                setTimeout((): void => {
                    loadingOverlay.remove();
                }, 500);
            }, 500);
        }
    }, 15000);
};

interface TagButton extends HTMLElement {
    getAttribute(name: string): string | null;
}

interface LogEntry extends HTMLElement {
    getAttribute(name: string): string | null;
}

document.addEventListener('DOMContentLoaded', (): void => {
    // ローディング画面を初期化
    initLoadingScreen();
    
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