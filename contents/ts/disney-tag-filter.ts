// Window型の拡張
declare global {
    interface Window {
        isPreview?: boolean;
    }
}

// HTMLエスケープ関数
const escapeHtml = (text: string): string => {
    const div = document.createElement("div");
    div.textContent = text;
    return div.innerHTML;
};

// 文字列を正規化する関数（検索やフィルタリングに使用）
const normalizeString = (str: string): string => {
    return str.toLowerCase().trim();
};

// ローディング機能
const initLoadingScreen = (): void => {
    const loadingOverlay: HTMLElement | null = document.getElementById("loading-overlay");
    const mainContent: HTMLElement | null = document.getElementById("main-content");
    const progressBar: HTMLElement | null = document.getElementById("progress-bar");
    const progressText: HTMLElement | null = document.getElementById("progress-text");
    const loadingDetails: HTMLElement | null = document.getElementById("loading-details");

    if (!loadingOverlay || !mainContent || !progressBar || !progressText || !loadingDetails) return;

    // プレビューモードかどうかを確認
    const isPreview = window.isPreview === true;

    // ログ出力関数（プレビューモードでのみ出力）
    const log = (message: string): void => {
        if (isPreview) {
            console.log(message);
        }
    };

    let progress = 0;
    const progressInterval: ReturnType<typeof setInterval> = setInterval((): void => {
        progress += Math.random() * 2;
        if (progress > 85) progress = 85; // 85%で止める
        progressBar.style.width = `${progress}%`;
        progressText.textContent = `${Math.floor(progress)}%`;
    }, 50);

    // すべてのリソースの読み込み完了を検知
    const waitForAllResources = async (): Promise<void> => {
        log("Starting resource loading detection...");
        const promises: Promise<void>[] = [];

        // フォントの読み込み完了を待つ
        log("Waiting for fonts to load...");
        promises.push(
            document.fonts.ready.then((): void => {
                log("Fonts loaded");
                progress = 90;
                progressBar.style.width = "90%";
                progressText.textContent = "90%";
                loadingDetails.textContent = "Loading images...";
            }),
        );

        // 画像の読み込み完了を待つ
        const images: NodeListOf<HTMLImageElement> = document.querySelectorAll("img");
        log(`Found ${images.length} images to load`);

        if (images.length === 0) {
            // 画像がない場合は即座に完了
            promises.push(
                Promise.resolve().then((): void => {
                    progress = 95;
                    progressBar.style.width = "95%";
                    progressText.textContent = "95%";
                    loadingDetails.textContent = "Loading stylesheets...";
                }),
            );
        } else {
            const imagePromises: Promise<void>[] = Array.from(images).map(
                (img: HTMLImageElement, index: number): Promise<void> => {
                    return new Promise((resolve: () => void): void => {
                        log(
                            `Image ${index + 1}/${images.length}: ${img.src}, complete: ${img.complete}, naturalHeight: ${img.naturalHeight}`,
                        );

                        // 画像が既に読み込まれている場合
                        if (img.complete && img.naturalHeight !== 0) {
                            log(`Image ${index + 1} already loaded`);
                            resolve();
                            return;
                        }

                        // 画像が読み込み中またはエラーの場合
                        const onLoad = (): void => {
                            log(`Image ${index + 1} loaded successfully`);
                            img.removeEventListener("load", onLoad);
                            img.removeEventListener("error", onError);
                            resolve();
                        };

                        const onError = (): void => {
                            log(`Image ${index + 1} failed to load`);
                            img.removeEventListener("load", onLoad);
                            img.removeEventListener("error", onError);
                            resolve(); // エラーでも続行
                        };

                        img.addEventListener("load", onLoad);
                        img.addEventListener("error", onError);

                        // 既に読み込みが完了している場合のフォールバック
                        if (img.complete) {
                            setTimeout((): void => {
                                if (!img.complete || img.naturalHeight === 0) {
                                    log(`Image ${index + 1} fallback: treating as loaded`);
                                    resolve();
                                }
                            }, 100);
                        }
                    });
                },
            );

            promises.push(
                Promise.all(imagePromises).then((): void => {
                    log("All images loaded");
                    progress = 95;
                    progressBar.style.width = "95%";
                    progressText.textContent = "95%";
                    loadingDetails.textContent = "Loading stylesheets...";
                }),
            );
        }

        // CSSの読み込み完了を待つ
        const stylesheets: NodeListOf<HTMLLinkElement> =
            document.querySelectorAll('link[rel="stylesheet"]');
        log(`Found ${stylesheets.length} stylesheets to load`);

        if (stylesheets.length === 0) {
            // CSSがない場合は即座に完了
            promises.push(
                Promise.resolve().then((): void => {
                    progress = 97;
                    progressBar.style.width = "97%";
                    progressText.textContent = "97%";
                    loadingDetails.textContent = "Loading JavaScript...";
                }),
            );
        } else {
            const cssPromises: Promise<void>[] = Array.from(stylesheets).map(
                (link: HTMLLinkElement, index: number): Promise<void> => {
                    return new Promise((resolve: () => void): void => {
                        log(
                            `Stylesheet ${index + 1}/${stylesheets.length}: ${link.href}, sheet: ${!!link.sheet}`,
                        );

                        // CSSが既に読み込まれている場合
                        if (link.sheet) {
                            log(`Stylesheet ${index + 1} already loaded`);
                            resolve();
                            return;
                        }

                        // CSSが読み込み中またはエラーの場合
                        const onLoad = (): void => {
                            log(`Stylesheet ${index + 1} loaded successfully`);
                            link.removeEventListener("load", onLoad);
                            link.removeEventListener("error", onError);
                            resolve();
                        };

                        const onError = (): void => {
                            log(`Stylesheet ${index + 1} failed to load`);
                            link.removeEventListener("load", onLoad);
                            link.removeEventListener("error", onError);
                            resolve(); // エラーでも続行
                        };

                        link.addEventListener("load", onLoad);
                        link.addEventListener("error", onError);

                        // 既に読み込みが完了している場合のフォールバック
                        if (link.sheet) {
                            setTimeout((): void => {
                                if (!link.sheet) {
                                    log(`Stylesheet ${index + 1} fallback: treating as loaded`);
                                    resolve();
                                }
                            }, 100);
                        }
                    });
                },
            );

            promises.push(
                Promise.all(cssPromises).then((): void => {
                    log("All stylesheets loaded");
                    progress = 97;
                    progressBar.style.width = "97%";
                    progressText.textContent = "97%";
                    loadingDetails.textContent = "Loading JavaScript...";
                }),
            );
        }

        // JavaScriptの読み込み完了を待つ
        const scripts: NodeListOf<HTMLScriptElement> = document.querySelectorAll("script[src]");
        log(`Found ${scripts.length} scripts to load`);

        if (scripts.length === 0) {
            // JavaScriptがない場合は即座に完了
            promises.push(
                Promise.resolve().then((): void => {
                    progress = 98;
                    progressBar.style.width = "98%";
                    progressText.textContent = "98%";
                    loadingDetails.textContent = "Initializing page...";
                }),
            );
        } else {
            const scriptPromises: Promise<void>[] = Array.from(scripts).map(
                (script: HTMLScriptElement, index: number): Promise<void> => {
                    return new Promise((resolve: () => void): void => {
                        // 複数の方法でスクリプトの読み込み状況を確認
                        const readyState = (script as HTMLScriptElement & { readyState?: string })
                            .readyState;
                        const isAsync = script.async;
                        const isDefer = script.defer;

                        log(`Script ${index + 1}/${scripts.length}: ${script.src}`);
                        log(`  - readyState: ${readyState}`);
                        log(`  - async: ${isAsync}`);
                        log(`  - defer: ${isDefer}`);

                        let resolved = false;

                        const onLoad = (): void => {
                            if (resolved) return;
                            resolved = true;
                            log(`Script ${index + 1} loaded successfully`);
                            script.removeEventListener("load", onLoad);
                            script.removeEventListener("error", onError);
                            resolve();
                        };

                        const onError = (): void => {
                            if (resolved) return;
                            resolved = true;
                            log(`Script ${index + 1} failed to load`);
                            script.removeEventListener("load", onLoad);
                            script.removeEventListener("error", onError);
                            resolve(); // エラーでも続行
                        };

                        // イベントリスナーを設定
                        script.addEventListener("load", onLoad);
                        script.addEventListener("error", onError);

                        // スクリプトが既に読み込まれているかを確定的に判断する関数
                        const isScriptAlreadyLoaded = (): boolean => {
                            const scriptSrc = script.src;

                            // 方法1: readyStateが有効な値の場合
                            if (readyState === "complete" || readyState === "loaded") {
                                log(
                                    `Script ${index + 1} already loaded (readyState: ${readyState})`,
                                );
                                return true;
                            }

                            // 方法2: 同じsrcのスクリプトが既に存在する場合
                            const existingScripts = Array.from(document.scripts);
                            const duplicateScripts = existingScripts.filter(
                                (existingScript: HTMLScriptElement): boolean => {
                                    return (
                                        existingScript.src === scriptSrc &&
                                        existingScript !== script
                                    );
                                },
                            );

                            if (duplicateScripts.length > 0) {
                                log(
                                    `Script ${index + 1} appears to be already loaded (duplicate found)`,
                                );
                                return true;
                            }

                            // 方法3: スクリプトの実行状態を確認
                            try {
                                // スクリプトが実行可能な状態かどうかを確認
                                const scriptReadyState = (
                                    script as HTMLScriptElement & { readyState?: string }
                                ).readyState;
                                if (
                                    scriptReadyState === "complete" ||
                                    scriptReadyState === "loaded"
                                ) {
                                    log(`Script ${index + 1} already loaded (execution ready)`);
                                    return true;
                                }
                            } catch (_e) {
                                // readyStateプロパティにアクセスできない場合
                            }

                            // 方法4: スクリプトの読み込み時間を確認
                            const loadStartTime = performance.now();
                            const navigationStart =
                                (
                                    performance as Performance & {
                                        timing?: { navigationStart?: number };
                                    }
                                ).timing?.navigationStart || 0;
                            const timeSincePageLoad = loadStartTime - navigationStart;

                            // ページ読み込みから一定時間経過している場合、既に読み込まれている可能性が高い
                            if (timeSincePageLoad > 1000) {
                                // 1秒以上経過
                                log(
                                    `Script ${index + 1} likely already loaded (page load time: ${timeSincePageLoad.toFixed(0)}ms)`,
                                );
                                return true;
                            }

                            return false;
                        };

                        // 既に読み込まれているかチェック
                        if (isScriptAlreadyLoaded()) {
                            setTimeout((): void => {
                                if (!resolved) {
                                    resolved = true;
                                    script.removeEventListener("load", onLoad);
                                    script.removeEventListener("error", onError);
                                    resolve();
                                }
                            }, 100);
                            return;
                        }

                        // readyStateがundefinedの場合の処理
                        if (readyState === undefined) {
                            log(
                                `Script ${index + 1} readyState is undefined, checking if already loaded...`,
                            );

                            // 非同期スクリプトの場合は短いタイムアウト
                            if (isAsync) {
                                log(`Script ${index + 1} is async, using short timeout`);
                                setTimeout((): void => {
                                    if (!resolved) {
                                        log(`Script ${index + 1} async script timeout`);
                                        resolved = true;
                                        script.removeEventListener("load", onLoad);
                                        script.removeEventListener("error", onError);
                                        resolve();
                                    }
                                }, 1000); // 1秒でタイムアウト
                                return;
                            }

                            // readyStateがundefinedで非同期でない場合、既に読み込まれている可能性が高い
                            log(
                                `Script ${index + 1} readyState undefined and not async, likely already loaded`,
                            );
                            setTimeout((): void => {
                                if (!resolved) {
                                    log(`Script ${index + 1} treating as already loaded`);
                                    resolved = true;
                                    script.removeEventListener("load", onLoad);
                                    script.removeEventListener("error", onError);
                                    resolve();
                                }
                            }, 500); // 0.5秒で完了判定
                            return;
                        }

                        // フォールバック: 一定時間後に強制的に完了とする
                        setTimeout((): void => {
                            if (!resolved) {
                                log(
                                    `Script ${index + 1} fallback: treating as loaded after timeout`,
                                );
                                resolved = true;
                                script.removeEventListener("load", onLoad);
                                script.removeEventListener("error", onError);
                                resolve();
                            }
                        }, 1000); // 1秒でタイムアウト
                    });
                },
            );

            promises.push(
                Promise.all(scriptPromises).then((): void => {
                    log("All scripts loaded");
                    progress = 98;
                    progressBar.style.width = "98%";
                    progressText.textContent = "98%";
                    loadingDetails.textContent = "Initializing page...";
                }),
            );
        }

        // DOMContentLoadedイベントを待つ
        log("Waiting for DOMContentLoaded...");
        promises.push(
            new Promise<void>((resolve: () => void): void => {
                if (document.readyState === "loading") {
                    document.addEventListener("DOMContentLoaded", (): void => {
                        log("DOMContentLoaded fired");
                        resolve();
                    });
                } else {
                    log("DOMContentLoaded already fired");
                    resolve();
                }
            }),
        );

        // loadイベントを待つ
        log("Waiting for load event...");
        promises.push(
            new Promise<void>((resolve: () => void): void => {
                if (document.readyState === "complete") {
                    log("Load event already fired");
                    resolve();
                } else {
                    window.addEventListener("load", (): void => {
                        log("Load event fired");
                        resolve();
                    });
                }
            }),
        );

        // 追加: すべてのリソースが読み込まれるまで待機
        log("Waiting for all resources to be ready...");
        promises.push(
            new Promise<void>((resolve: () => void): void => {
                const checkReady = (): void => {
                    log(`Current document.readyState: ${document.readyState}`);
                    if (document.readyState === "complete") {
                        log("Document is completely loaded");
                        resolve();
                    } else {
                        // まだ読み込み中の場合は少し待ってから再チェック
                        setTimeout(checkReady, 50); // 50ms間隔でチェック
                    }
                };
                checkReady();
            }),
        );

        // すべてのリソースの読み込み完了を待つ
        log("Waiting for all resources to complete...");
        await Promise.all(promises);
        log("All resources loaded successfully");
    };

    // リソース読み込み完了後の処理
    waitForAllResources()
        .then((): void => {
            log("Starting final loading completion...");
            // プログレスインターバルを停止
            clearInterval(progressInterval);

            // プログレスを100%にする
            progress = 100;
            progressBar.style.width = "100%";
            progressText.textContent = "100%";
            loadingDetails.textContent = "Done!";

            // 少し待ってからローディング画面を隠す
            setTimeout((): void => {
                log("Hiding loading overlay...");
                loadingOverlay.classList.add("hidden");
                mainContent.classList.add("loaded");

                // ローディング画面を完全に削除
                setTimeout((): void => {
                    log("Removing loading overlay...");
                    loadingOverlay.remove();
                    log("Loading process completed successfully!");
                }, 500);
            }, 500);
        })
        .catch((error: Error): void => {
            if (isPreview) {
                console.error("Error during resource loading:", error);
            }
            // プログレスインターバルを停止
            clearInterval(progressInterval);

            // エラーが発生した場合もローディング画面を隠す
            progress = 100;
            progressBar.style.width = "100%";
            progressText.textContent = "100%";
            loadingDetails.textContent = "Done!";

            setTimeout((): void => {
                loadingOverlay.classList.add("hidden");
                mainContent.classList.add("loaded");

                setTimeout((): void => {
                    loadingOverlay.remove();
                    log("Loading process completed with errors");
                }, 500);
            }, 500);
        });

    // フォールバック: 15秒後に強制的にローディング完了
    setTimeout((): void => {
        if (!mainContent.classList.contains("loaded")) {
            clearInterval(progressInterval);
            progress = 100;
            progressBar.style.width = "100%";
            progressText.textContent = "100%";

            setTimeout((): void => {
                loadingOverlay.classList.add("hidden");
                mainContent.classList.add("loaded");

                setTimeout((): void => {
                    loadingOverlay.remove();
                }, 500);
            }, 500);
        }
    }, 15000);
};

declare global {
    interface TagButton extends HTMLElement {
        getAttribute(name: string): string | null;
    }

    interface LogEntry extends HTMLElement {
        getAttribute(name: string): string | null;
    }
}

// Expose functions to global scope for testing
if (typeof window !== "undefined") {
    (
        window as typeof window & {
            escapeHtml: typeof escapeHtml;
            normalizeString: typeof normalizeString;
            initLoadingScreen: typeof initLoadingScreen;
        }
    ).escapeHtml = escapeHtml;
    (window as typeof window & { normalizeString: typeof normalizeString }).normalizeString =
        normalizeString;
    (window as typeof window & { initLoadingScreen: typeof initLoadingScreen }).initLoadingScreen =
        initLoadingScreen;
}

if (typeof document !== "undefined") {
    document.addEventListener("DOMContentLoaded", (): void => {
        // ローディング画面を初期化
        initLoadingScreen();

        const tagFilterButtons: HTMLCollectionOf<Element> =
            document.getElementsByClassName("tag-filter-btn");
        const logEntries: HTMLCollectionOf<Element> = document.getElementsByClassName("log-entry");
        const selectedTagsContainer: HTMLElement | null = document.getElementsByClassName(
            "selected-tags",
        )[0] as HTMLElement;
        const selectedTagsList: HTMLElement | null = document.getElementsByClassName(
            "selected-tags-list",
        )[0] as HTMLElement;
        const clearSelectionBtn: HTMLElement | null = document.getElementById("clear-selection");
        const toggleTagFilterBtn: HTMLElement | null = document.getElementById("toggle-tag-filter");
        const tagFilterContainer: HTMLElement | null = document.getElementsByClassName(
            "tag-filter",
        )[0] as HTMLElement;
        const searchInput: HTMLInputElement | null = document.getElementById(
            "search-input",
        ) as HTMLInputElement;

        const selectedTags: Set<string> = new Set();
        let searchQuery: string = ""; // 検索クエリの状態管理を追加

        // タグフィルターの表示/非表示切り替え
        if (toggleTagFilterBtn && tagFilterContainer) {
            toggleTagFilterBtn.addEventListener("click", (): void => {
                const isVisible: boolean = tagFilterContainer.style.display !== "none";

                if (!isVisible) {
                    // フィルターを表示
                    tagFilterContainer.style.display = "block";
                    toggleTagFilterBtn.className = toggleTagFilterBtn.className.replace(
                        "is-outlined",
                        "is-info",
                    );
                    if (!toggleTagFilterBtn.className.includes("is-info")) {
                        toggleTagFilterBtn.className += " is-info";
                    }
                } else {
                    // フィルターを非表示
                    tagFilterContainer.style.display = "none";
                    toggleTagFilterBtn.className = toggleTagFilterBtn.className.replace(
                        "is-info",
                        "is-outlined",
                    );
                    if (!toggleTagFilterBtn.className.includes("is-outlined")) {
                        toggleTagFilterBtn.className += " is-outlined";
                    }
                    // フィルターを閉じる際は選択をクリア
                    clearSelection();
                    filterLogEntries();
                }
            });
        }

        // タグフィルターボタンのクリックイベント
        for (const button of Array.from(tagFilterButtons)) {
            button.addEventListener("click", function (this: TagButton): void {
                const clickedTag: string | null = this.getAttribute("data-tag");

                if (clickedTag) {
                    if (this.classList.contains("active")) {
                        // タグの選択を解除
                        this.classList.remove("active");
                        this.classList.add("is-outlined");
                        selectedTags.delete(clickedTag);
                    } else {
                        // タグを選択
                        this.classList.add("active");
                        this.classList.remove("is-outlined");
                        selectedTags.add(clickedTag);
                    }

                    updateSelectedTagsDisplay();
                    filterLogEntries();
                }
            });
        }

        // クリアボタンのクリックイベント
        if (clearSelectionBtn) {
            clearSelectionBtn.addEventListener("click", (): void => {
                clearSelection();
                filterLogEntries();
            });
        }

        // 検索入力のイベントリスナー
        if (searchInput) {
            // デバウンス用のタイマー
            let searchDebounceTimer: ReturnType<typeof setTimeout> | null = null;

            searchInput.addEventListener("input", (event: Event): void => {
                const target = event.target as HTMLInputElement;
                searchQuery = target.value;

                // デバウンス処理
                if (searchDebounceTimer) {
                    clearTimeout(searchDebounceTimer);
                }

                searchDebounceTimer = setTimeout((): void => {
                    filterLogEntries();
                    searchDebounceTimer = null;
                }, 300); // 300ms の遅延
            });
        } else {
            // プレビューモードでのみログ出力
            if (window.isPreview === true) {
                console.warn("Search input element not found");
            }
        }

        // 選択をクリアする関数
        const clearSelection = (): void => {
            selectedTags.clear();
            searchQuery = ""; // 検索クエリもクリア

            if (searchInput) {
                searchInput.value = ""; // 検索入力欄をクリア
            }

            for (const btn of Array.from(tagFilterButtons)) {
                btn.classList.remove("active");
                btn.classList.add("is-outlined");
            }

            updateSelectedTagsDisplay();
        };

        // 選択されたタグの表示を更新する関数
        const updateSelectedTagsDisplay = (): void => {
            if (!selectedTagsContainer || !selectedTagsList) return;

            if (selectedTags.size === 0) {
                selectedTagsContainer.style.display = "none";
            } else {
                selectedTagsContainer.style.display = "block";
                const tagButtons: string = Array.from(selectedTags)
                    .map((tag: string): string => {
                        const button: TagButton | null = document.querySelector(
                            `[data-tag="${escapeHtml(tag)}"]`,
                        ) as TagButton;
                        const color: string =
                            button?.style.borderColor || button?.style.color || "#363636";
                        return `<span class="tag is-small" style="background-color: ${escapeHtml(color)}; color: white; margin-right: 0.5rem;">${escapeHtml(tag)}</span>`;
                    })
                    .join("");
                selectedTagsList.innerHTML = tagButtons;
            }
        };

        // ログエントリのフィルタリング関数
        const filterLogEntries = (): void => {
            const normalizedSearchQuery = normalizeString(searchQuery);

            for (const entry of Array.from(logEntries)) {
                const htmlEntry = entry as HTMLElement;

                // タグフィルタリングの判定
                let passesTagFilter = true;
                if (selectedTags.size > 0) {
                    const entryTags: string | null = entry.getAttribute("data-tags");
                    if (entryTags) {
                        const entryTagArray: string[] = entryTags
                            .split(",")
                            .map((tag: string): string => tag.trim());
                        passesTagFilter = Array.from(selectedTags).every(
                            (selectedTag: string): boolean => entryTagArray.includes(selectedTag),
                        );
                    } else {
                        passesTagFilter = false;
                    }
                }

                // 検索フィルタリングの判定
                let passesSearchFilter = true;
                if (normalizedSearchQuery !== "") {
                    const searchContent: string | null = entry.getAttribute("data-search-content");
                    if (searchContent) {
                        const normalizedContent = normalizeString(searchContent);
                        passesSearchFilter = normalizedContent.includes(normalizedSearchQuery);
                    } else {
                        passesSearchFilter = false;
                    }
                }

                // 両方の条件を満たす場合のみ表示（AND条件）
                if (passesTagFilter && passesSearchFilter) {
                    htmlEntry.style.display = "block";
                    entry.classList.remove("filtered-out");
                } else {
                    htmlEntry.style.display = "none";
                    entry.classList.add("filtered-out");
                }
            }

            // フィルタリング結果のアニメーション
            animateFilteredEntries();
        };

        // フィルタリング結果のアニメーション
        const animateFilteredEntries = (): void => {
            const visibleEntries: Element[] = Array.from(logEntries).filter(
                (entry: Element): boolean => !entry.classList.contains("filtered-out"),
            );

            for (const [index, entry] of visibleEntries.entries()) {
                const htmlEntry = entry as HTMLElement;
                htmlEntry.style.opacity = "0";
                htmlEntry.style.transform = "translateY(20px)";

                setTimeout((): void => {
                    htmlEntry.style.transition = "opacity 0.3s ease, transform 0.3s ease";
                    htmlEntry.style.opacity = "1";
                    htmlEntry.style.transform = "translateY(0)";
                }, index * 50);
            }
        };
    });
}
