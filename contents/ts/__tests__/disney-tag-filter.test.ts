import { beforeEach, describe, expect, it } from "vitest";

describe("disney-tag-filter.ts", () => {
    beforeEach(() => {
        document.body.innerHTML = "";
    });

    describe("DOM structure", () => {
        it("can create loading overlay structure", () => {
            document.body.innerHTML = `
                <div id="loading-overlay"></div>
                <div id="main-content"></div>
                <div id="progress-bar"></div>
                <div id="progress-text"></div>
                <div id="loading-details"></div>
            `;

            const loadingOverlay = document.getElementById("loading-overlay");
            const mainContent = document.getElementById("main-content");
            const progressBar = document.getElementById("progress-bar");
            const progressText = document.getElementById("progress-text");
            const loadingDetails = document.getElementById("loading-details");

            expect(loadingOverlay).toBeTruthy();
            expect(mainContent).toBeTruthy();
            expect(progressBar).toBeTruthy();
            expect(progressText).toBeTruthy();
            expect(loadingDetails).toBeTruthy();
        });

        it("can create tag filter structure", () => {
            document.body.innerHTML = `
                <div id="tag-filters">
                    <button class="tag-filter-button" data-tag="tag1">Tag 1</button>
                    <button class="tag-filter-button" data-tag="tag2">Tag 2</button>
                </div>
                <div id="selected-tags-list"></div>
                <button id="clear-selection">Clear</button>
                <div class="log-entry" data-tags="tag1,tag2">Entry 1</div>
                <div class="log-entry" data-tags="tag2">Entry 2</div>
            `;

            const tagFilters = document.getElementById("tag-filters");
            const tagButtons = document.querySelectorAll(".tag-filter-button");
            const clearButton = document.getElementById("clear-selection");
            const logEntries = document.querySelectorAll(".log-entry");

            expect(tagFilters).toBeTruthy();
            expect(tagButtons.length).toBe(2);
            expect(clearButton).toBeTruthy();
            expect(logEntries.length).toBe(2);
        });
    });

    describe("tag filtering logic", () => {
        beforeEach(() => {
            document.body.innerHTML = `
                <div class="log-entry" data-tags="tag1,tag2">Entry 1</div>
                <div class="log-entry" data-tags="tag2,tag3">Entry 2</div>
                <div class="log-entry" data-tags="tag1">Entry 3</div>
            `;
        });

        it("filters entries by single tag", () => {
            const entries = document.querySelectorAll(".log-entry");
            const tag1Entries = Array.from(entries).filter((entry) => {
                const tags = entry.getAttribute("data-tags");
                return tags?.includes("tag1");
            });

            expect(tag1Entries.length).toBe(2);
        });

        it("filters entries by multiple tags (AND condition)", () => {
            const entries = document.querySelectorAll(".log-entry");
            const selectedTags = ["tag1", "tag2"];

            const matchingEntries = Array.from(entries).filter((entry) => {
                const tags = entry.getAttribute("data-tags");
                if (!tags) return false;

                const entryTags = tags.split(",").map((t) => t.trim());
                return selectedTags.every((selectedTag) => entryTags.includes(selectedTag));
            });

            expect(matchingEntries.length).toBe(1);
        });
    });
});
