import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import "../disney-tag-filter";

describe("Disney tag filter search input", () => {
    const testWindow = window as typeof window & {
        initializeSearchInput: (
            searchInput: HTMLInputElement,
            searchQueryRef: { value: string },
            filterLogEntries: () => void,
        ) => void;
        searchDebounceDelayMs: number;
    };

    beforeEach(() => {
        vi.useFakeTimers();
        document.body.innerHTML = '<input type="text" id="search-input" />';
    });

    afterEach(() => {
        vi.clearAllTimers();
        vi.useRealTimers();
    });

    const initialize = (initialQuery = "") => {
        const searchInput = document.getElementById("search-input") as HTMLInputElement;
        const searchQueryRef = { value: initialQuery };
        const filterLogEntries = vi.fn();
        testWindow.initializeSearchInput(searchInput, searchQueryRef, filterLogEntries);

        const input = (value: string) => {
            searchInput.value = value;
            searchInput.dispatchEvent(new Event("input", { bubbles: true }));
        };

        return { filterLogEntries, input, searchQueryRef };
    };

    it("filters a normal input after the debounce delay", () => {
        const { filterLogEntries, input, searchQueryRef } = initialize();
        input("disney");
        expect(searchQueryRef.value).toBe("disney");

        vi.advanceTimersByTime(testWindow.searchDebounceDelayMs - 1);
        expect(filterLogEntries).not.toHaveBeenCalled();
        vi.advanceTimersByTime(1);
        expect(filterLogEntries).toHaveBeenCalledOnce();
    });

    it("only filters the last of multiple rapid inputs", () => {
        const { filterLogEntries, input, searchQueryRef } = initialize();
        for (const value of ["d", "di", "dis"]) {
            input(value);
            vi.advanceTimersByTime(50);
        }
        input("special");
        expect(searchQueryRef.value).toBe("special");

        vi.advanceTimersByTime(testWindow.searchDebounceDelayMs - 1);
        expect(filterLogEntries).not.toHaveBeenCalled();
        vi.advanceTimersByTime(1);
        expect(filterLogEntries).toHaveBeenCalledOnce();
    });

    it("filters an empty input after the debounce delay", () => {
        const { filterLogEntries, input, searchQueryRef } = initialize("disney");
        input("");
        expect(searchQueryRef.value).toBe("");

        vi.advanceTimersByTime(testWindow.searchDebounceDelayMs - 1);
        expect(filterLogEntries).not.toHaveBeenCalled();
        vi.advanceTimersByTime(1);
        expect(filterLogEntries).toHaveBeenCalledOnce();
    });
});
