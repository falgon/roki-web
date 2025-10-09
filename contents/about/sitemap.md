```{lang=mermaid}
graph TD;
    s[roki.dev]
    s[roki.dev]---b1[roki.log]
    s---b2[roki.diary]
    s---r[resume]
    s---d[Disney Experience Summary]
    subgraph "Blogs"
    b1
    b2
    end
    click s "https://roki.dev"
    click b1 "https://roki.dev/roki.log"
    click b2 "https://roki.dev/roki.diary"
    click r "https://roki.dev/resume"
    click d "https://roki.dev/disney_experience_summary/jp.html"
```