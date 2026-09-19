---
name: page-maker
description: 'Create a first-draft Quarto page for the AIMECON R workshop manuscript. Route integrated pages through contextual and operational review; append concise example pages to the end of the workbook. Use when asked to build, create, draft, or add a new page or chapter, demonstrate page creation, promote an example into the manuscript, or use /page-maker. Not for routine review of an existing page.'
argument-hint: 'Integrated page or example page, topic, and any placement or length preferences'
user-invocable: true
---

# Page Maker

## Outcome and Scope

Create a coherent, accurate, executable first draft, not a claim of publication readiness. Use one of two workflows: **integrated page** for content within the teaching sequence, or **example page** for a short demonstration appended to the workbook. Both require correct content and honest validation; their editorial depth and placement differ.

Work in the current repository. Paths below are relative to its root. Preserve existing user changes. Do not commit, publish, rewrite shared helpers, change global execution settings, or repair unrelated pages. Creating a page does not authorize paid API calls, sensitive-data submission, package installation, bulk jobs, or full-book rendering. Ask separately when such actions are needed. Use the existing reviewer skill for routine updates to an existing page; promoting an example into the main sequence uses this skill's integrated workflow.

## 1. Determine Page Type Before Doing Downstream Work

- If the build request explicitly specifies **integrated page** or **example page**, including unambiguous equivalent wording, use that mode without asking again. Honor a mode already explicitly established for this build in the conversation.
- If the user explicitly changes the mode later, follow the latest instruction without asking again and reassess remaining work under that workflow. Do not silently discard existing edits; explain any changes needed to placement or scope. Clarify only genuinely conflicting or ambiguous instructions.
- If the mode is missing, ambiguous, or conflicting, ask: **"Should this be an integrated page or an example page?"** Offer those two options, using the available question tool, and wait for the answer.
- Do not research the topic, inspect chapters for placement, draft content, or edit files until the mode is resolved. Do not infer mode from the topic, active file, or apparent simplicity. "A page about suppression variables" does not specify a mode.
- If the topic is missing, ask for it alongside the mode question when possible. Once both are known, state the selected mode and route to the matching workflow. Do not impose integrated-page intake or curriculum review on example pages.

## 2. Establish a Bounded Baseline

- Read applicable repository instructions, inspect the working diff, and read `_quarto.yml`. Use `book.chapters` to identify published source pages, reading order, and parts. Check for appendices or other configured end matter before promising final placement.
- Inspect only the nearby conventions and dependencies needed for the selected mode. Use active source pages, not generated HTML or archived pages, as the primary content reference. Do not scan the entire manuscript for a simple example's curriculum fit.
- Check the proposed filename, section IDs, chunk labels, and navigation entry for collisions. Never overwrite an existing page or add a duplicate chapter entry silently. If a prior build exists, ask whether to revise it or create a separately named page, unless the user already specified that choice.
- Read execution settings and relevant project hooks before running code. Evaluation may be enabled globally; a render can execute code, call APIs, and modify generated files beyond the target page.
- Use the project's citation conventions and `references.bib` when citations are needed. Reuse existing keys rather than duplicating entries. Do not invent sources, outputs, personal anecdotes, or author experiences. Keep editorial questions and operational notes in the final report, not in learner-facing prose.
- In both modes, verify substantive factual claims with appropriate primary or methodological sources. Time-sensitive claims require current documentation and a recorded verification date; example mode reduces prose and curriculum review, not factual scrutiny. Qualify or omit unsupported claims and disclose unresolved evidence in the report.

## 3A. Integrated Page Workflow

### Establish Context and Placement

- Identify the topic's angle, audience, prerequisites, learning outcomes, page type (explanation, activity, or reference), and approximate teaching time or length. Infer reasonable defaults from the request and relevant chapters; ask only about consequential gaps.
- Search published source pages for the proposed topic and read relevant passages, especially the preceding page and likely neighbors. Check existing ownership, terminology, prerequisites, examples, and duplication. Favor a short reminder and cross-reference over repeating a substantial explanation already taught elsewhere.
- If the proposal largely duplicates an existing chapter, explain that and ask whether to extend the existing page or create a distinct new page. Do not silently switch scope.
- Propose placement with a brief pedagogical rationale and outline. Treat explicit placement in the request as authorization for that placement; otherwise obtain approval before changing book order. Show any effects on neighboring transitions, prerequisites, or downloads, and obtain approval for edits beyond the new page, its registration, and necessary citations.

### Draft and Review

- Match the depth, conversational voice, notation, and code conventions of nearby pages without manufacturing first-person experiences. Explain relevance, develop the essential concept, demonstrate it where appropriate, interpret results, and address important misconceptions. Add an exercise, takeaways, or further reading only when useful; do not force identical headings on every page.
- Verify time-sensitive model, API, pricing, limit, and package claims using current official documentation or release notes. Record source URLs, verification date, and relevant versions for the report. Use reliable methodological sources for statistical claims. If evidence is unavailable, disclose the limitation and qualify or omit unsupported assertions.
- Review operations thoroughly: package and runtime prerequisites, data provenance, file paths and writes, API authentication, return structures, cost/scale, external submissions, and error behavior. Ensure preceding pages actually supply assumed knowledge and setup.
- For applicable Claude examples, read `downloads/call_claude_AIMECON26.R` and use its actual callable interface, dependencies, and return values. Do not infer a function name from its filename. If absent or ambiguous, ask rather than substituting another helper. Preserve examples intentionally teaching another provider or direct API; do not inspect or require this helper for unrelated statistics pages.
- Create companion notebooks or assets only when needed by the requested activity and established conventions. Verify source/download links. Do not execute unverified remote scripts.

### Integrate Without Unnecessary Renaming

- `_quarto.yml` controls reading order; numeric filename prefixes are a repository convention, not a Quarto ordering requirement. Register the new page at the approved location. Keep any existing example addendum at the end.
- Do not renumber existing pages merely because a page is inserted. If sequential renumbering is requested or necessary to satisfy an explicit convention, prepare an old-to-new filename map and obtain approval before moving existing files.
- Before an approved rename, inventory affected maintained source, configuration, scripts, and download references, including explicit `.qmd` and `.html` links. Check collisions, case-only changes, and the destination of every move. Use a collision-safe sequence, such as temporary intermediate names, rather than overwriting files.
- Preserve stable section IDs, update affected references and chapter entries, and consider changed published URLs and external bookmarks. Explain any compatibility limitation; seek approval before adding a redirect mechanism. Do not rewrite historical archives or hand-edit generated HTML. Report unrelated pre-existing broken links without fixing them in this task.

## 3B. Example Page Workflow

### Build a Small, Self-Contained Example

- The topic and intended demonstration usually suffice. Skip broad curriculum-alignment review and lengthy outline approval. An unrelated or mundane topic is valid; do not ask the user to justify its fit with the workshop.
- Use minimal text and easy-to-follow syntax. Default to this flexible structure:
  1. **Purpose:** one or two sentences explaining the concept or question.
  2. **Data:** short, readable data generation or setup when applicable.
  3. **Example:** sequential code performing the operation or analysis and showing relevant output.
  4. **Interpretation:** a few sentences explaining what the output demonstrates and any essential caveat.
- Prefer small synthetic datasets, a fixed seed when randomness is used, descriptive variable names, familiar operations, and visible intermediate steps. Prefer base R when it keeps the example simple; reuse installed packages when they materially improve clarity. Avoid unnecessary abstractions, dense pipelines, hidden session objects, and new dependencies.
- Include data generation on the page when applicable so the code runs from a clean session. Label synthetic data accurately. Do not require credentials, private files, downloads, or network services by default.
- Do not add a literature review, extensive background, mandatory objectives, exercises, or companion notebooks unless requested. Retain indispensable citations and qualifications. Minimal text is not permission to omit context essential to correctness.
- Validate that the example demonstrates the claimed phenomenon, not merely that it executes. For statistical suppression, inspect the relevant correlations and fitted-model comparisons and ensure the interpretation describes suppression without an unsupported causal claim. A random dataset is not sufficient evidence by itself.

### Append to the Workbook

- Use a stable descriptive filename such as `example-statistical-suppression.qmd`, with a unique section ID. Do not renumber or reorder existing pages for an example.
- Append the page after all existing workbook content. With the current `book.chapters` structure, create a final **Example Pages** part if none exists, or append after the existing examples in that final part. Reuse the part instead of adding a duplicate heading on subsequent runs.
- If later configuration introduces appendices, end matter, or a non-final example section that prevents this simple append, explain the conflict and ask before reorganizing existing content. Do not silently put the example before existing end matter or recategorize pages.
- Keep the existing chapter sequence unchanged. Register the new page exactly once. Example-page mode authorizes this append; no separate placement question is needed.
- If the user later promotes an example into the main manuscript, route it through the integrated workflow, including contextual and operational review, before relocating it.

## 4. Validate Within the Selected Scope

- After the first coherent edit, run the cheapest relevant syntax or behavior check before expanding the change. Parse YAML and R as applicable, verify source/resource paths, unique identifiers, citation keys, and the page's chapter registration.
- Run bounded, non-sensitive local examples in a clean R session, with their required setup in document order, unless execution was explicitly excluded. Check prerequisites without installing missing packages automatically. Verify the intended teaching result as well as successful execution.
- Before executing paid, external-data, or bulk operations, obtain authorization with the expected request count and scope; ask when cost or scale is unclear. Check credentials without revealing values. Never request secrets in chat or print environment dumps. Missing authorization is not permission to fabricate output or silently enable execution.
- Preserve `eval: false` / `eval=FALSE` on credential setup and intentionally non-executed examples. Ensure unauthorized side-effecting chunks cannot execute during validation or ordinary rendering of the new draft. Use explicit chunk-level controls rather than changing global execution settings, and disclose unexecuted examples.
- Render only the target page when feasible, after checking project hooks and side effects. Inspect visible output, warnings, formatting, links, and consistency between the prose and observed results. Do not claim output was refreshed based on parsing alone, and do not hand-edit rendered HTML.
- For example pages, verify final placement and that the prior chapter sequence is unchanged. For integrated pages, verify approved placement and all references affected by any rename. A successful single-page render does not prove cross-chapter links are intact. Ask before any required full-book render; approval for rendering does not authorize unrelated live API execution.
- Inspect the final diff and generated changes. Preserve pre-existing work and follow repository tracking conventions. Report broader render side effects; never indiscriminately revert files. If runtime, dependencies, sources, network, or credentials block checks, finish safe work and clearly distinguish verified content from unexecuted or unverified material.

## 5. Report the Result

Keep the report proportional to the selected mode. Include:

- **Mode and page:** integrated or example, source path, purpose, and placement.
- **Changes:** created or modified source, citations, companion files, navigation entries, and any approved renames or generated artifacts.
- **Validation:** checks actually run, whether examples executed and visible output was inspected, and any blockers or skipped checks. Do not claim predictable exact output from stochastic models.
- **Editorial follow-up:** remaining decisions or limitations. For integrated pages also summarize alignment/overlap review, operational risks, and currency evidence with source URLs and verification dates. Keep example reports short.

## Completion Checks

- An explicit mode was supplied or the user answered the routing question before downstream work began.
- The draft follows the selected editorial scope and contains no fabricated sources, output, or author experiences.
- Integrated placement and any rename map were approved; affected links and prerequisites were checked.
- Example pages are concise, reproducible, and appended at the end without changing existing chapter order or filenames.
- No duplicate page, part, citation key, or identifier was introduced by a repeated build.
- Validation succeeded or precise limitations were disclosed; paid or sensitive operations stayed within explicit authorization.

## Routing Acceptance Cases

Use these cases when reviewing changes to this skill; do not create extra pages just to exercise them during an ordinary build.

| Request | Expected behavior |
| --- | --- |
| `/page-maker integrated page about a new LLM capability` | Use integrated review; propose placement if absent. |
| `/page-maker example page about suppression variables` | Use concise, self-contained code and append to the end. |
| `/page-maker a page about suppression variables` | Ask the two-option mode question and wait; do not infer example mode. |
| `/page-maker a short page` | Ask for mode and topic before downstream work. |
| A request calling the same page both integrated and example | Clarify mode before drafting or changing the book. |
| Another example after one already exists | Append after it, reuse the final part, and leave earlier pages unchanged. |
| A request whose filename already exists | Ask revise versus alternate filename unless already specified; never overwrite silently. |
| Promote an existing example into the manuscript | Apply integrated review and approved placement before moving it. |