---
name: workshop-content-reviewer
description: 'Review and update an AIMECON R workshop page: correct grammar, improve clarity and conciseness, fact-check current LLM developments, avoid duplication across chapters, migrate examples to call_claude_AIMECON26.R, rerun R chunks, refresh actual output, and report changes. Use when asked to review, refresh, modernize, or update workshop content in a Quarto or R Markdown page, or to use workshop_content_reviewer.'
argument-hint: 'Workshop page path, or use the active page'
user-invocable: true
---

# workshop_content_reviewer

## Outcome and Scope

Produce a clear, current, executable workshop page and a concise change report. Work on the requested page; if none is named, use the active workshop `.qmd` or `.Rmd` page. Ask for a target if neither is unambiguous. Review the whole page, not just selected text.

Read other chapters to check overlap, but do not edit them without approval. Preserve existing user changes, learning objectives, audience level, section IDs, citations, and the workshop's voice. Do not rewrite the shared helper, change project-wide execution settings, or render the entire book without approval.

## Procedure

### 1. Establish the Local Baseline

- Read applicable repository instructions, the complete target page, its source dependencies, and relevant Quarto/R Markdown configuration. Inspect the working diff before editing.
- Locate `downloads/call_claude_AIMECON26.R` and read its current implementation. If it has moved, search for the exact filename and its references. Do not substitute a similarly named helper or invent its interface; ask when the canonical source is missing or ambiguous.
- Verify the helper's exported function name, arguments, defaults, dependencies, authentication method, return value, and error behavior from source. The filename and callable function name are not necessarily identical.
- Inventory every code chunk, source/download link, inline R expression, displayed result, and prose claim dependent on model behavior. Note old helper names, stale output, and non-executed teaching examples.
- Check execution options, caching/freezing, prerequisites, and runtime availability before running anything. This workshop may enable evaluation globally, so rendering can trigger live API calls.

### 2. Verify Currency and Chapter Ownership

- Identify time-sensitive claims about model availability, API parameters, pricing, limits, tools, packages, and recommended practices. Use current official provider documentation, release notes, and package documentation to verify material changes; do not rely on remembered model names or unsupported release claims.
- Record source URLs and the verification date for the report. Maintain the page's existing citation conventions when adding sources to the teaching material. If sources are inaccessible, mark those claims unverified instead of presenting guesses as updates.
- Use `_quarto.yml` to identify published workshop chapters, then search their source text for each proposed new topic or explanation. Read the matching passages, excluding generated HTML and archived pages from the primary duplication check.
- Keep substantial explanations in their existing owning chapter. On the target page, use a short contextual reminder and a working cross-reference instead of duplicating the explanation. Retain repetition when it is necessary for a self-contained exercise, and explain that choice in the report.
- Add developments only when they advance this page's learning objectives. If current guidance conflicts with the canonical helper, report the conflict and seek approval for a helper change rather than silently changing the shared implementation.

### 3. Edit Writing and Function Usage

- Correct grammar, punctuation, spelling, terminology, and inconsistent wording across headings, paragraphs, lists, tables, captions, callouts, instructions, and learner-facing code text. Improve clarity and conciseness without changing pedagogical meaning.
- Update factual claims based on verified sources. Avoid gratuitous expansion, unnecessary jargon, or broad stylistic rewrites.
- Update all applicable LLM examples, wrappers, setup instructions, source calls, and download links to use the canonical `call_claude_AIMECON26.R` helper and its actual current callable interface. Check stale filenames even when the exported function name is unchanged.
- Adjust arguments, response extraction, printing, and downstream processing to match the real return value. Preserve useful unrelated R functions and examples intentionally teaching other providers or direct API calls; do not force incompatible examples through this helper. Document exceptions or ask if migrating them would change the lesson.
- Prefer a local canonical source during validation. Preserve suitable workshop download/source conventions in learner-facing code, but verify that any remote helper is the intended version before executing it. Do not execute an unverified remote script.
- Make the smallest coherent edit, then run a focused syntax or behavior check before expanding the change. Repair defects in that same example before continuing.

### 4. Execute and Refresh Output

- Determine which chunks issue paid requests, submit data externally, install software, write files, or create batch jobs. Invoking this skill authorizes bounded live API calls for the selected page's ordinary examples using configured credentials and non-sensitive teaching data, unless the user requests a review without execution. Ask separately before submitting sensitive data, launching bulk jobs, or running unusually costly examples; if expected cost or scale is unclear, ask before proceeding. Bound the run to the necessary examples and minimal retries.
- Check required credentials without displaying their values. Never print environment dumps, embed real keys, request secrets in chat, or include them in rendered output or reports. Have the user configure missing secrets locally. Do not submit private learner data without explicit authorization.
- Run safe parsing and prerequisite checks first. Then execute the intended examples in document order in a clean R session, including required setup chunks. Use the repository's established runtime and render workflow when available.
- Preserve `eval: false` / `eval=FALSE` on credential setup and intentionally non-executed examples. Execute only safe authorized equivalents when needed; do not enable every chunk merely to obtain output.
- Refresh cached or frozen results only for the target page as necessary. Render only that page when feasible, accounting for project hooks and configuration that could execute additional code. Stop and request approval if validation requires broader execution.
- Capture genuine helper output and verify downstream chunks still work. Refresh generated output through execution/rendering, not hand-edited HTML. Update manually embedded sample output only from the observed run, and label stochastic output as an example where appropriate.
- Check that visible output actually appears in the rendered page; successful execution alone is insufficient when chunk options hide results. Inspect warnings/errors, formatting, links, and consistency between prose, code, and output. Do not assert an exact generated response will recur.
- If credentials, dependencies, network access, model access, or runtime tooling prevent execution, complete the safe edits and checks. Clearly mark output as not refreshed, identify any retained stale sample, and give the precise remaining steps. Never fabricate output or claim an unperformed render succeeded.
- Inspect the final diff for unintended changes or exposed secrets. Keep generated artifacts only when consistent with repository tracking conventions and within the approved scope. Do not delete or revert unrelated existing changes.

### 5. Report Changes

Provide a Markdown report in the final response by default. Include:

- **Page and summary:** target path, review date, and the main improvements.
- **Writing:** meaningful grammar, clarity, and conciseness changes, with section references.
- **LLM updates:** changed claims, primary-source URLs, verification date, and unresolved currency questions.
- **Duplication check:** chapters checked, overlap found, cross-references used, and intentional repetition retained.
- **Function migration:** canonical helper path, interface changes, updated source/download links, and exceptions.
- **Execution and output:** commands/checks actually run, which examples called the API, whether output was refreshed, render results, and any known usage/cost information without guessing costs.
- **Files and follow-up:** changed source/generated files, skipped checks, blockers, and required next actions. Distinguish completed work from unverified work.

Keep the report proportional to the change; state "None" for categories with no changes. Never include credentials or sensitive request/response data. Save a report file only when requested or required by an existing repository convention.

## Completion Criteria

- All writing on the selected page has been reviewed.
- Material LLM updates are source-verified or explicitly marked unverified.
- Proposed additions were checked against other published workshop chapters.
- Applicable function usage and associated links match the canonical helper.
- Syntax checks and authorized execution/rendering have passed, or precise blockers and stale outputs are disclosed.
- The report identifies actual changes, evidence, and remaining work without claiming checks that were not run.