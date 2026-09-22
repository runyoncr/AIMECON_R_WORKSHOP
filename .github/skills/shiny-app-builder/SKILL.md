---
name: shiny-app-builder
description: 'Turn a validated statistics example page created by page-maker into a companion local Shiny app, append a static preview and download instructions to that same page, and package the app. Use after page-maker creates an example-*.qmd page, or when asked to add Shiny interactivity to an existing example page. Not for workshop activity pages, core chapters, standalone app ideation, API-backed apps, or deployment.'
argument-hint: 'Path to a validated example-*.qmd page created by page-maker'
user-invocable: true
---

# Shiny App Builder

## Outcome and Scope

Extend one validated statistics example page with a focused, offline Shiny companion. Treat the source page as the authority for the topic, teaching claim, data, statistical method, terminology, and default results. Create a locally runnable app, a self-contained ZIP, and a clearly labeled static preview appended to the same page.

This skill is the second stage of the example-page workflow. `page-maker` owns creation and validation of the source example. This skill must not create the underlying lesson, accept workshop activity pages, modify chapters 01-32, add a separate showcase page or book part, deploy an app, or introduce APIs, credentials, persistence, uploads, or network access.

## 1. Resolve and Qualify the Source Page

- Use the `example-*.qmd` path named by the user. If omitted, use one unambiguous example page created in the current task context; otherwise ask for the path.
- Confirm the page is active Quarto source, not generated HTML or archived content, and is registered under the final `Example Pages` part in `_quarto.yml`.
- Confirm it has one unique top-level section ID, a concise statistical teaching claim, and executable local R code that demonstrates that claim.
- Reject activity pages, core chapters, freeform app requests, generated output, and examples that require private data or external services.
- If the page is incomplete or the claim is not meaningfully interactive, return it to `page-maker` or ask the user to narrow the claim. Do not invent new course content to make an app possible.

## 2. Preserve the Teaching Claim

Identify one central claim from the title, opening paragraph, code, and interpretation. Before editing, state:

1. the claim in one sentence;
2. the source calculations and outputs that demonstrate it;
3. the smallest useful controls and outputs that let a learner manipulate it; and
4. one default-state parity check that can falsify the app's fidelity.

Every control must directly illuminate the claim. Do not turn adjacent ideas into additional lessons. Keep the app appropriate for an introductory or intermediate statistics course.

The app's default state must reproduce the page's data, model, estimates, predictions, and visual conclusion within an explicit floating-point tolerance. If parity fails, repair the app rather than changing valid page results to match it.

## 3. Protect Ownership Boundaries

- Preserve the source page's filename, registration, section ID, prose, citations, code chunks, chunk labels, outputs, and section order.
- Append one `## Explore It with Shiny` section after the existing interpretation and references.
- Modify original material only when a correctness defect makes faithful app construction impossible. Explain the defect and obtain approval before broadening scope.
- Do not modify `_quarto.yml` when extending an already registered example page.
- Add only reusable `.shiny-preview-*` rules to `style.css`; do not restyle the book.
- Detect existing generated artifacts and the page extension marker before writing. Revise them in place; never duplicate the section, app directory, ZIP, or links.

## 4. Design the Companion App

Prefer base R plus `shiny`. Reuse another package only when the source page already requires it or it materially simplifies a learner-facing feature.

Create:

- `shiny-apps/<page-stem>/app.R`
- `shiny-apps/<page-stem>/README.md`
- optional local data or assets only when the source page uses them;
- `downloads/<page-stem>-shiny.zip`

The app must:

- run locally without credentials or network access;
- use the source page's defaults and terminology;
- expose only controls mapped to the central claim;
- coordinate controls, numerical summaries, and graphics;
- use accessible labels, keyboard-operable controls, readable color contrast, and a responsive layout;
- distinguish association from causation and retain material statistical limitations from the source page; and
- place statistical calculations in named top-level functions that validation can source without launching the app.

Include a function that returns default-state quantities needed for parity checks. Guard the `shinyApp()` call so validation can source calculation functions without starting a server.

## 5. Document and Preview

Use the bundled templates as structural guidance, not as topic text.

The README must cover purpose, source page, prerequisites, installation, local launch, controls and outputs, synthetic-data provenance, and statistical limitations.

Append exactly one section marked by these comments:

```markdown
<!-- shiny-app-builder:start -->
## Explore It with Shiny
...
<!-- shiny-app-builder:end -->
```

The section must include:

- one paragraph connecting the app to the page's teaching claim;
- a visible `Static preview` label and an explicit statement that it is non-interactive;
- semantic HTML representing one real default app state;
- disabled native controls or non-control representations that cannot appear live;
- output values generated from R objects rather than duplicated by hand;
- a ZIP download link;
- exact extraction, package, and `shiny::runApp()` instructions; and
- concise limitations.

Prefix every preview class with `shiny-preview-`. Ensure tables and text fit at narrow widths and provide useful text alternatives for plots.

## 6. Validate Before Packaging

After the first app edit, run the cheapest parity or parse check before further editing. Then:

1. parse every R file;
2. inventory packages without installing them;
3. resolve all local resources and reject absolute local paths;
4. scan for credentials, API helpers, URLs, network calls, uploads, and transient files;
5. execute source-page logic and the app calculation layer in clean R sessions;
6. compare default coefficients, summaries, predictions, and plotted values within a documented tolerance;
7. verify the extension markers occur exactly once;
8. launch locally without opening a browser when `shiny` is installed, exercise each control without external contact, and stop cleanly;
9. package only after validation succeeds and inspect the ZIP manifest; and
10. render only the extended source page, then inspect desktop and narrow layouts, original outputs, links, preview cues, and render side effects.

Do not install packages, perform a full-book render, edit generated HTML by hand, or repair unrelated failures. Preserve existing user changes.

## 7. Report the Result

Report:

- source page and extracted teaching claim;
- app directory, ZIP, and appended section;
- controls and outputs and how each maps to the claim;
- parity, parse, launch, package, and page-render checks actually completed;
- dependencies or tooling that blocked checks; and
- confirmation that core chapters, navigation, external services, and credentials were untouched.

## Completion Checks

- The source is one validated, registered example page.
- The original lesson remains intact and the app defaults reproduce it.
- Every interactive feature serves the central claim.
- The app works offline and contains no API, credential, upload, persistence, or deployment behavior.
- The canonical app, README, ZIP, and one in-page preview exist without duplicates.
- Validation succeeded or exact limitations are disclosed.
