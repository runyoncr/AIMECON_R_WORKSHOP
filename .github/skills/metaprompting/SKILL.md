---
name: metaprompting
description: 'Review and improve prompts before they are used in an LLM workflow. Use for metaprompting, meta-prompting, prompt review, prompt improvement, critique a prompt, revise a prompt, highlighted prompt, selected prompt, previously executed prompt, pasted prompt, identified prompt, R prompt strings, coding-agent prompts, scoring prompts, batch prompts, chained workflow prompts, or prompts used with call_claude.'
argument-hint: 'Prompt text, selected prompt, recent prompt, or file/chunk containing a prompt'
user-invocable: true
---

# Metaprompting

## Outcome and Scope

Review a prompt before it is used for a downstream LLM task. Produce a concise prompt review, a revised prompt, and testing guidance that helps the user decide whether the prompt is ready to use in a workflow.

Use this skill when the user asks to review, improve, critique, debug, clarify, revise, strengthen, or metaprompt a prompt. The prompt may be highlighted in the editor, pasted into chat, previously executed, stored in an R string, included in a code chunk, or otherwise identified by the user.

Do not execute the prompt, call an external model, submit data to an API, edit source files, or claim the revised prompt will produce deterministic output unless the user explicitly asks for those actions and the operation is safe and authorized. Treat generated changes as suggestions for human review.

## Input Resolution

1. Use the user's current selection when it clearly contains the prompt to review.
2. Otherwise, use the prompt text pasted or quoted in the user's request.
3. Otherwise, inspect the active or identified file for a likely prompt string, prompt template, code chunk, or recent prompt call.
4. If the request refers to a previously executed prompt, use visible terminal, notebook, editor, or chat context when available.
5. If multiple plausible prompts are present, ask which prompt to review before proceeding.
6. If no prompt is identifiable, ask the user to select, paste, or name the prompt.

When reading source files, preserve the user's surrounding code and workflow. Review only the prompt and directly relevant context unless the user asks for a broader workflow review.

## Review Procedure

### 1. Identify the Prompt's Job

Infer or ask for the prompt's intended use:

- downstream task and audience;
- model or tool if known;
- expected inputs and placeholders;
- desired output format;
- workflow type: conversational, transactional, batch, chained, coding-agent, scoring, content generation, feedback, summarization, or another use;
- constraints that must not change, such as rubric language, scoring scales, policy requirements, data-handling rules, or institutional boundaries.

Ask a concise clarification only when a missing detail would materially change the revised prompt.

### 2. Diagnose the Current Prompt

Evaluate the prompt for:

- clear task definition and success criteria;
- missing context or unstated assumptions;
- ambiguous terms, roles, criteria, scales, or edge cases;
- instruction hierarchy and possible conflicts;
- placeholder names and input boundaries;
- output format, schema, parseability, and validation checks;
- examples or counterexamples when they would improve consistency;
- reproducibility and stability for repeated use;
- privacy, security, sensitive-data, and external-submission risks;
- domain-specific risks, especially in educational measurement, scoring, feedback, and assessment design.

Preserve effective wording and the author's intent. Do not rewrite for style alone.

### 3. Revise the Prompt

Provide a revised prompt that is ready to test. Use explicit sections when helpful, such as:

- role or context;
- task;
- input variables;
- constraints;
- process or decision rules;
- output format;
- edge cases;
- self-checks or validation requirements.

Do not invent new rubric criteria, scoring rules, policies, facts, citations, private context, or data definitions. If the prompt needs a decision that only the user or a subject-matter expert can make, mark it as `Needs human decision`.

## Prompt-Type Guidance

### Scoring or Rubric Prompts

- Preserve all existing criteria, score scales, labels, and construct definitions unless the user asks to revise them.
- Flag vague rubric language, undefined score anchors, missing partial-credit rules, and edge cases.
- Recommend structured output when scores will be parsed downstream.
- Separate scoring instructions from feedback, rationale, or quality-control instructions.

### Transactional, Batch, or API Prompts

- Prefer stable output formats such as JSON when downstream parsing matters.
- Include required fields, allowed values, and instructions about text outside the structured output.
- Identify inputs that should be parameterized rather than hard-coded.
- Suggest a small test set before scaling.

### Chained Workflow Prompts

- Clarify what each step consumes and returns.
- Check whether early-step ambiguity can affect later steps.
- Recommend validation after each step before passing output downstream.

### Coding-Agent Prompts

- Clarify scope, target files, permissions, and stopping conditions.
- Ask for a plan before implementation when the task is broad or risky.
- Include validation expectations such as tests, render checks, linting, or diff review.
- State what the agent should not touch.

### Creative or Brainstorming Prompts

- Preserve openness where it is useful.
- Clarify audience, constraints, tone, and selection criteria.
- Avoid over-structuring unless the output must be compared, parsed, or reused.

## Response Format

Use this format by default. Keep the response brief for short prompts and more detailed for high-stakes prompts.

### Summary

State what the prompt is trying to do and whether it is ready to test, needs revision, or needs a human decision first.

### Strengths

List what is already clear or useful.

### Issues to Fix

Prioritize ambiguities, missing constraints, output-format risks, and downstream workflow risks. Explain why each issue matters.

### Revised Prompt

Provide the revised prompt in a fenced code block. Preserve placeholders and code-friendly formatting where applicable.

### Testing Plan

Suggest a small, concrete way to test the revised prompt before using it broadly. Include representative examples, edge cases, and output checks when relevant.

### Open Decisions

List decisions that require the user, a subject-matter expert, or an institutional policy. Use `None` if there are no open decisions.

## Boundaries and Safety

- Do not send prompt content to external services unless explicitly authorized.
- Do not run paid model calls unless explicitly authorized.
- Do not include or request secrets, API keys, private learner data, or restricted institutional data in chat.
- If the prompt contains sensitive or identifying information, suggest replacing it with placeholders or reviewing it only in an approved local environment.
- Do not claim that prompt changes guarantee accuracy, fairness, validity, reproducibility, or deterministic behavior.
- Do not silently change the task's construct, rubric, audience, or policy requirements.

## Completion Criteria

- The prompt under review is clearly identified, or the user was asked to identify it.
- The review preserves the user's intent and non-negotiable constraints.
- Material ambiguities, missing context, output-format risks, and workflow risks are surfaced.
- A revised prompt is provided unless clarification is required first.
- The response includes a practical testing plan and open decisions.
- No unauthorized API call, external submission, file edit, or sensitive-data exposure occurred.
