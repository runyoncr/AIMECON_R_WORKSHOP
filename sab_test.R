# ─────────────────────────────────────────────────────────────────────────────
# Science Essay Assessment Builder
# AIMECON R Workshop – Prompt Chaining Activity
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)
library(shinyjs)
library(glue)
library(jsonlite)
library(dplyr)
library(httr)

# ══════════════════════════════════════════════════════════════════════════════
# HELPERS
# ══════════════════════════════════════════════════════════════════════════════

call_claude <- function(prompt, max_tokens = 2000) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(api_key) == 0) stop("ANTHROPIC_API_KEY environment variable is not set.")
  resp <- POST(
    "https://api.anthropic.com/v1/messages",
    add_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ),
    body = toJSON(list(
      model      = "claude-sonnet-4-20250514",
      max_tokens = max_tokens,
      messages   = list(list(role = "user", content = prompt))
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  if (http_error(resp)) stop("API error: ", content(resp, as = "text"))
  content(resp, as = "parsed")$content[[1]]$text
}

clean_json <- function(x) {
  x <- gsub("```[a-zA-Z]*\\n?", "", x)
  x <- gsub("`", "", x)
  x <- trimws(x)
  m <- regmatches(x, regexpr("(\\{[\\s\\S]*\\}|\\[[\\s\\S]*\\])", x, perl = TRUE))
  if (length(m) > 0) x <- m
  tryCatch(
    fromJSON(x),
    error = function(e) stop("JSON parse failed: ", e$message)
  )
  x
}

# ══════════════════════════════════════════════════════════════════════════════
# LOAD EXTERNAL DATA (once at startup)
# ══════════════════════════════════════════════════════════════════════════════

load(url("https://raw.githubusercontent.com/runyoncr/AIMECON_R_WORKSHOP/main/data/science_los.Rdata"))
load(url("https://raw.githubusercontent.com/runyoncr/AIMECON_R_WORKSHOP/main/data/writing_los.Rdata"))

pld_guidance <- paste(
  readLines("https://raw.githubusercontent.com/runyoncr/AIMECON_R_WORKSHOP/main/data/pld_guidance.txt"),
  collapse = "\n"
)
feedback_guidance <- paste(
  readLines("https://raw.githubusercontent.com/runyoncr/AIMECON_R_WORKSHOP/main/data/feedback_guidance.txt"),
  collapse = "\n"
)

science_lo_text <- science_los |>
  mutate(entry = paste0("[Sub-domain] ", sub_domain, "\n[Learning Objective] ", learning_objective)) |>
  pull(entry) |>
  paste(collapse = "\n\n")

writing_lo_text <- writing_los |>
  mutate(entry = paste0("[Writing Learning Objective] ", writing_learning_objective)) |>
  pull(entry) |>
  paste(collapse = "\n\n")

all_writing_los <- writing_los$writing_learning_objective

# ══════════════════════════════════════════════════════════════════════════════
# PROMPT BUILDERS
# ══════════════════════════════════════════════════════════════════════════════

choose_topic_prompt <- function(science_lo_text) {
  glue(
    "You are developing a constructed-response item for a middle-school science assessment.

Below is the approved list of science sub-domains and learning objectives.
You may ONLY select from these options.

{science_lo_text}

TASK:
Select ONE sub-domain and ONE learning objective from the list above.
Do not invent or rephrase learning objectives; return them exactly as written.
Return only the selected items in this exact format with no other text:

[Sub-domain] <selected sub-domain>
[Learning Objective] <selected learning objective>"
  )
}

pick_writing_los <- function(science_topic, writing_lo_text) {
  glue(
    "You are assisting with the development of a constructed-response item \
for a middle-school science assessment.

The selected science topic is:
{science_topic}

Below is the approved list of writing learning objectives.
You may ONLY select from this list.

{writing_lo_text}

TASK:
Select TWO writing learning objectives that are most appropriate for this science topic.
Do not rephrase, invent, or combine objectives.

OUTPUT FORMAT:
Return ONLY a JSON object with no other text:

{{
  \"writing_lo1\": \"<first selected writing learning objective>\",
  \"writing_lo2\": \"<second selected writing learning objective>\"
}}"
  )
}

write_essay_prompt <- function(guiding_los) {
  glue(
    "You are assisting with the development of a constructed-response item \
for a middle-school science assessment.

Below is the approved context and learning objectives as a JSON object. \
All existing fields must be preserved exactly in your output.

{guiding_los}

TASK:
Write ONE constructed-response item stem aligned with the science learning objective.

REQUIREMENTS:
- Use clear language appropriate for grades 6-8.
- Prompt scientific explanation without mentioning rubrics, scoring, or writing skills.
- Be answerable in one short paragraph (approximately 5-8 sentences).

OUTPUT FORMAT:
Return the same JSON object with one additional field \"item_stem\". \
Do not include any text outside the JSON object."
  )
}

build_step4_prompt <- function(item_components, pld_guidance) {
  glue(
    "You are assisting with the development of an analytic scoring rubric \
for a middle-school science assessment.

ITEM SPECIFICATION:
The following JSON object contains the current item specification. \
All existing fields must be preserved exactly in your output.

{item_components}

GUIDANCE ON WRITING PERFORMANCE LEVEL DESCRIPTORS:
{pld_guidance}

TASK:
Using the learning objectives in the JSON above, write performance level \
descriptors for each learning objective. The rubric must include:
- One criterion for the science learning objective
- One criterion for each of the two writing learning objectives

For each criterion, define three performance levels: Weak, Developing, and Competent.

CONSTRAINTS:
- Do not refer to specific scores or point values.
- Do not include feedback directed at the student.
- Do not change the wording of any learning objective.
- Write descriptors in clear, practical language appropriate for educators.

OUTPUT FORMAT:
Return the same JSON object with one additional top-level field called \"rubric\". \
Do not include any text outside the JSON object.

{{
  \"rubric\": {{
    \"science_learning_objective\": {{
      \"weak\": \"<description>\",
      \"developing\": \"<description>\",
      \"competent\": \"<description>\"
    }},
    \"writing_learning_objective_1\": {{
      \"weak\": \"<description>\",
      \"developing\": \"<description>\",
      \"competent\": \"<description>\"
    }},
    \"writing_learning_objective_2\": {{
      \"weak\": \"<description>\",
      \"developing\": \"<description>\",
      \"competent\": \"<description>\"
    }}
  }}
}}"
  )
}

build_synthetic_essay_prompt <- function(rubric_json, science_lo, writing_lo1, writing_lo2) {
  glue(
    "You are generating a realistic synthetic essay written by a middle-school student \
in response to a science constructed-response item.

ITEM RECORD:
{rubric_json}

ASSIGNED PERFORMANCE LEVELS:
- Science learning objective:   {science_lo}
- Writing learning objective 1: {writing_lo1}
- Writing learning objective 2: {writing_lo2}

The three performance dimensions are independent. Write the essay so that each \
dimension is clearly at its assigned level without artificially inflating or \
deflating the others.

REALISM GUIDELINES:
- Voice, sentence structure, and vocabulary appropriate for grades 6-8.
- Weak: genuine misunderstanding or omission, not carelessness.
- Developing: partial knowledge, a real attempt, not simply less writing.
- Competent: meets grade-level expectations, not high school or adult level.
- Do not include meta-commentary or signals that reveal the intended performance levels.

OUTPUT FORMAT:
Return the same JSON object with one additional top-level field \"student_essay\". \
Do not include any text outside the JSON object."
  )
}

build_scoring_prompt <- function(essay_json) {
  glue(
    "You are scoring a middle-school student's essay response to a science \
constructed-response item.

ITEM RECORD:
{essay_json}

TASK:
Assign a performance level for each criterion based on the rubric descriptors \
and the student's essay.

SCORING GUIDELINES:
- Score each criterion independently.
- Base ratings entirely on what is present in the essay.
- Select the level whose descriptor best characterizes the response as written.
- When between levels, select the lower level.

OUTPUT FORMAT:
Return the same JSON object with three additional top-level fields. \
Do not include any text outside the JSON object.

{{
  \"science_learning_objective_rating\": \"<weak | developing | competent>\",
  \"writing_learning_objective_1_rating\": \"<weak | developing | competent>\",
  \"writing_learning_objective_2_rating\": \"<weak | developing | competent>\"
}}"
  )
}

build_step6_prompt <- function(scored_essay, feedback_guidance) {
  glue(
    "You are generating written feedback for a middle-school student \
based on their performance on a constructed-response science item.

ITEM RECORD:
{scored_essay}

GUIDANCE ON WRITING STUDENT FEEDBACK:
{feedback_guidance}

TASK:
Using the student's response, the rubric, and the assigned performance levels, \
write specific, constructive feedback for the student. \
Provide a separate comment for each learning objective.

CONSTRAINTS:
- Write directly to the student in age-appropriate language.
- Base each comment on what is actually present in the student's response.
- Do not name or imply the performance level label (weak, developing, competent).
- Do not restate rubric descriptor language verbatim.
- Do not include grades, points, or score values.
- Include one specific, actionable suggestion per objective where relevant.

OUTPUT FORMAT:
Return the same JSON object with one additional top-level field \"student_feedback\". \
Do not include any text outside the JSON object.

{{
  \"student_feedback\": {{
    \"science_learning_objective\": \"<feedback text>\",
    \"writing_learning_objective_1\": \"<feedback text>\",
    \"writing_learning_objective_2\": \"<feedback text>\"
  }}
}}"
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# THEME & CSS
# ══════════════════════════════════════════════════════════════════════════════

app_theme <- bs_theme(
  version      = 5,
  primary      = "#6495ED",
  bg           = "#f7f8fc",
  fg           = "#1c2033",
  base_font    = font_google("Source Sans 3"),
  heading_font = font_google("Lora"),
  "border-radius"     = "8px",
  "btn-border-radius" = "6px"
)

app_css <- "
  body { padding-bottom: 60px; }

  .step-card {
    background: white;
    border: 1px solid #dde3f0;
    border-radius: 12px;
    padding: 26px;
    margin-bottom: 20px;
    box-shadow: 0 2px 10px rgba(100,149,237,.07);
    transition: opacity .35s ease;
  }
  .step-card.locked {
    opacity: .3;
    pointer-events: none;
    user-select: none;
  }
  .step-header {
    display: flex;
    align-items: center;
    gap: 13px;
    margin-bottom: 18px;
    padding-bottom: 14px;
    border-bottom: 1px solid #eef0f8;
  }
  .step-num {
    width: 34px; height: 34px;
    border-radius: 50%;
    background: #6495ED;
    color: white;
    font-weight: 700; font-size: .88rem;
    display: flex; align-items: center; justify-content: center;
    flex-shrink: 0;
    transition: background .4s;
  }
  .step-num.done { background: #2e8b57; }
  .step-title { font-size: 1.05rem; font-weight: 600; margin: 0; color: #1c2033; }

  .info-box {
    background: #f0f4ff;
    border-left: 3px solid #6495ED;
    border-radius: 0 6px 6px 0;
    padding: 13px 16px;
    font-size: .92rem;
    margin-bottom: 14px;
    white-space: pre-wrap;
    line-height: 1.55;
  }

  /* PLD table */
  .pld-table { font-size: .82rem; margin-bottom: 0; }
  .pld-table thead th { color: white; font-weight: 600; }
  .pld-th-lo   { background: #4a5a78 !important; }
  .pld-th-weak { background: #a93226 !important; }
  .pld-th-dev  { background: #7d6608 !important; }
  .pld-th-comp { background: #1d6a3a !important; }
  .pld-table td { vertical-align: top; padding: 10px 12px; }
  .pld-table tr:nth-child(odd) td { background: #fafbff; }

  /* Rating badges */
  .badge-weak       { background:#fdecea; color:#8e1f17; border-radius:12px; padding:4px 12px; font-size:.81rem; font-weight:600; display:inline-block; }
  .badge-developing { background:#fef9e7; color:#7d6608; border-radius:12px; padding:4px 12px; font-size:.81rem; font-weight:600; display:inline-block; }
  .badge-competent  { background:#e9f7ef; color:#1a5c35; border-radius:12px; padding:4px 12px; font-size:.81rem; font-weight:600; display:inline-block; }

  /* Score + feedback cards */
  .score-card {
    border: 1px solid #dde3f0;
    border-radius: 8px;
    padding: 14px 12px;
    text-align: center;
    background: white;
  }
  .score-lo-name {
    font-size: .76rem;
    font-weight: 600;
    color: #5a6a8a;
    margin-bottom: 8px;
    line-height: 1.35;
  }
  .feedback-card {
    border: 1px solid #dde3f0;
    border-radius: 8px;
    padding: 16px 18px;
    margin-bottom: 14px;
    background: #fafbff;
  }
  .feedback-lo-label {
    font-size: .73rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: .07em;
    color: #6495ED;
    margin-bottom: 7px;
  }

  /* Hero */
  .app-hero {
    background: linear-gradient(135deg, #4270c8 0%, #6495ED 55%, #82abf5 100%);
    color: white;
    padding: 30px 34px;
    border-radius: 14px;
    margin-bottom: 28px;
  }
  .app-hero h2 { margin: 0 0 7px; font-size: 1.55rem; font-weight: 700; }
  .app-hero p  { margin: 0; opacity: .88; font-size: .93rem; }

  /* Misc */
  textarea { font-size: .92rem !important; }
  .form-check-inline { margin-right: 1.2rem; }
"

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- page_fluid(
  theme = app_theme,
  useShinyjs(),
  tags$head(tags$style(HTML(app_css))),
  
  div(class = "container py-4", style = "max-width: 900px;",
      
      # ── Hero ──────────────────────────────────────────────────────────────────
      div(class = "app-hero",
          tags$h2("Science Essay Assessment Builder"),
          p("Build a complete formative assessment — from learning objective selection \
         to student feedback — using a prompt chaining workflow with Claude.")
      ),
      
      # ── Step 1: Science Learning Objective ────────────────────────────────────
      div(class = "step-card", id = "card1",
          div(class = "step-header",
              div(class = "step-num", id = "num1", "1"),
              tags$h5(class = "step-title", "Select a Science Learning Objective")
          ),
          radioButtons("lo_mode", label = NULL,
                       choices  = c("Let AI choose for me" = "ai", "I'll choose manually" = "manual"),
                       selected = "ai", inline = TRUE
          ),
          conditionalPanel("input.lo_mode === 'manual'",
                           div(class = "row g-3 mt-1",
                               div(class = "col-md-5",
                                   selectInput("manual_subdomain", "Sub-domain", choices = NULL, width = "100%")
                               ),
                               div(class = "col-md-7",
                                   selectInput("manual_lo", "Learning Objective", choices = NULL, width = "100%")
                               )
                           )
          ),
          uiOutput("step1_result_ui"),
          actionButton("btn1", "Confirm & Continue →",
                       class = "btn btn-primary mt-2", icon = icon("circle-check"))
      ),
      
      # ── Step 2: Writing Learning Objectives ───────────────────────────────────
      div(class = "step-card locked", id = "card2",
          div(class = "step-header",
              div(class = "step-num", id = "num2", "2"),
              tags$h5(class = "step-title", "Select Writing Learning Objectives")
          ),
          p(class = "text-muted small mb-3",
            "Claude selects the two writing objectives best suited to your science topic. \
         You may override either using the dropdowns below."),
          uiOutput("step2_ai_display"),
          div(class = "row g-2",
              div(class = "col-12",
                  selectInput("wlo1", "Writing Learning Objective 1",
                              choices = NULL, width = "100%")
              ),
              div(class = "col-12",
                  selectInput("wlo2", "Writing Learning Objective 2",
                              choices = NULL, width = "100%")
              )
          ),
          actionButton("btn2", "Confirm & Continue →",
                       class = "btn btn-primary mt-2", icon = icon("circle-check"))
      ),
      
      # ── Step 3: Essay Prompt / Item Stem ──────────────────────────────────────
      div(class = "step-card locked", id = "card3",
          div(class = "step-header",
              div(class = "step-num", id = "num3", "3"),
              tags$h5(class = "step-title", "Review & Edit Essay Prompt")
          ),
          p(class = "text-muted small mb-3",
            "Review the generated item stem. Edit it directly, regenerate it, or keep it as-is."),
          textAreaInput("item_stem_edit", label = NULL, value = "", rows = 6, width = "100%",
                        placeholder = "Item stem will appear here..."),
          div(class = "d-flex gap-2 flex-wrap mt-2",
              actionButton("btn3_keep",  "Keep",        class = "btn btn-success",           icon = icon("check")),
              actionButton("btn3_save",  "Save Edits",  class = "btn btn-outline-primary",   icon = icon("floppy-disk")),
              actionButton("btn3_retry", "Try Again",   class = "btn btn-outline-secondary", icon = icon("arrows-rotate"))
          )
      ),
      
      # ── Step 4: Performance Level Descriptors ─────────────────────────────────
      div(class = "step-card locked", id = "card4",
          div(class = "step-header",
              div(class = "step-num", id = "num4", "4"),
              tags$h5(class = "step-title", "Review Performance Level Descriptors")
          ),
          p(class = "text-muted small mb-3",
            "Review the analytic rubric generated for your item, then confirm to continue."),
          uiOutput("pld_ui"),
          actionButton("btn4", "Confirm Rubric & Continue →",
                       class = "btn btn-primary mt-3", icon = icon("circle-check"))
      ),
      
      # ── Step 5: Student Essay ─────────────────────────────────────────────────
      div(class = "step-card locked", id = "card5",
          div(class = "step-header",
              div(class = "step-num", id = "num5", "5"),
              tags$h5(class = "step-title", "Student Essay")
          ),
          uiOutput("stem_display_ui"),
          radioButtons("essay_mode", label = NULL,
                       choices  = c("Write or paste an essay" = "manual",
                                    "Generate a synthetic essay" = "synthetic"),
                       selected = "manual", inline = TRUE
          ),
          conditionalPanel("input.essay_mode === 'synthetic'",
                           div(class = "row g-3 mt-1 mb-2",
                               div(class = "col-md-4",
                                   selectInput("syn_sci", "Science LO level",
                                               choices = c("Weak" = "weak", "Developing" = "developing", "Competent" = "competent"))
                               ),
                               div(class = "col-md-4",
                                   selectInput("syn_wlo1", "Writing LO 1 level",
                                               choices = c("Weak" = "weak", "Developing" = "developing", "Competent" = "competent"))
                               ),
                               div(class = "col-md-4",
                                   selectInput("syn_wlo2", "Writing LO 2 level",
                                               choices = c("Weak" = "weak", "Developing" = "developing", "Competent" = "competent"))
                               )
                           ),
                           actionButton("btn_gen_essay", "Generate Synthetic Essay",
                                        class = "btn btn-outline-primary mb-3", icon = icon("wand-magic-sparkles"))
          ),
          textAreaInput("essay_text", "Essay", value = "", rows = 9, width = "100%",
                        placeholder = "Write, paste, or generate a student essay here..."),
          hr(),
          actionButton("btn_score", "Score Essay & Generate Feedback",
                       class = "btn btn-primary btn-lg w-100 mt-1", icon = icon("star-half-stroke"))
      ),
      
      # ── Step 6: Scores + Feedback ─────────────────────────────────────────────
      div(class = "step-card locked", id = "card6",
          div(class = "step-header",
              div(class = "step-num", id = "num6", "6"),
              tags$h5(class = "step-title", "Scores & Student Feedback")
          ),
          uiOutput("scores_ui"),
          hr(class = "my-4"),
          uiOutput("feedback_ui")
      )
      
  ) # end container
) # end page_fluid

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── State ─────────────────────────────────────────────────────────────────
  rv <- reactiveValues(
    subdomain  = NULL,   # character
    lo         = NULL,   # character
    step2_json = NULL,   # JSON string: {subdomain, lo, writing_lo1, writing_lo2}
    step3_json = NULL,   # JSON string: step2 fields + item_stem
    step4_json = NULL,   # JSON string: step3 fields + rubric
    final_json = NULL    # JSON string: all fields + ratings + student_feedback
  )
  
  # ── Render helpers (defined first so they are in scope everywhere) ─────────
  
  render_pld_table <- function(parsed) {
    rubric <- parsed$rubric
    lo_labels <- c(
      paste0("Science: ", parsed$lo),
      paste0("Writing 1: ", parsed$writing_lo1),
      paste0("Writing 2: ", parsed$writing_lo2)
    )
    lo_keys <- c(
      "science_learning_objective",
      "writing_learning_objective_1",
      "writing_learning_objective_2"
    )
    rows <- lapply(seq_along(lo_keys), function(i) {
      key <- lo_keys[i]
      tags$tr(
        tags$td(tags$strong(lo_labels[i]),   style = "width:20%;"),
        tags$td(rubric[[key]]$weak,           style = "color:#7b2818;"),
        tags$td(rubric[[key]]$developing,     style = "color:#6b5700;"),
        tags$td(rubric[[key]]$competent,      style = "color:#195c38;")
      )
    })
    tags$table(class = "table table-bordered pld-table",
               tags$thead(tags$tr(
                 tags$th("Learning Objective", class = "pld-th-lo"),
                 tags$th("Weak",               class = "pld-th-weak"),
                 tags$th("Developing",         class = "pld-th-dev"),
                 tags$th("Competent",          class = "pld-th-comp")
               )),
               tags$tbody(rows)
    )
  }
  
  render_scores <- function(final) {
    lo_labels <- c(
      paste0("Science\n", final$lo),
      paste0("Writing 1\n", final$writing_lo1),
      paste0("Writing 2\n", final$writing_lo2)
    )
    ratings <- c(
      final$science_learning_objective_rating,
      final$writing_learning_objective_1_rating,
      final$writing_learning_objective_2_rating
    )
    div(
      tags$h6("Assigned Scores", class = "fw-bold mb-3"),
      div(class = "row g-3",
          lapply(seq_along(lo_labels), function(i) {
            div(class = "col-md-4",
                div(class = "score-card",
                    div(class = "score-lo-name", lo_labels[i]),
                    span(class = paste0("badge-", ratings[i]), ratings[i])
                )
            )
          })
      )
    )
  }
  
  render_feedback <- function(final) {
    fb <- final$student_feedback
    lo_labels <- c(
      paste0("Science: ", final$lo),
      paste0("Writing 1: ", final$writing_lo1),
      paste0("Writing 2: ", final$writing_lo2)
    )
    fb_texts <- c(
      fb$science_learning_objective,
      fb$writing_learning_objective_1,
      fb$writing_learning_objective_2
    )
    div(
      tags$h6("Student Feedback", class = "fw-bold mb-3"),
      lapply(seq_along(lo_labels), function(i) {
        div(class = "feedback-card",
            div(class = "feedback-lo-label", lo_labels[i]),
            p(fb_texts[i], style = "margin:0; font-size:.92rem; line-height:1.6;")
        )
      })
    )
  }
  
  # ── Step 1: Populate manual selectors ─────────────────────────────────────
  observe({
    updateSelectInput(session, "manual_subdomain",
                      choices = sort(unique(science_los$sub_domain)))
  })
  
  observeEvent(input$manual_subdomain, {
    los <- science_los |>
      filter(sub_domain == input$manual_subdomain) |>
      pull(learning_objective)
    updateSelectInput(session, "manual_lo", choices = los)
  })
  
  # ── Step 1: Confirm ────────────────────────────────────────────────────────
  observeEvent(input$btn1, {
    withProgress(message = "Selecting science learning objective...", value = .5, {
      
      if (input$lo_mode == "ai") {
        raw <- tryCatch(
          call_claude(choose_topic_prompt(science_lo_text), max_tokens = 300),
          error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
        )
        if (is.null(raw)) return()
        sub_m <- regmatches(raw, regexpr("(?<=\\[Sub-domain\\] )[^\n]+",         raw, perl = TRUE))
        lo_m  <- regmatches(raw, regexpr("(?<=\\[Learning Objective\\] )[^\n]+", raw, perl = TRUE))
        rv$subdomain <- trimws(if (length(sub_m) > 0) sub_m else "")
        rv$lo        <- trimws(if (length(lo_m)  > 0) lo_m  else "")
      } else {
        rv$subdomain <- input$manual_subdomain
        rv$lo        <- input$manual_lo
      }
      
      output$step1_result_ui <- renderUI({
        div(class = "info-box",
            tags$b("Sub-domain: "),      rv$subdomain, tags$br(),
            tags$b("Learning Objective: "), rv$lo
        )
      })
      
      runjs("document.getElementById('num1').classList.add('done');")
      trigger_step2()
    })
  })
  
  # ── Step 2: AI writing LO selection ───────────────────────────────────────
  trigger_step2 <- function() {
    withProgress(message = "Selecting writing learning objectives...", value = .5, {
      science_topic <- paste0("[Sub-domain] ", rv$subdomain,
                              "\n[Learning Objective] ", rv$lo)
      raw <- tryCatch(
        call_claude(pick_writing_los(science_topic, writing_lo_text), max_tokens = 400),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
      )
      if (is.null(raw)) return()
      
      parsed <- tryCatch(
        fromJSON(clean_json(raw)),
        error = function(e) { showNotification("Could not parse writing LO selection.", type = "error"); NULL }
      )
      if (is.null(parsed)) return()
      
      updateSelectInput(session, "wlo1", choices = all_writing_los, selected = parsed$writing_lo1)
      updateSelectInput(session, "wlo2", choices = all_writing_los, selected = parsed$writing_lo2)
      
      output$step2_ai_display <- renderUI({
        div(class = "info-box mb-3",
            tags$b("Claude selected:"), tags$br(),
            tags$b("WLO 1: "), parsed$writing_lo1, tags$br(),
            tags$b("WLO 2: "), parsed$writing_lo2, tags$br(),
            tags$em(style = "font-size:.82rem; color:#6a7391;",
                    "You may change these using the dropdowns below.")
        )
      })
      
      removeClass("card2", "locked")
    })
  }
  
  # ── Step 2: Confirm ────────────────────────────────────────────────────────
  observeEvent(input$btn2, {
    req(rv$subdomain, rv$lo)
    
    step2_list <- list(
      subdomain   = rv$subdomain,
      lo          = rv$lo,
      writing_lo1 = input$wlo1,
      writing_lo2 = input$wlo2
    )
    rv$step2_json <- toJSON(step2_list, auto_unbox = TRUE, pretty = TRUE)
    
    withProgress(message = "Writing item stem...", value = .5, {
      raw <- tryCatch(
        call_claude(write_essay_prompt(rv$step2_json), max_tokens = 800),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
      )
      if (is.null(raw)) return()
      
      clean <- tryCatch(
        clean_json(raw),
        error = function(e) { showNotification("Could not parse item stem.", type = "error"); NULL }
      )
      if (is.null(clean)) return()
      
      rv$step3_json <- clean
      updateTextAreaInput(session, "item_stem_edit",
                          value = fromJSON(clean)$item_stem)
      
      runjs("document.getElementById('num2').classList.add('done');")
      removeClass("card3", "locked")
    })
  })
  
  # ── Step 3: Keep / Save Edits / Try Again ─────────────────────────────────
  
  trigger_step4 <- function(json_str) {
    withProgress(message = "Generating performance level descriptors...", value = .5, {
      raw <- tryCatch(
        call_claude(build_step4_prompt(json_str, pld_guidance), max_tokens = 3500),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
      )
      if (is.null(raw)) return()
      
      clean <- tryCatch(
        clean_json(raw),
        error = function(e) { showNotification("Could not parse PLD response.", type = "error"); NULL }
      )
      if (is.null(clean)) return()
      
      rv$step4_json <- clean
      output$pld_ui <- renderUI(render_pld_table(fromJSON(clean)))
      
      runjs("document.getElementById('num3').classList.add('done');")
      removeClass("card4", "locked")
    })
  }
  
  observeEvent(input$btn3_keep, {
    req(rv$step3_json)
    trigger_step4(rv$step3_json)
  })
  
  observeEvent(input$btn3_save, {
    req(rv$step3_json)
    p <- fromJSON(rv$step3_json)
    p$item_stem <- input$item_stem_edit
    updated <- toJSON(p, auto_unbox = TRUE, pretty = TRUE)
    rv$step3_json <- updated
    trigger_step4(updated)
  })
  
  observeEvent(input$btn3_retry, {
    req(rv$step2_json)
    withProgress(message = "Regenerating item stem...", value = .5, {
      raw <- tryCatch(
        call_claude(write_essay_prompt(rv$step2_json), max_tokens = 800),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
      )
      if (is.null(raw)) return()
      clean <- tryCatch(clean_json(raw), error = function(e) NULL)
      if (is.null(clean)) return()
      rv$step3_json <- clean
      updateTextAreaInput(session, "item_stem_edit",
                          value = fromJSON(clean)$item_stem)
    })
  })
  
  # ── Step 4: Confirm ────────────────────────────────────────────────────────
  observeEvent(input$btn4, {
    req(rv$step4_json)
    parsed <- fromJSON(rv$step4_json)
    output$stem_display_ui <- renderUI({
      div(class = "info-box mb-3", style = "font-style: italic;",
          tags$b("Prompt: "), parsed$item_stem
      )
    })
    runjs("document.getElementById('num4').classList.add('done');")
    removeClass("card5", "locked")
  })
  
  # ── Step 5: Generate synthetic essay ──────────────────────────────────────
  observeEvent(input$btn_gen_essay, {
    req(rv$step4_json)
    withProgress(message = "Generating synthetic essay...", value = .5, {
      raw <- tryCatch(
        call_claude(
          build_synthetic_essay_prompt(
            rv$step4_json,
            input$syn_sci,
            input$syn_wlo1,
            input$syn_wlo2
          ),
          max_tokens = 1200
        ),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
      )
      if (is.null(raw)) return()
      clean <- tryCatch(clean_json(raw), error = function(e) NULL)
      if (is.null(clean)) return()
      updateTextAreaInput(session, "essay_text",
                          value = fromJSON(clean)$student_essay)
    })
  })
  
  # ── Step 5: Score + Feedback ───────────────────────────────────────────────
  observeEvent(input$btn_score, {
    req(rv$step4_json)
    validate(need(nchar(trimws(input$essay_text)) > 20, "Please enter an essay before scoring."))
    
    withProgress(message = "Scoring essay...", value = .2, {
      
      # Inject essay into rubric JSON
      base <- fromJSON(rv$step4_json)
      base$student_essay <- input$essay_text
      essay_json_str <- toJSON(base, auto_unbox = TRUE, pretty = TRUE)
      
      # Score
      scored_raw <- tryCatch(
        call_claude(build_scoring_prompt(essay_json_str), max_tokens = 600),
        error = function(e) { showNotification(paste("Scoring error:", conditionMessage(e)), type = "error"); NULL }
      )
      if (is.null(scored_raw)) return()
      
      scored_clean <- tryCatch(
        clean_json(scored_raw),
        error = function(e) { showNotification("Could not parse scoring response.", type = "error"); NULL }
      )
      if (is.null(scored_clean)) return()
      
      setProgress(.6, message = "Generating student feedback...")
      
      # Feedback
      feedback_raw <- tryCatch(
        call_claude(build_step6_prompt(scored_clean, feedback_guidance), max_tokens = 2000),
        error = function(e) { showNotification(paste("Feedback error:", conditionMessage(e)), type = "error"); NULL }
      )
      if (is.null(feedback_raw)) return()
      
      final_clean <- tryCatch(
        clean_json(feedback_raw),
        error = function(e) { showNotification("Could not parse feedback response.", type = "error"); NULL }
      )
      if (is.null(final_clean)) return()
      
      rv$final_json <- final_clean
      final <- fromJSON(final_clean)
      
      output$scores_ui   <- renderUI(render_scores(final))
      output$feedback_ui <- renderUI(render_feedback(final))
      
      runjs("document.getElementById('num5').classList.add('done');")
      runjs("document.getElementById('num6').classList.add('done');")
      removeClass("card6", "locked")
      setProgress(1)
      
      # Scroll to results
      runjs("setTimeout(function() {
        document.getElementById('card6').scrollIntoView({ behavior: 'smooth' });
      }, 400);")
    })
  })
  
} # end server

shinyApp(ui, server)
