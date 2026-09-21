source(".Rprofile")
library(jsonlite)

app_expressions <- parse("downloads/sab_test.R")
app_env <- new.env(parent = globalenv())
for (expression in app_expressions) {
  if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.call(expression[[3L]]) && identical(expression[[3L]][[1L]], as.name("function"))) {
    eval(expression, envir = app_env)
  }
}

expect_error <- function(expression) {
  result <- tryCatch({ force(expression); NULL }, error = identity)
  stopifnot(inherits(result, "error"))
}

base_record <- toJSON(list(lo = "Approved objective", item_stem = "Approved stem"), auto_unbox = TRUE)
reply <- toJSON(list(student_essay = "Synthetic example", lo = "Changed objective"), auto_unbox = TRUE)
merged <- fromJSON(app_env$merge_response(reply, list(student_essay = ""), base_record))
stopifnot(identical(merged$lo, "Approved objective"),
          identical(merged$item_stem, "Approved stem"),
          identical(merged$student_essay, "Synthetic example"))
expect_error(app_env$merge_response("{}", list(item_stem = "")))
expect_error(app_env$merge_response('{"item_stem": ""}', list(item_stem = "")))
expect_error(app_env$merge_response('{"item_stem": ["one", "two"]}', list(item_stem = "")))
expect_error(app_env$merge_response('{"rating": "excellent"}',
                                  list(rating = c("weak", "developing", "competent"))))
expect_error(app_env$merge_response('{"rubric": {}}',
                                  list(rubric = list(science = list(weak = "")))))
expect_error(app_env$merge_response("not JSON", list(item_stem = "")))
cat("PASS: app parsing, required fields, allowed ratings, and preserved context.\n")

live <- "--live" %in% commandArgs(trailingOnly = TRUE)
if (!live) Sys.setenv(ANTHROPIC_API_KEY = "offline-test-only")
stopifnot(nzchar(Sys.getenv("ANTHROPIC_API_KEY")))
source("downloads/sab_test.R", local = app_env)
real_call <- app_env$call_claude
call_count <- 0L
last_response <- NULL
writing_choices <- app_env$all_writing_los[1:2]
essay <- "This synthetic teaching response explains the science idea and provides an example."
replies <- list(
  list(writing_lo1 = writing_choices[1], writing_lo2 = writing_choices[2]),
  list(item_stem = "Explain the science concept using an example."),
  rapply(app_env$rubric_template, function(value) "Test descriptor", how = "replace"),
  list(student_essay = essay),
  setNames(rep(list("developing"), 3L), paste0(app_env$criterion_keys, "_rating")),
  rapply(app_env$feedback_template, function(value) "Explain one detail more clearly.", how = "replace")
)
app_env$call_claude <- function(prompt, ...) {
  call_count <<- call_count + 1L
  if (call_count > 6L) stop("Validation request limit exceeded.")
  last_response <<- if (live) real_call(prompt, ...) else toJSON(replies[[call_count]], auto_unbox = TRUE)
  last_response
}

shiny::testServer(app_env$server, {
  session$setInputs(lo_mode = "manual", manual_subdomain = app_env$science_los$sub_domain[1],
                   manual_lo = app_env$science_los$learning_objective[1], essay_text = "")
  session$setInputs(btn_score = 1)
  stopifnot(call_count == 0L)
  session$setInputs(btn1 = 1)
  stopifnot(rv$writing_ready, call_count == 1L)
  suggestions <- fromJSON(app_env$clean_json(last_response))
  session$setInputs(wlo1 = suggestions$writing_lo1, wlo2 = suggestions$writing_lo1)
  session$setInputs(btn2 = 1)
  stopifnot(is.null(rv$step3_json), call_count == 1L)
  session$setInputs(wlo2 = suggestions$writing_lo2)
  session$setInputs(btn2 = 2)
  stopifnot(!is.null(rv$step3_json), call_count == 2L)
  original_stem <- fromJSON(rv$step3_json)$item_stem
  session$setInputs(item_stem_edit = " ")
  session$setInputs(btn3_save = 1)
  stopifnot(is.null(rv$step4_json), call_count == 2L)
  edited_stem <- paste(original_stem, "Use an example in your explanation.")
  session$setInputs(item_stem_edit = edited_stem)
  session$setInputs(btn3_save = 2)
  stopifnot(!is.null(rv$step4_json), call_count == 3L,
            identical(fromJSON(rv$step4_json)$item_stem, edited_stem))
  session$setInputs(btn1 = 2, btn2 = 3, btn3_retry = 1)
  stopifnot(call_count == 3L)
  session$setInputs(btn_score = 2)
  stopifnot(call_count == 3L)
  session$setInputs(btn4 = 1)
  stopifnot(rv$rubric_confirmed)
  session$setInputs(syn_sci = "developing", syn_wlo1 = "developing", syn_wlo2 = "developing")
  session$setInputs(btn_gen_essay = 1)
  stopifnot(call_count == 4L)
  generated_essay <- fromJSON(app_env$clean_json(last_response))$student_essay
  session$setInputs(essay_text = generated_essay)
  session$setInputs(btn_score = 3)
  stopifnot(call_count == 6L, !is.null(rv$final_json))
  final <- fromJSON(rv$final_json)
  stopifnot(identical(final$item_stem, edited_stem),
            identical(final$student_essay, generated_essay),
            identical(final$lo, app_env$science_los$learning_objective[1]),
            all(unlist(final[paste0(app_env$criterion_keys, "_rating")]) %in% app_env$performance_levels),
            nzchar(output$scores_ui$html), nzchar(output$feedback_ui$html))
  session$setInputs(essay_text = paste(generated_essay, "An edit."))
  stopifnot(is.null(rv$final_json), call_count == 6L)
})
cat(if (live) "PASS: six live API calls" else "PASS: six mocked API calls",
    "through the workflow, edit handling, step guards, and visible ratings/feedback.\n")

if (!live) {
  app_env$call_claude <- function(...) "[Sub-domain] Invented\n[Learning Objective] Invented"
  shiny::testServer(app_env$server, {
    session$setInputs(lo_mode = "ai")
    session$setInputs(btn1 = 1)
    stopifnot(!rv$writing_ready, is.null(rv$lo))
  })

  selection_calls <- 0L
  app_env$call_claude <- function(...) {
    selection_calls <<- selection_calls + 1L
    if (selection_calls == 1L) {
      paste0("[Sub-domain] ", app_env$science_los$sub_domain[1],
             "\n[Learning Objective] ", app_env$science_los$learning_objective[1])
    } else {
      toJSON(list(writing_lo1 = writing_choices[1], writing_lo2 = writing_choices[1]),
             auto_unbox = TRUE)
    }
  }
  shiny::testServer(app_env$server, {
    session$setInputs(lo_mode = "ai")
    session$setInputs(btn1 = 1)
    stopifnot(!rv$writing_ready, selection_calls == 2L)
  })

  app_env$call_claude <- function(...) stop("Simulated request failure")
  shiny::testServer(app_env$server, {
    rv$step2_json <- base_record
    rv$step3_json <- base_record
    session$setInputs(item_stem_edit = "An unconfirmed edit.")
    session$setInputs(btn3_save = 1)
    stopifnot(is.null(rv$step4_json), identical(rv$step3_json, base_record))
    app_env$call_claude <- function(...) toJSON(replies[[3]], auto_unbox = TRUE)
    session$setInputs(btn3_keep = 1)
    stopifnot(identical(fromJSON(rv$step4_json)$item_stem, "Approved stem"))
  })
  cat("PASS: invalid science selections, duplicate AI objectives, and failed-edit recovery.\n")
}