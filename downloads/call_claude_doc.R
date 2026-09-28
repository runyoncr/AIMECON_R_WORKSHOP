library(httr2)
library(base64enc)
library(officer)

call_claude_doc <- function(prompt,
                            file_path,
                            model = "claude-sonnet-5",
                            system = NULL,
                            max_tokens = 4096,
                            effort = "low") {
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(api_key)) {
    stop("Set ANTHROPIC_API_KEY before calling call_claude_doc().")
  }
  if (length(prompt) != 1L || !is.character(prompt) || !nzchar(prompt)) {
    stop("prompt must be one non-empty character string.")
  }
  if (length(file_path) != 1L || !is.character(file_path) || !nzchar(file_path)) {
    stop("file_path must be one non-empty local path or URL.")
  }
  if (length(max_tokens) != 1L || !is.numeric(max_tokens) ||
      !is.finite(max_tokens) || max_tokens < 1 || max_tokens != as.integer(max_tokens)) {
    stop("max_tokens must be one positive whole number.")
  }
  if (!effort %in% c("low", "medium", "high", "xhigh", "max")) {
    stop("effort must be one of: low, medium, high, xhigh, or max.")
  }
  
  # ── Detect file type ──────────────────────────────────────────────────────
  ext <- tolower(tools::file_ext(sub("[?#].*$", "", file_path)))
  
  if (!ext %in% c("pdf", "docx")) {
    stop("Unsupported file type '.", ext, "'. Only PDF and Word (.docx) files are supported.")
  }
  
  # ── Handle URL vs. local path ─────────────────────────────────────────────
  if (grepl("^https?://", file_path)) {
    tmp <- tempfile(fileext = paste0(".", ext))
    tryCatch(
      download.file(file_path, tmp, mode = "wb", quiet = TRUE),
      error = function(e) stop("Could not download file: ", conditionMessage(e))
    )
    file_path <- tmp
    on.exit(unlink(tmp))
  } else if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # ── Build message content based on file type ──────────────────────────────
  if (ext == "pdf") {
    
    # PDFs are natively supported — send as base64-encoded document block
    file_base64 <- base64enc::base64encode(file_path)
    
    message_content <- list(
      list(
        type = "document",
        source = list(
          type       = "base64",
          media_type = "application/pdf",
          data       = file_base64
        )
      ),
      list(
        type = "text",
        text = prompt
      )
    )
    
  } else if (ext == "docx") {
    
    # Word docs are not natively supported — extract text via officer
    # and prepend it to the prompt as plain text
    doc       <- officer::read_docx(file_path)
    doc_text  <- paste(officer::docx_summary(doc)$text, collapse = "\n")
    
    combined_prompt <- paste0(
      "The following is the content of a Word document:\n\n",
      doc_text,
      "\n\n---\n\n",
      prompt
    )
    
    message_content <- list(
      list(
        type = "text",
        text = combined_prompt
      )
    )
    
  }
  
  # ── Build and send request ─────────────────────────────────────────────────
  messages <- list(
    list(
      role    = "user",
      content = message_content
    )
  )
  
  request_body <- list(
    model         = model,
    messages      = messages,
    max_tokens    = max_tokens,
    output_config = list(effort = effort)
  )
  
  if (!is.null(system)) {
    request_body$system <- system
  }
  
  response <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01"
    ) |>
    httr2::req_body_json(request_body) |>
    httr2::req_error(body = function(resp) {
      error_body <- tryCatch(
        httr2::resp_body_json(resp),
        error = function(e) NULL
      )
      if (!is.null(error_body) && !is.null(error_body$error$message)) {
        error_body$error$message
      } else {
        httr2::resp_body_string(resp)
      }
    }) |>
    httr2::req_perform()
  
  result <- httr2::resp_body_json(response, simplifyVector = FALSE)
  text_blocks <- vapply(
    result$content,
    function(block) if (identical(block$type, "text")) block$text else "",
    character(1)
  )
  paste(text_blocks[text_blocks != ""], collapse = "")
  
}

# Example usage:

# GitHub PDF
# cat(call_claude_doc(
#   prompt    = "Summarize the key guidance in this document.",
#   file_path = "https://raw.githubusercontent.com/runyoncr/AIMECON_R_WORKSHOP/main/data/feedback_guidance.pdf"
# ))

# GitHub Word document
# cat(call_claude_doc(
#   prompt    = "Summarize this document.",
#   file_path = "https://raw.githubusercontent.com/runyoncr/AIMECON_R_WORKSHOP/main/data/feedback_guidance.docx"
# ))
