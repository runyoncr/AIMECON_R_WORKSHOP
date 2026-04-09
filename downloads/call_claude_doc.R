library(httr)
library(jsonlite)
library(base64enc)
library(officer)

call_claude_doc <- function(prompt,
                            file_path,
                            model = "claude-sonnet-4-6",
                            system = NULL,
                            temperature = 0.5,
                            max_tokens = 4096,
                            effort = "low") {
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  
  # ── Detect file type ──────────────────────────────────────────────────────
  ext <- tolower(tools::file_ext(file_path))
  
  if (!ext %in% c("pdf", "docx")) {
    stop("Unsupported file type '.", ext, "'. Only PDF and Word (.docx) files are supported.")
  }
  
  # ── Handle URL vs. local path ─────────────────────────────────────────────
  if (grepl("^https?://", file_path)) {
    tmp <- tempfile(fileext = paste0(".", ext))
    download.file(file_path, tmp, mode = "wb", quiet = TRUE)
    file_path <- tmp
    on.exit(unlink(tmp))
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
    temperature   = temperature,
    output_config = list(effort = effort)
  )
  
  if (!is.null(system)) {
    request_body$system <- system
  }
  
  headers <- add_headers(
    "x-api-key"         = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type"      = "application/json"
  )
  
  response <- POST(
    url     = "https://api.anthropic.com/v1/messages",
    headers,
    body    = toJSON(request_body, auto_unbox = TRUE)
  )
  
  if (http_status(response)$category != "Success") {
    stop(paste("API request failed:", http_status(response)$message,
               "\nDetails:", content(response, "text", encoding = "UTF-8")))
  }
  
  result <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(result$content$text)
  
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
