call_claude_pdf <- function(prompt,
                            pdf_path,
                            model = "claude-sonnet-4-6",
                            system = NULL,
                            temperature = 0.5,
                            max_tokens = 4096,
                            effort = "low") {
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  
  # Read and encode the PDF as base64
  pdf_base64 <- base64enc::base64encode(pdf_path)
  
  # Build message with both the document and the text prompt
  messages <- list(
    list(
      role = "user",
      content = list(
        list(
          type = "document",
          source = list(
            type = "base64",
            media_type = "application/pdf",
            data = pdf_base64
          )
        ),
        list(
          type = "text",
          text = prompt
        )
      )
    )
  )
  
  request_body <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens,
    temperature = temperature,
    output_config = list(effort = effort)
  )
  
  if (!is.null(system)) {
    request_body$system <- system
  }
  
  headers <- add_headers(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type" = "application/json"
  )
  
  response <- POST(
    url = "https://api.anthropic.com/v1/messages",
    headers,
    body = toJSON(request_body, auto_unbox = TRUE)
  )
  
  if (http_status(response)$category != "Success") {
    stop(paste("API request failed:", http_status(response)$message,
               "\nDetails:", content(response, "text", encoding = "UTF-8")))
  }
  
  result <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(result$content$text)
  
}