library(httr2)

call_claude <- function(prompt,
                        model = "claude-sonnet-5",
                        system = NULL,
                        max_tokens = 4096,
                        effort = "low") {
  
  # Get API key from environment
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  
  request_body <- list(
    model = model,
    messages = list(list(role = "user", content = prompt)),
    max_tokens = max_tokens,
    output_config = list(effort = effort)
  )
  
  # Add system prompt if provided
  if (!is.null(system)) {
    request_body$system <- system
  }
  
  response <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01"
    ) |>
    req_body_json(request_body) |>
    req_perform()

  result <- resp_body_json(response)
  text_blocks <- vapply(
    result$content,
    function(block) if (identical(block$type, "text")) block$text else "",
    character(1)
  )

  paste(text_blocks[text_blocks != ""], collapse = "")
  
}
