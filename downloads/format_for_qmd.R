format_for_qmd <- function(genai_output){
  
  formatted_response <- str_replace_all(genai_output, "#", "*")
  
  formatted_response <- str_replace_all(formatted_response, 
                                       "(?m)^(\\*+)\\s+", 
                                       "\\1")
  
  formatted_response <- str_replace_all(formatted_response, 
                                       "(?m)^(\\*+)(.*?)(?<!\\*)(\\n)", 
                                       "\\1\\2\\1\\3")
  
  formatted_response <- str_replace_all(formatted_response,
                                       "\\*\\*(\\n)\\*\\*",
                                       "**\\1\\1**")
  return(formatted_response)
}
