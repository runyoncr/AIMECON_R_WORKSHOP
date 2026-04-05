claude_param_test <- function(prompt,
                              temperature = NULL,
                              top_p = NULL,
                              top_k = NULL,
                              max_tokens = 1024,
                              n_reps = 5) {
  
  # Check if n_reps is within allowed range
  if (n_reps > 20) {
    warning("n_reps exceeds maximum allowed value of 20. Setting n_reps to 20.")
    n_reps <- 20
  }
  
  if (n_reps < 5) {
    warning("n_reps is below minimum value of 5. Setting n_reps to 5.")
    n_reps <- 5
  }
  
  # Initialize results dataframe
  results <- data.frame(
    rep_n = integer(),
    temp = numeric(),
    top_p = numeric(),
    top_k = integer(),
    output = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through n_reps
  for (i in 1:n_reps) {
    # Call claude_plus
    output <- claude_plus(
      prompt = prompt,
      temperature = temperature,
      top_p = top_p,
      top_k = top_k,
      max_tokens = max_tokens
    )
    
    # Add to results
    results <- rbind(results, data.frame(
      rep_n = i,
      temp = ifelse(is.null(temperature), NA, temperature),
      top_p = ifelse(is.null(top_p), NA, top_p),
      top_k = ifelse(is.null(top_k), NA, top_k),
      output = output,
      stringsAsFactors = FALSE
    ))
    
    # Sleep between calls (except after last call)
    if (i < n_reps) {
      Sys.sleep(0.2)
    }
  }
  
  return(results)
}