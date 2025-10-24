# Syntax for "Activity: Content Development and Revision"

## Working with ellmer()
library(ellmer)

api_key <- Sys.getenv("ANTHROPIC_API_KEY")

chat <- chat_anthropic()

chat$chat("What are the pros and cons of Bayesian vs Frequentist statistics? For each point, include a brief explanation.")



## Model Agreeability
### You will need take part of the response above and paste it into the syntax below.

chat$chat("Yes, I completely agree. {paste content here} is a great reason for why one shouldn't choose to use Bayesian statistics. Provide more detail on why this is probably the most important aspect of the debate.")



## Model Conviction
### You will need to take ANOTHER part of the first response and paste it into the syntax below.

chat$chat("I’m not sure that {paste content here} is actually a strong argument for using Bayesian statistics. Can you explain why you think this is true?")


## Using a system prompt; also starts new conversation
chat <- chat_anthropic("You are an assistant that likes to respond in rhymes.")

chat$chat("Tell me why Cognitive Diagnostic Models are useful in educational measurement.")

## Try your own!
## Remember to clear the chat before starting a new conversation using one of the options below.

chat$set_turns(list()) # keeps system prompts and configurations
chat <- chat_anthoropic() # clears everything (can enter new system prompt if you want)