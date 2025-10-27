# Syntax for "Activity: Scoring with Rubrics"

ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
source('claude_plus.R')
source('format_for_qmd.R')

### Prompt Formulas 3 ways:

# Role + Task + Output

rto_prompt <- 
  "Role: You are en educational measurement specialist.
   
   Task: Explain the concept of *test reliability* to K–12 educators who are unfamiliar with psychometrics and want to better understand why student scores sometimes vary between test administrations.
   
  Output: Provide a concise explanation (2-3 short paragraphs) followed by one classroom-based example in bullet form."

reliability_rto <- claude_plus(rto_prompt)
reliability_rto <- format_for_qmd(reliability_rto)
knitr::asis_output(reliability_rto)


# "ACT" (Action - Context - Tone)
act_prompt <- "
  Action: Explain the concept of test reliability.
  
  Context: You’re writing to K–12 educators who are unfamiliar with psychometrics and want to better understand why student scores sometimes vary between test administrations.

  Tone: Friendly, conversational, and encouraging — as if you’re helping teachers connect a familiar classroom experience to an underlying measurement idea."

reliability_act <- claude_plus(act_prompt)
reliability_act <- format_for_qmd(reliability_act)
knitr::asis_output(reliability_act)


# "CARE" (Context - Action- Result - Explanation)
care_prompt <- "
  Context: Teachers have noticed that their students’ scores fluctuate across testing sessions and are unsure what that means.

  Action: Explain the concept of test reliability in a way that helps K–12 educators unfamiliar with psychometrics make sense of these score variations.

  Result: They should understand that reliability reflects the consistency of test scores and why it matters for interpreting student performance.

  Explanation: Include one concrete example that links reliability to real classroom assessment practices."

reliability_care <- claude_plus(care_prompt)
reliability_care <- format_for_qmd(reliability_care)
knitr::asis_output(reliability_care)




### Progressive Constraints

# 1
progressive_1 <- "Explain test validity."

validity_p1 <- claude_plus(progressive_1,
                           system = "You are a measurement educator who explains things concisely in a single paragraph")

validity_p1 <- format_for_qmd(validity_p1)
knitr::asis_output(validity_p1)

# 2
progressive_2 <- "Explain test validity to K–12 educators who are not familiar with psychometrics."

validity_p2 <- claude_plus(progressive_2,
                           system = "You are a measurement educator who explains things concisely in a single paragraph")

validity_p2 <- format_for_qmd(validity_p2)
knitr::asis_output(validity_p2)

# 3
progressive_3 <- "Explain test validity to K–12 educators unfamiliar with psychometrics, focusing on helping them understand why some test results may not reflect true student ability."

validity_p3 <- claude_plus(progressive_3,
                           system = "You are a measurement educator who explains things concisely in a single paragraph")

validity_p3 <- format_for_qmd(validity_p3)
knitr::asis_output(validity_p3)

# 4
progressive_4 <- "Explain test validity to K–12 educators unfamiliar with psychometrics, focusing on helping them understand why some test results may not reflect true student ability. Write in plain language suitable for a short teacher newsletter and avoid using the words “psychometrics” or “construct.”"

validity_p4 <- claude_plus(progressive_4,
                           system = "You are a measurement educator who explains things concisely in a single paragraph")

validity_p4 <- format_for_qmd(validity_p4)
knitr::asis_output(validity_p4)




### Including Examples (Zero-shot v Few-shot)

# Zero-shot
make_zero <- "
Classify the following sentences as wamples or doglets:

1. The concert was amazing and everyone was smiling.

2. The student felt frustrated after failing the exam.

3. The sunset filled the sky with brilliant colors.

4. The meeting dragged on and everyone was bored.

Respond _only_ with the sentence and either (wample) or (doglet) after the sentence."

zeroshot <- claude_plus(make_zero)
zeroshot <- format_for_qmd(zeroshot)
knitr::asis_output(zeroshot)


# Few-shot
make_few <- "
A doglet sentence describes a pleasant or positive experience.
A wample sentence describes an unpleasant or negative experience.

Examples:
– The people were having an enjoyable day. (doglet)
– It was raining and the woman was sad. (wample)
– The person was happy to be eating their favorite food. (doglet)
– The person had a stomach ache after eating too fast. (wample)

Classify the following sentences as wamples or doglets:

1. The concert was amazing and everyone was smiling.

2. The student felt frustrated after failing the exam.

3. The sunset filled the sky with brilliant colors.

4. The meeting dragged on and everyone was bored.

Respond _only_ with the sentence and either (wample) or (doglet) after the sentence."

fewshot <- claude_plus(make_few)
fewshot <- format_for_qmd(fewshot)
knitr::asis_output(fewshot)




### Prompt Improvement

prompt_prompt <- "I'd like your help improving the prompt below. Please review it and suggest ways to make it clearer and more effective.

Specifically:
•	Identify any missing context or details that would help generate better results
•	Point out areas where the prompt might be vague or ambiguous
•	Suggest how to better structure the request
•	Recommend any information I should add about my goals, audience, or desired format

Here is my prompt:

What are the pros and cons of Bayesian vs Frequentist statistics? For each point, include a brief explanation."

improved_prompt <- claude_plus(prompt_prompt)
improved_prompt <- format_for_qmd(improved_prompt)
knitr::asis_output(improved_prompt)

