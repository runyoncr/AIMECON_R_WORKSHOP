# Syntax for "Activity: Chained Workflows"


### Using Scored Rubric to Generate Feedback
load("data/analytic_response.Rdata")
api_key <- Sys.getenv("ANTHROPIC_API_KEY")

build_feedback_prompt <- function(completed_rubric){
  glue::glue(
    "Using the following completed rubric as a guide, provide constructive feedback to the student about their clinical case documentation. 
    
    The feedback should consist of two well-developed paragraphs:
    The first paragraph should offer a balanced assessment of the student's documentation. 
    Begin by acknowledging elements that were documented well (if any), then identify the most critical missing or inadequately addressed components based on the rubric. 
    Be specific about what was missing and, where applicable, note the quality or completeness of what was documented.
    
    The second paragraph should provide educational context by explaining why the missing or inadequate elements are clinically important for evaluating and managing a patient presenting with this specific chief concern. 
    Connect these documentation gaps to potential impacts on patient care, differential diagnosis, treatment planning, or patient safety. 
    Use this as a teaching opportunity to help the student understand the clinical reasoning behind thorough documentation.
    
    Here is the complete rubric on the student's performance: {completed_rubric}
    ")
}

feedback_prompt <- build_feedback_prompt(analytic_rubric)
feedback_response <- claude_plus(feedback_prompt,
                                 temperature = 0)

ibrary(stringr)
library(knitr)

# Some cleaning for the quarto output; not strictly necessary
source('downloads/format_for_qmd.R')

feedback_response <- format_for_qmd(feedback_response)

knitr::asis_output(feedback_response)



### Task: Generate Instructor Report

build_remediation_prompt <- function(feedback_output){
  glue::glue("
  You will analyze feedback that has been provided to a student about their performance. 
  Your task is to identify any potential gaps in the course materials or instruction that could be improved to further help the learning. 
  Based on the documentation deficiencies and missing clinical elements identified in the student feedback:
  
  1. Pattern Analysis: Determine whether these gaps likely reflect:
  •	Individual student oversight or understanding issues
  •	Systematic instructional gaps that may affect multiple students
  •	Unclear expectations in the assignment or rubric
  
  2. Curricular Recommendations: If the gaps suggest instructional needs, provide 2-3 specific, actionable recommendations for strengthening the course materials. 
  For each recommendation, specify:
  •	Which course component to enhance (e.g., lecture content, practice cases, rubric clarity, pre-assignment resources)
  •	What specific content or skill should be emphasized
  •	Why this would address the observed documentation gap
  
  3. Context Considerations: Note whether the missing elements are:
  •	Foundational knowledge that should have been covered previously
  •	Advanced concepts that may need more instructional time
  •	Clinical reasoning skills requiring additional practice opportunities

  Format your response as a brief analysis followed by concrete action items that an instructor can implement.
  
  Complete this task using this feedback: {feedback_output}
  ")
}

remediation_prompt <- build_remediation_prompt(feedback_response)
remediation_response <- claude_plus(remediation_prompt,
                                    temperature = 0)

remediation_response <- format_for_qmd(remediation_response)

knitr::asis_output(remediation_response)



## All 3 steps at one with a new student response

analytic_rubric <- "For each of the following criteria, determine whether the element is Included or Not Included in the response.

1. Chief concern of chest pain
2. Episodic pattern of symptoms
3. Poorly controlled history of hypertension
4. Vitals indicate hypertension
5. Pain radiates to the back
6. Likely diagnosis of acute coronary syndrome (ACS), NSTEMI, or STEMI"

osce_note_2 <- "
45yo m presents with shortness of breath due to intermittent chest pain.
Reports that the pain more noticable on exertion this morning (walking up stairs). 
Hypertensive, although on medication, suggesting it is poorly controlled.
Diagnostic testing to rule out ACS should be completed first."

# Scoring with Rubric
analytic_response_2 <- claude_plus(
  build_osce_analytic_prompt(osce_note_2, analytic_rubric),
  temperature = 0)
# Good to build in steps to save along the way for error tracing
# save(analytic_response_2, file = 'data/analytic_response_2.Rdata')

# Generating Feedback
feedback_response_2 <- claude_plus(
  build_feedback_prompt(analytic_response_2),
  temperature = 0)
# save(feedback_response_2, file = 'data/feedback_response_2.Rdata')

# Generating Instructor Report
remediation_response_2 <- claude_plus(
  build_remediation_prompt(feedback_response_2),
  temperature = 0)
# save(remediation_response_2, file = 'data/remediation_response_2.Rdata')

full_report <- paste(analytic_response_2,
                     feedback_response_2,
                     remediation_response_2,
                     collapse = "\n\n  ***  \n\n")

full_report <- format_for_qmd(full_report)
knitr::asis_output(full_report)
