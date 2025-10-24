analytic_rubric <- "For each of the following criteria, determine whether the element is Included or Not Included in the response.

1. Chief concern of chest pain
2. Episodic pattern of symptoms
3. Poorly controlled history of hypertension
4. Vitals indicate hypertension
5. Pain radiates to the back
6. Likely diagnosis of acute coronary syndrome (ACS), NSTEMI, or STEMI"

build_osce_analytic_prompt <- function(student_note, rubric){
  glue::glue("
  Using the rubric provided below, score the following OSCE post-encounter note. 
  For each criterion, indicate whether it was met and provide brief justification for your scoring decision. 
    
  Here is the student note:{student_note}
    
  Here is the rubric:{rubric}
    
  Return a completed rubric, indicating which criteria are met or not met and justifications for each component.
  ")
}