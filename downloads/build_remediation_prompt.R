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