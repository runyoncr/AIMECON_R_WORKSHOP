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
