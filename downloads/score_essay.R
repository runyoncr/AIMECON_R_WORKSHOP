score_essay <- function(essay) {
  prompt <- glue(
    "You are an expert essay grader with experience in evaluating argumentative writing
    at the secondary and post-secondary level.

    ## TASK
    Score the student essay provided below using the three criteria in the rubric.
    Each criterion is scored from 1 to 5 in increments of 0.5 (1, 1.5, 2, 2.5, 3,
    3.5, 4, 4.5, 5). Apply each criterion independently before calculating the total.

    ## ESSAY PROMPT GIVEN TO STUDENT
    'Do you think that smartphones have destroyed communication among family and friends?
    Give specific reasons and details to support your opinion.'

    ## SCORING RUBRIC

    ### CRITERION 1: Content (1-5)
    Score the degree to which the essay develops a clear argument with relevant reasons
    and specific supporting examples in response to the essay prompt.

    - 5: Argument is thoroughly developed with multiple specific, relevant reasons and
         concrete examples. All content directly supports the central claim.
    - 4: Argument is well-developed with mostly specific reasons and examples. Minor
         gaps in development or relevance.
    - 3: Argument is adequately developed but reasons are sometimes general or examples
         are vague. Some content may be loosely connected to the argument.
    - 2: Argument is underdeveloped. Reasons are largely general or repetitive and
         examples are missing or unclear.
    - 1: Little to no recognizable argument. Content is largely irrelevant, missing,
         or does not respond to the prompt.

    Scores between these anchors (e.g., 1.5, 2.5) should be used when the essay falls
    between two descriptors.

    ### CRITERION 2: Organization (1-5)
    Score the degree to which the essay is logically structured at both the essay level
    (introduction, body, conclusion) and the paragraph level (topic sentences, coherence
    devices, single main idea per paragraph).

    - 5: Essay has a clear introduction with a thesis, well-organized body paragraphs
         each focused on a single idea, and a conclusion. Transitions and coherence
         devices are used effectively throughout.
    - 4: Essay structure is clear and mostly effective. Minor issues with transitions
         or paragraph focus.
    - 3: Basic structure is present but inconsistently applied. Some paragraphs may
         lack focus or transitions may be absent or mechanical.
    - 2: Structure is difficult to follow. Paragraphs may lack topic sentences or blend
         multiple unrelated ideas. Few or no transitions.
    - 1: No discernible organizational structure. Ideas are presented randomly with no
         paragraph logic.

    ### CRITERION 3: Language (1-5)
    Score the overall quality of language use across three equally weighted
    sub-dimensions: (a) vocabulary range and accuracy, (b) grammar and usage
    correctness, and (c) spelling and punctuation accuracy. Average across these three
    sub-dimensions to arrive at a single Language score.

    - 5: (a) Sophisticated and varied vocabulary with accurate collocations; (b) grammar
         and usage are correct throughout; (c) spelling and punctuation are correct
         throughout.
    - 4: (a) Good vocabulary range with occasional imprecision; (b) mostly correct
         grammar with minor errors that do not impede meaning; (c) few spelling or
         punctuation errors.
    - 3: (a) Adequate but limited vocabulary, some word choice errors; (b) grammar
         errors are noticeable but meaning is generally clear; (c) some spelling and
         punctuation errors.
    - 2: (a) Narrow or frequently inaccurate vocabulary; (b) grammar errors are frequent
         and sometimes obscure meaning; (c) spelling and punctuation errors are frequent.
    - 1: (a) Very limited vocabulary with pervasive errors; (b) grammar errors throughout
         severely impede meaning; (c) spelling and punctuation errors throughout.

    ## HANDLING EDGE CASES
    - If the essay is off-topic or does not respond to the prompt, assign a Content
      score of 1 and note this in the justification.
    - If the essay is fewer than 50 words, assign a maximum score of 2 for Content
      and Organization, and score Language based on what is present.

    ## STUDENT ESSAY TO SCORE
    {essay}

    ## OUTPUT INSTRUCTIONS
    Return your response ONLY as a valid JSON object with no additional text before or
    after it. Use the following structure exactly:

    {{
      'content_score': <number>,
      'content_justification': '<1-2 sentence justification>',
      'organization_score': <number>,
      'organization_justification': '<1-2 sentence justification>',
      'language_score': <number>,
      'language_justification': '<1-2 sentence justification referencing vocabulary,
                                  grammar, and mechanics>',
      'total_score': <sum of three scores>
    }}

    Ensure that 'total_score' equals the sum of the three criterion scores. Do not
    include any text outside the JSON object."
  )
  return(prompt)
}
