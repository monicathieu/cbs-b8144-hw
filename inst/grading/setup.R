require(tidyverse)
require(gradethis)
require(testthat)
require(learnrhash)
require(rlang)

mock_this_exercise_hash <- function (user_code, solution_code = ~NULL) {
  ex_env <- mock_this_exercise(.user_code = !!user_code,
                               .solution_code = !!f_text(solution_code))
  
  return (ex_env)
}

grade_this_code_hash <- function (user_code, solution_code = ~NULL) {
  graded <- grade_this_code()(mock_this_exercise(.user_code = !!user_code,
                                                 .solution_code = !!f_text(solution_code)))
  
  return (graded$correct)
}

grade_custom_hash <- function (user_code, grader) {
  graded <- grader(mock_this_exercise(.user_code = !!user_code))
  return (graded$correct)
}
