require(tidyverse)
require(magrittr)
require(gradethis)
require(testthat)
# note!!! this worked with version of learnrhash af125d30
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

grade_result_hash <- function (user_code, solution_result) {

  grader <- grade_this({
    expect_equal(.result, solution_result)
    pass()
  })
  
  graded <- grader(mock_this_exercise(.user_code = !!user_code))
  
  if (graded$correct) {
    return ("correct")
  } else {
    return (graded$message)
  }
}

grade_plot_hash <- function (user_code, solution_plot) {
  solution_plot
  solution_plot
  ex_env <- mock_this_exercise_hash(user_code = user_code)
  user_plot <- ex_env$.result
  user_plot
  user_plot
  graded <- all.equal(user_plot, solution_plot)
  
  # Plot components to test:
  # mapping: which are mapped
  names(user_plot$mapping)
  # mapping: given that all the correct aesthetics are mapped, are they mapped to the correct things
  
  if (is.character(graded)) {
    return (graded)
  } else {
    return ("correct")
  }
}

grade_custom_hash <- function (user_code, grader, solution_code = ~NULL, return_message = FALSE) {
  graded <- grader(mock_this_exercise(.user_code = !!user_code,
                                      .solution_code = !!f_text(solution_code)))
  
  if (return_message) {
    if (graded$correct) {
      return ("correct")
    } else {
      return (graded$message)
    }
  } else {
    return (graded$correct)
  }
}

get_last_command <- function (user_code) {
  out <- user_code %>% 
    str_split("\\n") %>% 
    map(stringi::stri_remove_empty) %>% 
    map(tail, 1) %>% 
    unlist()
  
  return (out)
}
