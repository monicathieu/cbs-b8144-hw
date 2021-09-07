## setup ----

source(here::here("inst", "grading", "setup.R"))

## graders for more elaborate grading logic ----

grader_a1_q2 <- grade_this({
  
  expect_true("numVec" %in% env_names(.envir_result))
  
  expect_identical(sort(.envir_result$numVec), 1:10)
  
  expect_true(.user_code %>% str_split("\n") %>% unlist() %>% str_trim("both") %>% .[. != ""] %>% tail(1) == "sum(numVec)")
  
  pass()
})

grader_a1_q3 <- grade_this({
  expect_true("thisFunction" %in% env_names(.envir_result))
  
  expect_true(class(.envir_result$thisFunction) == "function")
  
  # first >= second: sum
  expect_identical(.envir_result$thisFunction(20, 10), 30)
  
  expect_identical(.envir_result$thisFunction(10, 10), 20)
  
  # second > first: product
  expect_identical(.envir_result$thisFunction(10, 20), 200)
  
  pass()
})

## do it ----

test_hash <- tibble(hash = "QlpoOTFBWSZTWV2ElqUABAP/kP/5bCxtf/5fqwQfyv/v/+oAIEABwAL8HWkRoISUJqgNNNNqepkMjT0mIAyANGho0aDJ6mgMgcZME0MhkZGTQ0AaDIwgGg0aZDENABFJUH6pk00AAZAAAAAAAAADQCKUymI0FPaBTJo0eoAyPUAAAAyGjINNqLmIZDyTvgyHEVRBARAJigcKFYFMgKgZDQs0WRCMoFNY7/+SWCawuEeK4DED0qaCUO7IE+pxQHzXAIONGB++AdFAyQmkIFqdOgGSQj4M9hvfPVvYe53eUN5LyXksRb6QAK6IQ5cIsQuBaSY761SqGKlgbbAfcZypjX4KzmiWaAM1VIVBIACFFWUICHyzJmTw/Ts6jVSOurXIzkB5J0QX0RmI2WNQloCTEJ5e9YjnssIAjgjtwDmUKJbyH8z9SyW50MtNEaZWhOm6xEFF2K+LRbMOpIhyFDHtIT467rMAmUkREHRqt4Q/ifDlewjhAncCROCkJ5ZHxnK0yA83ymni0qAEYDKICx2AXt+eS33ySRxpBfGukHuytzWJOWYBOA16kIqYfMNOgBZFVQS6xNy++67CH0gbVDBgcQjzrQTGCwpNiM8uJVrtwU2IXEKsdvg94/kDwgUoVpgXDdGpckUDxilPH/Vyjz93RFqEwIAgCgeMmP0GwfYNZhrvRyTXywRrHTIOpmr2zGfgZ/ocDMDEZzrIWa4oGA7eyv1TuDsWdIq4+RUWlEDWrFUKA3ywDFCBzZSriVGjlxbDhp1LhA7IaEOXBskNEdey8kXaTkrxCmMmrbeumk7Y91b8X7Y3ANwC0edaG0dDTrL6GIhsRgYvgGwUATJptwxlXJmoGS7FRJcgVHYX1btg1FhCEjZQsDAf6tlNI5gF0bDOtdxfgReNr1hMwmyZhoHGldg5swVAdEMFvi+3ANCMDFRJJzo3Sf4u5IpwoSC7CS1K")

exercises <- learnrhash::extract_exercises(test_hash, hash)
qs <- learnrhash::extract_questions(test_hash, hash)

exercises$code[exercises$exercise_id == "q1-2a"]

env_q3 <- mock_this_exercise_hash(exercises$code[exercises$exercise_id == "q3"])

scored_a1_q1_1 <- qs %>% 
  filter(question_id == "q1-1") %>% 
  select(exercise_id = question_id, answer) %>% 
  mutate(correct = map_lgl(answer, ~identical(sort(.x), c("character", "date", "integer",  "logical"))))

scored_a1_q1_2a <- exercises %>% 
  filter(exercise_id == "q1-2a") %>% 
  select(exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_this_code_hash, solution_code = ~is.logical(mysteryVar)))

scored_a1_q1_2b <- exercises %>% 
  filter(exercise_id == "q1-2b") %>% 
  select(exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_this_code_hash, solution_code = ~is.numeric(mysteryVar)))

scored_a1_q1_2c <- exercises %>% 
  filter(exercise_id == "q1-2c") %>% 
  select(exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_this_code_hash, solution_code = ~is.character(mysteryVar)))

scored_a1_q1_2d <- exercises %>% 
  filter(exercise_id == "q1-2d") %>% 
  select(exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_this_code_hash, solution_code = ~class(mysteryVar)))

scored_a1_q2 <- exercises %>% 
  filter(exercise_id == "q2") %>% 
  select(exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_custom_hash, grader = grader_a1_q2))

scored_a1_q3 <- exercises %>% 
  filter(exercise_id == "q3") %>% 
  select(exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_custom_hash, grader = grader_a1_q3))

scored_a1 <- bind_rows(scored_a1_q1_2a,
                       scored_a1_q1_2b,
                       scored_a1_q1_2c,
                       scored_a1_q1_2d,
                       scored_a1_q2,
                       scored_a1_q3) %>% 
  mutate(answer = as.list(answer)) %>% 
  bind_rows(scored_a1_q1_1)
