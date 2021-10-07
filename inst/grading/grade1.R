## setup ----

source(here::here("inst", "grading", "setup.R"))

## graders for more elaborate grading logic ----

grader_a1_q2 <- grade_this({
  
  expect_true("numVec" %in% env_names(.envir_result))
  
  expect_equal(sort(.envir_result$numVec), 1:10)
  
  expect_equal(.last_value, 55)
  
  # expect_true(.user_code %>% str_split("\n") %>% unlist() %>% str_trim("both") %>% .[. != ""] %>% tail(1) == "sum(numVec)")
  
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

acs_ny <- read_csv('https://jaredlander.com/data/acs_ny.csv')

acs_ny_base <- read.csv('https://jaredlander.com/data/acs_ny.csv', header = TRUE)

grader_a1_q4_1 <- grade_this({
  expect_true(identical(as_tibble(.result), head(acs_ny, 10)) | identical(.result, head(acs_ny_base, 10)))
  pass()
})

grader_a1_q4_2 <- grade_this({
  expect_true(identical(as_tibble(.result), tail(acs_ny, 10)) | identical(.result, tail(acs_ny_base, 10)))
  pass()
})


## read in submissions downloaded from canvas ----

submissions <- tibble(filename = list.files("~/Downloads/submissions", full.names = TRUE)) %>%
  separate(filename, into = c(NA, NA, NA, NA, NA, "student", NA, NA, NA), remove = FALSE) %>% 
  mutate(hash = map_chr(filename,
                    ~.x %>% 
                      read_lines() %>% 
                      .[nchar(.) == max(nchar(.))] %>% 
                      str_split("[<>]") %>% 
                      unlist() %>% 
                      .[nchar(.) == max(nchar(.))]),
         hash_length = map_int(hash, nchar))
  

## extract hashery ----

exercises <- learnrhash::extract_exercises(submissions, hash)
qs <- learnrhash::extract_questions(submissions, hash)

env_q2 <- mock_this_exercise_hash(exercises$code[exercises$exercise_id == "q2" & exercises$student == "atlasdaniel"])
env_q3 <- mock_this_exercise_hash(exercises$code[exercises$exercise_id == "q3"])
env_q4_1 <- mock_this_exercise_hash(exercises$code[exercises$exercise_id == "q4-1" & exercises$student == "wuzihao"])
env_q5 <- mock_this_exercise_hash(exercises$code[exercises$exercise_id == "q5" & exercises$student == "sordiazanellaandrea"])

## score individual questions ----

scored_a1_q1_1 <- qs %>% 
  filter(question_id == "q1-1") %>% 
  select(student, exercise_id = question_id, answer) %>% 
  mutate(correct = map_lgl(answer, ~identical(sort(.x), c("character", "date", "logical",  "numeric"))))

scored_a1_q1_2a <- exercises %>% 
  filter(exercise_id == "q1-2a") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(last_cmd = map_chr(answer,
                        get_last_command),
         correct = map_lgl(last_cmd, grade_this_code_hash, solution_code = ~is.logical(mysteryVar)))

scored_a1_q1_2b <- exercises %>% 
  filter(exercise_id == "q1-2b") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(last_cmd = map_chr(answer,
                            get_last_command),
         correct = map_lgl(last_cmd, grade_this_code_hash, solution_code = ~is.numeric(mysteryVar)))

scored_a1_q1_2c <- exercises %>% 
  filter(exercise_id == "q1-2c") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(last_cmd = map_chr(answer,
                            get_last_command),
         correct = map_lgl(last_cmd, grade_this_code_hash, solution_code = ~is.character(mysteryVar)))

scored_a1_q1_2d <- exercises %>% 
  filter(exercise_id == "q1-2d") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(last_cmd = map_chr(answer,
                            get_last_command),
         correct_class = map_lgl(last_cmd, grade_this_code_hash, solution_code = ~class(mysteryVar)),
         correct_typeof = map_lgl(last_cmd, grade_this_code_hash, solution_code = ~typeof(mysteryVar)),
         correct = correct_class | correct_typeof)

scored_a1_q2 <- exercises %>% 
  filter(exercise_id == "q2") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_custom_hash, grader = grader_a1_q2))

scored_a1_q3 <- exercises %>% 
  filter(exercise_id == "q3") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_custom_hash, grader = grader_a1_q3))

scored_a1_q4_1 <- exercises %>% 
  filter(exercise_id == "q4-1") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_custom_hash, grader = grader_a1_q4_1))

scored_a1_q4_2 <- exercises %>% 
  filter(exercise_id == "q4-2") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_custom_hash, grader = grader_a1_q4_2))

scored_a1_q5 <- exercises %>% 
  filter(exercise_id == "q5") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_lgl(answer, grade_this_code_hash, solution_code = ~class(acs_ny)))

## bind together ----

scored_a1 <- bind_rows(scored_a1_q1_2a,
                       scored_a1_q1_2b,
                       scored_a1_q1_2c,
                       scored_a1_q1_2d,
                       scored_a1_q2,
                       scored_a1_q3,
                       scored_a1_q4_1,
                       scored_a1_q4_2,
                       scored_a1_q5) %>% 
  select(student, exercise_id, answer, correct) %>% 
  mutate(answer = as.list(answer)) %>% 
  bind_rows(code = .,
            question = scored_a1_q1_1,
            .id = "answer_type") %>% 
  nest(answers = -student) %>%
  mutate(pct_correct = map_dbl(answers, ~mean(.x$correct)),
         pct_code_correct = map_dbl(answers, ~mean(.x$correct[.x$answer_type == "code"])))

cat("Ready for inspection for overall grade")
