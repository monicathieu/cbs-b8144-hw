## setup ----

source(here::here("inst", "grading", "setup.R"))

load(here::here("inst", "tutorials", "assignment3", "data", "diamonds_short.rda"))
load(here::here("inst", "tutorials", "assignment3", "data", "gdp_data.rda"))

## graders for more elaborate grading logic ----

solution_a3_q1 <- jsonlite::fromJSON("https://jaredlander.com/data/PizzaPlaces.json") %>% 
  as_tibble() %>% 
  unnest(Details)

grader_a3_q1 <- grade_this({
  fail_if(any(map_chr(.result, typeof) == "list"),
          message = "still nested")
  fail_if_equal(solution_a3_q1, message = "forgot head")
  fail_if_equal(head(solution_a3_q1), message = "head left as default n")
  pass_if_equal(head(solution_a3_q1, 10))
  fail(message = "incorrect")
})

solution_a3_q2 <- diamonds_short %>% 
  summarize(across(where(is.numeric), mean))

grader_a3_q2 <- grade_this({
  # Apparently across behaves appropriately when just the predicate function is called without where()!
  fail_if(!all(grepl("summarize|summarise", .user_code),
               grepl("across", .user_code),
               grepl("is.numeric", .user_code)),
          message = "did not use summarize-across-where")
  # Some solutions are failing to get the piped-in solution_code?
  pass_if_equal()
  fail()
})

solution_a3_q3 <- diamonds_short %>% 
  select(where(is.numeric)) %>% 
  map(mean)

grader_a3_q3 <- grade_this({
  fail_if(!all(grepl("select", .user_code),
               grepl("map\\(", .user_code),
               grepl("is.numeric", .user_code)),
          message = "did not use select-map")
  pass_if_equal(ggplot2::diamonds %>% 
                  select(where(is.numeric)) %>% 
                  map(mean),
                message = "correct with full data")
  pass_if_equal()
  fail()
})

## read in submissions downloaded from canvas ----

submissions <- tibble(filename = list.files("~/Downloads/submissions_a3", full.names = TRUE)) %>%
  # possible to have "late" in the filename after the student name
  separate(filename, into = c(NA, NA, NA, NA, NA, NA, "student", NA, NA, NA), remove = FALSE) %>% 
  mutate(hash = map_chr(filename,
                        ~.x %>% 
                          read_lines() %>% 
                          .[nchar(.) == max(nchar(.))] %>% 
                          str_split("[<>]") %>% 
                          unlist() %>% 
                          .[nchar(.) == max(nchar(.))]),
         hash_length = map_int(hash, nchar))

## extract hashery ----

exercises <- learnrhash::extract_exercises(submissions, hash) %>% 
  # remove carriage returns from windows machines
  mutate(code = map_chr(code, str_remove_all, "\r"))

## score questions 5-8 ----

scored_a3_q1 <- exercises %>% 
  filter(exercise_id == "q1") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_chr(answer,
                           grade_custom_hash,
                           grader = grader_a3_q1,
                           solution_code = ~jsonlite::fromJSON("https://jaredlander.com/data/PizzaPlaces.json") %>% 
                             as_tibble() %>% 
                             unnest(Details) %>% 
                             head(10),
                           return_message = TRUE))

scored_a3_q2 <- exercises %>% 
  filter(exercise_id == "q2") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_chr(answer,
                           grade_custom_hash,
                           solution_code = ~diamonds_short %>% 
                             summarize(across(where(is.numeric), mean)),
                           grader = grader_a3_q2,
                           return_message = TRUE))

scored_a3_q3 <- exercises %>% 
  filter(exercise_id == "q3") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_chr(answer,
                           grade_custom_hash,
                           solution_code = ~diamonds_short %>% 
                             select(where(is.numeric)) %>% 
                             map(mean),
                           grader = grader_a3_q3,
                           return_message = TRUE))
