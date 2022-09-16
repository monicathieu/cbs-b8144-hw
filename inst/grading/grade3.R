## setup ----

source(here::here("inst", "grading", "setup.R"))

load(here::here("inst", "tutorials", "assignment3", "data", "diamonds_short.rda"))
load(here::here("inst", "tutorials", "assignment3", "data", "gdp_data.rda"))

path_to_submissions <- here::here("ignore", "submissions", "a3")

## graders for more elaborate grading logic ----

solution_a3_q1 <- jsonlite::fromJSON("https://jaredlander.com/data/PizzaPlaces.json") %>% 
  as_tibble() %>% 
  unnest(Details)

grader_a3_q1 <- grade_this({
  fail_if(!is.null(.error),
          message = "code returns error")
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
  fail_if(!is.null(.error),
          message = "code returns error")
  # Apparently across behaves appropriately when just the predicate function is called without where()!
  fail_if(!all(grepl("summarize|summarise", .user_code),
               grepl("across", .user_code),
               grepl("is.numeric", .user_code)),
          message = "did not use summarize-across-where")
  # Some solutions are failing to get the piped-in solution_code?
  pass_if_equal()
  fail()
})

grader_a3_q3 <- grade_this({
  fail_if(!is.null(.error),
          message = "code returns error")
  fail_if(grepl("summarize|summarise", .user_code),
          message = "used summarize")
  fail_if(!all(grepl("select", .user_code),
               grepl("map\\(", .user_code),
               grepl("is.numeric", .user_code)),
          message = "did not use select-map")
  fail_if_equal(ggplot2::diamonds %>% 
                  select(where(is.numeric)) %>% 
                  map(mean),
                message = "correct with full data")
  pass_if_equal()
  fail()
})

grader_a3_q4 <- grade_this({
  fail_if(!is.null(.error),
          message = "code returns error")
  fail_if(grepl("summarize|summarise", .user_code),
          message = "used summarize")
  fail_if(!all(grepl("select", .user_code),
               grepl("map_df\\(", .user_code),
               grepl("is.numeric", .user_code)),
          message = "did not use select-map_df")
  fail_if_equal(ggplot2::diamonds %>% 
                  select(where(is.numeric)) %>% 
                  map_df(mean),
                message = "correct with full data")
  pass_if_equal()
  fail()
})

grader_a3_q5 <- grade_this({
  fail_if(!is.null(.error),
          message = "code returns error")
  fail_if(grepl("summarize|summarise", .user_code),
          message = "used summarize")
  fail_if(!all(grepl("map_if\\(", .user_code),
               grepl("is.numeric", .user_code)),
          message = "did not use map_if")
  fail_if_equal(diamonds_short %>% 
                  map_if(is.numeric, mean),
                message = "missed .else")
  fail_if_equal(ggplot2::diamonds %>% 
                  map_if(is.numeric, mean),
                message = "missed .else")
  fail_if_equal(ggplot2::diamonds %>% 
                  map_if(is.numeric, mean, .else = ~NULL),
                message = "correct with full data")
  pass_if_equal()
  fail()
})

grader_a3_q6 <- grade_this({
  fail_if(!grepl("pivot_wider\\(", .user_code),
          message = "did not use pivot_wider")
  fail_if(!is.null(.error),
          message = "code returns error")
  fail_if(nrow(.result) > 5,
          message = "too many rows")
  fail_if(nrow(.result) < 5,
          message = "too few rows")
  fail_if(any(str_replace_all(names(.result), "gpd", "gdp") != names(gdp_data)),
          message = "colnames don't match")
  pass_if_equal(x = .result %>% 
                  ungroup() %>% 
                  rename_with(~str_replace(., "gpd", "gdp"), contains("gpd")),
                y = gdp_data)
  pass_if(nrow(.result) == 5 & all(str_replace_all(names(.result), "gpd", "gdp") == names(gdp_data)))
  fail()
})

solution_a3_q7 <- airquality %>%
  as_tibble() %>% 
  pivot_longer(cols = Ozone:Temp, names_to = "MeasurementType", values_to = "Measurement") %>% 
  arrange(Month, Day, MeasurementType)

grader_a3_q7 <- grade_this({
  fail_if(!is.null(.error),
          message = "code returns error")
  fail_if(!grepl("pivot_longer\\(", .user_code),
          message = "did not use pivot_longer")
  fail_if(ncol(.result) != 4,
          message = "incorrect ncol")
  pass_if_equal(x = ungroup(.result),
                y = head(solution_a3_q7, 10))
  pass_if(nrow(.result) == 10 & identical(.result %>% 
                                            ungroup() %>% 
                                            filter(Month == 5, Day <= 2) %>% 
                                            arrange(across(everything())),
                                          solution_a3_q7 %>% 
                                            filter(Month == 5, Day <= 2)))
  fail_if_equal(x = .result %>% ungroup() %>% arrange(across(everything())),
                y = solution_a3_q7,
                message = "forgot head")
  fail_if_equal(x = .result %>% ungroup() %>% arrange(across(everything())),
                y = head(solution_a3_q7),
                message = "head left as default n")
  fail_if(!identical(sort(unique(.result[[3]])), sort(unique(solution_a3_q7[[3]]))),
          message = "not all columns pivoted")
  # allow if it's correct but the names aren't set. Won't be picky about this
  pass_if_equal(x = .result %>% 
                  rename_with(~"MeasurementType", 3) %>% 
                  rename_with(~"Measurement", 4) %>% 
                  ungroup() %>% 
                  arrange(across(everything())),
                y = head(solution_a3_q7, 10))
  fail_if_equal(x = .result %>% 
                  rename_with(~"MeasurementType", 3) %>% 
                  rename_with(~"Measurement", 4) %>% 
                  ungroup() %>% 
                  arrange(across(everything())),
                y = solution_a3_q7,
                message = "forgot head and misspecified out names")
  fail_if_equal(x = .result %>% 
                  rename_with(~"MeasurementType", 3) %>% 
                  rename_with(~"Measurement", 4) %>% 
                  ungroup() %>% 
                  arrange(across(everything())),
                y = head(solution_a3_q7),
                message = "head left as default n and misspecified out names")
  fail()
})

gov_type <- read_csv("https://jaredlander.com/data/GovType.csv")

solution_a3_q8 <- gdp_data %>% 
  left_join(gov_type, by = c("country" = "Country"))

grader_a3_q8 <- grade_this({
  fail_if(!is.null(.error),
          message = "code returns error")
  fail_if(nrow(.result) > 5,
          message = "too many rows")
  fail_if(nrow(.result) < 5,
          message = "too few rows")
  fail_if(any(str_replace_all(names(.result), "gpd", "gdp") != names(solution_a3_q8)),
          message = "colnames don't match")
  fail_if_equal(x = .result %>% ungroup(),
                y = .solution %>% filter(!is.na(GovernmentType)))
  pass_if_equal(x = .result %>% 
                  ungroup() %>% 
                  rename_with(~str_replace(., "gpd", "gdp"), contains("gpd")),
                y = .solution)
  pass_if(is_tibble(.result) & nrow(.result) == 5 & all(str_replace_all(names(.result), "gpd", "gdp") == names(solution_a3_q8)))
  fail()
})

## read in submissions downloaded from canvas ----

submissions <- tibble(filename = list.files(path_to_submissions, full.names = TRUE),
                      student = list.files(path_to_submissions)) %>%
  # possible to have "late" in the filename after the student name
  # which will throw an "additional pieces" warning for separate()
  separate(student, into = c("student", NA, NA, NA)) %>% 
  mutate(hash = map_chr(filename,
                        ~.x %>% 
                          read_lines() %>% 
                          .[nchar(.) == max(nchar(.))] %>% 
                          str_split("[<>]") %>% 
                          unlist() %>% 
                          .[nchar(.) == max(nchar(.))]),
         # One student didn't copy and paste the entire hash so it wasn't rendering
         hash = if_else(!startsWith(hash, "Q"), paste0("Q", hash), hash),
         hash_length = map_int(hash, nchar))

## extract hashery ----

exercises <- learnrhash::extract_exercises(submissions, "hash") %>% 
  # remove carriage returns from windows machines
  mutate(code = map_chr(code, str_remove_all, "\r"))

## score questions ----

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

scored_a3_q4 <- exercises %>% 
  filter(exercise_id == "q4") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_chr(answer,
                           grade_custom_hash,
                           solution_code = ~diamonds_short %>% 
                             select(where(is.numeric)) %>% 
                             map_df(mean),
                           grader = grader_a3_q4,
                           return_message = TRUE))

scored_a3_q5 <- exercises %>% 
  filter(exercise_id == "q5") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_chr(answer,
                           grade_custom_hash,
                           solution_code = ~diamonds_short %>% 
                             map_if(is.numeric, mean, .else = ~NULL),
                           grader = grader_a3_q5,
                           return_message = TRUE))

scored_a3_q6 <- exercises %>% 
  filter(exercise_id == "q6") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_chr(answer,
                           grade_custom_hash,
                           grader = grader_a3_q6,
                           return_message = TRUE))

scored_a3_q7 <- exercises %>% 
  filter(exercise_id == "q7") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_chr(answer,
                           grade_custom_hash,
                           grader = grader_a3_q7,
                           return_message = TRUE))

scored_a3_q8 <- exercises %>% 
  filter(exercise_id == "q8") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(correct = map_chr(answer,
                           grade_custom_hash,
                           solution_code = ~gdp_data %>% 
                             left_join(gov_type,
                                       by = c("country" = "Country")),
                           grader = grader_a3_q8,
                           return_message = TRUE))

## assembling stuff together for grading ----

scored_all <- list(scored_a3_q1,
                   scored_a3_q2,
                   scored_a3_q3,
                   scored_a3_q4,
                   scored_a3_q5,
                   scored_a3_q6,
                   scored_a3_q7,
                   scored_a3_q8) %>% 
  bind_rows() %>% 
  nest(answers = -student) %>% 
  mutate(n_correct = map_int(answers, ~sum(.x$correct == "correct")))
