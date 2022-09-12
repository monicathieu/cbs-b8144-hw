## setup ----

source(here::here("inst", "grading", "setup.R"))
source(here::here("inst", "grading", "setup_ggcheck.R"))

require(ggcheck)

load(here::here("inst", "tutorials", "assignment2", "data", "nycflights13_short.rda"))

path_to_submissions <- here::here("ignore", "submissions", "a2")

## graders for more elaborate grading logic ----

grader_a2_q5_1 <- grade_this({
  expect_identical(.result, flights_short %>% select(contains("dep")))
  expect_match(.user_code, "contains")
  pass()
})

grader_a2_q5_2 <- grade_this({
  expect_identical(.result, flights_short %>% select(starts_with("s")))
  expect_match(.user_code, "starts_with")
  pass()
})

grader_a2_q6 <- grade_this({
  expect_identical(.result,
                   flights_short %>% 
                     group_by(origin) %>% 
                     summarize(across(c(dep_delay, arr_delay, air_time, distance),
                                      list(mean = mean, sd = sd), 
                                      na.rm = T)))
  pass()
})

grader_a2_q7 <- grade_this({
  expect_identical(.result %>% 
                     arrange(year, month, day, dep_time),
                   flights_short %>%
                     filter(origin == "JFK", dep_time > 1200, dep_time < 2100) %>% 
                     arrange(year, month, day, dep_time))
  pass()
})

grader_a2_q8 <- grade_this({
  expect_identical(.result,
                        flights_short %>% 
                          mutate(speed = distance / air_time) %>% 
                          group_by(origin) %>% 
                          summarize(avg_speed = mean(speed, na.rm = T)))
  pass()
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
         hash_length = map_int(hash, nchar))

## extract hashery ----

exercises <- learnrhash::extract_exercises(submissions, "hash") %>% 
  # remove carriage returns from windows machines
  mutate(code = map_chr(code, str_remove_all, "\r"))

env_q1 <- mock_this_exercise_hash(exercises$code[exercises$exercise_id == "q1"][1],
                                  solution_code = ~ggplot(dplyr::starwars, aes(x = height, y = mass)) +
                                    geom_point() +
                                    labs(x = "Character height (cm)",
                                         y = "Character mass (kg)",
                                         title = "Scatterplot of Star Wars characters' mass by height"))

env_q2 <- mock_this_exercise_hash(exercises$code[exercises$exercise_id == "q2"][1],
                                  solution_code = ~ggplot(dplyr::starwars, aes(x = -birth_year)) +
                                    geom_histogram() +
                                    labs(x = "Character birth year (years before Episode 4)",
                                         y = "# of characters",
                                         title = "Yoda is the outlier on this histogram"))

env_q3 <- mock_this_exercise_hash(exercises$code[exercises$exercise_id == "q3"][1],
                                  solution_code = ~ggplot(dplyr::starwars, aes(x = height)) +
                                    geom_density(fill = "skyblue") +
                                    facet_wrap(~gender, scales = "free_y") +
                                    labs(x = "Character height (cm)",
                                         y = "Density (Probability units)") +
                                    theme_bw())

## score question 1 ----

scored_a2_q1 <- exercises %>% 
  filter(exercise_id == "q1") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer_env = map(answer,
                          ~mock_this_exercise_hash(.x)),
         answer_plot = map(answer_env, pluck, ".result"),
         answer_is_ggplot = map_lgl(answer_plot, is.ggplot)) %>% 
  nest(answers = -c(exercise_id, answer_is_ggplot))

scored_a2_q1$answers[[1]] %<>%
  mutate(has_geom = map_lgl(answer_plot,
                            uses_geoms,
                            "point"),
         has_mappings = map_lgl(answer_plot,
                                ~.x %>% 
                                  # in the event that people specify it inside the geom
                                  # must pull the point layer
                                  # because uses_mappings will inherit the global aes
                                  # if called on the layer with local_only = F
                                  # but will not ever detect local aes
                                  # if called on the global ggplot object
                                  get_geom_layer("point") %>% 
                                  uses_mappings(aes(x = height, y = mass))),
         has_set_labs = map_lgl(answer_plot,
                                uses_labs,
                                geom = "point",
                                labs = c("x", "y", "title")))

scored_a2_q1 %<>%
  unnest(answers) %>% 
  mutate(across(where(is.logical), coalesce, FALSE),
         correct = answer_is_ggplot & has_geom & has_mappings & has_set_labs,
         # people whose plots appear correct but set data or labs weirdly
         correct = if_else(student %in% c("benyanitushar",
                                          "yeeedmond"),
                           TRUE,
                           correct))

## score question 2 ----

scored_a2_q2 <- exercises %>% 
  filter(exercise_id == "q2") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer_env = map(answer,
                          ~mock_this_exercise_hash(.x)),
         answer_plot = map(answer_env, pluck, ".result"),
         answer_is_ggplot = map_lgl(answer_plot, is.ggplot)) %>% 
  nest(answers = -c(exercise_id, answer_is_ggplot))

scored_a2_q2$answers[[1]] %<>%
  mutate(has_geom = map_lgl(answer_plot,
                            uses_geoms,
                            "histogram"),
         mapping_x_valid = map_lgl(answer_plot,
                               function (x) {
                                 mapping <- x %>% 
                                   get_geom_layer("histogram") %>% 
                                   get_mappings() %>% 
                                   pluck("x")
                                 # if col was calculated before aes mapping
                                 if (is_symbol(quo_get_expr(mapping))) {
                                  return(as_name(mapping) %in% names(x$data))
                                 } else {
                                   return (grepl("-", as_label(mapping)) & grepl("birth_year", as_label(mapping)))
                                 }
                               }),
         has_set_labs = map_lgl(answer_plot,
                                uses_labs,
                                geom = "histogram",
                                labs = c("x", "y", "title")))

scored_a2_q2 %<>%
  unnest(answers) %>% 
  mutate(across(where(is.logical), coalesce, FALSE),
         correct = answer_is_ggplot & has_geom & mapping_x_valid & has_set_labs,
         # people whose plots appear correct but set data or labs weirdly
         correct = if_else(student %in% c("benyanitushar",
                                          "bjorklundross",
                                          "yeeedmond"),
                           TRUE,
                           correct))

## score question 3 ----

scored_a2_q3 <- exercises %>% 
  filter(exercise_id == "q3") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer_env = map(answer,
                          ~mock_this_exercise_hash(.x)),
         answer_plot = map(answer_env, pluck, ".result"),
         answer_is_ggplot = map_lgl(answer_plot, is.ggplot)) %>% 
  nest(answers = -c(exercise_id, answer_is_ggplot))

scored_a2_q3$answers[[1]] %<>%
  mutate(has_geom = map_lgl(answer_plot,
                            uses_geoms,
                            "density")) %>% 
  nest(answers = -has_geom)

scored_a2_q3$answers[[1]]$answers[[1]] %<>% 
  mutate(has_set_labs = map_lgl(answer_plot,
                                uses_labs,
                                geom = "density",
                                labs = c("x", "y")),
         has_facet_var = map_lgl(answer_plot,
                                 ~identical(names(.x$facet$params$facets), "gender")),
         has_facet_free_y = map_lgl(answer_plot,
                                    ~identical(.x$facet$params$free$y, TRUE)),
         has_fill = map_lgl(answer_plot,
                            function (x) {
                              constant_aes <- x %>% 
                                get_geom_layer("density") %>% 
                                pluck("layer", "aes_params")
                              if ("fill" %in% names(constant_aes)) {
                                if (constant_aes[["fill"]] == "skyblue") {
                                  return (TRUE)
                                } else {
                                  return (FALSE)
                                }
                              } else {
                                return (FALSE)
                              }
                            }),
         calls_theme = map_lgl(answer, ~grepl("theme_bw()", .x)))

scored_a2_q3$answers[[1]] %<>%
  unnest(answers)

scored_a2_q3 %<>%
  unnest(answers) %>% 
  mutate(across(where(is.logical), coalesce, FALSE),
         correct = answer_is_ggplot & has_geom & has_set_labs & has_facet_var & has_facet_free_y & has_fill & calls_theme,
         # people whose plots appear correct but set labs, free scales, or colors weirdly
         correct = if_else(student %in% c("benyanitushar",
                                          "salwanrishabh",
                                          "yeeedmond",
                                          "wuzihao"),
                           TRUE,
                           correct))

## score question 4 ----

# Some of these have the labels set in scale_x_? or scale_y_? (which is bad)
# but are technically correct and should get full credit
scored_a2_q4 <- exercises %>% 
  filter(exercise_id == "q4") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer_env = map(answer,
                          ~mock_this_exercise_hash(.x)),
         answer_plot = map(answer_env, pluck, ".result"),
         answer_is_ggplot = map_lgl(answer_plot, is.ggplot),
         has_geom = map2_lgl(answer_plot, answer_is_ggplot,
                             function (this_plot, is_plot) {
                               if (!is_plot) return (FALSE)
                               uses_geoms(this_plot, "violin")
                             }
         ),
         data_are_filtered = map_lgl(answer_plot,
                                     ~.x %>% 
                                       pluck("data") %>% 
                                       identical(filter(dplyr::starwars,
                                                        species %in% c("Human", "Droid", "Wookiee")))),
         has_set_labs = map2_lgl(answer_plot, answer_is_ggplot,
                                 function (this_plot, is_plot) {
                                   if (!is_plot) return (FALSE)
                                   uses_labs(this_plot,
                                             geom = "violin",
                                             labs = c("x", "y", "title"))
                                 }
         ),
         calls_theme = map_lgl(answer, ~grepl("theme_economist()", .x)),
         correct = answer_is_ggplot & has_geom & data_are_filtered & has_set_labs & calls_theme,
         # people whose plots appear correct but set labs weirdly
         correct = if_else(student %in% c("benyanitushar",
                                          "lualdifrancesco",
                                          "yeeedmond"),
                           TRUE,
                           correct))

## score questions 5-8 ----

# all who are !correct are wrong
scored_a2_q5_1 <- exercises %>% 
  filter(exercise_id == "q5-1") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer = str_replace_all(answer, "flights ", "flights_short "),
         correct = map_lgl(answer, grade_custom_hash, grader = grader_a2_q5_1))

# all correct!
scored_a2_q5_2 <- exercises %>% 
  filter(exercise_id == "q5-2") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer = str_replace_all(answer, "flights ", "flights_short "),
         correct = map_lgl(answer, grade_custom_hash, grader = grader_a2_q5_2))

# most who are !correct are wrong in varying degrees
# allow those who are correct save for spelling of cols, sad
scored_a2_q6 <- exercises %>% 
  filter(exercise_id == "q6") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer = str_replace_all(answer, "flights ", "flights_short "),
         correct = map_lgl(answer, grade_custom_hash, grader = grader_a2_q6),
         correct = if_else(student %in% c("qiuaurora",
                                          "yangsean"),
                           TRUE,
                           correct))

# all who are !correct are wrong, but some are more wrong than others
scored_a2_q7 <- exercises %>% 
  filter(exercise_id == "q7") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer = str_replace_all(answer, "flights ", "flights_short "),
         correct = map_lgl(answer, grade_custom_hash, grader = grader_a2_q7))

# all who are !correct are wrong
scored_a2_q8 <- exercises %>% 
  filter(exercise_id == "q8") %>% 
  select(student, exercise_id, answer = code) %>% 
  mutate(answer = str_replace_all(answer, "flights ", "flights_short "),
         correct = map_lgl(answer, grade_custom_hash, grader = grader_a2_q8))

## assembling stuff together for grading

scored_a2 <- list(scored_a2_q1,
                   scored_a2_q2,
                   scored_a2_q3,
                   scored_a2_q4,
                   scored_a2_q5_1,
                   scored_a2_q5_2,
                   scored_a2_q6,
                   scored_a2_q7,
                   scored_a2_q8) %>% 
  map(~.x %>% select(student, exercise_id, answer, correct)) %>% 
  bind_rows() %>% 
  nest(answers = -student) %>% 
  mutate(n_correct = map_int(answers, ~sum(.x$correct)))
