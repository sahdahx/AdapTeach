install.packages("dplyr")
library(dplyr)
library(tibble)

# Membuat data frames
demographics <- tibble(
  id = c("1001", "1002", "1003", "1004", "1005"),
  dob = as.Date(c("1968-12-14", "1952-08-03", "1949-05-27", "1955-03-12", "1942-06-07")),
  race_eth = c(1, 2, 2, 4, 3)
)

grip_strength <- tibble(
  id = rep(c("1001", "1002", "1003", "1004"), each = 2),
  visit = rep(c("pre", "post"), 4),
  grip_r = c(32, 33, 28, 27, 32, 34, 22, 27),
  grip_l = c(30, 32, 30, 30, 28, 30, 22, 26)
)

emr <- tibble(
  id = rep(c("1001", "1002", "1003", "1004"), each = 2),
  visit = rep(c("pre", "post"), 4),
  weight = c(105, 99, 200, 201, 136, 133, 170, 175)
)

# Perform left join
left_join_result <- left_join(demographics, grip_strength, by = "id") %>%
  left_join(emr, by = c("id", "visit"))

# Perform right join
right_join_result <- right_join(grip_strength, demographics, by = "id") %>%
  right_join(emr, by = c("id", "visit"))

# Perform full join
full_join_result <- full_join(demographics, grip_strength, by = "id") %>%
  full_join(emr, by = c("id", "visit"))

# Perform inner join
inner_join_result <- inner_join(demographics, grip_strength, by = "id") %>%
  inner_join(emr, by = c("id", "visit"))

# Display results
left_join_result
right_join_result
full_join_result
inner_join_result
