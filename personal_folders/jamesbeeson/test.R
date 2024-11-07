# QUESTIONS
# I'd like to see the difference between MATH 112D and the other MATH 112's
# Do individuals stay around the same time?
# What is the best measure for how long someone stays?
# Does it vary semester to semester?

library(tidyverse)

clean <- read_csv("raw_data/Clean_lab_data.csv")
df <- clean

glimpse(df)
View(df)

df <- df %>% rename(
  course = Course,
  professor = Professor,
  class = "Student's class",
  duration = "Visit duration (in minutes)",
  datetime = "Date (of the appointment/visit)",
  type = "Status (of the appointment/visit)",
  appt_id = "Appointment Extended properties Id",
  student_id = ID
)

# Shows distribution of time, breaks
df %>% ggplot(aes(datetime, duration)) +
  geom_point()

# skewed, but HUGE spike
df %>% ggplot(aes(duration)) +
  geom_density()

df %>% ggplot(aes(duration))

# 60 most common
median(df$duration)

# num rows with 60
num_60s <- df %>% filter(duration == 60) %>% nrow()

# ratio of 60's - it's 1/4....... YIKES
num_60s / nrow(df) * 100

# I'd like to see the difference between MATH 112D and the rest

# This is the dataframe that threw caution to the wind
# Remove values that are clearly data errors (60 is logged when they don't log out)
dropit <- df %>% filter(duration != 60, duration > 1)

# The spikes are at 11 and 60 minutes naturally
dropit %>% filter(duration < 70) %>% ggplot(aes(duration)) +
  geom_density() +
  geom_vline(xintercept = 60) +
  geom_vline(xintercept = 11)

dropit %>% 
  mutate(
    wday = wday(datetime)
  ) %>% 
  group_by(wday) %>% 
  summarize(
    num_people = n()
  ) %>% 
  ggplot(aes(wday, num_people)) +
    geom_col()
