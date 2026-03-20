# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- STATS 203: Statistics and Probability for CS/IT Students ----
# ---- Members: Aviles, Artates, Acob, Alapag (BSCS 2-4) ----
# ---- Assignment #3: Exploratory Data Analysis Instructions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---- Pre requisites ----
library(dplyr)
library(ggplot2)
library(moments)
df <- read.csv("enhanced_student_habits_performance_dataset.csv")

# ---- Univariate ----
# 1. Most common learning style
print("What is the most common learning style among the students?: ")
learning_style_counts <- table(df$learning_style)
most_common_learning <- names(which.max(learning_style_counts))
print(most_common_learning)

# 2. Average amount of time spent studying
print("What is the average stress level reported by the students?: ")
avg_stress_level <- mean(df$stress_level, na.rm = TRUE)
print(avg_stress_level)


# 3. How is the attendance percentage distributed among the students
ggplot(df, aes(x = attendance_percentage)) +
  geom_histogram(fill = "#2E6FAB", color = "white", bins = 25) +
  labs(title = "Frequency Distribution of Attendance",
       x = "Attendance (%)",
       y = "Number of Students") +
  theme_light() +
  theme(plot.title = element_text(face = "bold", size = 14))

# 4. Distribution of Majors
print("How are students distributed across the various majors?:")
major_distribution <- table(df$major)
print(major_distribution)

# 5. What is the distribution of part-time employment among the students?
job_counts <- as.data.frame(table(df$part_time_job))
colnames(job_counts) <- c("Status", "Count")

ggplot(job_counts, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Yes" = "#2E6FAB", "No" = "#C0392B")) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4, fontface = "bold") +
  expand_limits(y = 45000) + 
  labs(title = "Student Employment Distribution",
       x = "Part-time Job",
       y = "Count") +
  theme_light() +
  theme(legend.position = "none")


# ---- Bivariate ----

# 1. Question: Which academic majors show the highest and lowest average exam performance?
print("Average Exam Score by Major:")
aggregate(exam_score ~ major, data = df, mean)


# 2. Question: How does the level of exam anxiety impact the final exam scores of students?
anxiety_trend <- df %>%
  group_by(exam_anxiety_score) %>%
  summarise(avg_exam = mean(exam_score, na.rm = TRUE))

ggplot(anxiety_trend, aes(x = exam_anxiety_score, y = avg_exam)) +
  geom_line(color = "#C0392B", size = 1.2) +
  geom_point(color = "#2E6FAB", size = 3) +
  labs(title = "Impact of Anxiety on Exam Performance",
       x = "Anxiety Score",
       y = "Average Exam Score") +
  theme_light()


# 3.Question: Does a student's reported stress level affect their level of motivation?
stress_motivation_summary <- df %>%
  group_by(stress_level) %>%
  summarise(avg_motivation = mean(motivation_level, na.rm = TRUE))

ggplot(stress_motivation_summary, aes(x = stress_level, y = avg_motivation)) +
  geom_line(color = "#C0392B", size = 1.0) +
  geom_point(color = "#2E6FAB", size = 2) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Stress vs. Motivation",
       x = "Stress Level (1-10)",
       y = "Average Motivation Level") +
  theme_light()


# 4. Question: How do the distribution and consistency of study hours differ between students with part-time job or not?
summary_table <- df %>%
  group_by(part_time_job) %>%
  summarise(
    Mean = mean(study_hours_per_day, na.rm = TRUE),
    SD = sd(study_hours_per_day, na.rm = TRUE),
    Skewness = skewness(study_hours_per_day, na.rm = TRUE),
    Kurtosis = kurtosis(study_hours_per_day, na.rm = TRUE)
  )


# 5. Question: What is the frequency of part-time job across the various majors?
raw_tab <- table(df$major, df$part_time_job)
ctab_df <- as.data.frame.matrix(raw_tab)
colnames(ctab_df) <- paste("Has Job:", colnames(ctab_df))

# ---- End of Program ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~