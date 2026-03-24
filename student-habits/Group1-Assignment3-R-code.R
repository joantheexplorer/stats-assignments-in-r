# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- STATS 203: Statistics and Probability for CS/IT Students ----
# ---- Members: Aviles, Acob, Alapag (BSCS 2-4) ----
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

# 2. What is the average stress level reported by the students?
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
major_counts <- table(df$major)

major_dist_table <- as.data.frame(major_counts)

print("Table: Frequency Distribution of Students by Major")

print(as.data.frame(major_dist_table), row.names = FALSE)

# 5. How are the daily study hours distributed across the students in this dataset?
ggplot(df, aes(x = study_hours_per_day)) +
  geom_histogram(binwidth = 1, fill = "#2E6FAB", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(study_hours_per_day, na.rm = TRUE)), 
             color = "#C0392B", linetype = "dashed", size = 1.2) +
  labs(title = "Histogram of Daily Study Per Day",
       subtitle = "The red dashed line indicates is the average study time",
       x = "Study Hours Per Day",
       y = "Number of Students") +
  theme_light() +
  theme(plot.title = element_text(face = "bold", size = 14))
print(mean(df$study_hours_per_day, na.rm = TRUE))


# ---- Bivariate ----

# 1. Question: Which academic majors show the highest and lowest average exam performance?
major_performance_raw <- df %>%
  group_by(major) %>%
  summarise(
    Average_Exam_Score = mean(exam_score, na.rm = TRUE),
  ) %>%
  arrange(desc(Average_Exam_Score))

# Print as a clean data frame with all decimals
print("Table: Raw Average Exam Performance by Academic Major")
print(as.data.frame(major_performance_raw), row.names = FALSE)


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
  geom_point(color = "#2E6FAB", size = 1.5) +
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

# 6. Question: How do different levels of stress and motivation interact to influence the exam scores of the students?

heatmap_data <- df %>%
  group_by(stress_level, motivation_level) %>%
  summarise(avg_score = mean(exam_score, na.rm = TRUE), .groups = 'drop')

ggplot(heatmap_data, aes(x = stress_level, y = motivation_level, fill = avg_score)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Heatmap of Exam Scores",
       subtitle = "Stress vs. Motivation",
       x = "Stress Level",
       y = "Motivation Level",
       fill = "Avg Score") +
  theme_minimal()

# 7. Question: How do the varying frequencies of exercise and qualities of diet interact to influence the average stress levels among students?
diet_ex_summary <- df %>%
  group_by(diet_quality, exercise_frequency) %>%
  summarise(mean_stress = mean(stress_level, na.rm = TRUE), .groups = 'drop')
ggplot(diet_ex_summary, aes(x = exercise_frequency, y = mean_stress, group = diet_quality, color = diet_quality)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Stress Trends by Exercise and Diet",
       x = "Exercise Frequency",
       y = "Avg Stress Level",
       color = "Diet Quality") +
  theme_light()


# ---- End of Program ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~