# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- STATS 203: Statistics and Probability for CS/IT Students ----
# ---- Members: Aviles, Artates, Acob, Alapag (BSCS 2-4) ----
# ---- Assignment #3: Exploratory Data Analysis Instructions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---- Pre requisites ----
library(ggplot2)
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




