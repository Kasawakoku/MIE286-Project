
# ==============================================================================
# Investigating the Effects of Visual Vs. Auditory Feedback on Time and Accuracy of Integral Solving
# ==============================================================================

# MIE286
# Group 68

# install.packages("tidyverse")
library(tidyverse)


# ==============================================================================
# 1. DATA SETUP & PREPARATION
# ==============================================================================

# Load the data
visual_data <- data.frame(
  Mode = "Visual",
  Gender = c("F", "F", "M", "M", "F", "M", "M", "F", "M", "F", "M", "F"),
  Time = c(530, 398, 546, 546, 614, 757, 563, 434, 849, 754, 473, 503),
  Accuracy = c(20, 23, 21, 19, 22, 21, 21, 12, 18, 22, 24, 22)
)

audio_data <- data.frame(
  Mode = "Auditory",
  Gender = c("F", "F", "F", "M", "M", "M", "M", "M", "F", "F", "F", "M"),
  Time = c(608, 550, 365, 410, 738, 484, 660, 280, 540, 510, 280, 647),
  Accuracy = c(23, 22, 19, 19, 21, 23, 23, 19, 21, 18, 14, 22)
)

# Combine into one main dataframe
df <- bind_rows(visual_data, audio_data)

# Convert categorical variables to factors for proper statistical modeling
df$Mode <- as.factor(df$Mode)
df$Gender <- as.factor(df$Gender)


# ==============================================================================
# 2. DESCRIPTIVE STATISTICS (RESULTS SECTION)
# ==============================================================================

# Calculate counts, means, medians, standard deviations, and variance
summary_stats <- df %>%
  group_by(Mode) %>%
  summarise(
    Count = n(),
    
    # Time Statistics
    Mean_Time = mean(Time, na.rm = TRUE),
    Median_Time = median(Time, na.rm = TRUE),
    SD_Time = sd(Time, na.rm = TRUE),
    Var_Time = var(Time, na.rm = TRUE),
    
    # Accuracy Statistics
    Mean_Acc = mean(Accuracy, na.rm = TRUE),
    Median_Acc = median(Accuracy, na.rm = TRUE),
    SD_Acc = sd(Accuracy, na.rm = TRUE),
    Var_Acc = var(Accuracy, na.rm = TRUE)
  )

print("--- DESCRIPTIVE STATISTICS ---")
print(summary_stats)


# ==============================================================================
# 3. VISUALIZATIONS (RESULTS SECTION)
# ==============================================================================

# Box and Whisker Plot: Time by Mode
plot_time_box <- ggplot(df, aes(x = Mode, y = Time, fill = Mode)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Completion Time by Feedback Mode", y = "Time (s)")
print(plot_time_box)

# Box and Whisker Plot: Accuracy by Mode
plot_acc_box <- ggplot(drop_na(df, Accuracy), aes(x = Mode, y = Accuracy, fill = Mode)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Accuracy by Feedback Mode", y = "Accuracy (/25)")
print(plot_acc_box)

# QQ Plots to check for Normal Distribution in Time
plot_qq <- ggplot(df, aes(sample = Time, color = Mode)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Mode) +
  theme_minimal() +
  labs(title = "Quantile-Quantile Plot: Time Distribution Check",
       x = "Theoretical Quantiles (Standard Deviations)",
       y = "Sample Quantiles (Time in Seconds)")
print(plot_qq)

# QQ Plot to check for Normal Distribution in Accuracy
plot_qq_acc <- ggplot(drop_na(df, Accuracy), aes(sample = Accuracy, color = Mode)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Mode) +
  theme_minimal() +
  labs(title = "Quantile-Quantile Plot: Accuracy Distribution Check",
       x = "Theoretical Quantiles (Standard Deviations)",
       y = "Sample Quantiles (Accuracy Score /25)")
print(plot_qq_acc)


# ==============================================================================
# 4. INFERENTIAL STATISTICS (ANALYSIS & RESULTS SECTIONS)
# ==============================================================================

print("--- INFERENTIAL STATISTICS ---")

# ---------------------------------------------------------
# 4a. Assumption Checks (Normality and Variance)
# ---------------------------------------------------------

print(">>> ASSUMPTION CHECKS: TIME <<<")

# Shapiro-Wilk Test for Normality (Time)
shapiro_visual_time <- shapiro.test(df$Time[df$Mode == "Visual"])
shapiro_audio_time <- shapiro.test(df$Time[df$Mode == "Auditory"])
print("Shapiro-Wilk Test (Visual Time):"); print(shapiro_visual_time)
print("Shapiro-Wilk Test (Auditory Time):"); print(shapiro_audio_time)

# F-Test for Equal Variances (Time)
var_test_time <- var.test(Time ~ Mode, data = df)
print("F-Test for Equal Variances (Time):"); print(var_test_time)


print(">>> ASSUMPTION CHECKS: ACCURACY <<<")

# Shapiro-Wilk Test for Normality (Accuracy) 
# Note: We use !is.na() to ignore the empty accuracy rows
shapiro_visual_acc <- shapiro.test(df$Accuracy[df$Mode == "Visual" & !is.na(df$Accuracy)])
shapiro_audio_acc <- shapiro.test(df$Accuracy[df$Mode == "Auditory" & !is.na(df$Accuracy)])
print("Shapiro-Wilk Test (Visual Accuracy):"); print(shapiro_visual_acc)
print("Shapiro-Wilk Test (Auditory Accuracy):"); print(shapiro_audio_acc)

# F-Test for Equal Variances (Accuracy)
var_test_acc <- var.test(Accuracy ~ Mode, data = df)
print("F-Test for Equal Variances (Accuracy):"); print(var_test_acc)


# ---------------------------------------------------------
# 4b. Group Comparisons (T-Tests)
# ---------------------------------------------------------

print(">>> TWO-TAILED T-TESTS <<<")

# Two-Tailed Student's T-Test for Time
t_test_time_two <- t.test(Time ~ Mode, data = df, var.equal = TRUE)
print("Two-Tailed T-Test (Time):"); print(t_test_time_two)

# Two-Tailed Student's T-Test for Accuracy
t_test_acc_two <- t.test(Accuracy ~ Mode, data = df, var.equal = TRUE)
print("Two-Tailed T-Test (Accuracy):"); print(t_test_acc_two)


# Sensitivity Test for Accuracy
print(">>> SENSITIVITY ANALYSIS (Checking the Outliers 12 and 14) <<<")
# We identify the outliers: 12 (Visual) and 14 (Auditory).
# Let's create a temporary dataset without those specific participants
df_clean <- df %>% filter(!Accuracy %in% c(12, 14) | is.na(Accuracy))

# Re-run the Student's TWO-TAILED t-test without the outliers
# (Removed 'alternative = "greater"')
t_test_acc_clean <- t.test(Accuracy ~ Mode, data = df_clean, var.equal = TRUE)

print("Original Accuracy P-Value (With Outliers):")
# Referencing the original two-tailed test variable
print(t_test_acc_two$p.value) 

print("Cleaned Accuracy P-Value (Without Outliers):")
print(t_test_acc_clean$p.value)
# Note for paper: If both p-values are > 0.05, the conclusion DOES NOT change. Keep outliers in final report.

# ---------------------------------------------------------
# 4c. Speed-Accuracy Trade-off (Correlation & Regression)
# ---------------------------------------------------------

print(">>> SPEED-ACCURACY TRADE-OFF <<<")

# SCATTER PLOTS

# 1. COMBINED PLOT (Both modes on the same graph, distinguished by color)
plot_combined_simple <- ggplot(drop_na(df, Accuracy), aes(x = Time, y = Accuracy, color = Mode)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Combined Scatter Plot: Time vs Accuracy", 
       x = "Time (s)", 
       y = "Accuracy (/25)")

print(plot_combined_simple)


# 2. SEPARATE PLOTS (Two distinct, standalone graphs)

# Visual Mode Only
plot_visual_only <- ggplot(df %>% filter(Mode == "Visual" & !is.na(Accuracy)), aes(x = Time, y = Accuracy)) +
  geom_point(size = 3, color = "#00BFC4") + # using standard ggplot teal for Visual
  theme_minimal() +
  labs(title = "Time vs Accuracy: Visual Mode", 
       x = "Time (s)", 
       y = "Accuracy (/25)")

print(plot_visual_only)

# Auditory Mode Only
plot_auditory_only <- ggplot(df %>% filter(Mode == "Auditory" & !is.na(Accuracy)), aes(x = Time, y = Accuracy)) +
  geom_point(size = 3, color = "#F8766D") + # using standard ggplot salmon for Auditory
  theme_minimal() +
  labs(title = "Time vs Accuracy: Auditory Mode", 
       x = "Time (s)", 
       y = "Accuracy (/25)")

print(plot_auditory_only)


# 3. FACETED PLOT (Bonus: The "R way" to show separate graphs side-by-side cleanly)
plot_faceted_simple <- ggplot(drop_na(df, Accuracy), aes(x = Time, y = Accuracy, color = Mode)) +
  geom_point(size = 3) +
  facet_wrap(~ Mode) + # This physically separates the modes into two panels
  theme_minimal() +
  theme(legend.position = "none") + # Hides the legend since the titles explain it
  labs(title = "Time vs Accuracy: Side-by-Side Comparison", 
       x = "Time (s)", 
       y = "Accuracy (/25)")

print(plot_faceted_simple)


print(">>> CORRELATIONS OF COMBINED DATASET <<<")

# Overall Pearson Correlation (Null Hypothesis: Time and Accuracy are NOT correlated)
cor_overall <- cor.test(df$Time, df$Accuracy, method = "pearson", use = "complete.obs")
print("Null: No Correlation | Alternative: True correlation is not 0"); print(cor_overall)

print(">>> CORRELATIONS BY FEEDBACK MODE <<<")

# Correlation for Auditory Group Only
print("Pearson Correlation (Auditory):")
cor_auditory <- cor.test(df$Time[df$Mode == "Auditory"], df$Accuracy[df$Mode == "Auditory"])
print(cor_auditory)

# Correlation for Visual Group Only
print("Pearson Correlation (Visual):")
cor_visual <- cor.test(df$Time[df$Mode == "Visual"], df$Accuracy[df$Mode == "Visual"])
print(cor_visual)


# ==============================================================================
# 5. EXPLORATORY ANALYSIS: COVARIATE (GENDER)
# ==============================================================================
print(">>> EXPLORATORY ANALYSIS: GENDER vs TIME & ACCURACY <<<")

# Student's T-Test: Did Gender affect Completion Time?
t_test_gender_time <- t.test(Time ~ Gender, data = df, var.equal = TRUE)
print("T-Test (Time by Gender):")
print(t_test_gender_time)

# Student's T-Test: Did Gender affect Accuracy?
t_test_gender_acc <- t.test(Accuracy ~ Gender, data = df, var.equal = TRUE)
print("T-Test (Accuracy by Gender):")
print(t_test_gender_acc)


# ---------------------------------------------------------
# SENSITIVITY ANALYSIS: GENDER (Removing Outliers 12 and 14)
# ---------------------------------------------------------

# Unused...

print(">>> GENDER SENSITIVITY ANALYSIS (Without Outliers) <<<")

# Use the cleaned dataset (without the 12 and 14 accuracy scores)
df_clean_gender <- df %>% filter(!Accuracy %in% c(12, 14) | is.na(Accuracy))

# Re-run the Two-Tailed Student's T-Test for Gender
t_test_gender_clean <- t.test(Accuracy ~ Gender, data = df_clean_gender, var.equal = TRUE)

print("Original Gender P-Value (With Outliers):")
print(t_test_gender_acc$p.value) # Should be 0.3618

print("Cleaned Gender P-Value (Without Outliers):")
print(t_test_gender_clean$p.value)
