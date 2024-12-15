library(tidyverse)
library(patchwork)




diabetes <- read.csv("diabetes_data.csv")

##initial data processing from part 1

diabetes <- diabetes %>%
  mutate(heart_disease = as.factor(heart_disease)) %>%
  mutate(hypertension = as.factor(hypertension)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(diabetes = as.factor(diabetes))

levels(diabetes$heart_disease) = c("no", "yes")
levels(diabetes$diabetes) = c("no", "yes")
levels(diabetes$gender) = c("female", "male")
levels(diabetes$hypertension) = c("no","yes")
levels(diabetes$diabetes) = c("no", "yes")

diabetes <- diabetes %>%
  mutate(age = case_when(
    age < 17.00 ~ "Child",
    age >= 17.00 & age < 31.00 ~ "Young Adult",
    age >= 31.00 & age < 55.00 ~ "Middle Age",
    age >= 55.00 ~ "Senior"))
##A1C and diabetes

diabetes2 <- diabetes %>%
  group_by(diabetes) %>%
  summarise(avgA1C = mean(HbA1c_level))
A1c_graph <- ggplot(diabetes2, aes(x = diabetes, y = avgA1C, fill = diabetes)) +
  geom_col(aes(x = diabetes, y = avgA1C)) +
  labs(title = "Average Hemiglobin A1C Level of Healthy Individuals vs. non-Diabetic Individuals", subtitle = "Those with a confirmed diabetes diagnosis had an average A1C value of 6.92, in comparison to 5.93 in healthy patients", caption = "Did You Know? Hemoglobin A1C is a measure of average blood sugar level over a timespan of 2-3 months. It is typically the first method used to diagnose diabetes.", x = "Non-Diabetic vs. Diabetic", y = "Average Hemoglobin A1C") +
  scale_fill_manual(values = c("no" = "lightpink", "yes" = "red")) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 11, hjust = 0.5, face = "bold" ),
    plot.caption = element_text(size = 7, hjust = 0.1),
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 4),
    axis.text.y = element_text(size = 4),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6))

##blood glucose level
glucose_graph <- ggplot(diabetes, aes(x = diabetes, y = blood_glucose_level, fill = diabetes)) +
  geom_boxplot(width = 0.3) +
  labs(title = "Distribution of Blood Glucose Level in Diabetic
       vs Non-Diabetic Individuals", x = "Non-Diabetic vs Diabetic", y = "Blood Glucose Level",
       subtitle = "Blood Glucose Appears Markedly Higher in Individuals with Diabetes", caption = "Blood Glucose is a measure of the whole-blood glucose levels at any time. It is also used to determine Hemoglobin A1C. ") +
  scale_fill_manual(values = c("no" = "pink2", "yes" = "grey")) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 11, hjust = 0.5, face = "bold" ),
          plot.caption = element_text(size = 7),
          plot.subtitle = element_text(size = 9),
          axis.text.x = element_text(size = 4),
          axis.text.y = element_text(size = 4),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6))

##incidence of diabetes among age groups


AgeIncidence_graph <- ggplot(diabetes, aes(x = age, y = blood_glucose_level, fill = diabetes)) +
  geom_col(aes(x = age, y = blood_glucose_level)) +
  scale_fill_manual(values = c("no" = "pink", "yes" = "turquoise4")) +
  labs(title = "Cases of Diabetes and Blood Sugar Level by Age Group", subtitle = "The risk of diabetes is much greater in individuals over the age of 55, and lowest in those under 17.", caption = "There are many factors that may put seniors at a greater risk for elevated blood sugar and diabetes; Lifestyle changes and other health conditions may be contributors.", x = "Age Cohort", y = "Blood Glucose") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 11, face = "bold" ),
        plot.caption = element_text(size = 7),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        axis.text.x = element_text(size = 4),
        axis.text.y = element_text(size = 4),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))


dia_dash <- (AgeIncidence_graph + glucose_graph) / A1c_graph +
  plot_annotation(title="A Snapshot of Diabetes Risk Factors: Know Your Health",
    caption="Source: Diabetes Dataset (2024)") +
  plot_layout(heights=c(2,1))

