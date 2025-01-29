###################
#Daten vorbereiten#
###################

# Import / update data
library(afex)
library(emmeans)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(effectsize)
pathToExcel <- "pathHere"
data <- read_excel(pathToExcel, range = "A2:BU20")

# Relevante Spalten selektieren
data <- data %>% select(codeWord, age,
                        highScoreDI, averageScoreDI, roundsDI, farTapsDI, veryFarTapsDI,
                        highScoreIT, averageScoreIT, roundsIT, farTapsIT, veryFarTapsIT,
                        highScoreIS, averageScoreIS, roundsIS, farTapsIS, veryFarTapsIS,
                        tlxDI, tlxIT, tlxIS,
                        susDI, susIT, susIS,
                        ranking)

# Spalte für Altersgruppen bis 39 und über 39 hinzufügen
data <- data %>%
  mutate(AgeGroup = ifelse(age <= 39, "Bis 39", "Über 39")) %>%
  filter(!is.na(AgeGroup))

# Daten in Altersgruppen aufteilen
data_till39 <- data %>% 
  filter(AgeGroup == "Bis 39")

data_over39 <- data %>% 
  filter(AgeGroup == "Über 39")


#############################
#Erster Überblick über Daten#
#############################

# Struktur der Daten
head(data)
summary(data)
View(data)

# Histogramm Altersverteilung
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", boundary = 0) +
  labs(title = "Altersverteilung der Teilnehmenden",
       x = "Altersgruppe (10 Jahresschritte)",
       y = "Anzahl der Personen") +
  theme_minimal()

# Bar Plot Anzahl gespielter Runden
rounds_avg <- data %>%
  summarise(
    rounds_DI_avg = mean(roundsDI, na.rm = TRUE),
    rounds_IT_avg = mean(roundsIT, na.rm = TRUE),
    rounds_IS_avg = mean(roundsIS, na.rm = TRUE)
  )

rounds_avg_plot <- rounds_avg %>%
  pivot_longer(cols = everything(),
               names_to = "Method", values_to = "AverageRounds") %>%
  mutate(Method = recode(Method, 
                         "rounds_DI_avg" = "Direct", 
                         "rounds_IT_avg" = "Indirect Tap", 
                         "rounds_IS_avg" = "Indirect Swipe"))

ggplot(rounds_avg_plot, aes(x = Method, y = AverageRounds, fill = Method)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(AverageRounds, 1)), vjust = -0.5, size = 4) +
  labs(title = "Durchschnittliche gespielte Runden nach Steuerungsmethode",
       x = "Steuerungsmethode", y = "Gespielte Runden", fill = "") +
  theme_minimal()

# Bar Plot Anzahl gespielter Runden nach Altersgruppe
rounds_avg_age <- data %>%
  group_by(AgeGroup) %>%
  summarise(
    rounds_all_avg = mean(c(roundsDI, roundsIT, roundsIS), na.rm = TRUE)
  )

ggplot(rounds_avg_age, aes(x = AgeGroup, y = rounds_all_avg, fill = AgeGroup)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(rounds_all_avg, 1)), vjust = -0.5, size = 4) +
  labs(title = "Durchschnittliche gespielte Runden nach Altersgruppe",
       x = "Altersgruppe", y = "Gespielte Runden", fill = "") +
  theme_minimal()


##############################
#High Score und Average Score#
##############################

#------------#
#Daten Prüfen#
#------------#

# Normalverteilung prüfen
# high score
combined_high_scores <- c(data$highScoreDI, data$highScoreIT, data$highScoreIS)
shapiro.test(combined_high_scores)

hist(combined_high_scores,
     main = "Highscores",
     xlab = "Scores",
     ylab = "Häufigkeit",
     col = "red",
     border = "black")
# average score
combined_average_scores <- 
  c(data$averageScoreDI, data$averageScoreIT, data$averageScoreIS)
shapiro.test(combined_average_scores)

hist(combined_average_scores,
     main = "Average Scores",
     xlab = "Scores",
     ylab = "Häufigkeit",
     col = "blue",
     border = "black")


# High Score
# Signifikanz prüfen mit Repeated Mesures ANOVA
highscore_anova_rm <- data.frame(
  code_word = data$codeWord,
  Steuerungsmethode = rep(c("DI", "IT", "IS"), each = length(data$highScoreDI)),
  high_Score = c(data$highScoreDI, data$highScoreIT, data$highScoreIS)
)
anova_result <- aov_ez(
  id = "code_word",          # Id
  dv = "high_Score",       # Abhängige Variable
  within = "Steuerungsmethode",   # Variable auf der Signifikanz zu prüfen ist 
  data = highscore_anova_rm             # Datensatz
)
print(anova_result)

# Paarweise Vergleiche
posthoc <- emmeans(anova_result, pairwise ~ Steuerungsmethode)
print(posthoc)


# Average Score 
# Signifikanz prüfen mit Repeated Mesures ANOVA
averageScore_anova_rm <- data.frame(
  code_word = data$codeWord,
  Steuerungsmethode = rep(c("DI", "IT", "IS"), each = length(data$averageScoreDI)),
  average_score = c(data$averageScoreDI, data$averageScoreIT, data$averageScoreIS)
)
anova_result <- aov_ez(
  id = "code_word",
  dv = "average_score",
  within = "Steuerungsmethode",
  data = averageScore_anova_rm
)
print(anova_result)

# Paarweise Vergleiche
posthoc <- emmeans(anova_result, pairwise ~ Steuerungsmethode)
print(posthoc)


#-------------------------------#
#Daten Modelieren und Evaluieren#
#-------------------------------#

# Bar Plots erreichte Scores je Steuerungsmethode
score_avg <- data %>%
  summarise(
    highScore_DI_avg = mean(highScoreDI, na.rm = TRUE),
    highScore_IT_avg = mean(highScoreIT, na.rm = TRUE),
    highScore_IS_avg = mean(highScoreIS, na.rm = TRUE),
    averageScore_DI_avg = mean(averageScoreDI, na.rm = TRUE),
    averageScore_IT_avg = mean(averageScoreIT, na.rm = TRUE),
    averageScore_IS_avg = mean(averageScoreIS, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Metric_Method", values_to = "AverageScore") %>%
  separate(Metric_Method, into = c("Metric", "Method"), sep = "_") %>%
  mutate(Method = recode(Method, 
                         "DI" = "Direct", 
                         "IT" = "Indirect Tap", 
                         "IS" = "Indirect Swipe"),
         Metric = recode(Metric, 
                         "highScore" = "High Score", 
                         "averageScore" = "Average Score"))

ggplot(score_avg, aes(x = Method, y = AverageScore, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(AverageScore, 1)), position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Durchschnittliche Scores nach Steuerungsmethode",
       x = "Steuerungsmethode", y = "Score", fill = "") +
  theme_minimal()

# Bar Plots Erreichte Scores je Steuerungsmethode nach Altersgruppe
score_avg_age <- data %>%
  group_by(AgeGroup) %>%
  summarise(
    averageScore_DI_avg = mean(averageScoreDI, na.rm = TRUE),
    averageScore_IT_avg = mean(averageScoreIT, na.rm = TRUE),
    averageScore_IS_avg = mean(averageScoreIS, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -AgeGroup, names_to = "Method", values_to = "AverageScore") %>%
  mutate(Method = recode(Method, 
                         "averageScore_DI_avg" = "Direct", 
                         "averageScore_IT_avg" = "Indirect Tap", 
                         "averageScore_IS_avg" = "Indirect Swipe"))

ggplot(score_avg_age, aes(x = Method, y = AverageScore, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(AverageScore, 1)), position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Durchschnittliche Average Scores nach Steuerungsmethode und Altersgruppe",
       x = "Steuerungsmethode", y = "Score", fill = "") +
  theme_minimal()

# Box Plots errreicht scores je Steuerungsmethode
score_avg_bp <- data %>%
  pivot_longer(
    cols = c(highScoreDI, highScoreIS, highScoreIT, averageScoreDI, averageScoreIS, averageScoreIT),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    ScoreType = ifelse(grepl("highScore", Metric), "High Score", "Average Score"),
    Group = case_when(
      grepl("DI", Metric) ~ "Direct",
      grepl("IT", Metric) ~ "Indirect Tap",
      grepl("IS", Metric) ~ "Indirect Swipe"
    )
  )
medians <- score_avg_bp %>%
  group_by(Group, ScoreType) %>%
  summarise(Median = median(Value, na.rm = TRUE), .groups = "drop")

ggplot(score_avg_bp, aes(x = Group, y = Value, fill = ScoreType)) +
  geom_boxplot() +
  geom_text(data = medians, aes(x = Group, y = Median, label = round(Median, 2)), 
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +
  labs(
    title = "High Scores und Average Scores Boxplots",
    x = "Steuerungsmethode",
    y = "Score",
    fill = ""
  ) +
  theme_minimal()

# Box Plots errreicht average scores je Steuerungsmethode nach Altersgruppe
score_avg_bp_age <- data %>%
  group_by(AgeGroup) %>%
  pivot_longer(
    cols = c(averageScoreDI, averageScoreIS, averageScoreIT),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Group = case_when(
      grepl("DI", Metric) ~ "Direct",
      grepl("IT", Metric) ~ "Indirect Tap",
      grepl("IS", Metric) ~ "Indirect Swipe"
    )
  )

medians <- score_avg_bp_age %>%
  group_by(Group, AgeGroup) %>%
  summarise(Median = median(Value, na.rm = TRUE), .groups = "drop")
ggplot(score_avg_bp_age, aes(x = Group, y = Value, fill = AgeGroup)) +
  geom_boxplot() +
  geom_text(data = medians, aes(x = Group, y = Median, label = round(Median, 2)), 
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +
  labs(
    title = "Average Scores nach Altersgruppen Boxplots",
    x = "Steuerungsmethode",
    y = "Score",
    fill = "Altersgruppe"
  ) +
  theme_minimal()

###########
#SUS Score#
###########

#------------#
#Daten Prüfen#
#------------#

# Normalverteilung prüfen
combined_sus_scores <- c(data$susDI, data$susIT, data$susIS)
shapiro.test(combined_sus_scores)
hist(combined_sus_scores,
     main = "SUS-Scores Histogramm",  
     xlab = "SUS-Score",               
     ylab = "Häufigkeit",            
     col = "blue",                   
     border = "black")


# Signifikanz prüfen mit Repeated Mesures ANOVA
susScore_anova_rm <- data.frame(
  code_word = data$codeWord,
  Steuerungsmethode = rep(c("DI", "IT", "IS"), each = length(data$susDI)),
  average_sus = c(data$susDI, data$susIT, data$susIS)
)
anova_result <- aov_ez(
  id = "code_word",
  dv = "average_sus",
  within = "Steuerungsmethode",
  data = susScore_anova_rm 
)
print(anova_result)

# Paarweise Vergleiche
posthoc <- emmeans(anova_result, pairwise ~ Steuerungsmethode)
print(posthoc) 

# Signifikanz prüfen mit Repeated Mesures ANOVA für SUS Score bis 39
susScore_anova_rm <- data.frame(
  code_word = data_till39$codeWord,
  Steuerungsmethode = rep(c("DI", "IT", "IS"), each = length(data_till39$susDI)),
  average_sus = c(data_till39$susDI, data_till39$susIT, data_till39$susIS)
)
anova_result <- aov_ez(
  id = "code_word",          
  dv = "average_sus",       
  within = "Steuerungsmethode",
  data = susScore_anova_rm
)
print(anova_result)

# Paarweise Vergleiche SUS Score bis 39
posthoc <- emmeans(anova_result, pairwise ~ Steuerungsmethode)
print(posthoc)

# Repeated Mesures ANOVA für SUS Score über 39
susScore_anova_rm <- data.frame(
  code_word = data_over39$codeWord,
  Steuerungsmethode = rep(c("DI", "IT", "IS"), each = length(data_over39$susDI)),
  average_sus = c(data_over39$susDI, data_over39$susIT, data_over39$susIS)
)
anova_result <- aov_ez(
  id = "code_word",         
  dv = "average_sus",
  within = "Steuerungsmethode",
  data = susScore_anova_rm
)
print(anova_result)

#-------------------------------#
#Daten Modelieren und Evaluieren#
#-------------------------------#

# Bar Plots erreichte SUS Scores je Steuerungsmethode
sus_avg <- data %>%
  summarise(
    sus_DI_avg = mean(susDI, na.rm = TRUE),
    sus_IT_avg = mean(susIT, na.rm = TRUE),
    sus_IS_avg = mean(susIS, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "AverageSUS") %>%
  mutate(Method = recode(Method, 
                         "sus_DI_avg" = "Direct", 
                         "sus_IT_avg" = "Indirect Tap", 
                         "sus_IS_avg" = "Indirect Swipe"))

ggplot(sus_avg, aes(x = Method, y = AverageSUS, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(AverageSUS, 1)), position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Durchschnittliche SUS-Scores nach Steuerungsmethode",
       x = "Steuerungsmethode", y = "SUS-Score", fill = "") +
  theme_minimal()

# Bar Plots erreichte SUS Scores je Steuerungsmethode nach Altersgruppe
sus_avg_age <- data %>%
  group_by(AgeGroup) %>%
  summarise(
    sus_DI_avg = mean(susDI, na.rm = TRUE),
    sus_IT_avg = mean(susIT, na.rm = TRUE),
    sus_IS_avg = mean(susIS, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -AgeGroup, names_to = "Method", values_to = "AverageSUS") %>%
  mutate(Method = recode(Method, 
                         "sus_DI_avg" = "Direct", 
                         "sus_IT_avg" = "Indirect Tap", 
                         "sus_IS_avg" = "Indirect Swipe"))

ggplot(sus_avg_age, aes(x = Method, y = AverageSUS, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(AverageSUS, 1)), position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Durchschnittliche SUS-Scores nach Steuerungsmethode und Altersgruppe",
       x = "Steuerungsmethode", y = "SUS-Score", fill = "Altersgruppe") +
  theme_minimal()

# Box Plots errreichte sus scores je Steuerungsmethode
sus_boxplot <- data %>%
  pivot_longer(
    cols = c(susDI, susIT, susIS),
    names_to = "Method",
    values_to = "SUSScore"
  ) %>%
  mutate(
    Method = recode(Method, 
                    "susDI" = "Direct", 
                    "susIT" = "Indirect Tap", 
                    "susIS" = "Indirect Swipe")
  )

medians <- sus_boxplot %>%
  group_by(Method) %>%
  summarise(Median = median(SUSScore, na.rm = TRUE), .groups = "drop")
ggplot(sus_boxplot, aes(x = Method, y = SUSScore, fill = Method)) +
  geom_boxplot() +
  geom_text(data = medians, aes(x = Method, y = Median, label = round(Median, 2)), 
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +
  labs(
    title = "SUS-Scores nach Steuerungsmethode Boxplot",
    x = "Steuerungsmethode",
    y = "SUS-Score",
    fill = ""
  ) +
  theme_minimal()

# Box Plots errreichte sus scores je Steuerungsmethode nach Altersgruppe
sus_boxplot_age <- data %>%
  pivot_longer(
    cols = c(susDI, susIT, susIS),
    names_to = "Method",
    values_to = "SUSScore"
  ) %>%
  mutate(
    Method = recode(Method, 
                    "susDI" = "Direct", 
                    "susIT" = "Indirect Tap", 
                    "susIS" = "Indirect Swipe")
  )

medians_age <- sus_boxplot_age %>%
  group_by(Method, AgeGroup) %>%
  summarise(Median = median(SUSScore, na.rm = TRUE), .groups = "drop")
ggplot(sus_boxplot_age, aes(x = Method, y = SUSScore, fill = AgeGroup)) +
  geom_boxplot() +
  geom_text(data = medians_age, aes(x = Method, y = Median, label = round(Median, 2)), 
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +
  labs(
    title = "SUS-Scores nach Steuerungsmethode und Altersgruppe Boxplot",
    x = "Steuerungsmethode",
    y = "SUS-Score",
    fill = "Altersgruppe"
  ) +
  theme_minimal()

################
#NASA TLX Score#
################

#------------#
#Daten Prüfen#
#------------#

# Normalverteilung prüfen
combined_tlx_scores <- c(data$tlxDI, data$tlxIT, data$tlxIS)
shapiro.test(combined_tlx_scores)
hist(combined_tlx_scores,
     main = "TLX-Scores Histogramm",  
     xlab = "TLX-Score",               
     ylab = "Häufigkeit",            
     col = "blue",                   
     border = "black")

# Signifikanz prüfen
# Repeated Mesures ANOVA für tlx Score
tlxScore_anova_rm <- data.frame(
  code_word = data$codeWord,
  Steuerungsmethode = rep(c("DI", "IT", "IS"), each = length(data$tlxDI)),
  average_tlx = c(data$tlxDI, data$tlxIT, data$tlxIS)
)
anova_result <- aov_ez(
  id = "code_word",
  dv = "average_tlx",
  within = "Steuerungsmethode",
  data = tlxScore_anova_rm
)
print(anova_result)

# Paarweise Vergleiche
posthoc <- emmeans(anova_result, pairwise ~ Steuerungsmethode)
print(posthoc)


# auch nochmal nach Altersgruppen
# Repeated Mesures ANOVA für tlx Score unter 39
tlxScore_anova_rm <- data.frame(
  code_word = data_till39$codeWord,
  Steuerungsmethode = rep(c("DI", "IT", "IS"), each = length(data_till39$tlxDI)),
  average_tlx = c(data_till39$tlxDI, data_till39$tlxIT, data_till39$tlxIS)
)
anova_result <- aov_ez(
  id = "code_word",
  dv = "average_tlx",   
  within = "Steuerungsmethode", 
  data = tlxScore_anova_rm 
)
print(anova_result)

# Paarweise Vergleiche TLX Score unter 39
posthoc <- emmeans(anova_result, pairwise ~ Steuerungsmethode)
print(posthoc)


# Repeated Mesures ANOVA für tlx Score über 39
tlxScore_anova_rm <- data.frame(
  code_word = data_over39$codeWord,
  Steuerungsmethode = rep(c("DI", "IT", "IS"), each = length(data_over39$tlxDI)),
  average_tlx = c(data_over39$tlxDI, data_over39$tlxIT, data_over39$tlxIS)
)
anova_result <- aov_ez(
  id = "code_word",
  dv = "average_tlx",
  within = "Steuerungsmethode",
  data = tlxScore_anova_rm
)
print(anova_result)

#-------------------------------#
#Daten Modelieren und Evaluieren#
#-------------------------------#

# Bar Plots Erreichte TLX Scores je Steuerungsmethode
tlx_avg <- data %>%
  summarise(
    tlx_DI_avg = mean(tlxDI, na.rm = TRUE),
    tlx_IT_avg = mean(tlxIT, na.rm = TRUE),
    tlx_IS_avg = mean(tlxIS, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "AverageTLX") %>%
  mutate(Method = recode(Method, 
                         "tlx_DI_avg" = "Direct", 
                         "tlx_IT_avg" = "Indirect Tap", 
                         "tlx_IS_avg" = "Indirect Swipe"))

ggplot(tlx_avg, aes(x = Method, y = AverageTLX, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(AverageTLX, 1)), position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Durchschnittliche NASA TLX-Scores nach Steuerungsmethode",
       x = "Steuerungsmethode", y = "NASA TLX-Score", fill = "") +
  theme_minimal()

# Bar Plots erreichte TLX Scores je Steuerungsmethode nach alter
tlx_avg_age <- data %>%
  group_by(AgeGroup) %>%
  summarise(
    tlx_DI_avg = mean(tlxDI, na.rm = TRUE),
    tlx_IT_avg = mean(tlxIT, na.rm = TRUE),
    tlx_IS_avg = mean(tlxIS, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -AgeGroup, names_to = "Method", values_to = "AverageTLX") %>%
  mutate(Method = recode(Method, 
                         "tlx_DI_avg" = "Direct", 
                         "tlx_IT_avg" = "Indirect Tap", 
                         "tlx_IS_avg" = "Indirect Swipe"))

ggplot(tlx_avg_age, aes(x = Method, y = AverageTLX, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(AverageTLX, 1)), position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Durchschnittliche NASA TLX-Scores nach Steuerungsmethode und Altersgruppe",
       x = "Steuerungsmethode", y = "NASA TLX-Score", fill = "Altersgruppe") +
  theme_minimal()

# Box Plots erreichte TLX Scores je Steuerungsmethode
tlx_boxplot <- data %>%
  pivot_longer(
    cols = c(tlxDI, tlxIT, tlxIS),
    names_to = "Method",
    values_to = "TLXScore"
  ) %>%
  mutate(
    Method = recode(Method, 
                    "tlxDI" = "Direct", 
                    "tlxIT" = "Indirect Tap", 
                    "tlxIS" = "Indirect Swipe")
  )

medians_tlx <- tlx_boxplot %>%
  group_by(Method) %>%
  summarise(Median = median(TLXScore, na.rm = TRUE), .groups = "drop")
ggplot(tlx_boxplot, aes(x = Method, y = TLXScore, fill = Method)) +
  geom_boxplot() +
  geom_text(data = medians_tlx, aes(x = Method, y = Median, label = round(Median, 2)), 
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +
  labs(
    title = "NASA TLX-Scores nach Steuerungsmethode Boxplot",
    x = "Steuerungsmethode",
    y = "NASA TLX-Score",
    fill = ""
  ) +
  theme_minimal()

# Box Plots erreichte TLX Scores je Steuerungsmethode nach alter
tlx_boxplot_age <- data %>%
  pivot_longer(
    cols = c(tlxDI, tlxIT, tlxIS),
    names_to = "Method",
    values_to = "TLXScore"
  ) %>%
  mutate(
    Method = recode(Method, 
                    "tlxDI" = "Direct", 
                    "tlxIT" = "Indirect Tap", 
                    "tlxIS" = "Indirect Swipe")
  )

medians_tlx_age <- tlx_boxplot_age %>%
  group_by(Method, AgeGroup) %>%
  summarise(Median = median(TLXScore, na.rm = TRUE), .groups = "drop")
ggplot(tlx_boxplot_age, aes(x = Method, y = TLXScore, fill = AgeGroup)) +
  geom_boxplot() +
  geom_text(data = medians_tlx_age, aes(x = Method, y = Median, label = round(Median, 2)), 
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +
  labs(
    title = "NASA TLX-Scores nach Steuerungsmethode und Altersgruppe Boxplot",
    x = "Steuerungsmethode",
    y = "NASA TLX-Score",
    fill = "Altersgruppe"
  ) +
  theme_minimal()

#######################
#Far und Very Far Taps#
#######################

#------------#
#Daten Prüfen#
#------------#

# Normalverteilung prüfen -> wie schon während Studie vermutet Daten leider nicht verwendbar
combined_taps <- c(data$farTapsDI, data$farTapsIT, data$farTapsIS)
shapiro.test(combined_taps)
hist(combined_tlx_scores,
     main = "Far Taps Histogramm",  
     xlab = "Far Taps Anzahl",               
     ylab = "Häufigkeit",            
     col = "blue",                   
     border = "black")


#-------------------------------#
#Daten Modelieren und Evaluieren#
#-------------------------------#

# Far und very Far Taps, Bar Plot -> wie schon während Studie vermutet Daten leider nicht verwendbar
taps_avg <- data %>%
  summarise(
    mean_farTaps_DI = mean(farTapsDI, na.rm = TRUE),
    mean_farTaps_IS = mean(farTapsIS, na.rm = TRUE),
    mean_farTaps_IT = mean(farTapsIT, na.rm = TRUE),
    mean_veryFarTaps_DI = mean(veryFarTapsDI, na.rm = TRUE),
    mean_veryFarTaps_IS = mean(veryFarTapsIS, na.rm = TRUE),
    mean_veryFarTaps_IT = mean(veryFarTapsIT, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    ScoreType = ifelse(grepl("farTaps", Metric), "Far Taps", "Very Far Taps"),
    Group = case_when(
      grepl("DI", Metric) ~ "Direct",
      grepl("IT", Metric) ~ "Indirect Tap",
      grepl("IS", Metric) ~ "Indirect Swipe"
    )
  )

ggplot(taps_avg, aes(x = Group, y = Value, fill = ScoreType)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 0)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Durchschnittswerte von Far und Very Far Taps",
    x = "Steuerungsmethode",
    y = "Anzahl Taps",
    fill = ""
  ) +
  theme_minimal()

#######################
#Teilnehmendenranking#
#######################

#------------#
#Daten Prüfen#
#------------#

# /, da Normalverteilung und Signifiaknz hier nicht relevant

#-------------------------------#
#Daten Modelieren und Evaluieren#
#-------------------------------#

# je Wahl auf PLatz 1 +1 Punkt, auf PLatz 3 -1 Punkt -> aggregiert über alle Teilnehmenden
participants_ranking <- data %>%
  mutate(
    Platz1 = substr(ranking, 1, 2), # Zeichen 1,2 als Platz 1 extrahieren
    Platz3 = substr(ranking, 5, 6)  # Zeichen 5,6 als Platz 3 extrahieren
  ) %>%
  summarise(
    Punkte_DI = sum(Platz1 == "DI", na.rm = TRUE) * 1 + sum(Platz3 == "DI", na.rm = TRUE) * (-1),
    Punkte_IT = sum(Platz1 == "IT", na.rm = TRUE) * 1 + sum(Platz3 == "IT", na.rm = TRUE) * (-1),
    Punkte_IS = sum(Platz1 == "IS", na.rm = TRUE) * 1 + sum(Platz3 == "IS", na.rm = TRUE) * (-1)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Group",
    values_to = "Punkte"
  ) %>%
  mutate(
    Group = case_when(
      Group == "Punkte_DI" ~ "Direct",
      Group == "Punkte_IT" ~ "Indirect Tap",
      Group == "Punkte_IS" ~ "Indirect Swipe"
    )
  )
# Plot
ggplot(participants_ranking, aes(x = Group, y = Punkte, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color="black") +
  geom_text(aes(label = Punkte), vjust = -0.3, color = "black") +
  labs(
    title = "Aggregierte Rankingpunkte",
    x = "Steuerungsmethode",
    y = "Punkte",
    fill = "Steuerungsmethode"
  ) +
  theme_minimal()

# nach altersgruppe
participants_ranking_age <- data %>%
  group_by(AgeGroup) %>%
  mutate(
    AgeGroup = ifelse(age <= 39, "Bis 39", "Über 39"),
    Platz1 = substr(ranking, 1, 2),
    Platz3 = substr(ranking, 5, 6)
  ) %>%
  group_by(AgeGroup) %>%
  summarise(
    Punkte_DI = sum(Platz1 == "DI", na.rm = TRUE) * 1 + sum(Platz3 == "DI", na.rm = TRUE) * (-1),
    Punkte_IT = sum(Platz1 == "IT", na.rm = TRUE) * 1 + sum(Platz3 == "IT", na.rm = TRUE) * (-1),
    Punkte_IS = sum(Platz1 == "IS", na.rm = TRUE) * 1 + sum(Platz3 == "IS", na.rm = TRUE) * (-1)
  ) %>%
  pivot_longer(
    cols = -AgeGroup,
    names_to = "Group",
    values_to = "Punkte"
  ) %>%
  mutate(
    Group = case_when(
      Group == "Punkte_DI" ~ "Direct",
      Group == "Punkte_IT" ~ "Indirect Tap",
      Group == "Punkte_IS" ~ "Indirect Swipe"
    )
  )
# Plot
ggplot(participants_ranking_age, aes(x = Group, y = Punkte, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = Punkte), vjust = -0.3, color = "black", position = position_dodge(width = 0.8)) +
  labs(
    title = "Aggregierte Rankingpunkte nach Altersgruppen",
    x = "Steuerungsmethode",
    y = "Punkte",
    fill = "Altersgruppe"
  ) +
  theme_minimal()