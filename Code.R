---
title: "Speech Therapy Project"
author: "Sarah Harders"
format: html
editor: visual
---

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)
library(readxl)
library(rstatix)
library(jtools)
library(lavaan)
library(psych)
library(lmerTest)
library(car)
library(lsr)


#### Uploading Data ####

data <- read_excel("C:/Users/harde/Desktop/data-logo-project/Data.xlsx")


# Creating Sub-scores

data$Sprachverständnispre <- ((data$Handlungsanweisung1/4) + (data$Leseverständnis1/18) + (data$`Farb-Figur1`/20) + (data$`Auditives Sprachverständnis1`/18))/4
data$Sprachverständnispost <- ((data$Handlungsanweisung/4) + (data$Leseverständnis/18) + (data$`Farb-Figur`/20) + (data$`Auditives Sprachverständnis`/18))/4

data$Benennenpre <- ((data$B1/10) + (data$Supermarkt1/10) + (data$Benennen0/18))/3
data$Benennenpost <- ((data$B/10) + (data$Supermarkt/10) + (data$Benennen/18))/3

data$Nachsprechenpre <- (data$Nachsprechen1/18)
data$Nachsprechenpost <- (data$Nachsprechen2/18)

data$Schriftsprachepre <- ((data$Lesen1/18) + (data$Schreiben1/18))/2
data$Schriftsprachepost <- ((data$Lesen/18) + (data$Schreiben/18))/2

# Internal Validity

psych::alpha(subset(data, select = c(Handlungsanweisung1, Leseverständnis1, `Farb-Figur1`,
                                     `Auditives Sprachverständnis1`)))
psych::alpha(subset(data, select = c(B1, Supermarkt1, Benennen0)))
psych::alpha(subset(data, select = c(Lesen1, Schreiben1)))

# Calculate change scores

data$Benennen_change <- data$Benennenpost - data$Benennenpre
data$Nachsprechen_change <- data$Nachsprechenpost - data$Nachsprechenpre
data$Schriftsprache_change <- data$Schriftsprachepost - data$Schriftsprachepre
data$Sprachverständnis_change <- data$Sprachverständnispost - data$Sprachverständnispre
data$difference_ceti <- data$`CETI 2` - data$`CETI 1`


# Create participant variable

data$subj.number <- 1:nrow(data)

# Exclude Outliers

data$difference_ceti_noout <- data$difference_ceti # assign data with outliers to new column
data$difference_ceti_noout[identify_outliers(difference_ceti, data = data)$subj.number] <- NA # identify and exclud outlier

data$CETI1_noout <- data$`CETI 1`
data$CETI1_noout[identify_outliers(difference_ceti, data = data)$subj.number] <- NA # identify and exclud outlier

data$CETI2_noout <- data$`CETI 2`
data$CETI2_noout[identify_outliers(difference_ceti, data = data)$subj.number] <- NA # identify and exclud outlier

data$Sprachverständnis_change_noout <- data[!(1:19 %in% identify_outliers(Sprachverständnis_change, data = data)$subj.number),]$Sprachverständnis_change # no outliers detected

data$Benennen_change_noout <-  data$Benennen_change
data$Benennen_change_noout[identify_outliers(Benennen_change, data = data)$subj.number] <- NA

data$Benennen1_noout <-  data$Benennenpre
data$Benennen1_noout[identify_outliers(Benennen_change, data = data)$subj.number] <- NA

data$Benennen2_noout <-  data$Benennenpost
data$Benennen2_noout[identify_outliers(Benennen_change, data = data)$subj.number] <- NA

data$Schriftsprache_change_noout <- data[!(1:19 %in% identify_outliers(Schriftsprache_change, data = data)$subj.number),]$Schriftsprache_change # no outliers

data$Nachsprechen_change_noout <- data$Nachsprechen_change
data$Nachsprechen_change_noout[identify_outliers(Nachsprechen_change, data = data)$subj.number] <- NA

data$Nachsprechen1_noout <- data$Nachsprechenpre
data$Nachsprechen1_noout[identify_outliers(Nachsprechen_change, data = data)$subj.number] <- NA

data$Nachsprechen2_noout <- data$Nachsprechenpost
data$Nachsprechen2_noout[identify_outliers(Nachsprechen_change, data = data)$subj.number] <- NA

########################################################
#First task checking Assumptions
########################################################

# Histogram

hist(data$Benennen_change_noout, main = "Histogram of Bennenen Change Score", xlab = "Change Score")
hist(data$Nachsprechen_change_noout, main = "Histogram of Nachsprechen Change Score", xlab = "Change Score")
hist(data$Schriftsprache_change_noout, main = "Histogram of Schriftsprache Change Score", xlab = "Change Score")
hist(data$Sprachverständnis_change_noout, main = "Histogram of Sprachverständnis Change Score", xlab = "Change Score")
hist(data$difference_ceti_noout, main = "Histogram of CETI Change Score", xlab = "Change Score")


# Shapiro-Wilk test for normality

shapiro.test(data$Benennen_change_noout)
shapiro.test(data$Nachsprechen_change_noout)
shapiro.test(data$Schriftsprache_change_noout)
shapiro.test(data$Sprachverständnis_change_noout)
shapiro.test(data$difference_ceti_noout)

# Levene's test for equality of variances

levene_benennen <- data.frame(benennen = c(data$Benennen1_noout, data$Benennen2_noout), group_b = factor(c(rep(1, 19), rep(2, 19))))
leveneTest(y = levene_benennen$benennen, group = levene_benennen$group_b)

levene_schriftsprache <- data.frame(schriftsprache = c(data$Schriftsprachepre, data$Schriftsprachepost), group_b = factor(c(rep(1, 19), rep(2, 19))))
leveneTest(y = levene_schriftsprache$schriftsprache, group = levene_schriftsprache$group_b)

levene_Nachsprechen<- data.frame(Nachsprechen = c(data$Nachsprechen1_noout, data$Nachsprechen2_noout), group_b = factor(c(rep(1, 19), rep(2, 19))))
leveneTest(y = levene_Nachsprechen$Nachsprechen, group = levene_Nachsprechen$group_b)

levene_Sprachverständnis <- data.frame(Sprachverständnis = c(data$Sprachverständnispre, data$Sprachverständnispost), group_b = factor(c(rep(1, 19), rep(2, 19))))
leveneTest(y = levene_Sprachverständnis$Sprachverständnis, group = levene_Sprachverständnis$group_b)

levene_CETI <- data.frame(CETI = c(data$`CETI 1`, data$`CETI 2`), group_b = factor(c(rep(1, 19), rep(2, 19))))
leveneTest(y = levene_CETI$CETI, group = levene_CETI$group_b)

# Linearity
plot(data$Benennen2_noout, data$Benennen1_noout)
plot(data$Nachsprechen2_noout, data$Nachsprechen1_noout,)
plot(data$Schriftsprachepost, data$Schriftsprachepre)
plot(data$Sprachverständnispost, data$Sprachverständnispre)
plot(data$CETI2_noout, data$CETI1_noout)

#######################################################
# First Analysis Improvement
#######################################################

mean((data$Benennen1_noout), na.rm = T)
mean((data$Sprachverständnispre), na.rm = T)
mean((data$Nachsprechen1_noout), na.rm = T)
mean((data$Schriftsprachepre), na.rm = T)

mean_per_participant_Benennen1 <- aggregate(Benennen_change_noout ~ subj.number, data = data, FUN = mean, na.rm = TRUE)
mean_per_participant_Sprachverständnispre <- aggregate(Sprachverständnis_change_noout ~ subj.number, data = data, FUN = mean, na.rm = TRUE)
mean_per_participant_Nachsprechen1 <- aggregate(Nachsprechen_change_noout ~ subj.number, data = data, FUN = mean, na.rm = TRUE)
mean_per_participant_Schriftsprachepre <- aggregate(Schriftsprache_change_noout ~ subj.number, data = data, FUN = mean, na.rm = TRUE)
combined_matrix <- cbind(mean_per_participant_Benennen1, mean_per_participant_Sprachverständnispre, mean_per_participant_Nachsprechen1, mean_per_participant_Schriftsprachepre)

sd((data$Benennen1_noout), na.rm = T)
sd((data$Sprachverständnispre), na.rm = T)
sd((data$Nachsprechen1_noout), na.rm = T)
sd((data$Schriftsprachepre), na.rm = T)

t.test(data$Benennen2_noout, data$Benennen1_noout, paired = TRUE)
t.test(data$Nachsprechen2_noout, data$Nachsprechen1_noout, paired = TRUE)
t.test(data$Schriftsprachepost, data$Schriftsprachepre, paired = TRUE)
t.test(data$Sprachverständnispost, data$Sprachverständnispre, paired = TRUE)
t.test(data$CETI2_noout, data$CETI1_noout, paired = TRUE)

# Calculate Cohen's d effect size

cohensD(data$Benennen2_noout, data$Benennen1_noout)
cohensD(data$Nachsprechenpost, data$Nachsprechen1_noout)
cohensD(data$Schriftsprachepost, data$Schriftsprachepre)
cohensD(data$Sprachverständnispost, data$Sprachverständnispre)
cohensD(data$CETI2_noout, data$CETI1_noout)

################# make Graph #################

######## Change-Score

data_long <- data %>%
  mutate(Naming = (Benennen_change_noout*100), Repetition = (Nachsprechen_change_noout*100),
         'Written Language' = (Schriftsprache_change*100), 'Auditory Comprehension' = (Sprachverständnis_change*100) ) %>%
  select(Naming, Repetition, 'Written Language', 'Auditory Comprehension', subj.number) %>%
  mutate(subj.number = subj.number) %>%
  pivot_longer(cols = -subj.number, names_to = "variable", values_to = "prescore")

# Plot with Change-score per participant

svg("plot1.svg")
ggplot(data_long, aes(x = variable, y = prescore, group = subj.number)) +
  geom_line(alpha = 0.5, aes(color = as.factor(subj.number))) +
  geom_point(aes(color = as.factor(subj.number))) +
  labs(x = NULL, y = "Change-score (%)") +
  theme_minimal()+
  theme_apa() + ylim(-20, 45) + theme(legend.position="none")
dev.off()
tmp <- data_long %>% group_by(variable) %>% summarise(prescore = mean(prescore, na.rm =T ))


svg("plot2.svg")
ggplot(tmp, aes(x = variable, y = prescore)) +
  geom_line(alpha = 0.5, group = 1, size = 2.5) +
  geom_point() +
  labs(x = NULL, y = "Change-score (%)") +
  theme_minimal()+
  theme_apa()  + ylim(-20, 45)
dev.off()


######## Pre-Score

data_long <- data %>%
  mutate(Naming = (Benennen1_noout*100), Repetition = (Nachsprechen1_noout*100),'Written Language' = (Schriftsprachepre*100), 'Auditory Comprehension' = (Sprachverständnispre*100)) %>%
  select(Naming, Repetition, 'Written Language', 'Auditory Comprehension', subj.number) %>%
  mutate(subj.number = subj.number) %>%
  pivot_longer(cols = -subj.number, names_to = "variable", values_to = "Prescore")


# Plot with pre-scores per participant
svg("plot3.svg")
ggplot(data_long, aes(x = variable, y = Prescore, group = subj.number, color = as.factor(subj.number))) +
  geom_line(alpha = 0.75) +
  geom_point() +
  labs(x = NULL, y = "Pre-score (%)") +
  theme_minimal() +
  theme_apa() +
  guides(color = guide_legend(title = "Legend")) + theme(legend.position = "none") + ylim(0,100)
dev.off()
tmp <- data_long %>% group_by(variable) %>% summarise(prescore = mean(Prescore, na.rm =T ))

svg("plot4.svg")
ggplot(tmp, aes(x = variable, y = prescore)) +
  geom_line(alpha = 0.5, group = 1, size = 2.5) +
  geom_point() +
  labs(x = NULL, y = "Change-score (%)") +
  theme_minimal()+
  theme_apa()  + ylim(0, 100)
dev.off()


######## CETI

# Create a data frame with CETI 1 and CETI 2 scores (excluding missing values)

 df <- data.frame(
    CETI = c(rep("CETI 1", length(data$CETI1_noout)), rep("CETI 2", length(data$CETI2_noout))),
    Score = c(data$CETI1_noout, data$CETI2_noout),
    Observation = rep(1:length(data$CETI1_noout), times = 2)
  )

# Create the plot

  ggplot(df, aes(x = CETI, y = Score, group = Observation, color = as.factor(Observation))) +
    geom_line() +
    geom_point() +
    labs(x = "CETI Group", y = "Score") +
    scale_color_manual(values = rainbow(length(unique(df$Observation))), labels = unique(df$Observation)) +
    theme_minimal()+
    theme_apa()

######################################################################
# Second Analysis
######################################################################

######## Creating an Overall Mean for Correlational Analysis without written language

pre_mean = ((data$Benennen1_noout) + (data$Sprachverständnispre) + (data$Nachsprechen1_noout)+ (data$Schriftsprachepre)/4)
post_mean = ((data$Benennen2_noout) + (data$Sprachverständnispost) + (data$Nachsprechen2_noout) +(data$Schriftsprachepost)/4)
ACL_change <- post_mean - pre_mean

t.test(post_mean, pre_mean, paired = TRUE)
plot(ACL_change)

# Correlation

cor.test(ACL_change, data$difference_ceti_noout, na.rm = T)

# Plot Correlation

plot(data$difference_ceti_noout, ACL_change,
     main = paste("Correlation:", correlation, "\n",
                  "p-value:", p_value),
     xlab = "CETI",
     ylab = "ACL")

######## Correlation with Individual Sub-Scores of the ACL

# Create a matrix of the variables you want to plot

variable_matrix <- cbind(data$Nachsprechen_change_noout, data$Benennen_change_noout,
                         data$Sprachverständnis_change_noout,
                         data$difference_ceti_noout)

# Remove rows with missing values in CETI_change

complete_cases <- complete.cases(data$difference_ceti_noout)
variable_matrix <- variable_matrix[complete_cases, ]

# Compute the correlation matrix and p-values

cor_matrix <- cor(variable_matrix)
p_values <- matrix(NA, nrow = ncol(variable_matrix), ncol = ncol(variable_matrix))
for (i in 1:(ncol(variable_matrix)-1)) {
  for (j in (i+1):ncol(variable_matrix)) {
    cor_result <- cor.test(variable_matrix[, i], variable_matrix[, j])
    p_values[i, j] <- cor_result$p.value
    p_values[j, i] <- cor_result$p.value
  }
}
# Create a blank plot with four subplots

par(mfrow = c(2, 2))

# Define the variable names

variable_names <- c("Nachsprechen_change", "Benennen_change",
                    "Sprachverständnis_change", "CETI_change")

# Define the specific variable pairs

variable_pairs <- list(
  c(1, 4),  # Nachsprechen_change with CETI_change
  c(2, 4),  # Benennen_change with CETI_change
  c(3, 4)  # Sprachverständnis_change with CETI_change
)

# Loop through the variable pairs and plot each pair with correlation

for (pair in variable_pairs) {
  i <- pair[1]
  j <- pair[2]
}
  # Compute correlation and p-value

  cor_result <- cor.test(variable_matrix[, i], variable_matrix[, j], na.rm = T)
  correlation <- round(cor_matrix[i, j], 2)
  p_value <- round(cor_result$p.value, 3)

  # Plot the variables with correlation and p-value

  plot(variable_matrix[, i], variable_matrix[, j],
       main = paste("Correlation:", correlation, "\n",
                    "p-value:", p_value),
       xlab = variable_names[i],
       ylab = variable_names[j])

############################################################
#Third Analysis
############################################################


# testing homoskedasticity
cor((data$Alter, data$Yearsincestroke), na.rm = T)
cor(data$Alter, data$Benennen1_noout)
cor(data$Alter, data$Sprachverständnispre)
cor(data$Alter, data$Schriftsprachepre)
cor(data$Alter, data$Nachsprechen1_noout)
cor(data$Yearsincestroke, data$Benennen1_noout)
cor(data$Yearsincestroke, data$Sprachverständnispre)
cor(data$Yearsincestroke, data$Schriftsprachepre)
cor(data$Yearsincestroke, data$Nachsprechen1_noout)

# Creating models
model1<- lm(Benennen_change_noout ~ Alter + Yearsincestroke + Benennen1_noout, data = data)
model2<- lm(Sprachverständnis_change_noout ~ Alter + Yearsincestroke + Sprachverständnispre, data = data)
model3<- lm(Schriftsprache_change_noout ~ Alter + Yearsincestroke + Schriftsprachepre, data = data)
model4<- lm(Nachsprechen_change_noout ~ Alter + Yearsincestroke + Nachsprechen1_noout, data = data)

# Visual inspection of Q-Q plot for normality of residuals
densityPlot(model1$residuals)
densityPlot(model2$residuals)
densityPlot(model3$residuals)
densityPlot(model4$residuals)

# Visual inspection of Scatterplot for linearity
plot(model1)
plot(model2)
plot(model3)
plot(model4)

# Results of Regression
model1<- lm(Benennen_change_noout ~ Alter + Yearsincestroke + Benennen1_noout, data = data)%>% summary()
model2<- lm(Sprachverständnis_change_noout ~ Alter + Yearsincestroke + Sprachverständnispre, data = data) %>% summary()
model3<- lm(Schriftsprache_change_noout ~ Alter + Yearsincestroke + Schriftsprachepre, data = data) %>% summary()
model4<- lm(Nachsprechen_change_noout ~ Alter + Yearsincestroke + Nachsprechen1_noout, data = data) %>% summary()


############################################################
# Creating the Individual Plot with three time points
############################################################

# Create the variables

Bennenenx <- c(0.03703704, 0.111111, 0.2444333)
Schriftsprachex <- c(0.5833333, 0.805556, 0.6666665)
Nachsprechenx <- c(0.2888889, 0.333333, 0.3888889)
Verstehenx <- c(0.8569445, 0.95, 0.743)

# Create the time variable

time <- c("Pre-score", "Post-score", "Follow-up")

# Create the data frame

df <- data.frame(time, Bennenenx, Schriftsprachex, Nachsprechenx, Verstehenx)

data_long <- df %>%
  mutate(Naming = (Bennenenx*100), Repetition = (Nachsprechenx*100),'Written Language' = (Schriftsprachex*100), 'Auditory Comprehension' = (Verstehenx*100)) %>%
  select(Naming, Repetition, 'Written Language', 'Auditory Comprehension', time) %>%
  mutate(time = time) %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "Prescore")

# Create a vector of color codes

color_codes <- c("blue", "red", "black")

# Modify the ggplot code

ggplot(data_long, aes(x = variable, y = Prescore, group = time, color = as.factor(time))) +
  geom_line(alpha = 0.75) +
  geom_point() +
  labs(x = NULL, y = "Score (%)") +
  theme_minimal() +
  theme_apa() +
  guides(color = guide_legend(title = "Legend")) +
  scale_color_manual(values = color_codes)

