# R/functions.R

descriptives <- function(data){
  data %>% summarise(percentage_women = mean(ifelse(.$Gender == "M", 0, 1)),
                    mean_age = mean(.$Alter, na.rm = T))
}

# Outlier exclusion
outlier_excluded_data <- function(data){
  data[!(1:19 %in% identify_outliers(difference_ceti, data = data)$subj.number),]
}

# Assumptions
assumptions <- function(data){
  tmp_new <- data %>% mutate(difference_score = `Score 1` - `Score 2`,
                             difference_ceti = `CETI 1` - `CETI 2`)

  tmp_new %>% ggplot(aes(difference_score)) + geom_density()
  identify_outliers(difference_score, data = tmp_new) %>% print()
  tmp_new %>% ggplot(aes(difference_ceti)) + geom_density()
  identify_outliers(difference_ceti, data = tmp_new) %>% print()
}

# Plot qqplots
qqplots_paired_ttest <- function(data){
  # QQ plot for the difference
  ggqqplot(data, "difference_score") + ggtitle("QQ Plot: Scores") + ggqqplot(data, "difference_ceti") + ggtitle("QQ Plot: CETI")
}


# no outlier data CETI assumptions
assumptions_nooutCETI <- function(data){
  data %>% ggplot(aes(difference_ceti)) + geom_density()
}

# no outlier data CETI qqplot
qqplots_paired_ttest_noout_CETI <- function(data){
  # QQ plot for the difference
  ggqqplot(data, "difference_ceti") + ggtitle("QQ Plot: CETI")
}

# Correlations
correlations_allcolumns <- function(data){
  correlation(data)
}

# Correlations Plot
correlations_plot <- function(data){
  plot(correlation(data))
}

# Create Violin Plots
violin_plots <- function(data){
  age_violin <- data %>% ggplot(aes(y = Alter, x = 1)) +
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill="white")+
    labs(title="",x="", y = "Age") +
    theme_apa() +
    theme(text = element_text(family="Myriad Pro", size=25), axis.title.y = element_text(family="Myriad Pro", size=25)) +
    scale_x_discrete(breaks=c("HUM"),
                     labels=c("Human"))


  score_violin <- data %>% ggplot(aes(y = difference_score, x = 1)) +
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill="white")+
    labs(title="",x="", y = "Score Difference") +
    theme_apa() +
    theme(text = element_text(family="Myriad Pro", size=25), axis.title.y = element_text(family="Myriad Pro", size=25)) +
    scale_x_discrete(breaks=c("HUM"),
                     labels=c("Human"))

  ceti_violin <- data %>% ggplot(aes(y = difference_ceti, x = 1)) +
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill="white")+
    labs(title="",x="", y = "CETI Difference") +
    theme_apa() +
    theme(text = element_text(family="Myriad Pro", size=25), axis.title.y = element_text(family="Myriad Pro", size=25)) +
    scale_x_discrete(breaks=c("HUM"),
                     labels=c("Human"))

  age_violin + score_violin + ceti_violin
}




Non_para_test <- function(data){
  data %>% {wilcox.test(.$`CETI 1`, .$`CETI 2`, paired = T)}
}

para_test <- function(data){
  data %>% {t.test(.$`Score 1`, .$`Score 2`, paired = T)}
}

para_test_ceti_noout <- function(data){
  data %>% {t.test(.$`CETI 1`, .$`CETI 2`, paired = T)}
}


regression <- function(data){
  Gender <- lm(`Score 2`-`Score 1` ~ Gender_numeric, data = data) %>% summary() %>% tidy()
  Alter <- lm(`Score 2`-`Score 1` ~ Alter, data = data) %>% summary() %>% tidy()

  bind_rows(Gender, Alter)
}




