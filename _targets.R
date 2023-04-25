# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
# Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "broom", "rstatix", "ggpubr", "patchwork", "correlation",
               "jtools", "tarchetypes", "quarto"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(file, "../Data.csv", format = "file"),
  tar_target(data, read_csv(file) %>%
               mutate(subj.number = 1:19,
                 difference_score = `Score 1` - `Score 2`,
                      difference_ceti = `CETI 1` - `CETI 2`,
                      Gender_numeric = ifelse(.$Gender == "M", 0, 1))),

  tar_target(nooutlierdata, outlier_excluded_data(data)),

  tar_target(description, descriptives(data)),

  tar_target(assumption, assumptions(data)),
  tar_target(qqplot, qqplots_paired_ttest(data)),
  tar_target(assumption_noout_CETI, assumptions_nooutCETI(nooutlierdata)),
  tar_target(qqplot_noout_CETI, qqplots_paired_ttest_noout_CETI(nooutlierdata)),

  tar_target(correlations, correlations_allcolumns(data)),
  tar_target(correlations_figure, correlations_plot(data)),
  tar_target(violin_plot, violin_plots(data)),

  tar_target(t_test_score, para_test(data)),
  tar_target(CETI_nooout_t_test_score, para_test_ceti_noout(nooutlierdata)),
  tar_target(wilcox_test_ceti, Non_para_test(data)),

  tar_target(regression_analysis, regression(data)),

  tar_quarto(report, "logo-project.qmd", execute_params = list(your_param = data))
)


