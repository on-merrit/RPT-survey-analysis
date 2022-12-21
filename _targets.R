library(targets)
library(tarchetypes)

source("R/functions.R")
source("R/helpers.R")

options(tidyverse.quiet = TRUE)

# ggchicklet is not available from CRAN
# install.packages("ggchicklet", repos = "https://cinc.rud.is")

tar_option_set(packages = c("scales", "tidyverse", "ggchicklet", "hrbrthemes",
                            "janitor"))

owner <- FALSE # set this to FALSE if you are not Thomas Klebel

if (owner) {
  # full pipeline for original data
  list(
    tar_target(
      raw_data_file,
      "data/raw/results-survey.csv",
      format = "file"
    ),
    tar_target(
      recoded_disciplines_file,
      "data/processed/disciplines_lookup.xlsx",
      format = "file"
    ),
    tar_target(
      wb_file,
      "data/external/world_bank_country_classification.xlsx",
      format = "file"
    ),
    tar_target(
      label_file,
      "data/processed/labels_expanded.csv",
      format = "file"
    ),
    tar_target(
      var_overview,
      create_var_overview(raw_data_file)
    ),
    tar_target(
      raw_data,
      read_csv(raw_data_file, col_names = FALSE, skip = 1)
    ),
    tar_target(
      expanded_labels,
      read_csv(label_file, col_types = "cc")
    ),
    tar_target(
      recoded_disciplines,
      readxl::read_excel(recoded_disciplines_file, na = "NA")
    ),
    tar_target(
      wb_countries,
      read_clean_wb_countries(wb_file)
    ),
    tar_target(
      clean_data,
      clean_raw_data(raw_data, wb_countries) %>%
        merge_disciplines(recoded_disciplines)
    ),
    tar_target(
      shareable_data,
      clean_data %>%
        select(-X12, -X16, -X89) # remove country and institution information
    ),
    tar_target(
      share_data,
      write_csv(shareable_data, "data/processed/shareable_data.csv")
    ),
    tar_render(demographics, "analysis-notebooks/01-demography.Rmd"),
    tar_render(institutional_context, "analysis-notebooks/02-institutional-context.Rmd"),
    tar_render(attitudes, "analysis-notebooks/03-attitudes-towards-promotion-criteria.Rmd"),
    tar_render(opinion_vs_practice, "analysis-notebooks/04-opinion-vs-practice.Rmd"),
    tar_render(research_vs_policy, "analysis-notebooks/05-research-vs-policies.Rmd"))
} else {
  # abbreviated target pipeline for publicly shared data
  list(
    tar_target(
      survey_file,
      "data/processed/shareable_data.csv",
      format = "file"
    ),
    tar_target(
      shareable_data,
      read_csv(survey_file)
    ),
    tar_target(
      var_overview_file,
      "data/processed/var_overview.csv",
      format = "file"
    ),
    tar_target(
      var_overview,
      read_csv(var_overview_file, col_types = "cc")
    ),
    tar_target(
      label_file,
      "data/processed/labels_expanded.csv",
      format = "file"
    ),
    tar_target(
      expanded_labels,
      read_csv(label_file, col_types = "cc")
    ),
    tar_render(institutional_context, "analysis-notebooks/02-institutional-context.Rmd"),
    tar_render(attitudes, "analysis-notebooks/03-attitudes-towards-promotion-criteria.Rmd"),
    tar_render(opinion_vs_practice, "analysis-notebooks/04-opinion-vs-practice.Rmd"),
    tar_render(research_vs_policy, "analysis-notebooks/05-research-vs-policies.Rmd")
  )
}


