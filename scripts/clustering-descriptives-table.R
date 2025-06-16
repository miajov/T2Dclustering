library(dplyr)

##read df
clusters <- read.csv("/Users/miajovanova/t2d/cluster_descriptives.csv")

##1. plot categorical variables 

# Helper function: % (N) format, now for gender == 1
percent_n <- function(x) {
  pct <- round(mean(x == 1, na.rm = TRUE) * 100, 2)
  n <- sum(x == 1, na.rm = TRUE)
  paste0(pct, "% (", n, ")")
}

# Helper function for gender == 2 (e.g. Male)
percent_gender_2 <- function(x) {
  pct <- round(mean(x == 2, na.rm = TRUE) * 100, 2)
  n <- sum(x == 2, na.rm = TRUE)
  paste0(pct, "% (", n, ")")
}

# Overall summary with sample size
overall_comp <- clusters %>%
  dplyr::summarise(
    Complications = percent_n(Complications),
    Gender = percent_gender_2(Gender..Female.1..Male.2.),
    N = n()
  ) %>%
  mutate(Cluster = "Overall") %>%
  select(Cluster, Complications, Gender, N)

# cluster_all_2clusters summary with sample size
comp_all_2clusters <- clusters %>%
  group_by(cluster_all_2clusters) %>%
  dplyr::summarise(
    Complications = percent_n(Complications),
    Gender = percent_gender_2(Gender..Female.1..Male.2.),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(Cluster = paste0("cluster_all_2clusters_", cluster_all_2clusters)) %>%
  select(Cluster, Complications, Gender, N)

# cluster_only_cgm summary with sample size
comp_only_cgm <- clusters %>%
  group_by(cluster_only_cgm) %>%
  dplyr::summarise(
    Complications = percent_n(Complications),
    Gender = percent_gender_2(Gender..Female.1..Male.2.),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(Cluster = paste0("cluster_only_cgm_", cluster_only_cgm)) %>%
  select(Cluster, Complications, Gender, N)

# cluster_nutri summary with sample size
comp_nutri <- clusters %>%
  group_by(cluster_nutri) %>%
  dplyr::summarise(
    Complications = percent_n(Complications),
    Gender = percent_gender_2(Gender..Female.1..Male.2.),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(Cluster = paste0("cluster_nutri_", cluster_nutri)) %>%
  select(Cluster, Complications, Gender, N)

# Combine all
complications_summary <- bind_rows(
  overall_comp,
  comp_all_2clusters,
  comp_only_cgm,
  comp_nutri
)

# Print result
print(complications_summary)

## 2. plot numeric variables 

vars_to_summarize <- c(
  "Age..years.",
  "BMI",
  "Duration.of.diabetes..years.",
  "sum_total_g",
  "ratio_staples_total_24",
  "ratio_dairy_total_24",
  "ratio_fruits_total_24",
  "ratio_veg_total_24",
  "ratio_animals_total_24",
  "ratio_legum_total_24",
  "premeal_1h_mean_glu_mean_24",
  "premeal_1h_mean_glu_sd_24",
  "ratio_ppg"
)

format_mean_sd <- function(x) {
  m <- round(mean(x, na.rm = TRUE), 2)
  s <- round(sd(x, na.rm = TRUE), 2)
  paste0(m, " (", s, ")")
}


# Overall
overall_summary <- clusters %>%
  dplyr::summarise(across(all_of(vars_to_summarize), format_mean_sd)) %>%
  dplyr::mutate(Cluster = "Overall") %>%
  dplyr::select(Cluster, everything())

# cluster_all_2clusters
summary_all_2clusters <- clusters %>%
  dplyr::group_by(cluster_all_2clusters) %>%
  dplyr::summarise(across(all_of(vars_to_summarize), format_mean_sd), .groups = "drop") %>%
  dplyr::mutate(Cluster = paste0("cluster_all_2clusters_", cluster_all_2clusters)) %>%
  dplyr::select(Cluster, everything(), -cluster_all_2clusters)

# cluster_only_cgm
summary_only_cgm <- clusters %>%
  dplyr::group_by(cluster_only_cgm) %>%
  dplyr::summarise(across(all_of(vars_to_summarize), format_mean_sd), .groups = "drop") %>%
  dplyr::mutate(Cluster = paste0("cluster_only_cgm_", cluster_only_cgm)) %>%
  dplyr::select(Cluster, everything(), -cluster_only_cgm)

# cluster_nutri
summary_nutri <- clusters %>%
  dplyr::group_by(cluster_nutri) %>%
  dplyr::summarise(across(all_of(vars_to_summarize), format_mean_sd), .groups = "drop") %>%
  dplyr::mutate(Cluster = paste0("cluster_nutri_", cluster_nutri)) %>%
  dplyr::select(Cluster, everything(), -cluster_nutri)

# Combine all
table1_summary <- dplyr::bind_rows(
  overall_summary,
  summary_all_2clusters,
  summary_only_cgm,
  summary_nutri
)

# Print numeric variable summary
print(table1_summary)




