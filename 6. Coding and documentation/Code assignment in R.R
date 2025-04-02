
# File paths (adjust these paths as needed)
task_data_path <- "/Users/a657138/Desktop/study/Reproducible research/RRcourse2025/Data/onet_tasks.csv"
employment_data_path <- "Data/Eurostat_employment_isco.xlsx"

# --- Load required libraries ---
library(readxl)
library(dplyr)
library(stringr)
library(Hmisc)

# --- Read Task Data ---
task_data <- read.csv(task_data_path)

# --- Employment Data ---
read_employment_data <- function(sheet_name) {
  df <- read_excel(employment_data_path, sheet = sheet_name)
  # Extract the numeric part from sheet name "ISCO1", "ISCO2", etc.
  df$ISCO <- as.numeric(gsub("ISCO", "", sheet_name))
  return(df)
}

# Read sheets ISCO1 to ISCO9 using a loop
isco_sheets <- paste0("ISCO", 1:9)
isco_list <- lapply(isco_sheets, read_employment_data)

all_data <- bind_rows(isco_list)

# --- Compute Total Workers and Shares for Selected Countries ---
countries <- c("Belgium", "Spain", "Poland")
compute_total <- function(country) {
  total <- sum(sapply(isco_list, function(df) sum(df[[country]], na.rm = TRUE)))
  return(total)
}

totals <- sapply(countries, compute_total)
for (country in countries) {
  total_val <- totals[[country]]
  total_col <- paste0("total_", country)
  share_col <- paste0("share_", country)
  all_data[[total_col]] <- total_val  # same total for all rows
  all_data[[share_col]] <- all_data[[country]] / total_val
}

# --- Process Task Data ---
task_data$isco08_1dig <- as.numeric(str_sub(task_data$isco08, 1, 1))
aggdata <- aggregate(. ~ isco08_1dig, data = task_data, FUN = function(x) mean(x, na.rm = TRUE))
aggdata$isco08 <- NULL

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# --- Standardize Task Variables ---
standardize_task <- function(data, task_var, weight_var) {
  mean_val <- wtd.mean(data[[task_var]], data[[weight_var]], na.rm = TRUE)
  sd_val <- sqrt(wtd.var(data[[task_var]], data[[weight_var]], na.rm = TRUE))
  (data[[task_var]] - mean_val) / sd_val
}

task_vars <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

for (country in countries) {
  weight_var <- paste0("share_", country)
  for (task in task_vars) {
    std_col <- paste0("std_", country, "_", task)
    combined[[std_col]] <- standardize_task(combined, task, weight_var)
  }
}

# --- Compute and Standardize Non-Routine Cognitive Analytical (NRCA) Scores ---
# Sum
for (country in countries) {
  std_cols <- paste0("std_", country, "_", task_vars)
  nrca_col <- paste0(country, "_NRCA")
  combined[[nrca_col]] <- rowSums(combined[, std_cols], na.rm = TRUE)
  
  # Standardize the NRCA score for the country
  share_var <- paste0("share_", country)
  mean_nrca <- wtd.mean(combined[[nrca_col]], combined[[share_var]], na.rm = TRUE)
  sd_nrca <- sqrt(wtd.var(combined[[nrca_col]], combined[[share_var]], na.rm = TRUE))
  combined[[paste0("std_", country, "_NRCA")]] <- (combined[[nrca_col]] - mean_nrca) / sd_nrca
}

# --- Compute Country-Level Weighted NRCA Trends Over Time ---
compute_weighted_NRCA <- function(data, country) {
  std_nrca_col <- paste0("std_", country, "_NRCA")
  share_col <- paste0("share_", country)
  data$weighted_NRCA <- data[[std_nrca_col]] * data[[share_col]]
  aggregate(weighted_NRCA ~ TIME, data = data, FUN = sum, na.rm = TRUE)
}

# Compute aggregated NRCA trends for each country
agg_results <- lapply(countries, function(country) compute_weighted_NRCA(combined, country))
names(agg_results) <- countries

# --- Plotting NRCA Trends ---
par(mfrow = c(1, 3))
for (country in countries) {
  agg_data <- agg_results[[country]]
  plot(agg_data$weighted_NRCA, type = "b", main = paste("NRCA Trend:", country),
       xlab = "Time", ylab = "Weighted NRCA")
  axis(1, at = 1:nrow(agg_data), labels = agg_data$TIME)
}
