library(readr)
library(dplyr)
library(ggplot2)
library(skimr)     # install.packages("skimr") if you don't have it
library(janitor)    # install.packages("janitor") if you don't have it

# ---------------------------------------------------------------------------
# 1. LOAD DATA
# ---------------------------------------------------------------------------
# IMPORTANT: use the RAW file URL, not the GitHub webpage (blob) URL.
# Blob URL (wrong):  .../blob/main/data/...csv   <- returns an HTML page
# Raw URL (correct): .../raw/main/data/...csv    <- returns the actual CSV

data_path <- "https://raw.githubusercontent.com/mdenofsky26/ebola_drc_26/main/data/drc_ebola_cases_consolidated.csv"

ebola_data <- read_csv(data_path, show_col_types = FALSE) %>%
  clean_names()   # standardizes column names (lowercase, snake_case)

# Sanity check: make sure we actually got a CSV, not an HTML page
if (ncol(ebola_data) <= 1) {
  stop("Only 1 column loaded — check that you're using the RAW GitHub URL, ",
       "not the blob/webpage URL.")
}

# ---------------------------------------------------------------------------
# 2. FIRST LOOK AT THE DATA
# ---------------------------------------------------------------------------
dim(ebola_data)          # rows x columns
names(ebola_data)        # column names
glimpse(ebola_data)      # types + preview of each column
head(ebola_data, 10)     # first 10 rows

# ---------------------------------------------------------------------------
# 3. SUMMARY STATISTICS
# ---------------------------------------------------------------------------
skim(ebola_data)         # rich summary: missing values, distributions, etc.

# If a 'date' column exists, make sure it's parsed correctly
if ("date" %in% names(ebola_data)) {
  ebola_data$date <- as.Date(ebola_data$date)
  cat("Date range:", as.character(min(ebola_data$date, na.rm = TRUE)),
      "to", as.character(max(ebola_data$date, na.rm = TRUE)), "\n")
}

# ---------------------------------------------------------------------------
# 4. MISSING DATA CHECK
# ---------------------------------------------------------------------------
colSums(is.na(ebola_data))

# ---------------------------------------------------------------------------
# 5. CATEGORICAL VARIABLES (e.g. region)
# ---------------------------------------------------------------------------
if ("region" %in% names(ebola_data)) {
  ebola_data %>%
    count(region, sort = TRUE)
}

# ---------------------------------------------------------------------------
# 6. NUMERIC DISTRIBUTIONS (e.g. cases, deaths)
# ---------------------------------------------------------------------------
if ("cases" %in% names(ebola_data)) {
  summary(ebola_data$cases)

  ggplot(ebola_data, aes(x = cases)) +
    geom_histogram(bins = 30, fill = "darkred", color = "white") +
    labs(title = "Distribution of Ebola Cases", x = "Cases", y = "Count") +
    theme_minimal()
}

if ("deaths" %in% names(ebola_data)) {
  summary(ebola_data$deaths)

  ggplot(ebola_data, aes(x = deaths)) +
    geom_histogram(bins = 30, fill = "black", color = "white") +
    labs(title = "Distribution of Ebola Deaths", x = "Deaths", y = "Count") +
    theme_minimal()
}

# ---------------------------------------------------------------------------
# 7. TIME TREND (if date + cases exist)
# ---------------------------------------------------------------------------
if (all(c("date", "cases") %in% names(ebola_data))) {
  ebola_data %>%
    group_by(date) %>%
    summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
    ggplot(aes(x = date, y = total_cases)) +
    geom_line(color = "darkred") +
    labs(title = "Total Ebola Cases Over Time", x = "Date", y = "Total Cases") +
    theme_minimal()
}

# ---------------------------------------------------------------------------
# 8. CASES BY REGION OVER TIME (if region + date + cases exist)
# ---------------------------------------------------------------------------
if (all(c("date", "region", "cases") %in% names(ebola_data))) {
  ebola_data %>%
    group_by(date, region) %>%
    summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = date, y = total_cases, color = region)) +
    geom_line() +
    labs(title = "Ebola Cases by Region Over Time", x = "Date", y = "Cases") +
    theme_minimal()
}

# ---------------------------------------------------------------------------
# 9. CASE FATALITY RATE (if cases + deaths exist)
# ---------------------------------------------------------------------------
if (all(c("cases", "deaths") %in% names(ebola_data))) {
  ebola_data %>%
    summarise(
      total_cases = sum(cases, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE),
      cfr_percent = round(total_deaths / total_cases * 100, 2)
    )
}
