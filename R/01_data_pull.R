# 01_data_pull.R
# Pull CU, SL, and ST pitches from Baseball Savant for 2022-2025.
# Saves to data/raw/statcast_breaking.csv (read by reports/analysis.Rmd).
#
# Uses direct httr calls with a temp-file round-trip to avoid BOM parsing
# errors that occur when passing the raw response text to read_csv().

library(httr)
library(readr)
library(dplyr)

YEARS <- 2022:2025

# End-of-month helper (base R only).
eom <- function(yr, mo) {
  as.character(as.Date(sprintf("%d-%02d-01", yr, mo %% 12 + 1)) - 1)
}

pull_savant_chunk <- function(start_date, end_date) {
  url <- paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
    "&hfPT=CU%7CSL%7CST%7C",
    "&hfGT=R%7C",
    "&type=details",
    "&player_type=pitcher",
    "&game_date_gt=", start_date,
    "&game_date_lt=", end_date,
    "&min_pitches=0&min_results=0&min_pas=0"
  )

  resp <- tryCatch(
    GET(url,
        add_headers(`User-Agent` = "Mozilla/5.0 (compatible; R research)"),
        timeout(120)),
    error = function(e) NULL
  )

  if (is.null(resp) || http_error(resp)) {
    warning(sprintf("HTTP error: %s – %s", start_date, end_date))
    return(NULL)
  }

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeBin(content(resp, as = "raw"), tmp)

  tryCatch(
    read_csv(tmp, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      warning(sprintf("Parse error %s – %s: %s", start_date, end_date, conditionMessage(e)))
      NULL
    }
  )
}

months <- expand.grid(
  year  = YEARS,
  month = 4:9,
  stringsAsFactors = FALSE
)
starts <- sprintf("%d-%02d-01", months$year, months$month)
ends   <- mapply(eom, months$year, months$month)

raw_list <- mapply(function(s, e) {
  Sys.sleep(1)
  message(sprintf("Pulling %s to %s...", s, e))
  pull_savant_chunk(s, e)
}, starts, ends, SIMPLIFY = FALSE)

raw <- bind_rows(Filter(Negate(is.null), raw_list))

message(sprintf("\nTotal rows pulled: %s", format(nrow(raw), big.mark = ",")))
print(table(raw$pitch_type, raw$game_year))

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
write_csv(raw, "data/raw/statcast_breaking.csv")
message(sprintf("Saved %s rows to data/raw/statcast_breaking.csv", format(nrow(raw), big.mark = ",")))
