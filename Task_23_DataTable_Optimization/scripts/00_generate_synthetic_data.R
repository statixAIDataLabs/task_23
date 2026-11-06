# Generate small, reproducible clinical-style datasets for this training project.
# All records are simulated. They do not describe real people.

# Create the data folder if this script is run from the project root.
dir.create("data", showWarnings = FALSE, recursive = TRUE)

# set.seed() makes the random values repeatable each time the script is run.
set.seed(23023)

# Create one subject-level row for each of 80 simulated subjects.
subject_count <- 80
adsl <- data.frame(
  USUBJID = sprintf("SUBJ-%03d", seq_len(subject_count)),
  AGE = sample(18:85, subject_count, replace = TRUE),
  SEX = sample(c("F", "M"), subject_count, replace = TRUE),
  TRT01A = sample(c("Placebo", "Drug A", "Drug B"), subject_count, replace = TRUE),
  stringsAsFactors = FALSE
)

# Create four lab records for every subject. Each row is one subject and test.
parameter_codes <- c("ALT", "AST", "HGB", "CREAT")
adlb <- expand.grid(
  USUBJID = adsl$USUBJID,
  PARAMCD = parameter_codes,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Generate plausible, varied values for the four tests.
adlb$AVAL <- round(
  ifelse(adlb$PARAMCD == "ALT", rnorm(nrow(adlb), 28, 12),
  ifelse(adlb$PARAMCD == "AST", rnorm(nrow(adlb), 25, 10),
  ifelse(adlb$PARAMCD == "HGB", rnorm(nrow(adlb), 13, 1.5),
         rnorm(nrow(adlb), 0.9, 0.25)))),
  1
)

# Add a small number of missing lab results to make missing-value handling visible.
missing_rows <- sample(seq_len(nrow(adlb)), 8)
adlb$AVAL[missing_rows] <- NA_real_

# Write plain CSV files so the lessons can read them with base R or data.table.
write.csv(adsl, "data/synthetic_adsl.csv", row.names = FALSE, na = "")
write.csv(adlb, "data/synthetic_adlb.csv", row.names = FALSE, na = "")

cat("Created", nrow(adsl), "ADSL rows and", nrow(adlb), "ADLB rows.\n")
