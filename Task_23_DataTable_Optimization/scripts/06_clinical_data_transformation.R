# Section 6: a small end-to-end laboratory summary.
library(data.table)

# fread() reads each CSV file into a data.table.
adsl <- fread("data/synthetic_adsl.csv")
adlb <- fread("data/synthetic_adlb.csv")

# Keep the selected tests and non-missing results required for this report.
selected_adlb <- adlb[PARAMCD %chin% c("ALT", "AST") & !is.na(AVAL)]

# Add an age category to the subject-level data.
adsl[, AGEGR1 := fifelse(AGE < 40, "<40", "40+")]

# Match each selected lab row to its subject's age, sex, and treatment.
analysis_records <- adsl[selected_adlb, on = "USUBJID"]

# Calculate useful treatment summaries. .N is row count; uniqueN counts subjects.
final_result <- analysis_records[
  , .(
    N_RECORDS = .N,
    N_SUBJECTS = uniqueN(USUBJID),
    MEAN_AVAL = mean(AVAL),
    MEDIAN_AVAL = median(AVAL),
    MIN_AVAL = min(AVAL),
    MAX_AVAL = max(AVAL)
  ),
  by = .(PARAMCD, TRT01A, AGEGR1)
][order(PARAMCD, TRT01A, AGEGR1)]

# Create the output folder if it does not exist.
dir.create("results", showWarnings = FALSE, recursive = TRUE)

# Write a plain data.frame to CSV to keep the export simple.
write.csv(as.data.frame(final_result), "results/clinical_lab_summary.csv", row.names = FALSE)
cat("Saved", nrow(final_result), "summary rows to results/clinical_lab_summary.csv\n")
print(final_result)
