# Section 4: summarize records by one or more groups.
library(data.table)

adsl <- fread("data/synthetic_adsl.csv")
adlb <- fread("data/synthetic_adlb.csv")

# by names the groups. .N counts rows in each group.
# uniqueN() counts distinct subject identifiers within each group.
subject_counts <- adsl[, .(N_RECORDS = .N, N_SUBJECTS = uniqueN(USUBJID)), by = TRT01A]
print(subject_counts)

# .() is a short way to make a list of output columns.
# na.rm = TRUE tells the summary functions to skip missing AVAL values.
lab_by_treatment <- adlb[
  , .(
    N_RECORDS = .N,
    N_VALID = sum(!is.na(AVAL)),
    MEAN_AVAL = mean(AVAL, na.rm = TRUE),
    MEDIAN_AVAL = median(AVAL, na.rm = TRUE),
    MIN_AVAL = min(AVAL, na.rm = TRUE),
    MAX_AVAL = max(AVAL, na.rm = TRUE)
  ),
  by = .(PARAMCD)
]
print(lab_by_treatment)

# Join subject treatment information before summarizing by parameter and treatment.
lab_with_treatment <- adsl[adlb, on = "USUBJID"]
treatment_summary <- lab_with_treatment[
  , .(
    N_RECORDS = .N,
    N_VALID = sum(!is.na(AVAL)),
    MEAN_AVAL = mean(AVAL, na.rm = TRUE),
    MEDIAN_AVAL = median(AVAL, na.rm = TRUE),
    MIN_AVAL = min(AVAL, na.rm = TRUE),
    MAX_AVAL = max(AVAL, na.rm = TRUE)
  ),
  by = .(PARAMCD, TRT01A)
]
print(treatment_summary[order(PARAMCD, TRT01A)])
