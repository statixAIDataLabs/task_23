# Section 3: select rows and columns with simple conditions.
library(data.table)

adsl <- fread("data/synthetic_adsl.csv")
adlb <- fread("data/synthetic_adlb.csv")

# Base R uses square brackets with a row condition and column selection.
adults_base <- as.data.frame(adsl)
adults_base <- adults_base[adults_base$AGE >= 65, c("USUBJID", "AGE", "TRT01A")]

# data.table puts the row condition before the comma and selected columns after it.
adults_dt <- adsl[AGE >= 65, .(USUBJID, AGE, TRT01A)]
stopifnot(isTRUE(all.equal(adults_base, as.data.frame(adults_dt), check.attributes = FALSE)))
cat("Age filter returns", nrow(adults_dt), "rows in both methods.\n")

# Filter one treatment group.
drug_a <- adsl[TRT01A == "Drug A", .(USUBJID, AGE, SEX, TRT01A)]

# Filter selected laboratory parameters and exclude missing values.
selected_labs <- adlb[PARAMCD %chin% c("ALT", "AST") & !is.na(AVAL)]

# Multiple conditions can be joined with & (and) or | (or).
older_drug_a <- adsl[AGE >= 65 & TRT01A == "Drug A"]

print(head(adults_dt))
cat("Drug A subjects:", nrow(drug_a), "\n")
cat("Non-missing ALT/AST rows:", nrow(selected_labs), "\n")
cat("Older Drug A subjects:", nrow(older_drug_a), "\n")
