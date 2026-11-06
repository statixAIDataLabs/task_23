# Section 5: match subject-level records to repeated laboratory records.
library(data.table)

adsl <- fread("data/synthetic_adsl.csv")
adlb <- fread("data/synthetic_adlb.csv")

# Base R merge() matches equal USUBJID values. Each lab row is retained.
base_join <- merge(as.data.frame(adsl), as.data.frame(adlb), by = "USUBJID", all = FALSE)

# A keyed join uses the ADSL identifier as an index.
adsl_keyed <- copy(adsl)
setkey(adsl_keyed, USUBJID) # setkey() sorts the table and marks the key.
keyed_join <- adsl_keyed[adlb, on = "USUBJID"]

# Sort both results into the same row order before comparing them.
base_check <- as.data.table(base_join)
dt_check <- copy(keyed_join)
setorder(base_check, USUBJID, PARAMCD)
setorder(dt_check, USUBJID, PARAMCD)
stopifnot(isTRUE(all.equal(
  as.data.frame(base_check), as.data.frame(dt_check), check.attributes = FALSE
)))
cat("Base merge and keyed join contain the same matched records.\n")

# ADSL[ADLB] means: look up every ADLB row in ADSL.
# Because each subject has multiple lab rows, subject columns are repeated.
print(head(keyed_join))
cat("Join rows:", nrow(keyed_join), "\n")

# A left join keeps every ADSL subject, even if a subject has no lab record.
# This synthetic file has lab rows for every subject, so row counts are equal here.
left_join <- merge(as.data.frame(adsl), as.data.frame(adlb), by = "USUBJID", all.x = TRUE)
cat("Left join rows:", nrow(left_join), "\n")

# setkey() changes row order in adsl_keyed. Use copy() above to protect adsl.
cat("ADSL rows after keyed copy:", nrow(adsl), "\n")
