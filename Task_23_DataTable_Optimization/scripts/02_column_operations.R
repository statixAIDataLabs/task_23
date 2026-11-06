# Section 2: create and change columns without unnecessary copies.
library(data.table)

adsl <- fread("data/synthetic_adsl.csv")

# := adds or changes a column by reference: the existing table is updated.
adsl[, AGEGR1 := fifelse(AGE < 40, "<40", "40+")]
adsl[, AGE_NEXT_YEAR := AGE + 1]

# A normal data.frame assignment follows R's copy-on-modify behavior.
base_adsl <- as.data.frame(adsl)
ordinary_copy <- base_adsl
ordinary_copy$AGE[1] <- ordinary_copy$AGE[1] + 1
cat("Base R original age:", base_adsl$AGE[1], "\n")
cat("Base R changed copy age:", ordinary_copy$AGE[1], "\n")

# With data.table, a simple second name refers to the same table.
reference_demo <- copy(adsl) # copy() makes an independent data.table first.
reference_alias <- reference_demo
reference_demo[, AGE := AGE + 1]
cat("data.table alias sees the changed age:", reference_alias$AGE[1], "\n")

# Use copy() before a change when the original values must be preserved.
safe_copy <- copy(adsl)
safe_copy[, AGE := AGE + 1]
cat("Original age remains:", adsl$AGE[1], "\n")
cat("Copied table age:", safe_copy$AGE[1], "\n")

# Changing an existing variable is also done with :=.
adsl[, AGE := AGE + 0] # A harmless example that keeps the original age values.
print(head(adsl[, .(USUBJID, AGE, AGEGR1, AGE_NEXT_YEAR)]))
