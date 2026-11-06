# Section 1: a first look at data.table
# Run this script from the Task_23_DataTable_Optimization project folder.

# Install data.table once in the R console if it is not already available:
# install.packages("data.table", repos = "https://cloud.r-project.org/")

library(data.table) # Loads data.table functions for this R session.

# data.table() creates a table with named columns, much like data.frame().
small_dt <- data.table(
  USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
  AGE = c(34, 67, 52),
  TRT01A = c("Drug A", "Placebo", "Drug A")
)

# head() displays the first rows so we can inspect the values.
print(head(small_dt))

# str() displays column names, data types, and a preview of the data.
str(small_dt)

# dim() returns the number of rows and columns.
print(dim(small_dt))

# A data.frame can be converted in place with setDT().
small_df <- data.frame(
  USUBJID = c("SUBJ-004", "SUBJ-005"),
  AGE = c(45, 70),
  stringsAsFactors = FALSE
)
setDT(small_df) # Converts small_df itself; it does not make a second table.
print(class(small_df))
print(small_df)

# Read the supplied synthetic file using fread(), data.table's CSV reader.
adsl <- fread("data/synthetic_adsl.csv")
print(head(adsl))
print(dim(adsl))
