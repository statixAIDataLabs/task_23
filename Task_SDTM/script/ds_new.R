#disposition
library(data.table)
#--------------------------------------------------
# Protocol Milestone Record
#--------------------------------------------------
dm <- as.data.table(readxl::read_excel("dm.xlsx"))
ds_ic <- dm[, .(
  STUDYID,
  DOMAIN = "DS",
  USUBJID,
  RFSTDTC,
  DSTERM = "INFORMED CONSENT OBTAINED",
  DSDECOD = "INFORMED CONSENT OBTAINED",
  DSCAT = "PROTOCOL MILESTONE",
  DSSTDTC = format(as.Date(RFICDTC), "%Y-%m-%d")
)]

#--------------------------------------------------
# Disposition Event Records
#--------------------------------------------------

ds_disp <- as.data.table(copy(ds_combined))[SUBJID %in% dm$SUBJID]
ds_disp[, DSDECOD := toupper(trimws(DSDECOD))]
ds_disp <- ds_disp[DSDECOD %in% c("COMPLETED", "PHYSICIAN DECISION")]

dm_ds <- dm[, .(SUBJID, USUBJID, RFSTDTC, DMSTUDYID = STUDYID)]
ds_disp1 <- merge(ds_disp, dm_ds, by = "SUBJID", all.x = TRUE, sort = FALSE)
ds_disp1[, `:=`(
  STUDYID = DMSTUDYID,
  DOMAIN = "DS",
  DSCAT = "DISPOSITION EVENT",
  DSSTDTC = format(as.Date(DSSTDTC), "%Y-%m-%d"),
  DSTERM = fcase(
    DSDECOD == "COMPLETED", "COMPLETED",
    DSDECOD == "PHYSICIAN DECISION" & !is.na(DSREASND) & trimws(DSREASND) != "",
    toupper(DSREASND),
    default = DSDECOD
  )
)]
ds_disp1[, DMSTUDYID := NULL]
ds_disp1 <- ds_disp1[, .(
  STUDYID, DOMAIN, USUBJID, RFSTDTC, DSTERM, DSDECOD, DSCAT, DSSTDTC
)]

#--------------------------------------------------
# Combine
#--------------------------------------------------

ds <- rbindlist(list(ds_ic, ds_disp1), use.names = TRUE)
ds[, DSSTDY := fifelse(
  !is.na(DSSTDTC),
  as.integer(as.Date(DSSTDTC) - as.Date(RFSTDTC)) +
    fifelse(as.Date(DSSTDTC) >= as.Date(RFSTDTC), 1L, 0L),
  NA_integer_
)]
setorderv(ds, c("USUBJID", "DSDECOD", "DSSTDTC"), na.last = TRUE)
ds[, DSSEQ := seq_len(.N), by = .(USUBJID)]

ds_vars <- c(
  "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT",
  "DSSTDTC", "DSSTDY"
)
ds <- ds[, ..ds_vars]
setorderv(ds, c("STUDYID", "USUBJID", "DSDECOD", "DSSTDTC"), na.last = TRUE)
writexl::write_xlsx(as.data.frame(ds), "ds.xlsx")


#Validation
# Key uniqueness
uniqueN(ds, by = c("STUDYID", "USUBJID", "DSDECOD", "DSSTDTC")) < nrow(ds)

# Required variables
sum(is.na(ds$USUBJID))
sum(is.na(ds$DSSEQ))
sum(is.na(ds$DSTERM))
sum(is.na(ds$DSDECOD))

# Controlled terminology
sort(unique(ds$DSCAT))
# DISPOSITION EVENT
# PROTOCOL MILESTONE

sort(unique(ds$DSDECOD))
# COMPLETED
# INFORMED CONSENT OBTAINED
# PHYSICIAN DECISION
