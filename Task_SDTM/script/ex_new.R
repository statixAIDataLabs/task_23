#exposure
library(data.table)


dm <- as.data.table(readxl::read_excel("dm.xlsx"))

dm_ex <- dm[, .(SUBJID, USUBJID, RFSTDTC, DMSTUDYID = STUDYID)]
ex_rows <- as.data.table(copy(ex_combined))[SUBJID %in% dm$SUBJID]
ex <- merge(ex_rows, dm_ex, by = "SUBJID", all.x = TRUE, sort = FALSE)
ex[, `:=`(
  STUDYID = DMSTUDYID,
  DOMAIN = "EX",
  EXSTDTC = format(EXSTDTC, "%Y-%m-%d"),
  EXENDTC = format(EXENDTC, "%Y-%m-%d"),
  EXDOSU = toupper(EXDOSU),
  EXROUTE = toupper(EXROUTE),
  EXDOSFRM = NA_character_,
  EXSTDY = as.integer(as.Date(EXSTDTC) - as.Date(RFSTDTC)) +
    fifelse(as.Date(EXSTDTC) >= as.Date(RFSTDTC), 1L, 0L),
  EXENDY = as.integer(as.Date(EXENDTC) - as.Date(RFSTDTC)) +
    fifelse(as.Date(EXENDTC) >= as.Date(RFSTDTC), 1L, 0L)
)]
ex[, DMSTUDYID := NULL]
setorderv(ex, c("USUBJID", "EXTRT", "EXSTDTC"), na.last = TRUE)
ex[, EXSEQ := seq_len(.N), by = .(USUBJID)]

ex_vars <- c(
  "STUDYID", "DOMAIN", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU",
  "EXDOSFRM", "EXROUTE", "EXSTDTC", "EXENDTC", "EXSTDY", "EXENDY"
)
ex <- ex[, ..ex_vars]
setorderv(ex, c("STUDYID", "USUBJID", "EXTRT", "EXSTDTC"), na.last = TRUE)
writexl::write_xlsx(as.data.frame(ex), "ex.xlsx")

#validation
uniqueN(ex) < nrow(ex)
unique(ex$EXDOSU)
unique(ex$EXROUTE)
str(ex)
all(ex$USUBJID %chin% dm$USUBJID)
