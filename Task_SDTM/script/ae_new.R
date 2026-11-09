#adverse events
library(data.table)

dm <- as.data.table(readxl::read_excel("dm.xlsx"))

ae_rows <- as.data.table(copy(ae_combined))[SUBJID %in% dm$SUBJID]
dm_ids <- dm[, .(STUDYID, SUBJID, USUBJID)]
ae1 <- merge(ae_rows, dm_ids, by = "SUBJID", all.x = TRUE, sort = FALSE)
ae1[, `:=`(
  DOMAIN = "AE",
  AESTDTC = format(AESTDTC, "%Y-%m-%d"),
  AEENDTC = fifelse(
    is.na(AEENDTC),
    NA_character_,
    format(AEENDTC, "%Y-%m-%d")
  ),
  AESEV = toupper(AESEV),
  AESER = fcase(
    toupper(AESER) %in% c("YES", "Y"), "Y",
    toupper(AESER) %in% c("NO", "N"), "N",
    default = NA_character_
  ),
  AEREL = toupper(AEREL),
  AEOUT = fcase(
    toupper(AEOUT) == "RECOVERED", "RECOVERED/RESOLVED",
    toupper(AEOUT) == "RECOVERING", "RECOVERING/RESOLVING",
    toupper(AEOUT) == "NOT RECOVERED", "NOT RECOVERED/NOT RESOLVED",
    toupper(AEOUT) == "UNKNOWN", "UNKNOWN",
    default = toupper(AEOUT)
  )
)]
setorder(ae1, USUBJID, AESTDTC, AETERM)
ae1[, AESEQ := seq_len(.N), by = .(USUBJID)]

dm_dates <- dm[, .(USUBJID, RFSTDTC)]
ae2 <- merge(ae1, dm_dates, by = "USUBJID", all.x = TRUE, sort = FALSE)
ae2[, AESTDY := as.integer(as.Date(AESTDTC) - as.Date(RFSTDTC)) +
      fifelse(as.Date(AESTDTC) >= as.Date(RFSTDTC), 1L, 0L)
]
ae2[, AEENDY := fifelse(
  !is.na(AEENDTC),
  as.integer(as.Date(AEENDTC) - as.Date(RFSTDTC)) +
    fifelse(as.Date(AEENDTC) >= as.Date(RFSTDTC), 1L, 0L),
  NA_integer_
)]

ae2[, `:=`(
  AEDECOD = NA_character_,
  AELLT = NA_character_,
  AELLTCD = NA_real_,
  AEPTCD = NA_real_,
  AEHLT = NA_character_,
  AEHLTCD = NA_real_,
  AEHLGT = NA_character_,
  AEHLGTCD = NA_real_,
  AEBODSYS = NA_character_,
  AEBDSYCD = NA_real_,
  AESOC = NA_character_,
  AESOCCD = NA_real_
)]
ae_vars <- c(
  "STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AEDECOD",
  "AEBODSYS", "AESOC", "AEHLGT", "AEHLT", "AESTDTC", "AEENDTC",
  "AESTDY", "AEENDY", "AESEV", "AESER", "AEREL", "AEOUT"
)
ae <- ae2[, ..ae_vars]
setorder(ae, STUDYID, USUBJID, AEDECOD, AESTDTC)

writexl::write_xlsx(as.data.frame(ae), "ae.xlsx")

#Validation
anyDuplicated(ae, by = c("STUDYID", "USUBJID", "AESEQ")) > 0L
ae[, .N, by = .(STUDYID, USUBJID, AESEQ)][N > 1L]
unique(ae$AESER)
unique(ae$AESEV)