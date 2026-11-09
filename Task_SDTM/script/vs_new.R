#vitals
library(data.table)


dm <- as.data.table(readxl::read_excel("dm.xlsx"))

dm_vs <- dm[, .(SUBJID, USUBJID, RFSTDTC, DMSTUDYID = STUDYID)]
vs_rows <- as.data.table(copy(vs_combined))[SUBJID %in% dm$SUBJID]
vs1 <- merge(vs_rows, dm_vs, by = "SUBJID", all.x = TRUE, sort = FALSE)
vs1[, `:=`(
  STUDYID = DMSTUDYID,
  DOMAIN = "VS",
  VSDTC = format(as.Date(VSDTC), "%Y-%m-%d"),
  VSORRES = as.character(VSORRES),
  VSSTRESC = as.character(VSORRES),
  VSSTRESN = as.numeric(VSORRES),
  VSORRESU = fcase(
    toupper(VSORRESU) == "MMHG", "mmHg",
    toupper(VSORRESU) == "BEATS/MIN", "beats/min",
    toupper(VSORRESU) == "KG", "kg",
    toupper(VSORRESU) == "C", "C",
    default = VSORRESU
  ),
  VSLOBXFL = NA_character_,
  VISITNUM = NA_real_,
  VSDY = as.integer(as.Date(VSDTC) - as.Date(RFSTDTC)) +
    fifelse(as.Date(VSDTC) >= as.Date(RFSTDTC), 1L, 0L)
)]
vs1[, VSSTRESU := VSORRESU]
vs1[, DMSTUDYID := NULL]
setorderv(vs1, c("USUBJID", "VSDTC", "VSTESTCD"), na.last = TRUE)
vs1[, VSSEQ := seq_len(.N), by = .(USUBJID)]

vs_vars <- c(
  "STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VSTESTCD", "VSTEST",
  "VSORRES", "VSORRESU", "VSSTRESC", "VSSTRESN", "VSSTRESU", "VSLOBXFL",
  "VISITNUM", "VSDTC"
)
vs <- vs1[, ..vs_vars]
setorderv(vs, c("STUDYID", "USUBJID", "VSTESTCD"), na.last = TRUE)
writexl::write_xlsx(as.data.frame(vs), "vs.xlsx")


# validation
# Key uniqueness
uniqueN(vs, by = c("USUBJID", "VSSEQ")) < nrow(vs)

# Required vars
sum(is.na(vs$VSTESTCD))
sum(is.na(vs$VSTEST))

# Numeric consistency
all(vs$VSSTRESN == as.numeric(vs$VSSTRESC), na.rm = TRUE)

