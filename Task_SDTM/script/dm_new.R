library(data.table)
library(writexl)
dm_combined1 <- as.data.table(copy(dm_combined))
dm_combined1[, `:=`(
  USUBJID = paste0(STUDYID, "-", SITEID, "-", SUBJID),
  SITEID = as.character(SITEID),
  DOMAIN = "DM",
  SEX = fcase(
    SEX %in% c("Male", "M"), "M",
    SEX %in% c("Female", "F"), "F",
    default = SEX
  ),
  RACE = toupper(RACE),
  ETHNIC = toupper(ETHNIC)
)]

ex_dm <- as.data.table(copy(ex_combined))[
  , .(RFSTDTC = min(EXSTDTC), RFENDTC = max(EXENDTC)), by = .(SUBJID)
]

dm_ex <- merge(dm_combined1, ex_dm, by = "SUBJID", all.x = TRUE, sort = FALSE)
dm_ex[, `:=`(
  RFSTDTC = format(RFSTDTC, "%Y-%m-%d"),
  RFENDTC = format(RFENDTC, "%Y-%m-%d"),
  RFICDTC = format(RFICDTC, "%Y-%m-%d"),
  RFXSTDTC = format(RFSTDTC, "%Y-%m-%d"),
  RFXENDTC = format(RFENDTC, "%Y-%m-%d"),
  DTHDTC = NA_character_,
  DTHFL = NA_character_,
  ARMCD = NA_character_,
  ARM = NA_character_,
  ACTARMCD = NA_character_,
  ACTARM = NA_character_,
  ARMNRS = NA_character_,
  ACTARMUD = NA_character_
)]
dm_ex[, AGE := fifelse(
  nchar(BRTHDTC) == 10L,
  as.numeric(as.Date(RFSTDTC) - as.Date(BRTHDTC)) / 365.25,
  NA_real_
)]
dm_ex[, AGEU := fifelse(!is.na(AGE), "YEARS", NA_character_)]

rfp_ex <- as.data.table(copy(ex_combined))[
  , .(DT = max(EXENDTC, na.rm = TRUE)), by = .(SUBJID)
]

rfp_ae <- as.data.table(copy(ae_combined))[!is.na(AEENDTC),
                                           .(DT = max(AEENDTC, na.rm = TRUE)), by = .(SUBJID)
]

rfp_vs <- as.data.table(copy(vs_combined))[
  , .(DT = max(VSDTC, na.rm = TRUE)), by = .(SUBJID)
]

rfp_ds <- as.data.table(copy(ds_combined))[!is.na(DSSTDTC),
                                           .(DT = max(DSSTDTC, na.rm = TRUE)), by = .(SUBJID)
]

rfp <- rbindlist(list(rfp_ex, rfp_ae, rfp_vs, rfp_ds))[
  , .(RFPENDTC = max(DT, na.rm = TRUE)), by = .(SUBJID)
]

dm <- merge(dm_ex, rfp, by = "SUBJID", all.x = TRUE, sort = FALSE)
dm_vars <- c(
  "STUDYID", "DOMAIN", "USUBJID", "SUBJID", "RFSTDTC", "RFENDTC",
  "RFXSTDTC", "RFXENDTC", "RFICDTC", "RFPENDTC", "DTHDTC", "DTHFL",
  "SITEID", "BRTHDTC", "AGE", "AGEU", "SEX", "RACE", "ETHNIC",
  "ARMCD", "ARM", "ACTARMCD", "ACTARM", "ARMNRS", "ACTARMUD"
)
setorderv(dm, c("STUDYID", "USUBJID"))
dm <- dm[, ..dm_vars]

write_xlsx(as.data.frame(dm), "dm.xlsx")

#validation
# Key uniqueness
nrow(dm) == uniqueN(dm$USUBJID)

# Required variables not missing
sum(is.na(dm$STUDYID))
sum(is.na(dm$DOMAIN))
sum(is.na(dm$USUBJID))
sum(is.na(dm$SUBJID))

# Controlled terminology
unique(dm$SEX)

# USUBJID uniqueness
anyDuplicated(dm$USUBJID) > 0L

# Date ordering
sum(dm$RFENDTC < dm$RFSTDTC, na.rm = TRUE)

# AGE consistency
table(is.na(dm$AGE), is.na(dm$AGEU))