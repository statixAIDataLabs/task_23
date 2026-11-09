#SDTM specification workbook (documents current mapping in global.R and *_new.R)
library(data.table)
library(openxlsx)

out_file <- file.path("spec", "SDTM_Specification.xlsx")
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)

spec_cols <- c(
  "Variable", "Label", "Type", "Length", "Display Format", "Codelist", "Core",
  "Origin", "Source: MARVIN", "Source: RAVE", "Source: VEEVA", "Derivation",
  "Issues / Notes"
)

read_spec <- function(txt) {
  dt <- fread(text = txt, sep = "|", quote = "", header = TRUE,
              colClasses = "character", na.strings = NULL)
  setnames(dt, spec_cols)
  dt
}

#--------------------------------------------------
# Dataset level metadata
#--------------------------------------------------
contents <- data.table(
  Dataset   = c("DM", "AE", "EX", "VS", "DS"),
  Label     = c("Demographics", "Adverse Events", "Exposure", "Vital Signs",
                "Disposition"),
  Class     = c("Special-Purpose", "Events", "Interventions", "Findings", "Events"),
  Structure = c(
    "One record per subject",
    "One record per adverse event per subject",
    "One record per constant-dosing interval per subject",
    "One record per vital sign measurement per time point per visit per subject",
    "One record per disposition status or protocol milestone per subject"
  ),
  Keys = c(
    "STUDYID, USUBJID",
    "STUDYID, USUBJID, AEDECOD, AESTDTC",
    "STUDYID, USUBJID, EXTRT, EXSTDTC",
    "STUDYID, USUBJID, VSTESTCD, VSDTC",
    "STUDYID, USUBJID, DSDECOD, DSSTDTC"
  ),
  `Input Data` = c(
    "MARVIN_DM, RAVE_DM, VEEVA_DM; EX, AE, VS, DS (reference dates)",
    "MARVIN_AE, RAVE_AE, VEEVA_AE; dm.xlsx",
    "MARVIN_EX, RAVE_EX, VEEVA_EX; dm.xlsx",
    "MARVIN_VS, RAVE_VS, VEEVA_VS; dm.xlsx",
    "MARVIN_DS, RAVE_DS, VEEVA_DS; dm.xlsx"
  ),
  `Sorted by` = c(
    "STUDYID, USUBJID",
    "STUDYID, USUBJID, AEDECOD, AESTDTC",
    "STUDYID, USUBJID, EXTRT, EXSTDTC",
    "STUDYID, USUBJID, VSTESTCD",
    "STUDYID, USUBJID, DSDECOD, DSSTDTC"
  ),
  Program = c("global.R, dm_new.R", "global.R, ae_new.R", "global.R, ex_new.R",
              "global.R, vs_new.R", "global.R, ds_new.R"),
  Output  = c("dm.xlsx", "ae.xlsx", "ex.xlsx", "vs.xlsx", "ds.xlsx"),
  Notes = c(
    "COUNTRY (Req) is not mapped - no source in raw DM. No death or randomization data in raw sources.",
    "MedDRA coding not performed; dictionary variables are null.",
    "Confirm all EXTRT values (Filgrastim, Ondansetron, Paracetamol, Prednisone, Acyclovir) are protocol study treatments; otherwise map to CM.",
    "No visit information in raw data, so VISITNUM/VISIT cannot be derived.",
    "Raw statuses PROGRESSIVE DISEASE and WITHDRAWN are excluded by the current filter."
  )
)

#--------------------------------------------------
# Variable level metadata
#--------------------------------------------------
dm_spec <- read_spec("
V|L|T|N|F|C|R|O|M|RV|VV|D|I
STUDYID|Study Identifier|Char|12|||Req|Collected|STUDYID|STUDY|Study|Source value as-is.|
DOMAIN|Domain Abbreviation|Char|2|||Req|Assigned||||Set to \"DM\".|
USUBJID|Unique Subject Identifier|Char|24|||Req|Derived||||paste0(STUDYID, \"-\", SITEID, \"-\", SUBJID).|SITEID is numeric after fread, so leading zeros are dropped (e.g. MOCK-ONC-001-21-MV-0001).
SUBJID|Subject Identifier for the Study|Char|7|||Req|Collected|SUBJECT|SUBJID|Subject ID|Source value as-is.|
RFSTDTC|Subject Reference Start Date/Time|Char|10|||Exp|Derived||||Earliest EX.EXSTDTC per subject, ISO 8601.|
RFENDTC|Subject Reference End Date/Time|Char|10|||Exp|Derived||||Latest EX.EXENDTC per subject, ISO 8601.|
RFXSTDTC|Date/Time of First Study Treatment|Char|10|||Exp|Derived||||Same as RFSTDTC.|
RFXENDTC|Date/Time of Last Study Treatment|Char|10|||Exp|Derived||||Same as RFENDTC.|
RFICDTC|Date/Time of Informed Consent|Char|10|||Exp|Collected|INFORMED_CONSENT_DATE|ICFDT|ICF Date|Formatted as ISO 8601 (YYYY-MM-DD).|
RFPENDTC|Date/Time of End of Participation|Char|10|||Exp|Derived||||Latest of EX.EXENDTC, AE.AEENDTC, VS.VSDTC, DS.DSSTDTC per subject.|
DTHDTC|Date/Time of Death|Char|10|||Exp|Assigned||||Set to null.|No death data in raw sources.
DTHFL|Subject Death Flag|Char|1||NY|Exp|Assigned||||Set to null.|No death data in raw sources.
SITEID|Study Site Identifier|Char|3|||Req|Collected|SITEID|CENTER|Site Number|Converted to character.|Leading zeros lost (021 -> 21); read with keepLeadingZeros = TRUE.
BRTHDTC|Date/Time of Birth|Char|4|||Perm|Collected|BIRTH_YEAR|BRTHYR|Year of Birth|Year of birth only (YYYY).|Stored as numeric; should be character.
AGE|Age|Num|8|||Exp|Derived||||(RFSTDTC - BRTHDTC) / 365.25 when BRTHDTC is a full date; otherwise null.|Always null - BRTHDTC is year only. Not floored to complete years.
AGEU|Age Units|Char|5||AGEU|Exp|Assigned||||\"YEARS\" when AGE is not null.|Always null because AGE is null.
SEX|Sex|Char|1||SEX|Req|Collected|SEX|SEXCD|Gender|\"Male\"/\"M\" -> \"M\"; \"Female\"/\"F\" -> \"F\".|
RACE|Race|Char|32||RACE|Exp|Collected|RACE|RACECD|Race1|Upper-cased source value.|
ETHNIC|Ethnicity|Char|22||ETHNIC|Perm|Collected|ETHNIC|ETHNICCD|Ethnicity|Upper-cased source value.|
ARMCD|Planned Arm Code|Char|TBD|||Exp|Assigned||||Set to null.|No randomization data.
ARM|Description of Planned Arm|Char|TBD|||Exp|Assigned||||Set to null.|No randomization data.
ACTARMCD|Actual Arm Code|Char|TBD|||Exp|Assigned||||Set to null.|No randomization data.
ACTARM|Description of Actual Arm|Char|TBD|||Exp|Assigned||||Set to null.|No randomization data.
ARMNRS|Reason Arm and/or Actual Arm is Null|Char|TBD|||Exp|Assigned||||Set to null.|Must be populated when ARMCD/ACTARMCD is null.
ACTARMUD|Description of Unplanned Actual Arm|Char|TBD|||Exp|Assigned||||Set to null.|
")

ae_spec <- read_spec("
V|L|T|N|F|C|R|O|M|RV|VV|D|I
STUDYID|Study Identifier|Char|12|||Req|Derived||||DM.STUDYID merged by SUBJID.|
DOMAIN|Domain Abbreviation|Char|2|||Req|Assigned||||Set to \"AE\".|
USUBJID|Unique Subject Identifier|Char|24|||Req|Derived||||DM.USUBJID merged by SUBJID.|Records for subjects not in DM are dropped.
AESEQ|Sequence Number|Num|8|||Req|Derived||||Sequential per USUBJID after sorting by USUBJID, AESTDTC, AETERM.|
AETERM|Reported Term for the Adverse Event|Char|11|||Req|Collected|AE_TERM|AETERM|Reported Term|Source value as-is.|
AEDECOD|Dictionary-Derived Term|Char|TBD|||Req|Assigned||||Set to null.|MedDRA coding not done; Required variable is empty.
AEBODSYS|Body System or Organ Class|Char|TBD|||Exp|Assigned||||Set to null.|MedDRA coding not done.
AESOC|Primary System Organ Class|Char|TBD|||Exp|Assigned||||Set to null.|MedDRA coding not done.
AEHLGT|High Level Group Term|Char|TBD|||Exp|Assigned||||Set to null.|MedDRA coding not done.
AEHLT|High Level Term|Char|TBD|||Exp|Assigned||||Set to null.|MedDRA coding not done.
AESTDTC|Start Date/Time of Adverse Event|Char|10|||Exp|Collected|AE_START|AESTDT|Onset Date|Formatted as ISO 8601 (YYYY-MM-DD).|
AEENDTC|End Date/Time of Adverse Event|Char|10|||Exp|Collected|AE_END|AEENDT|Resolution Date|Formatted as ISO 8601; null if missing.|
AESTDY|Study Day of Start of Adverse Event|Num|8|||Perm|Derived||||AESTDTC - DM.RFSTDTC (+1 if on/after RFSTDTC).|
AEENDY|Study Day of End of Adverse Event|Num|8|||Perm|Derived||||AEENDTC - DM.RFSTDTC (+1 if on/after RFSTDTC).|
AESEV|Severity/Intensity|Char|8||AESEV|Perm|Collected|AE_SEV|AESEV|Severity|Upper-cased source value.|
AESER|Serious Event|Char|1||NY|Exp|Collected|AE_SERIOUS|AESER|Serious?|\"YES\"/\"Y\" -> \"Y\"; \"NO\"/\"N\" -> \"N\".|
AEREL|Causality|Char|11||AEREL|Exp|Collected|AE_RELATION|AEREL|Relatedness|Upper-cased source value.|Sponsor-defined codelist.
AEOUT|Outcome of Adverse Event|Char|26||OUT|Perm|Collected|AE_OUTCOME|AEOUT|Status|RECOVERED -> RECOVERED/RESOLVED; RECOVERING -> RECOVERING/RESOLVING; NOT RECOVERED -> NOT RECOVERED/NOT RESOLVED; UNKNOWN -> UNKNOWN.|
")

ex_spec <- read_spec("
V|L|T|N|F|C|R|O|M|RV|VV|D|I
STUDYID|Study Identifier|Char|12|||Req|Derived||||DM.STUDYID merged by SUBJID.|
DOMAIN|Domain Abbreviation|Char|2|||Req|Assigned||||Set to \"EX\".|
USUBJID|Unique Subject Identifier|Char|24|||Req|Derived||||DM.USUBJID merged by SUBJID.|Records for subjects not in DM are dropped.
EXSEQ|Sequence Number|Num|8|||Req|Derived||||Sequential per USUBJID after sorting by USUBJID, EXTRT, EXSTDTC.|
EXTRT|Name of Treatment|Char|11|||Req|Collected|DRUG_NAME|EXTRT|Treatment|Source value as-is.|
EXDOSE|Dose|Num|8|||Exp|Collected|DOSE_MG|EXDOSE|Dose|Source value as-is.|
EXDOSU|Dose Units|Char|2||UNIT|Exp|Collected|DOSE_UNIT|EXDOSU|Dose UOM|Upper-cased source value.|Produces \"MG\"; CT submission value is \"mg\".
EXDOSFRM|Dose Form|Char|TBD||FRM|Exp|Assigned||||Set to null.|No source for dose form.
EXROUTE|Route of Administration|Char|12||ROUTE|Perm|Collected|ROUTE|EXROUTE|Administration Route|Upper-cased source value.|
EXSTDTC|Start Date/Time of Treatment|Char|10|||Exp|Collected|START_DATE|EXSTDTC|First Dose Date|Formatted as ISO 8601 (YYYY-MM-DD).|
EXENDTC|End Date/Time of Treatment|Char|10|||Perm|Collected|END_DATE|EXENDTC|Last Dose Date|Formatted as ISO 8601 (YYYY-MM-DD).|
EXSTDY|Study Day of Start of Treatment|Num|8|||Perm|Derived||||EXSTDTC - DM.RFSTDTC (+1 if on/after RFSTDTC).|
EXENDY|Study Day of End of Treatment|Num|8|||Perm|Derived||||EXENDTC - DM.RFSTDTC (+1 if on/after RFSTDTC).|
")

vs_spec <- read_spec("
V|L|T|N|F|C|R|O|M|RV|VV|D|I
STUDYID|Study Identifier|Char|12|||Req|Derived||||DM.STUDYID merged by SUBJID.|
DOMAIN|Domain Abbreviation|Char|2|||Req|Assigned||||Set to \"VS\".|
USUBJID|Unique Subject Identifier|Char|24|||Req|Derived||||DM.USUBJID merged by SUBJID.|Records for subjects not in DM are dropped.
VSSEQ|Sequence Number|Num|8|||Req|Derived||||Sequential per USUBJID after sorting by USUBJID, VSDTC, VSTESTCD.|
VSTESTCD|Vital Signs Test Short Name|Char|6||VSTESTCD|Req|Collected|VITAL_CODE|VSTESTCD|Parameter|Source value as-is.|
VSTEST|Vital Signs Test Name|Char|24||VSTEST|Req|Collected|VITAL_NAME|VSTEST|Parameter Name|Source value as-is.|
VSORRES|Result or Finding in Original Units|Char|5|||Exp|Collected|VALUE|VSORRES|Assessment Value|Converted to character.|Trailing zero dropped (139.0 -> 139).
VSORRESU|Original Units|Char|9||VSRESU|Exp|Collected|UNIT|VSORRESU|Assessment Unit|MMHG -> mmHg; BEATS/MIN -> beats/min; KG -> kg; C -> C.|
VSSTRESC|Character Result/Finding in Std Format|Char|5|||Exp|Derived||||Copy of VSORRES.|No unit conversion applied.
VSSTRESN|Numeric Result/Finding in Standard Units|Num|8|||Exp|Derived||||Numeric VSORRES.|
VSSTRESU|Standard Units|Char|9||VSRESU|Exp|Derived||||Copy of VSORRESU.|
VSLOBXFL|Last Observation Before Exposure Flag|Char|1||NY|Exp|Assigned||||Set to null.|Can be derived from VSDTC and DM.RFXSTDTC.
VISITNUM|Visit Number|Num|8|||Exp|Assigned||||Set to null.|No visit information in raw data.
VSDTC|Date/Time of Measurements|Char|10|||Exp|Collected|VISIT_DATE|VSDTC|Assessment Date|Formatted as ISO 8601 (YYYY-MM-DD).|
VSDY|Study Day of Vital Signs|Num|8|||Perm|Derived||||VSDTC - DM.RFSTDTC (+1 if on/after RFSTDTC).|Derived in vs_new.R but not kept in vs_vars.
")

ds_spec <- read_spec("
V|L|T|N|F|C|R|O|M|RV|VV|D|I
STUDYID|Study Identifier|Char|12|||Req|Derived||||DM.STUDYID.|
DOMAIN|Domain Abbreviation|Char|2|||Req|Assigned||||Set to \"DS\".|
USUBJID|Unique Subject Identifier|Char|24|||Req|Derived||||DM.USUBJID.|
DSSEQ|Sequence Number|Num|8|||Req|Derived||||Sequential per USUBJID after sorting by USUBJID, DSDECOD, DSSTDTC.|Order is alphabetical by DSDECOD, not chronological.
DSTERM|Reported Term for the Disposition Event|Char|25|||Req|Derived|STATUS / REASON|DSDECOD / DSREASND|Disposition Status / Discontinuation Reason|Milestone: \"INFORMED CONSENT OBTAINED\". Event: \"COMPLETED\"; PHYSICIAN DECISION -> upper-cased reason when present, else DSDECOD.|DSTERM should hold the verbatim term; review use of reason text.
DSDECOD|Standardized Disposition Term|Char|25||NCOMPLT|Req|Collected|STATUS|DSDECOD|Disposition Status|Milestone: \"INFORMED CONSENT OBTAINED\". Event: upper-cased status; only COMPLETED and PHYSICIAN DECISION kept.|PROGRESSIVE DISEASE (35) and WITHDRAWN (24) records dropped; WITHDRAWN is not a CT term.
DSCAT|Category for Disposition Event|Char|18||DSCAT|Exp|Assigned||||\"PROTOCOL MILESTONE\" for consent; \"DISPOSITION EVENT\" for status records.|
DSSTDTC|Start Date/Time of Disposition Event|Char|10|||Exp|Collected|STATUS_DATE|DSSTDTC|Disposition Date|Milestone: DM.RFICDTC. Event: source date, ISO 8601.|
DSSTDY|Study Day of Start of Disposition Event|Num|8|||Perm|Derived||||DSSTDTC - DM.RFSTDTC (+1 if on/after RFSTDTC).|
")

#--------------------------------------------------
# Controlled terminology used
#--------------------------------------------------
codelists <- fread(sep = "|", quote = "", colClasses = "character", na.strings = NULL, text = "
Codelist|NCI Code|Term|Raw Values Mapped|Used By|Notes
SEX|C66731|M|M; Male|DM.SEX|
SEX|C66731|F|F; Female|DM.SEX|
RACE|C74457|AMERICAN INDIAN OR ALASKA NATIVE|AMERICAN INDIAN OR ALASKA NATIVE; American Indian Or Alaska Native|DM.RACE|
RACE|C74457|ASIAN|ASIAN; Asian|DM.RACE|
RACE|C74457|BLACK OR AFRICAN AMERICAN|BLACK OR AFRICAN AMERICAN; Black Or African American|DM.RACE|
RACE|C74457|WHITE|WHITE; White|DM.RACE|
RACE|C74457|OTHER|OTHER; Other|DM.RACE|Verify against the study CT version.
ETHNIC|C66790|HISPANIC OR LATINO|HISPANIC OR LATINO; Hispanic Or Latino|DM.ETHNIC|
ETHNIC|C66790|NOT HISPANIC OR LATINO|NOT HISPANIC OR LATINO; Not Hispanic Or Latino|DM.ETHNIC|
ETHNIC|C66790|NOT REPORTED|NOT REPORTED; Not Reported|DM.ETHNIC|
AGEU|C66781|YEARS||DM.AGEU|
NY|C66742|Y|Y; Yes|AE.AESER|
NY|C66742|N|N; No|AE.AESER|
AESEV|C66769|MILD|MILD; Mild|AE.AESEV|
AESEV|C66769|MODERATE|MODERATE; Moderate|AE.AESEV|
AESEV|C66769|SEVERE|SEVERE; Severe|AE.AESEV|
AEREL||NOT RELATED|NOT RELATED; Not Related|AE.AEREL|Sponsor-defined
AEREL||UNLIKELY|UNLIKELY; Unlikely|AE.AEREL|Sponsor-defined
AEREL||POSSIBLE|POSSIBLE; Possible|AE.AEREL|Sponsor-defined
AEREL||PROBABLE|PROBABLE; Probable|AE.AEREL|Sponsor-defined
OUT|C66768|RECOVERED/RESOLVED|RECOVERED; Recovered|AE.AEOUT|
OUT|C66768|RECOVERING/RESOLVING|RECOVERING; Recovering|AE.AEOUT|
OUT|C66768|NOT RECOVERED/NOT RESOLVED|NOT RECOVERED; Not Recovered|AE.AEOUT|
OUT|C66768|UNKNOWN|UNKNOWN; Unknown|AE.AEOUT|
UNIT|C71620|mg|mg|EX.EXDOSU|Current code outputs MG.
ROUTE|C66729|INTRAVENOUS|INTRAVENOUS; Intravenous|EX.EXROUTE|
ROUTE|C66729|ORAL|ORAL; Oral|EX.EXROUTE|
ROUTE|C66729|SUBCUTANEOUS|SUBCUTANEOUS; Subcutaneous|EX.EXROUTE|
VSTESTCD|C66741|DIABP|DIABP|VS.VSTESTCD|
VSTESTCD|C66741|HR|HR|VS.VSTESTCD|
VSTESTCD|C66741|SYSBP|SYSBP|VS.VSTESTCD|
VSTESTCD|C66741|TEMP|TEMP|VS.VSTESTCD|
VSTESTCD|C66741|WEIGHT|WEIGHT|VS.VSTESTCD|
VSTEST|C67153|Diastolic Blood Pressure|Diastolic Blood Pressure|VS.VSTEST|
VSTEST|C67153|Heart Rate|Heart Rate|VS.VSTEST|
VSTEST|C67153|Systolic Blood Pressure|Systolic Blood Pressure|VS.VSTEST|
VSTEST|C67153|Temperature|Temperature|VS.VSTEST|
VSTEST|C67153|Weight|Weight|VS.VSTEST|
VSRESU|C66770|mmHg|mmHg|VS.VSORRESU; VS.VSSTRESU|
VSRESU|C66770|beats/min|beats/min|VS.VSORRESU; VS.VSSTRESU|
VSRESU|C66770|C|C|VS.VSORRESU; VS.VSSTRESU|
VSRESU|C66770|kg|kg|VS.VSORRESU; VS.VSSTRESU|
DSCAT|C74558|PROTOCOL MILESTONE||DS.DSCAT|
DSCAT|C74558|DISPOSITION EVENT||DS.DSCAT|
NCOMPLT|C66727|COMPLETED|COMPLETED; Completed|DS.DSDECOD|
NCOMPLT|C66727|PHYSICIAN DECISION|PHYSICIAN DECISION; Physician Decision|DS.DSDECOD|
NCOMPLT|C66727|PROGRESSIVE DISEASE|PROGRESSIVE DISEASE; Progressive Disease|DS.DSDECOD|Present in raw data but filtered out.
NCOMPLT|C66727|WITHDRAWAL BY SUBJECT|WITHDRAWN; Withdrawn|DS.DSDECOD|Not mapped; raw WITHDRAWN is filtered out.
")

#--------------------------------------------------
# Workbook
#--------------------------------------------------
wb <- createWorkbook()

hdr_style   <- createStyle(textDecoration = "bold", fgFill = "#D9D9D9",
                           border = "TopBottomLeftRight", wrapText = TRUE,
                           valign = "top")
cell_style  <- createStyle(border = "TopBottomLeftRight", wrapText = TRUE,
                           valign = "top")
label_style <- createStyle(textDecoration = "bold")
issue_style <- createStyle(fgFill = "#FCE4D6")

write_table <- function(sheet, x, start_row = 1L) {
  writeData(wb, sheet, x, startRow = start_row, headerStyle = hdr_style)
  if (nrow(x)) {
    addStyle(wb, sheet, cell_style, rows = start_row + seq_len(nrow(x)),
             cols = seq_along(x), gridExpand = TRUE, stack = TRUE)
  }
  freezePane(wb, sheet, firstActiveRow = start_row + 1L)
}

addWorksheet(wb, "Contents")
write_table("Contents", contents)
setColWidths(wb, "Contents", cols = seq_along(contents),
             widths = c(9, 16, 16, 40, 34, 40, 34, 20, 10, 50))

spec_widths <- c(11, 34, 6, 7, 9, 10, 6, 10, 22, 14, 24, 50, 45)
specs <- list(DM = dm_spec, AE = ae_spec, EX = ex_spec, VS = vs_spec, DS = ds_spec)

write_spec_sheet <- function(sheet, ds_name) {
  meta <- contents[Dataset == ds_name]
  header <- data.table(
    Item  = c("Dataset", "Label", "Class", "Structure", "Keys", "Notes",
              "Input Data", "Sorted by"),
    Value = c(meta$Dataset, meta$Label, meta$Class, meta$Structure, meta$Keys,
              meta$Notes, meta$`Input Data`, meta$`Sorted by`)
  )
  addWorksheet(wb, sheet)
  writeData(wb, sheet, header, colNames = FALSE)
  addStyle(wb, sheet, label_style, rows = seq_len(nrow(header)), cols = 1)

  start_row <- nrow(header) + 2L
  spec <- specs[[ds_name]]
  write_table(sheet, spec, start_row)
  issue_rows <- which(spec$`Issues / Notes` != "")
  if (length(issue_rows)) {
    addStyle(wb, sheet, issue_style, rows = start_row + issue_rows,
             cols = seq_along(spec), gridExpand = TRUE, stack = TRUE)
  }
  setColWidths(wb, sheet, cols = seq_along(spec), widths = spec_widths)
}

# Worked example of the layout, using DM
write_spec_sheet("template", "DM")

for (ds_name in names(specs)) {
  write_spec_sheet(ds_name, ds_name)
}

addWorksheet(wb, "Codelists")
write_table("Codelists", codelists)
setColWidths(wb, "Codelists", cols = seq_along(codelists),
             widths = c(11, 10, 30, 50, 24, 40))

saveWorkbook(wb, out_file, overwrite = TRUE)
