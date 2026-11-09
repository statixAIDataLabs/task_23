library(data.table)
library(dplyr)

sdtm_raw <- "Mock_EDC//raw_csv"
dm_marvin <- fread(file.path(sdtm_raw,"MARVIN_DM.csv"))
ae_marvin <- fread(file.path(sdtm_raw,"MARVIN_AE.csv"))
ds_marvin <- fread(file.path(sdtm_raw,"MARVIN_DS.csv"))
ex_marvin <- fread(file.path(sdtm_raw,"MARVIN_EX.csv"))
vs_marvin <- fread(file.path(sdtm_raw,"MARVIN_VS.csv"))
dm_rave <- fread(file.path(sdtm_raw,"RAVE_DM.csv"))
ae_rave <- fread(file.path(sdtm_raw,"RAVE_AE.csv"))
ds_rave <- fread(file.path(sdtm_raw,"RAVE_DS.csv"))
ex_rave <- fread(file.path(sdtm_raw,"RAVE_EX.csv"))
vs_rave <- fread(file.path(sdtm_raw,"RAVE_VS.csv"))
dm_veeva <- fread(file.path(sdtm_raw,"VEEVA_DM.csv"))
ae_veeva <- fread(file.path(sdtm_raw,"VEEVA_AE.csv"))
ds_veeva <- fread(file.path(sdtm_raw,"VEEVA_DS.csv"))
ex_veeva <- fread(file.path(sdtm_raw,"VEEVA_EX.csv"))
vs_veeva <- fread(file.path(sdtm_raw,"VEEVA_VS.csv"))

dm1 <- dm_veeva %>%  rename(STUDYID=Study,SITEID=`Site Number`,SUBJID=`Subject ID`,
                            RFICDTC = `ICF Date`,BRTHDTC=`Year of Birth`,SEX=Gender,
                            RACE=Race1, ETHNIC=Ethnicity)

dm2 <- dm_rave %>% rename(STUDYID=STUDY, SITEID=CENTER,SUBJID=SUBJID,
                          RFICDTC=ICFDT,BRTHDTC=BRTHYR, SEX=SEXCD,
                          RACE=RACECD, ETHNIC=ETHNICCD)

dm3 <- dm_marvin %>% rename(SUBJID=SUBJECT,RFICDTC=INFORMED_CONSENT_DATE,
                            BRTHDTC=BIRTH_YEAR)
dm_combined <- bind_rows(dm1,dm2,dm3)


ex1 <- ex_marvin %>%
  rename(
    SUBJID = SUBJECT,
    EXTRT   = DRUG_NAME,
    EXDOSE  = DOSE_MG,
    EXDOSU  = DOSE_UNIT,
    EXROUTE = ROUTE,
    EXSTDTC = START_DATE,
    EXENDTC = END_DATE
  )

ex2 <- ex_rave 
# Other columns are already SDTM compliant

ex3 <- ex_veeva %>%
  rename(
    SUBJID = `Subject ID`,
    EXTRT   = Treatment,
    EXDOSE  = Dose,
    EXDOSU  = `Dose UOM`,
    EXROUTE = `Administration Route`,
    EXSTDTC = `First Dose Date`,
    EXENDTC = `Last Dose Date`
  )

ex_combined <- bind_rows(ex1, ex2, ex3)


ae1 <- ae_marvin %>%
  rename(
    SUBJID   = SUBJECT,
    AETERM   = AE_TERM,
    AESTDTC  = AE_START,
    AEENDTC  = AE_END,
    AESEV    = AE_SEV,
    AESER    = AE_SERIOUS,
    AEREL    = AE_RELATION,
    AEOUT    = AE_OUTCOME
  )

ae2 <- ae_rave %>%
  rename(
    AESTDTC = AESTDT,
    AEENDTC = AEENDT
  )

ae3 <- ae_veeva %>%
  rename(
    SUBJID   = `Subject ID`,
    AETERM   = `Reported Term`,
    AESTDTC  = `Onset Date`,
    AEENDTC  = `Resolution Date`,
    AESEV    = Severity,
    AESER    = `Serious?`,
    AEREL    = Relatedness,
    AEOUT    = Status
  )
ae_combined <- bind_rows(ae1, ae2, ae3)



vs1 <- vs_marvin %>%
  rename(
    SUBJID   = SUBJECT,
    VSDTC    = VISIT_DATE,
    VSTESTCD = VITAL_CODE,
    VSTEST   = VITAL_NAME,
    VSORRES  = VALUE,
    VSORRESU = UNIT
  )

vs2 <- vs_rave

vs3 <- vs_veeva %>%
  rename(
    SUBJID   = `Subject ID`,
    VSDTC    = `Assessment Date`,
    VSTESTCD = Parameter,
    VSTEST   = `Parameter Name`,
    VSORRES  = `Assessment Value`,
    VSORRESU = `Assessment Unit`
  )

vs_combined <- bind_rows(
  vs1,
  vs2,
  vs3
)

ds1 <- ds_marvin %>%
  rename(
    SUBJID   = SUBJECT,
    DSDECOD  = STATUS,
    DSSTDTC  = STATUS_DATE,
    DSREASND = REASON
  )

ds2 <- ds_rave

ds3 <- ds_veeva %>%
  rename(
    SUBJID   = `Subject ID`,
    DSDECOD  = `Disposition Status`,
    DSSTDTC  = `Disposition Date`,
    DSREASND = `Discontinuation Reason`
  )

ds_combined <- bind_rows(
  ds1,
  ds2,
  ds3
)