library(targets)
source("R/helper.R")
source("R/load.R")
source("R/transform.R")
options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")
tar_option_set(packages = c("tidyverse","readr","lubridate", "vroom", "Hmisc", "dtplyr", "stringdist", "XML", "stringr"))

list(
        # ________________________-----
        # READ IN ----
        # +------Files ----
                tar_target(files,   list.files(recursive = TRUE)),
        # +------DOCS ----
                tar_target(dfDoc,   readTxtFiles(path = "input/DOCS/TXT/")),
                tar_target(df_phys, readPhysicianList(path = "input/PMD/NEUR/NGPA_NEUR-ABESC+.xls")),
        # +------MEDS ----
                tar_target(atc,     readATCClass(path = "input/Arzneimittelliste/atc_codes.csv")),
                tar_target(drugs,   readDrugList(path = "input/Arzneimittelliste/Erweiterte_Arzneimittelliste HAM.xlsx")),
                tar_target(mdx,     readMedxList(file = "Y0000035", files = files)),
        # +------LABS, KENN, DEMOGR ----
                tar_target(labs,    readISHFiles(file = "LAB.csv")),
                tar_target(kenn,    readISHFiles(file = "KENNZAHLEN.csv")),
                tar_target(demog,   readISHFiles(file = "DEMOGR.csv")),
                tar_target(ops,     readISHFiles(file = "OPS.csv")),
        # +------HEIREKA-DB ----
                tar_target(db_heireka, readDB(file = "LysePat")),
        # +------MISC-DB ----
                tar_target(db_stamm, readDB(file = "Stammdaten")),
                tar_target(db_dop, readDB(file = "Doppler2002")),
        # +------dPDF ----
                tar_target(dok_nr, readDokNr("ZVNDOCDRAW", files)),
                tar_target(dpdf, readDPDFs("DPDF", files)),
        # +------NECHO ----        
                tar_target(necho_17ff, readISHFiles(file = "ZNEUR00000000000")),
                tar_target(necho_17ff_txt, readISHFiles(file = "N2PMDETEXT", sep = "\t")),
                tar_target(necho_02_db, readDB(file = "Kardiologie2002")),
        # ________________________-----
        # TRANSFORM ----
        # +------DOCS ----
                tar_target(docs, transformToDocTypeList(dfDoc)),
                tar_target(docType, getDocTypes(dfDoc)),
                tar_target(df_neur_not, getNewVar(dfDoc, docType = "NEUR-NOT__")),
                tar_target(df_neur_aufnb, getNewVar(dfDoc, docType = "NEUR-AUFNB")),
                tar_target(df_neur_stwbr, getNewVar(dfDoc, docType = "NEUR-STWBR")),
               
        # +------MEDS ----
                tar_target(df_meds_prior, getMedsPrior(df_neur_not, df_neur_aufnb, df_neur_stwbr)),
                tar_target(atc_drugs, joinAtcToDrugs(atc, drugs)),
                tar_target(df_atc_drugs, prepDfAtc_drugs(atc_drugs)),
                tar_target(df_drugs, getNewDrugVar(drugs, atc_drugs, df_meds_prior)),
                tar_target(df_mdx_discharge, readMedxDischarge("Y0000035", files)),
        # +------LABS ----
                tar_target(lab_vars_names, getLabVarsNames(labs)),
                tar_target(labs_all, getLabs(labs, case_id, lab_vars_names, kenn)),
        # +------HEIREKA-DB ----
                tar_target(df_hei, transformHeirekaDB(db_heireka, demog)),
                tar_target(case_id, getCaseId(df_hei)),
                tar_target(df_hei_stamm, joinHeiStamm(df_hei, db_stamm)),
                
        # +------MISC-DB ----
                tar_target(dop, getDop(db_dop, df_hei_stamm, mode = "ALL")),
                tar_target(dop2, extractFromDPDFs(file = "NEUR_NSONO", dpdf = dpdf, dok_nr = dok_nr)),
                tar_target(df_dop, combineDopSources(dop, dop2)),
                
        # +------NECHO ----               
                tar_target(necho_16pre, getNewVar(dfDoc, docType = "NEUR-NECHO")),
                tar_target(df_necho, combineNechoSources(necho_17ff, necho_17ff_txt, necho_16pre, necho_02_db))
        # ________________________-----
        # GET NEW VARIABLES ----
        
)






