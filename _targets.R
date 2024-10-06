library(targets)
source("R/helper.R")
source("R/load.R")
source("R/transform.R")
source("R/res.R")
source("R/plot.R")
source("R/chatgpt.R")
source("R/mmi.R")
source("R/predict_mod.R")
source("R/parse_ris.R")
# source("R/mod_print.blrm.R")
options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")
tar_option_set(packages = c("tidyverse","readr","lubridate", "vroom", "Hmisc", "dtplyr", "stringdist", "XML", "stringr", "tools", "haven", "yaml","parallelly"))

list(
        # ________________________-----
        # READ IN ----
        ## +------Files ----
                tar_target( files,                   list.files(recursive = TRUE)),
        # +------DOCS ----
                tar_target( dfDoc,                   readTxtFiles(path = "input/DOCS/TXT/")),
                tar_target( df_phys,                 readPhysicianList(path = "input/PMD/NEUR/NGPA_NEUR-ABESC+.xls")),
        # +------MEDS ----
                tar_target( atc,                     readATCClass(path = "input/Arzneimittelliste/atc_codes.csv")),
                tar_target( drugs,                   readDrugList(path = "input/Arzneimittelliste/Erweiterte_Arzneimittelliste HAM.xlsx")),
                tar_target( mdx,                     readMedxList(file = "Y0000035", files = files)),
                tar_target( mmi,                     readMMIFiles(path="input/Arzneimittelliste/mmi/MMI_RohdatenR3/mmiPharmindexR3_20210315MAIN/")),
        # +------LABS, KENN, DEMOGR ----
                tar_target( dok_nr,                  readDokNr("ZVNDOCDRAW", files)),
                tar_target( labs,                    readISHFiles(file = "LAB.csv")),
                tar_target( kenn,                    readISHFiles(file = "KENNZAHLEN.csv")),
                tar_target( demog,                   readISHFiles(file = "DEMOGR.csv")),
                tar_target( ops,                     readISHFiles(file = "OPS.csv")),
                tar_target( dia,                     readISHFiles(file = "DIA_SEC.csv")),
                tar_target( fakar,                   readISHFiles(file = "FAKAR.csv")),
                tar_target( ze,                      readISHFiles(file = "ZE.csv")),
                tar_target( qs,                      readPMDFile(files, file = "ZIQS80_1.xls", dok_nr)),
                tar_target( qs_cas,                  readPMDFile(files, file = "ZIQS10_2", dok_nr)),
                tar_target( dia_neur,                readPMDFile(files, file = "NDIA_NEUR", dok_nr)),
                tar_target( pat_neur,                readPMDFile(files, file = "NPAT_NEUR", dok_nr)), # for in-hospital-death / date / time
        
        # +------HEIREKA-DB ----
                tar_target( db_heireka,              readDB(files, file = "LysePat")),
        # +------MISC-DB ----
                tar_target( db_stamm,                readDB(file = "Stammdaten")),
                tar_target( db_dop,                  readDB(file = "Doppler2002")),
        # +------dPDF ----
                tar_target( dpdf,                    readDPDFs("DPDF", files)),
        # +------NECHO ----        
                tar_target( necho_17ff,              readISHFiles(file = "ZNEUR00000000000")),
                tar_target( necho_17ff_txt,          readISHFiles(file = "N2PMDETEXT", sep = "\t")),
                tar_target( necho_02_db,             readDB(file = "Kardiologie2002")),
        # ________________________-----
        # TRANSFORM ----
        # +------DOCS ----
                tar_target( docs,                    transformToDocTypeList(dfDoc)),
                tar_target( docType,                 getDocTypes(dfDoc)),
                tar_target( df_neur_not,             getNewVar(dfDoc, docType = "NEUR-NOT__")),
                tar_target( df_neur_aufnb,           getNewVar(dfDoc, docType = "NEUR-AUFNB")),
                tar_target( df_neur_stwbr,           getNewVar(dfDoc, docType = "NEUR-STWBR")),
                tar_target( df_neur_int,             getNewVar(dfDoc, docType = "NEUR-INT__")),
                tar_target( df_neur_stabr,           getNewVar(dfDoc, docType = "NEUR-STABR")),
        # +------MEDS ----
                tar_target( df_meds_prior,           getMedsPrior(df_neur_not, df_neur_aufnb, df_neur_stwbr, df_neur_int, atc)), # mmi not needed any more
                tar_target( df_mdx_discharge,        readMedxDischarge("Y0000035", files, mmi, atc)),
                # tar_target(atc_drugs, joinAtcToDrugs(atc, drugs)),
                # tar_target(df_atc_drugs, prepDfAtc_drugs(atc_drugs)),
                # tar_target(df_drugs, getNewDrugVar(drugs, atc_drugs, df_meds_prior)),
                
        # +------LABS ----
                tar_target( lab_vars_names,          getLabVarsNames(labs)),
                tar_target( lab_lipid,               extractLab(labs, case_id, lab_select = c("Triglyceride", "HDL-Cholesterin", "LDL-Cholesterin", "LDL-Chol.(berechn.)", "Cholesterin", "VLDL-Cholesterin"), kenn)),
                tar_target( lab_HbA1c,               extractLab(labs, case_id, lab_select = c("HbA1c", "HBA1IFCC","HbA1c (IFCC)"), kenn, timeCutEnd = 999)),
                tar_target( lab_glu,                 extractLab(labs, case_id, lab_select = c("Glucose/Tstr.","Glucose nue.","Glucose nue. (BGA)","Glucose (Plasma)","Glucose nue. POCT","Glucose","Glucose/Plasma"), kenn, timeCutEnd = 10080, N=TRUE, max=TRUE, median=TRUE, q25=TRUE, q75=TRUE, min=TRUE)),
                tar_target( lab_cardio,              extractLab(labs, case_id, lab_select = c("CK-MB","hs-TroponinT","TNT","NT-ProBNP", "NT-BNP"), kenn, timeCutEnd = 10080, max=TRUE, median=TRUE, q25=TRUE, q75=TRUE, min=TRUE, classify=TRUE)),
                tar_target( lab_haem,                extractLab(labs, case_id, lab_select = c("Haemoglobin", "Haematokrit", "MCV", "MCH", "MCHC"), kenn, timeCutEnd = 999, max=TRUE, median=TRUE, q25=TRUE, q75=TRUE, min=TRUE)),
                tar_target( lab_nephr,               extractLab(labs, case_id, lab_select = c("Kreatinin", "Harnstoff", "Harnsaeure", "GFR n. CKD-EPI", "GFR, CKD-EPI", "GFR/Cockcroft"), kenn, timeCutEnd = 999, max=TRUE, median=TRUE, q25=TRUE, q75=TRUE, min=TRUE)),
                tar_target( lab_thyr,                extractLab(labs, case_id, lab_select = c("TSH", "freies Trijodthyronin (fT3)", "freies T3", "FT3", "reies Thyroxin (fT4)", "freies T4", "FT4"), kenn, timeCutEnd = 999, max=TRUE, median=TRUE, q25=TRUE, q75=TRUE, min=TRUE)),
                tar_target( lab_infl,                extractLab(labs, case_id, lab_select = c("Leukozyten", "CRP", "PCT sens.", "PCTST", "Procalcitonin-ST", "Procalcitonin sens.", "Fibrinogen", "IL-6"), kenn, timeCutEnd = 999, max=TRUE, median=TRUE, q25=TRUE, q75=TRUE, min=TRUE)),
                tar_target( lab_liver,               extractLab(labs, case_id, lab_select = c("GGT", "GPT/ALT", "GOT/AST", "CHE", "LDH", "Ammoniak","Ges.Bilirubin", "Dir.Bilirubin", "Albumin quant."), kenn, timeCutEnd = 999, max=TRUE, median=TRUE, q25=TRUE, q75=TRUE, min=TRUE)),
                tar_target( lab_coag,                extractLab(labs, case_id, lab_select = c("INR", "INR - ber.", "aPTT", "Thrombinzeit", "Thrombozyten"), kenn, timeCutEnd = 999, max=TRUE, median=TRUE, q25=TRUE, q75=TRUE, min=TRUE)),
                tar_target( lab_ustix,               extractLab(labs, case_id, lab_select = c("Ketone /Tstr.", "Leuko  /Tstr.", "Urobil /Tstr.", "Bilirub/Tstr.", "Nitrit /Tstr.", "pH     /Tstr.", "Erys   /Tstr.", "Glucose/Tstr.","Eiweiss/Tstr.", "SpezGew/Tstr.berechnet", "SpezGew/Tstr.", "Eiweiß/Tstr.", "Bili/Tstr.", "pH/Tstr."), kenn, timeCutEnd = 999, max=FALSE, median=FALSE, q25=FALSE, q75=FALSE, min=FALSE, slicing=FALSE)),
                tar_target( lab_adm,                 extractLab(labs, case_id, lab_select = paste0(lab_vars_names$value), kenn, timeCut = TRUE, timeCutStart = "-20", timeCutEnd = "120", slicing = TRUE, classify=TRUE)),
        # +------HEIREKA-DB ----
                tar_target( df_hei,                  transformHeirekaDB(db_heireka, demog)),
                tar_target( case_id,                 getCaseId(df_hei)),
                tar_target( df_hei_stamm,            joinHeiStamm(df_hei, db_stamm)),
                tar_target( pat_ident,               getPatientIdentifier(df_hei, demog, dok_nr, year_min=2015)),
                tar_target( df_base,                 chunkDf_Hei(df_hei)),
        
        # +------MISC-DB ----
                tar_target( dop,                     getDop(db_dop, df_hei_stamm, mode = "ALL")),
                tar_target( dop2,                    extractFromDPDFs(file = "NEUR_NSONO", dpdf = dpdf, dok_nr = dok_nr)),
                tar_target( df_dop,                  combineDopSources(dop, dop2, df_hei)),
        
        # +------NECHO ----               
                tar_target( necho_16pre,             getNewVar(dfDoc, docType = "NEUR-NECHO")),
                tar_target( df_necho_c,              combineNechoSources(necho_17ff, necho_17ff_txt, necho_16pre, necho_02_db, df_hei, df_hei_stamm)),
                tar_target( df_necho,                getNechoNewVar(df_necho_c)),
        # ________________________-----
        # NEW VARIABLES ----        
        ## Characteristics -----
                tar_target( pRS,                     getPraemorbidRankin(df_hei, qs)),
                tar_target( var_handedness,          getHandedness(df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr, case_id)),
                tar_target( var_aphasia,             getAphasia(db_heireka, df_hei, qs, dia, case_id, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( var_aphasia_adm,         getAphasiaAdmission(case_id, df_neur_not, df_neur_aufnb, df_neur_stwbr, df_neur_int)),
                tar_target( var_neglect,             getNeglect(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( var_vigil_adm,           getVigilanceAdmission(case_id, df_neur_not, df_neur_aufnb, df_neur_int, df_neur_stwbr, df_neur_stabr)),
                tar_target( var_eyedev_adm,          getEyeDeviationAdmission(case_id, df_neur_not, df_neur_aufnb, df_neur_int, df_neur_stwbr, df_neur_stabr)),
                tar_target( var_microangio,          getMicroangiopathie(case_id, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( var_prev_stroke,         getPrevStroke(case_id, qs, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr, df_hei)),
                tar_target( var_prev_mi,             getPrevMi(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( var_prev_thyr,           getPrevThyr(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr, lab_thyr, labs, kenn)),
                tar_target( var_prev_alc,            getPrevAlc(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr, labs, kenn)),
                tar_target( risk_factor_alc_adm,     getAlcoholAdmission(case_id, df_neur_aufnb)),
                tar_target( dx_dementia,             getDxDementia(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( var_prev_dementia,       getPrevDementia(case_id, df_neur_aufnb, df_neur_not, df_neur_int)),
                tar_target( dx_prev_copd,            getPrevCOPD(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( var_stroke_care,         getStrokeCare(case_id, ops, fakar, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( dx_rf_smoke,             getPrevSmoke(case_id, df_hei, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)),
        ## Etiology ----
                tar_target( etio_pfo,                getPfo(case_id, dia)),
                tar_target( etio_endocarditis,       getEndocarditis(case_id, dia)),
                tar_target( etio_cs,                 getCarotidStenosis(case_id, dia)),
        ## Outcome -----        
                tar_target( outcome_delir,           getDelir(case_id, dia, df_neur_stwbr, df_neur_int, df_neur_stabr, labs, kenn)),  
                tar_target( outcome_mi,              getMi(case_id, dia)),
                tar_target( outcome_nihss_discharge, getNihssDischarge(case_id, df_neur_stwbr, df_neur_int, df_neur_stabr, df_hei)),
                tar_target( outcome_lengthHospStay,  getLengthHospStay(case_id, kenn)),
                tar_target( outcome_ventilationDays, getVentilationDays(case_id, kenn)),
                tar_target( outcome_pneumonia,       getPneumonia(case_id, qs, dia, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( outcome_uti,             getUrinaryTractInfection(case_id, dia, df_neur_stwbr, df_neur_int, df_neur_stabr, lab_ustix)),
                tar_target( outcome_sepsis,          getSepsis(case_id, dia, df_neur_stwbr, df_neur_int, df_neur_stabr)),
                tar_target( df_barthel_index,        getBarthelHospDischarge(case_id, qs)),                
                tar_target( outcome_palliativeCare,  getPalliativeCare(case_id, qs)),
                tar_target( outcome_tvt,             getTvt(case_id, dia)),
                tar_target( outcome_lae,             getLae(case_id, dia)),
        #tar_target(outcome_ihd, getIhd(case_id, pat_neur, df_hei)), # in-hospital death / date / time
        #?allergisch reaktion / epistaxis R04.0
        
        # _________________________-----
        # PREPARE DATASET----  
                tar_target(df_pack, prepDf(lab_lipid, lab_HbA1c, lab_glu, lab_cardio, lab_haem, lab_nephr, lab_thyr, lab_infl, lab_liver, lab_coag, lab_adm,
                                      df_base, df_necho, df_dop, pRS, var_handedness, var_vigil_adm, var_aphasia, var_aphasia_adm, var_neglect, var_eyedev_adm, var_prev_stroke, var_prev_mi,
                                      var_prev_thyr, var_prev_alc, var_microangio, risk_factor_alc_adm, var_prev_dementia, dx_prev_copd, var_stroke_care, dx_rf_smoke, etio_pfo,
                                      etio_endocarditis, etio_cs, outcome_delir, outcome_mi, outcome_nihss_discharge, outcome_lengthHospStay,
                                      outcome_ventilationDays, outcome_pneumonia, outcome_uti, outcome_sepsis, df_barthel_index,
                                      outcome_palliativeCare,outcome_tvt, outcome_lae,
                                      df_meds_prior,
                                      print=FALSE,
                                      caseid=FALSE)),
        ## Admission DF ----
        # including meds GROUP
                tar_target(df_adm, joinPacks(list(df_pack$base, df_pack$date_time_old %>% select(case_id, time_window), df_pack$risk_factor, df_pack$logistics %>% select(-Doktor), df_pack$clinic, df_pack$labs_HEI, df_pack$lab_adm, df_pack$meds_prior_GROUP, df_pack$outcome$outcome_delir, df_pack$therapy[[1]]))),
                tar_target(df_adm_f, filterDf(df_adm)),
        # including meds SUBSTANCE
                tar_target(df_adm_s, filterDf(joinPacks(list(df_pack$base, df_pack$date_time_old %>% select(case_id, time_window), df_pack$risk_factor, df_pack$logistics %>% select(-Doktor), df_pack$clinic, df_pack$labs_HEI, df_pack$lab_adm, df_pack$meds_prior_SUBSTANCE, df_pack$outcome$outcome_delir, df_pack$therapy[[1]])))),
        # including meds CLASS
                tar_target(df_adm_c, filterDf(joinPacks(list(df_pack$base, df_pack$date_time_old %>% select(case_id, time_window), df_pack$risk_factor, df_pack$logistics %>% select(-Doktor), df_pack$clinic, df_pack$labs_HEI, df_pack$lab_adm, df_pack$meds_prior_CLASS, df_pack$outcome$outcome_delir, df_pack$therapy[[1]])))),
                
        
        
                # tar_target(df_adm2, joinPacks(list(df_pack$base, df_pack$date_time_old %>% select(case_id, time_window), df_pack$risk_factor, df_pack$logistics %>% select(-Doktor), df_pack$clinic, df_pack$labs_HEI, df_pack$lab_adm, df_pack$meds_prior_SUBSTANCE, df_pack$outcome$outcome_delir))),
                tar_target(df_full, joinPacks(list(df_pack$base, df_pack$date_time_old %>% select(case_id, time_window), df_pack$risk_factor, df_pack$logistics %>% select(-Doktor), df_pack$clinic,
                                                   df_pack$lab_packs$lab_cardio,
                                                   df_pack$lab_adm,
                                                   df_pack$etiology %>% select(case_id, etio_mtoast, etio_afib_new, etio_dissection, etio_stroke_mimic, etio_pfo),
                                                   df_pack$meds_prior_SUBSTANCE,
                                                   df_pack$outcome$outcome_delir,
                                                   df_pack$outcome$outcome_HEI,
                                                   df_pack$outcome$outcome_nihss_discharge,
                                                   df_pack$outcome$outcome_stroke_care,
                                                   df_pack$outcome$outcome_mi,
                                                   df_pack$outcome$outcome_pneumonia,
                                                   df_pack$outcome$outcome_sepsis,
                                                   df_pack$outcome$outcome_lae,
                                                   df_pack$outcome$outcome_tvt,
                                                   df_pack$outcome$df_barthel_index %>% select(case_id, contains("dis")),
                                                   df_pack$therapy[[1]]))),
                 tar_target(df_full_f,             filterDf(df_full)),
        
        
        # tar_target(df_d3)
                # tar_target(df_dis)
                tar_target(data_dic,               read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)),
                # tar_target(d, clearForAnalysis(df))
        # # ________________________-----
        # ANALYSIS----
        tar_target(res_del_age, getResDelAge(df_adm_f)),
        # tar_target(res_del_prs, getResDelPrs(df_adm_f)),
        tar_target(res_del_nihss, getResDelNihss(df_adm_f)),
        tar_target(res_del_year, getResDelYear(df_adm_f))

         
)







