#' @title getDocTypes
#' @description Function provides a list of available document types 
#' @param dfDoc Provide data frame where documents are stored, 
#' @param doc_type param "doc_type" as document type in dfDoc
#'
#' @return A list of available document types
#' @export
#'
#' @examples
getDocTypes <- function(dfDoc, doc_type = doc_type){
        return(unique(dfDoc$doc_type))
}


#' @title getRegexVarDef
#' 
#' @description - Constructs a df/tibble with variable definitions for use with regex to extract text information
#' @param docType default = all, user may select individual options (e.g. "NEUR-AUFNB")
#' @return Data Frame with (variable definition names, string, and category to be used in)
#' @export
getRegexVarDef <- function(docType = getDocTypes(dfDoc)) {
        docType <- enquo(docType)
        v <- tribble(
                ~varDef_name, ~varDef_str,
                "adm_meds_aufnb", "(?i)(medikamente:|medikation:)(.*?)(?=Händig|AUFNAHMEBEFUND)",
                "adm_nihss_validation" , "(?<=NIHS{1,3}.{0,4})\\d{1,2}",
                "adm_pRS_validation" , "(?<=(([p|P][m|M]?[r|R][s|S])| pr[e|ä](morbid)?).{0,10})\\d",
                "handed_right" , "(?<=Händigkeit:\\W)\\d",
                "handed_left" , "(?<=Händigkeit: \\d\\Wrechts\\W)\\d(?=\\Wlinks)",
                "dx_prev_allergy" , "(?<=Allergien:)(.*?)(?=VOR) | (Allergie)",
                "dx_prev_stroke_str" , "(?<=Frühere Infarkte:\\W)(.*?)(?=KHK)",
                "dx_prev_chd_str" , "(?<=KHK/Herzinfarkt:\\W)(.*?)(?=pAVK)",
                "dx_prev_pad_str" , "(?<=pAVK:\\W)(.*?)(?=Krebs)",
                "dx_prev_cancer_str" , "(?<=Krebs:\\W)(.*?)(?=Augenerkrankungen)",
                "dx_prev_opthalm_str" , "(?<=Augenerkrankungen:\\W)(.*?)(?=Nierenerkrankungen)",
                "dx_prev_nephr_str" , "(?<=Nierenerkrankungen:\\W)(.*?)(?=SD-Erkrankungen)",
                "dx_prev_thyr_str" , "(?<=SD-Erkrankungen:\\W)(.*?)(?=Metall)",
                "dx_rf_htn" , "(?<=Hypertonus:\\W)(.*?)(?=Diabetes)",
                "dx_rf_dm" , "(?<=Diabetes:\\W)(.*?)(?=Hyper)",
                "dx_rf_hch" , "(?<=Hypercholesterinämie\\W{1,2})(.*?)(?=Vorhofflimmern)",
                "dx_rf_afib" , "(?<=Vorhofflimmern:\\W)(.*?)(?=Rauchen)",
                "dx_rf_smoke_str" , "(?<=Rauchen:\\W)(.*?)(?=Alkohol)",
                "dx_rf_smoke_py" , "([p|P](ack)?[Y|y](ears?)?\\W{0,2})\\d{1,2}",
                "dx_rf_alc_str" , "(?<=Alkohol:\\W)(.*?)(?=Familie|Pille)",
                "dx_rf_hormone_str" , "(?<=Pille:\\W)(.*?)(?=Migräne|Familie)",
                "dx_rf_migraine_str" , "(?<=Migräne:\\W)(.*?)(?=Familie|TECHN)",
                "dx_rf_family" , "(?<=Familie)(.*?)(?=TECHN)"
        ) %>% mutate(varDef_cat = "NEUR-AUFNB")
        
        v2 <- tribble(
                ~varDef_name, ~varDef_str,
                "er_vitals", "(?<=VITALPARAMETER bei Aufnahme:\\W)(.*?)(?=Relevante V)",
                "er_vitals_rr", "(?<=RR:)(.*?)(?=HF)",
                "er_vitals_rrsys", "\\d{2,3}(?=\\W)",
                "er_vitals_rrdia", "(?<=\\d{2,3}\\W)\\d{2,3}(?=\\W)",
                "er_vitals_hr", "(?<=HF:\\W?)\\d{1,3}(?=\\W)",
                # "er_vitals_spo2", "(?<=SpO2:)(.*?)(?=T:)",
                "er_vitals_spo2", "(?<=SpO2:\\W?)\\d{1,3}(?=\\W?%)",
                "er_vitals_temp", "(?<=T:\\W?)(.*?)(?=°C)",
                "er_vitals_tempsite_rect", "(?<=°C\\W?\\W?)\\d(?=rektal)",
                "er_vitals_tempsite_tymp", "(?<=rektal\\W?)\\d(?=tympanal)",
                "er_tx_str", "(?<=Notfalltherapie:)(.*?)(?=Proced)",
                "er_physician", "(?<=Entlassung gegen ärztlichen Rat)(.*?)(?=rzt)",
                "adm_meds_not", "(?i)(medikamente:|medikation:)(.*?)(?=(kurz)?anamnese)"
        ) %>% mutate(varDef_cat = "NEUR-NOT__")
        
        v3 <- tribble(
                ~varDef_name, ~varDef_str,
                # NECHO | String definitions for NECHO necho variables
                # variables to use
                "necho_pic_quality", "(?<=Bildqualität:)(.*?)(?=Nummer)",
                "necho_aortic_root", "(?<=Aortenwurzel\\W)\\d{1,3}",
                "necho_left_atrium", "(?<=Linker Vorhof\\W)\\d{1,3}",
                "necho_septum", "(?<=Septum\\W)\\d{1,3}",
                "necho_hinterwand", "(?<=Hinterwand\\W)\\d{1,3}",
                "necho_lv_enddia", "(?<=LV enddiastol\\.\\W)\\d{1,3}",
                "necho_ef", "(?<=EF\\W)\\d{1,3}",
                "necho_rv", "(?<=RV\\W)\\d{1,3}",
                "necho_vci", "(?<=Vena cava inf\\W)\\d{1,3}",
                "necho_pa_pressure", "(?<=PA\\WDruck\\W)\\d{1,3}",
                "necho_heart_rhythm", "(?<=Herzrhythmus\\W)\\w",
                "necho_aortic_valve", "(?<=Aortenklappe)(.+?)(?=Mitralklappe)",
                "necho_aortic_insuf", "",
                "necho_aortic_pht", "",
                "necho_aortic_dez", "",
                "necho_aortic_sten", "",
                "necho_aortic_ppg", "",
                "necho_aortic_mpg", "",
                "necho_aortic_kof", "",
                "necho_mitral_valve", "(?<=Mitralklappe)(.+?)(?=Tricuspidalklappe)",
                "necho_mitral_insuf", "",
                "necho_mitral_sten", "",
                "necho_mitral_ppg", "",
                "necho_mitral_mpg", "",
                "necho_mitral_kof", "",
                "necho_mitral_padr", "",
                "necho_tric_valve",  "(?<=Tricuspidalklappe)(.+?)(?=Pulmonalklappe)",
                "necho_tric_insuf", "",
                "necho_pulm_valve",  "(?<=Pulmonalklappe)(.+?)(?=Befund)",
                "necho_question", "(?<=Fragestellung:)(.+?)(?=Medikation)",
                "necho_search_endok", "(?<=Diagnose)(.+?)Endokarditis(.+?)(?=Medikation)",
                "necho_befund", "(?<=Befund:).+"
        ) %>% mutate(varDef_cat = "NEUR-NECHO")
        
        v4 <- tribble(
                ~varDef_name, ~varDef_str,
                "adm_meds_stwbr", "(?i)(medikamente:|medikation:)(.*?)(?=AUFNAHMEBEFUND|NEURORAD)"
                ) %>% mutate(varDef_cat = "NEUR-STWBR")
        
        use <- bind_rows(v, v2, v3, v4)
        names(use$varDef_str) <- use$varDef_name
        
        use <- use %>% 
                # nest(data = c(varDef_name, varDef_str)) %>%
                 filter(varDef_cat %in% !!docType) %>% 
                select(varDef_str)
        return(use)
}


#' @title getNewVar
#' 
#' @description From a data frame with character strings in "txt" column new variables are extracted using regex string depending on chosen docType
#' @param df - A dataframe with column "txt" to extract data from
#' @param docType - docType to extract data from - default is all of supplied dataframe 
#' @return DataFrame (that is supplied) including new variables in columns
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract
#' @export
getNewVar <- function(df, txt = txt, docType = getDocTypes(df)) {
        docType <- enquo(docType)
        var <- getRegexVarDef(!!docType)[[1]]
        df <- df %>% filter(doc_type == !!docType) %>%
                mutate(map_df(txt, function(x)
                        map_df(var, function(y)
                                str_extract(x, y))))
}


#' @title getMedsPrior
#'
#' @description Meds are extracted from several documents in the following priority order STWBR > AUFNB > NOT 
#' @return A dataframe
#' @export
#' @import tidyverse
#'
#' @examples
getMedsPrior <- function(df_neur_not,
                         df_neur_aufnb,
                         df_neur_stwbr) {
        df_neur_not <- df_neur_not %>% select(fallnummer, adm_meds_not)
        df_neur_aufnb <- df_neur_aufnb %>% select(fallnummer, adm_meds_aufnb)
        df_neur_stwbr <- df_neur_stwbr %>% select(fallnummer, adm_meds_stwbr)
        
        m <- full_join(
                full_join(df_neur_not,
                          df_neur_aufnb,
                          by = c("fallnummer" = "fallnummer")),
                df_neur_stwbr,
                by = c("fallnummer" = "fallnummer")
        ) %>%
                mutate(meds = coalesce(adm_meds_stwbr,
                                       adm_meds_aufnb,
                                       adm_meds_not)) %>%
                select(-c(adm_meds_stwbr, adm_meds_aufnb, adm_meds_not))

        m$meds <- str_replace_all(m$meds, "- ","-")
        m$meds <- str_replace_all(m$meds, " -","-")
        m$meds <- str_replace_all(m$meds, "½","0,5")
        return(m)
}


#' @title joinAtcToDrugs
#'
#' @param atc 
#' @param drugs 
#' @import tidyverse
#' @return joined dataframe
#' @export
joinAtcToDrugs <- function(atc, drugs){
        left_join(atc,
                  drugs %>%  select(atc_code, drug_name, subst1),
                  by = c("ATC" = "atc_code"))
}


#' @title createStrFromATCs
#'
#' @description #' creatStrFromATCs String is constructed from 1) Substance Name 2) Drug Name 3) abbreviated version of 1 & 2 with first 6 letters
#' @param df A data.frame or tibble from getATC_Level function
#'
#' @return a Regex string 
#' @export
#'
#' @examples test <- atc_drugs %>% split(.$grp_name) 
#' @examples createStrFromATCs(test$Cholinesterasehemmer)
#' @examples "(?i)(cholinesterasehemmer|tacrin|donepezil|rivastigmin|galantamin|ipidacrin|donepezil und memantin|neostigmin|pyridostigmin|distigmin|ambenonium|aricept|exelon|rivastigmine|reminyl|prostigmin|mestinon|robinul|cholin|donepe|rivast|galant|ipidac|neosti|pyrido|distig|ambeno|aricep|reminy|prosti|mestin|robinu)"
createStrFromATCs <- function(df) {
        # Check if adäquate DF provided
        err <- try(df[[2]], silent = T)
        if (is(err, "try-error")) {
                ifelse(is(try(df[[1]][[2]], silent = T)
                          ,"try-error"), 
                       stop("please provide data frame from getATC_Level Funktion"),
                       df <- df[[1]])
        }
        # Substance Names + Drug Names 
        df <- df[[2]] %>% append(df[[6]]) %>% unique()
        # adding version with only the first 6 letters - e.g. tegretal -> tegret
        df <- append(df, left(df, 6)) %>% unique()
        df <- df[!is.na(df)]
        s <- tolower(paste0(df, collapse = "|"))
        s <- s %>% str_remove_all("[\\(\\)]") %>%
                str_remove_all("verschiedene\\|") %>%
                str_remove_all("kombinationen\\|") %>% 
                str_remove_all("[\\[\\]]")
        # str_remove_all("\\bmethyl") %>%
        # str_remove_all("\\bamino") %>%
        # str_remove_all("\\bcarboxy") %>%
        # str_replace_all("y", "\\(y\\|i\\)") %>%
        # str_replace_all("ä", "\\(ä\\|ae\\)") %>%
        # str_replace_all("ö", "\\(ö\\|oe\\)") %>%
        # str_replace_all("ü", "\\(ä\\|ue\\)") %>%
        # str_replace_all("f|ph", "\\(f\\|ph\\)") %>%
        # # str_replace_all("c|z|k","\\[c\\|z\\|k\\]") %>%
        # # str_replace_all("a","a\\?") %>%
        # str_replace_all("ui", "\\(ui\\)")
        s <- paste0("(?i)", "(", s, ")")
        return(s)
}





#' @title getATCLevel
#'
#' @description Custom function with similar properties as base::split function in dataframes. Here used to prepare different levels of ATC dataframe+drugs with variabels on each level 
#' @description e.g. Level 3 selection "C03 - Diuretika" lists all diuretics 
#' @description e.g. Level 4 selection "G03D - Gestagene" lists all gestagen and their parent groups 
#' @description e.g. Level 4 selection "G02AD - Prostaglandine" lists all prostaglandines 
#' @import tidyverse
#' @importFrom stringr str_detect
#' @param grp_level Specify the Group Level
#' @param elem_level Specify the Element Level (usually level 7)
#' @param atc Supply atc_drugs dataframe
#'
#' @return data frame
#' @export
getATCLevel <- function(grp_level, elem_level, atc) {
        atc_list <- list()
        for (i in 1:length(which(atc$cat == grp_level))) {
                atc_grp_lev <- atc[atc$cat == grp_level, 4][[1]][[i]]
                atc_list[[{
                        {
                                paste0(atc_grp_lev,
                                       " - ",
                                       atc[atc$cat == grp_level, 5][[1]][[i]])
                        }
                }]] <-
                        atc %>% filter(str_detect(grp, atc_grp_lev),
                                       cat == elem_level)
        }
        return(atc_list)
}



#' @title prepDfAtc_drugs
#'
#' @description uses function getATCLevels to generate several levels, each level containing full list of respective sub-levels
#' @param atc_drugs 
#'
#' @return A nested list
#' @export
prepDfAtc_drugs <- function(atc_drugs = atc_drugs) {
        atc_level1 <- getATCLevel(1, 7, atc_drugs)
        atc_level3 <- getATCLevel(3, 7,  atc_drugs)
        atc_level4 <- getATCLevel(4, 7, atc_drugs)
        atc_level5 <- getATCLevel(5, 7,  atc_drugs)
        atcs <-
                append(atc_level1, atc_level3) %>% 
                append(atc_level4) %>% 
                append(atc_level5)
        return(atcs)
}


#' @title getDrug
#'
#' @param drug Desired drug string to be searched for and extracted
#' @param abr Abbrevation for drug (serves as column name)
#' @param txt_col Column of character string to be searched
#' @param dat Dataframe that contains a column with character strings to be searched
#'
#' @return A nested tibble
#' @export
getDrug <- function(drug, abr, txt_col, dat) {
        # drug - name drug string e.g. "ASS"
        # abr - abbreveations e.g. "ass"
        # txt_col - which column of dataframe should be analyzed first?
        # dat - data.frame
        
        drug2 <- paste0(drug,".*?(?=,\\W|;\\W|\\n|\\.\\W)")
        presc <- "\\d[.|,|\\d|\\/]?\\d?-.{1,3}-\\d[\\.|,|\\d|\\/]?\\d?"
        dose <- "\\d(,|\\d)?\\d?"
        
        str_count <- paste0(abr,"_count")
        drug_name <- paste0(abr,"_name")
        drug_str <- paste0(abr,"_str")
        drug_dose_str <- paste0(abr,"_dose")
        drug_presc_str <- paste0(abr,"_presc")
        
        dat <- dat %>% 
                mutate({{str_count}} := str_count(eval(as.name(txt_col)), drug2)) %>% 
                mutate({{drug_str       }} := str_extract(eval(as.name(txt_col)), drug2)) %>% 
                mutate({{abr            }} := str_detect( eval(as.name(txt_col)), drug)/1) %>% 
                mutate({{drug_name      }} := str_extract(eval(as.name(txt_col)), drug)) %>% 
                mutate({{drug_dose_str  }} := str_extract(eval(as.name(paste0(abr,"_str"))), dose)) %>%
                mutate({{drug_dose_str  }} := str_extract(eval(as.name(drug_dose_str)), dose)) %>% 
                mutate({{drug_presc_str }} := str_extract(eval(as.name(paste0(abr,"_str"))), presc)) %>%  
                nest({{abr}} := -fallnummer)
        
        # str_extract_all would als be possible, but decision to use function for only one drug at a time
        # t <- dat %>% 
        #         mutate({{drug_str       }} := str_extract_all(eval(as.name(txt_col)), drug2, simplify = F)) %>%                 unnest_wider({{drug_str       }})
        # 
        return(dat)
}  



#' @title getNewDrugVar
#'
#' @param drugs 
#' @param atc_drugs 
#' @param df_meds_prior 
#'
#' @return
#' @export
#'
#' @examples
getNewDrugVar <- function(drugs = drugs, atc_drugs = atc_drugs, df_meds_prior = df_meds_prior){
        
        # get ATC from most substances
        # atc <- drugs$atc_code %>% as_tibble %>% unique() %>% arrange(-desc(value))
        atc <- drugs %>% 
                mutate(abr = left(subst1, 7)) %>% 
                select(atc_code, abr) %>% 
                unique() %>% 
                arrange(-desc(atc_code))
                
        atc$abr <- str_remove_all(atc$abr, "[\\(\\)]")
        atc$abr <- str_remove_all(atc$abr, "[\\[\\]]")
        # atc$subst1 <- str_remove_all(atc$subst1, "[\\(\\)]")
        # atc$subst1 <- str_remove_all(atc$subst1, "[\\[\\]")
        # single
        # t <- atc_drugs %>% filter(ATC == atc$value[100])
        
        atc <- atc %>%
                mutate(list = map(atc_code,
                                  function(x)
                                          filter(atc_drugs, ATC == x)))
        names(atc$list) <- atc$abr
        
        atc <- atc %>% 
                mutate(str = map_chr(atc$list, function(x) createStrFromATCs(x)))
           
        df_meds_prior$fallnummer <- as.integer(df_meds_prior$fallnummer)
        
        # version 1a
        # df_meds <- map2(atc$str[1:100], atc$abr[1:100], ~ getDrug(.x, .y, "meds", head(df_meds_prior)))
        # df_meds <- df_meds %>% reduce(function(x, y) merge(x, y, by = "fallnummer", all = TRUE))
         
        # version 1b - not working 
        # df_meds <- map2(atc$str[1:100], atc$abr[1:100], ~ getDrug(.x, .y, "meds", head(df_meds_prior)))
        # df_meds <- df_meds %>% reduce(left_join(by = "fallnummer"))
        
        
        #version 2
        df_meds <- map2(atc$str, atc$abr, ~ getDrug(.x, .y, "meds", df_meds_prior)) 
                # rename(fnr = fallnummer...1) %>%
                # select(-starts_with("fallnummer")) %>%
                # rename(fallnummer = fnr)
        return(df_meds)
}


#' @title renameLysePat
#'
#' @description renames variables from table "LysePatienten" from DB Heireka
#' @param df A dataframe "LysePatienten" from Heireka DB
#'
#' @return renamed variables in df
renameLysePat <- function(df){
        df <- df %>% rename(
                age = Alter,
                name = Name,
                sex = Sex,
                wake_up = wakeUp,
                side_stroke = Seite,
                therapy = Modus,
                therapy_intracranial = IC.Therapy,
                therapy_extracranial = EC.Therapy,
                sedation_type = SedierungsModus,
                therapy_add_gbIIbIIIa = GbIIbIIIa,
                tici = Recan,
                img_d0_mri = MRT,
                img_d0_ct = CCT,
                img_d0_cta = CTA,
                img_d0_aspects = ASPECTS,
                img_vessel = VesselStatus,
                nihss_d0 = NIH.d0,
                offlabel = OffLabel,
                offlabel_reason = OffLabelReason,
                risk_factor_htn = Hyperton,
                risk_factor_dm = Diabetes,
                risk_factor_hchol = Hyperchol,
                risk_factor_current_smoker = CurrentSmoker,
                risk_factor_prev_stroke = PrevStroke,
                risk_factor_khd = KHK,
                risk_factor_pavk = pAVK,
                risk_factor_af = VHF,
                risk_factor_dialysis = Dialyse,
                pre_med_apt = TAH,
                pre_med_oak = OAK,
                pre_med_statin = Statin,
                vitals_bb_missing = RR.missing,
                vitals_bb_systolic = RR.sys,
                vitals_bb_diastolic = RR.dias,
                lab_time = LabTime,
                lab_ext = LabExtern,
                lab_d0_glu = BZ.d0,
                lab_d0_leu = Leuko.d0,
                lab_d0_thr = Thrombos.d0,
                lab_d0_crea = Krea.d0,
                lab_d0_urea = Harnstoff.d0,
                lab_d0_uric_acid = Harnsäure,
                lab_d0_inr = INR.d0,
                lab_d0_hba1c = HbA1c,
                lab_d0_chol = Chol,
                lab_d0_ldl = LDL.Chol,
                lab_d0_hdl = HDL.Chol,
                lab_d0_trigl = Triglyceride,
                etiology_toast = Toast,
                img_d1_infsize_onethird = InfSize,
                img_ich = Blutung,
                img_ich_ecass2 = ICH.Typ.E2,
                img_ich_hbc = ICH.Typ.HBC,
                outcome_nihss_discharge = NIHpost,
                outcome_fatal_ich = FatalICH,
                outcome_ihd = InHouseDeath,
                outcome_mrsd90 = mRS.d90,
                discharge_towhere = Ziel,
                comment = Kommentar,
                birth_date = GebDat,
                date_therapy = Therapiedatum,
                time_admission = Aufnahmezeit,
                time_onset = OnsetTime,
                time_iv = Time.IV,
                time_ia = Time.IA,
                time_window = Zeitfenster,
                door_to_needle = door2needle,
                door_to_vessel = door2vessel,
                time_lab_adm = LabTime
        )
        return(df)
}


#' @title cleanDateTime
#'
#' @description Cleaning of dates and times in table "LysePatienten" from DB Heireka
#' @param df A dataframe "LysePatienten" from Heireka DB
#' @import tidyverse
#' @import lubridate
#' @import data.table
#' @import dtplyr
#' @import stringdist
#' @return df
cleanDateTime <- function(df){
        df <- 
                df %>% 
                lazy_dt() %>%
                mutate(birth_date = as.Date(parse_date_time2(birth_date, orders = "mdy HMS", cutoff_2000 = 10))) %>% 
                mutate(date_therapy = parse_date_time2(date_therapy, orders = "mdy HMS", cutoff_2000 = 98)) %>% 
                mutate(date_therapy_year = year(date_therapy)) %>% 
                mutate(date_therapy_month = month(date_therapy)) %>% 
                mutate(date_therapy_day = day(date_therapy)) %>% 
                mutate(time_iv = mdy_hms(time_iv)) %>% 
                mutate(time_ia = mdy_hms(time_ia)) %>% 
                mutate(time_window = hms(substr(as.character(time_window), 10,17))) %>% 
                mutate(door_to_needle = hms(substr(as.character(door_to_needle), 10,17))) %>%
                mutate(door_to_vessel = hms(substr(as.character(door_to_vessel), 10,17))) %>% 
                mutate(time_lab_adm = mdy_hms(df$time_lab_adm))
                # mutate(time_admission = mdy_hms(df$time_admission))
                # mutate(time_onset = hms(substr(as.character(df$time_onset), 10,17))

        # Selecting all times 
        # times <- colnames(select_if(df, function(col)
        #                 is.POSIXct(col) | is.Date(col)))
        return(df)
}


#' @title joinDemog
#'
#' @description joins demography file (and with it missing case id numbers) to HeirekaDF by searching for the best combined match of 1) Name 2) birth date 3) year 4) month treatment. 
#' @param df Heireka "LysePat" table
#' @param demog Demography file including "Fallnummern"/case ids
#'
#' @import tidyverse
#' @import lubridate
#' @import stringdist
#' @import vroom
#' @return joined df as tibble
#' @export
#' COMMENT: code could be improved
#' COMMENT: Additionally Function has side effect -> produces output other than supposed to, that is csv-export Namenskorrekturen, csv-export case_id/Fallnummern
joinDemog <- function(df, demog){
        # JOIN Heireka_Base + Demog
        
        demog$Aufn.Datum <- ymd(demog$Aufn.Datum)
        demog$adm_year <- year(demog$Aufn.Datum)
        demog$adm_month <- month(demog$Aufn.Datum)
        demog$adm_day <- day(demog$Aufn.Datum)
        demog$`Geb.` <- as.Date(ymd(demog$`Geb.`))
        df$name <- as.character(df$name)
        
        
        
        # Join by exakt name, birth_date, admission month and therapy date 
        # admission date is not consistent available in Heireka database 01/2021
        # As there may be a difference in admission date and therapy date calculate difference in dates afterwards
        
        join <- left_join(
                x = df, 
                y = demog, 
                by = c(
                        "name" = "Name",
                        "birth_date" = "Geb.",
                        "date_therapy_year" = "adm_year",
                        "date_therapy_month" = "adm_month"
                )
        )
        
        # # ANTIJOIN - what was not joined?
        aj <-
                anti_join(
                        x = df, 
                        y = demog, 
                        by = c(
                                "name" = "Name",
                                "birth_date" = "Geb.",
                                "date_therapy_year" = "adm_year",
                                "date_therapy_month" = "adm_month"
                        )
                )
        # Patients missing in join who have the same birth date, but probably some minor difference in Name e.g. Mueller vs. Müller
        join_missing <-
                left_join(
                        aj,
                        demog,
                        by = c(
                                "birth_date" = "Geb.",
                                "date_therapy_year" = "adm_year",
                                "date_therapy_month" = "adm_month"
                        )
                )
        join_missing$dist <-
                stringdist(join_missing$name, join_missing$Name, method =
                                   "osa")
        join_missing <- join_missing[join_missing$dist < 4,]
        join_missing <- join_missing[!is.na(join_missing$LfdNr),]
        join_missing <-
                join_missing[!duplicated(join_missing),] %>% select(LfdNr, Fall, name, Name)
        join_missing$LfdNr <- parse_number(as.character(join_missing$LfdNr))
        join$LfdNr <- parse_number(as.character(join$LfdNr))
        
        join <- left_join(join, join_missing, by = c("LfdNr" = "LfdNr"))
        join$Fallnummer <- coalesce(join$Fall.x, join$Fall.y)
        
        # ADDING Best List of case_ids for missing cases
        fallnummern_13072019 <- vroom("input/FALLNR/fallnummern_13072019.csv", 
                                           ";", escape_double = FALSE, trim_ws = TRUE, col_types = c(LfdNr = "d", case_id_j = "c"))
        colnames(fallnummern_13072019) <- c("LfdNr","Fallnummer")
        join <- left_join(join, fallnummern_13072019, by = c("LfdNr" = "LfdNr"))
        join$case_id <- coalesce(join$Fallnummer.x, as.character(join$Fallnummer.y))
        
        # Check Duplicated AFTER JOIN
        # Calculate difference between admission date and therapy date
        join$adm_diff <- join$adm_day-join$date_therapy_day
        
        # df$LfdNr <- parse_number(as.character(df$LfdNr))
        # join$LfdNr <- parse_number(as.character(join$LfdNr))
        # join_dupl <- join[duplicated(join$name),]$LfdNr[!join[duplicated(join$name),]$LfdNr %in% df[duplicated(df$name),]$LfdNr]
        # join$join_dupl <- NA
        # join$join_dupl[join$LfdNr %in% join_dupl] <- 1
        # join_dupl_df <- join[join$LfdNr %in% join_dupl,]
        
        # Remove duplicate cases AFTER JOIN keeping the ones with lowest adm_diff to therapy date
        join <- join %>% group_by(LfdNr) %>% arrange(abs(adm_diff)) %>% slice_head()
        
        # cut at around 7 days for in house stroke was best fit
        # join <- join[abs(join$adm_diff)<7 | is.na(join$adm_diff),]
        
        # Namenskorrekture Heireka
        Namenskorrekturen <- join %>% select(LfdNr, name=name.x, name_corr=Name) %>% na.omit(name_corr) %>% unique()
        filename <- paste0("output/HEIREKA/HEIREKA_Namenskorrektur_2021-01-21.csv")
        write.csv2(x = Namenskorrekturen, filename)
        
        # FALLNUMMER | CASE_ID LIST generated from JOINS
        fallnummer_20210121 <- join %>% select(LfdNr, case_id)
        write.csv2(fallnummer_20210121, file = "output/HEIREKA/HEIREKA_Fallnummern_2021-01-21.csv")
        
        
        
        # # # REPLACE names with corrected names OPTIONAL
        # for (i in 1:nrow(df)) {
        #   df$name[i] <-
        #     ifelse(is.na(jj_df_old$name_full.y[i]),
        #            jj_df_old$name_full.x[i],
        #            jj_df_old$name_full.y[i])
        # }

        return(join)
}


#' @title transformHeirekaDB
#'
#' @description performs the following functions: 1) renameLysePat, 2) cleanDateTime 3)joinDemog on Heireka "LysePatienten" DF (input is whole DB)
#' 
#' @param file
#' @import tidyverse 
transformHeirekaDB <- function(file, demog = demog){
        df <- file['LysePatienten'][[1]]
        df <- df %>% 
                renameLysePat() %>% 
                cleanDateTime() %>% 
                as_tibble %>% 
                joinDemog(demog = demog)
        return(df)
}


#' @title getCaseId - Function to retrieve case_ids from HEIREKA with options
#'
#' @param df_hei Heireka df
#' @param includeDbIndex Default is FALSE, when TRUE (LfdNr is included) 
#' @param removeNA 
#' 
#' @import tidyverse
#' @return a tibble
#' @export
#'
#' @examples id <- getCaseId(df_hei, includeDbIndex = FALSE, removeNA = TRUE)
getCaseId <- function(df_hei = df_hei, includeDbIndex = FALSE, removeNA = TRUE){
        if (includeDbIndex == FALSE) case_id <- df_hei[!is.na(df_hei$case_id),]['case_id']
        if (includeDbIndex == TRUE ) {
                case_id <- df_hei %>% select(case_id) 
                if (removeNA == TRUE) case_id <- case_id %>% drop_na(case_id)
        }
        return(case_id)
}


#' @title getLabVarsNames
#'
#' @description Retrieves unique names from lab file
#' @param labs 
#'
#' @return charcter strings
#' @export A tibble with a named character vector
#'
#' @examples
getLabVarsNames <- function(labs){
        labVarsNames <- unique(labs$Text) %>% as_tibble()
        names(labVarsNames$value) <- labVarsNames$value
        return(labVarsNames)
}


#' @title extractLab
#'
#' @description to extract SINGLE Lab Value in long data format
#' @param labs Raw labs file (ISH) should be provided
#' @param case_id Case Ids 
#' @param lab_select desired lab value e.g. "Natrium"
#' @param kenn "Kennzahlen" file 
#'
#' @return A tibble
#' @export
#'
#' @examples
extractLab <- function(labs = labs, case_id = case_id, lab_select, kenn = kenn) {
        # Filtering double value, case_ids and lab_selection
        status <- names(labs[str_detect(names(labs), "Status")])[1]
        labs <- labs %>%
                filter(Fall %in% case_id[[1]]) %>% 
                filter(Text %in% lab_select) %>%
                filter({{status}} != "HI") %>%
                distinct() %>% 
                mutate(Fall = parse_number(Fall)) %>% 
                mutate(labDateTime = ymd_hms(paste(DokDatum, DokZeit)))
        
        # Need Kennzahl for hospital admission times
        kenn <- kenn %>% 
                mutate(Fall = parse_number(Fall)) %>% 
                mutate(hospDateTime = ymd_hms(paste(Aufn.Datum, Aufn.Zeit))) %>% 
                select(-Einheit)
        
        # Standard range of local lab 
        lab_norm <- paste0("LABNORM_",{{lab_select}})
        
        # Join
        dfLab <-        
                left_join(labs, kenn, by = "Fall") %>%
                        # Calculate Difference between hospital admission and lab Time
                mutate(timeDiff = difftime(labDateTime, hospDateTime, units = "mins")) %>% 
                        # remove "<"
                mutate(Wert = str_replace_all(Wert, "<", "")) %>% 
                mutate(Normal = gsub(x = Normal, pattern = " ", replacement = "", perl=TRUE)) %>% 
                select(Fall, Text, Wert, Einheit, {{lab_norm}} := Normal, timeDiff, labDateTime, hospDateTime) %>%
                distinct() %>%
                group_by(Fall) %>%
                arrange(hospDateTime, timeDiff)
        
        # # Plot for cut off value
        # cut <- data.frame(time = c(1:360))
        # n = 1
        # for (n in 1:360) {
        #   cut[n, 2] <- dim(dfLab[abs(dfLab$timeDiff) < n, ])[1]
        # }
        # # plot(cut$V2 ~ cut$time)
        # print(paste0("Count for ",LABSELECT," for < 60min since admission - N: ", dim(dfLab[abs(dfLab$timeDiff) < 60, ])[1]))
        # print(paste0("Count for ",LABSELECT," for <120min since admission - N: ", dim(dfLab[abs(dfLab$timeDiff) < 120, ])[1]))
        # print(paste0("Count for ",LABSELECT," for <180min since admission - N: ", dim(dfLab[abs(dfLab$timeDiff) < 180, ])[1]))
        return(dfLab)
}


#' @title labLong2Wide - To convert SINGLE Lab Value from LONG to WIDE data format 
#'
#' @param LAB - Provide df/tibble of desired lab value data file processed by function extractLab
#'
#' @return A tibble
#' @export
#'
#' @examples na <- labLong2Wide(extractLab(labs, case_id, "Natrium", kenn))
labLong2Wide <- function(LAB){
        # getting Name of working lab value
        var <- substr(LAB$Text[1], 0,3)
        lab <- LAB
        lab <- na.omit(lab)
        # Calculate Standard Parameters from all values
        
        
        var_mean <- paste0("lab_",var,"_mean")
        var_min <- paste0("lab_",var,"_min")
        var_max <- paste0("lab_",var,"_max")
        var_sd <- paste0("lab_",var,"_sd")
        var_median <- paste0("lab_",var,"_median")
        var_count <- paste0("lab_",var,"_N")
        
        lab <- lab %>%
                mutate(Wert = as.numeric(Wert)) %>%
                arrange(timeDiff) %>%
                group_by(Fall) %>%
                mutate( {{var_mean}} := mean(Wert) ) %>%
                mutate( {{var_min}} := min(Wert) ) %>%
                mutate( {{var_max}} := max(Wert) ) %>%
                mutate( {{var_sd}} := sd(Wert) ) %>% 
                mutate( {{var_median}} := median(Wert) ) %>% 
                mutate( {{var_count}} := n())
        
        # Recode date of lab reading to categories 0 (admission) - discharge
        var_day <- paste0("lab_",var,"_day")
    
        lab <- lab %>% 
                mutate(timeDiff = round(as.numeric(timeDiff)/1440 ,0)) %>% 
                mutate({{var_day}} := case_when(
                        timeDiff <1 ~ paste0("lab_",var,"_d0"),
                        timeDiff == 1 ~ paste0("lab_",var,"_d1"),
                        timeDiff == 2 ~ paste0("lab_",var,"_d2"),
                        timeDiff == 3 ~ paste0("lab_",var,"_d3"),
                        timeDiff == 4 ~ paste0("lab_",var,"_d4"),
                        timeDiff == 5 ~ paste0("lab_",var,"_d5"),
                        timeDiff == 6 ~ paste0("lab_",var,"_d6"),
                        timeDiff == 7 ~ paste0("lab_",var,"_d7"),
                        timeDiff >= 8 ~ paste0("lab_",var,"_>d7")
                ))
        # some values after 7 days have to go in order to use "spread"
        # Todo: calculate mean value of those
        lab <- lab %>% 
                group_by(Fall) %>% 
                # distinct(timeDiff, .keep_all = TRUE) %>% 
                distinct({{var_day}}, .keep_all = TRUE)
        
        lab_wide <- lab %>% 
                select(-labDateTime,-hospDateTime,-timeDiff) %>%
                spread(key = var_day, value = "Wert") %>% 
                select(-contains("day"))
        
        return(lab_wide)
}



#' @title getLabs - Constructs a list from all available labs from desired case_id list
#'
#' @param labs - Supply 
#' @param case_id - vector with Selection of case_ids 
#' @param lab_vars_names - Variable names of desired lab value, Default is all, 
#' @param kenn - Kennzahlen files
#' 
#' @import tidyverse
#' 
#' @return A list
#' @export
#'
#' @examples labAll <- getLabs(labs, case_id, lab_vars_names, kenn)
getLabs <- function(labs = labs, case_id = case_id, lab_vars_names = lab_vars_names, kenn = kenn){
        if (is_tibble(lab_vars_names)) lab_vars_names <- lab_vars_names[[1]]
        l <- map(lab_vars_names,
                 ~ labLong2Wide(extractLab(labs, case_id, .x, kenn)))
        names(l) <- lab_vars_names
        return(l)
}


#' @title joinHeiStamm - Joins "Stammdaten" with Heireka DF
#'
#' @param df_hei 
#' @param db_stamm 
#'
#' @return A tibble
#' @export
joinHeiStamm <- function(df_hei = df_hei, db_stamm = db_stamm) {
    
    df_stamm <- db_stamm[["StammdatenTab"]] %>% 
        mutate(name = paste0(Nachname, ", ", Vorname)) %>%
        mutate(birth_date = as.Date(parse_date_time2(GebDat, orders = "mdy HMS", cutoff_2000 = 0))) %>% 
        mutate(echo_date = as.Date(parse_date_time2(EingabeDatum, orders = "mdy HMS", cutoff_2000 = 30))) %>%
        mutate(age = echo_date - birth_date) %>% 
        select(PID, name, birth_date, echo_date, age, EingabeDatum, Nachname, Vorname, GebDat) %>% 
        mutate(PID = as.numeric(as.character(PID)))
    
    j <-
        left_join(
            df_hei %>% select(LfdNr, name.x, birth_date, case_id, date_therapy_year),
            df_stamm %>% select(name, PID, birth_date, echo_date),
            by = c("name.x" = "name", "birth_date" = "birth_date")
        ) %>% unique() %>%
        mutate(PID = as.numeric(as.character(PID)))
    return(j)
}


#' @title getDopMode - Constructs a tibble from desired Doppler mode (e.g. TCD, Duplex, Bubble) joining desired data.frame (including case_ids)
#' @description  Joins PID to DuplexTab via UID AND; Joins to Stammdaten via PID
#' @description  Heirekapatients have case_id (Fallnummer) - others may not! If more patients are required (more than HeirekaPts) then df has to be supplied (must contain PID)
#' 
#' @param db_dop - Doppler DB
#' @param db_stamm - "Stammdaten" DB
#' @param df - a data.frame/tibble doppler data should be joined to (must contain PID)
#' 
#' @import tidyverse
#' @import lubridate
#' @return A Tibble
#' @export
#'
#' @examples
extractDopModality <-
    function(modality,
             db_d,
             df) {
        j <-
            left_join(
                left_join(
                db_d[[{{modality}}]],
                db_d[['UntersuchungsGrpTab']] %>%
                    select(UID, PID, Jahrgang) %>%
                    mutate(PID = as.numeric(as.character(PID))),
                by = c("UID" = "UID")
            ),
            df,
            by = c("PID" = "PID"))
        return(j)
    }

#' @title getDop - Retrieves a list of dataframes from all Doppler modes in former Doppler DB (e.g. Duplex, TCD, ...)
#' @description Uses sub function extractDopModality to extract tibbles of "DuplexTab", "TCDTab", "BubbleTab", "ECDTab", "TCDupTab"
#' @param mode 
#' @param db_dop 
#' @param df_hei_stamm 
#'
#' @return A list of tibbles
#' @export
#'
#' @examples dop_list <- getDop(db_dop, df_hei_stamm, mode = "ALL")
getDop <-
    function(db_dop = db_dop, df_hei_stamm = df_hei_stamm, mode = c("DuplexTab", "TCDTab", "BubbleTab", "ECDTab", "TCDupTab")) {
        if (mode == "ALL") {
            mode <- c("DuplexTab", "TCDTab", "BubbleTab", "ECDTab", "TCDupTab")
        }
        list_dop <-
            map(mode,
                ~ extractDopModality(
                    modality = .x,
                    db_d = db_dop,
                    df = df_hei_stamm
                )) %>% 
            set_names(mode)
        return(list_dop)
    }


combineNechoSources <- function(necho_17ff, necho_17ff_txt, necho_16pre, necho_02_db) {
    
    n1 <- necho_17ff %>%
        left_join(
            .,
            necho_17ff_txt %>% select(REFKEY, necho_finding_txt = CONTENT),
            by = c("HS_BEFTXT" = "REFKEY")
        ) %>%
        left_join(
            .,
            necho_17ff_txt %>% select(REFKEY, necho_intpret_txt = CONTENT),
            by = c("HS_BEURTXT" = "REFKEY")
        )
    
    
    # n2 <- necho_2016pre ...
    
    # n3 <- necho_2002_db ...
}    


combineDopSources <- function(dop, dop2){
    
    
    
}













# fallnummer
# [1] "EchoID"
# [2] "PID"
# [3] "Jahrgang"
# [4] "UTyp"
# [5] "Überweiser"
# [6] "Status"
# [7] "Beurteilung"
# [8] "UDatum"
# [9] necho_befund = "Fragestellung"
# [10] "Bemerkung"
# [11] "JahresIndex"
# [12] "Untersucher"
# [13] "Band."
# necho_aortic_root = "Aorten.D",
# [15] necho_left_atrium = "LA.D"
# [16] necho_rv = "RA.D"
# [17] necho_vci = "VCI.D"
# [18] "VCI.Stau"
# [19] necho_septum = "Septum.D"
# [20] "Septum.Beweglichkeit"
# [21] necho_hinterwand = "HW.D"
# [22] "HW.Beweglichkeit"
# [23] necho_lv_enddia = "LV.D.dias"
# [24] "LV.D.sys"
# [25] necho_aortic_valve = "X2d.AK"
# [26] necho_mitral_valve = "X2d.MK"
# [27] necho_tric_valve = "X2d.TK"
# [28] necho_pulm_valve = "X2d.PK"
# [29] "X2d.RV"
# [30] "X2d.LV"
# [31] "Regionale.KontraktStrg"
# [32] "PE.VW.syst"
# [33] "PE.VW.dias"
# [34] "PE.HW.Syst"
# [35] "PE.HW.dias"
# [36] necho_aortic_valve = "Doppler.Ak"
# [37] "VMax.Ak"
# [38] "PPG.Ak"
# [39] "MPG.Ak"
# [40] "Doppler.MK"
# [41] "MOF"
# [42] "MPG.MK"
# [43] "Doppler.TK"
# [44] "Vmax.TK"
# [45] necho_pa_pressure = "PA.Druck"
# [46] "Doppler.PK"
# [47] "Vmax.PK"
# [48] "VH.Ohr"
# [49] "Spontankontrast"
# 
# 
# 
# necho_aortic_root = NE_BE_AORT,
# necho_left_atrium = NE_BE_LVOR,
# necho_septum = NE_BE_SEPT,
# necho_hinterwand = NE_BE_HINT,
# necho_lv_enddia = NE_BE_LVED,
# necho_ef = NE_BE_EF,
# necho_rv = NE_BE_RV,
# necho_vci = NE_BE_VENA,
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
