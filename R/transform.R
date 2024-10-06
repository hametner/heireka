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
                "adm_meds_aufnb", "(?i)(?<=medikamente:?|medikation:?)(.*?)(?=Händig|(AUFNAHME)?BEFUND)",
                "adm_diag_aufnb", "(?i)(AUFNAHME-DIAGNOSE:?\\s?)(.*?)(ANAMNESE)",
                "adm_nihss_validation" , "(?<=NIHS{1,3}.{0,4})\\d{1,2}",
                "adm_pRS_validation" , "(?<=(([p|P][m|M]?[r|R][s|S])| pr[e|ä](morbid)?).{0,10})\\d",
                "handed_right" , "(?<=Händigkeit:\\W)\\d",
                "handed_left" , "(?<=Händigkeit: \\d\\Wrechts\\W)\\d(?=\\Wlinks)",
                "dx_prev_allergy" , "(?<=Allergien:)(.*?)(?=VOR) | (Allergie)",
                # "dx_prev_stroke_str" , "(?<=Frühere Infarkte:\\W)(.*?)(?=KHK)",
                "dx_prev_stroke" , "(?<=Frühere Infarkte:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_prev_chd_str" , "(?<=KHK/Herzinfarkt:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_prev_pad_str" , "(?<=pAVK:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_prev_cancer_str" , "(?<=Krebs:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_prev_opthalm_str" , "(?<=Augenerkrankungen:\\W)(.*?)(?=Nierenerkrankungen)",
                "dx_prev_nephr_str" , "(?<=Nierenerkrankungen:\\W)(.*?)(?=SD-Erkrankungen)",
                "dx_prev_thyr_str" , "(?<=SD-Erkrankungen:\\W)(.*?)(?=Metall)",
                "dx_rf_htn" , "(?<=Hypertonus:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_rf_dm" , "(?<=Diabetes:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_rf_hch" , "(?<=Hypercholesterinämie\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_rf_afib" , "(?<=Vorhofflimmern:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_rf_smoke_str" , "(?<=Rauchen:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_rf_smoke_py" , "([p|P](ack)?[Y|y](ears?)?\\W{0,2})\\d{1,2}",
                "dx_rf_alc_str" , "(?<=Alkohol:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_rf_hormone_str" , "(?<=Pille:\\W\\d\\Wnein\\W)\\d(?=\\Wja)",
                "dx_rf_migraine_str" , "(?<=Migräne:\\W)(.*?)(?=Familie|TECHN)",
                "dx_rf_family" , "(?<=Familie)(.*?)(?=TECHN)",
                "adm_nexam_aufnb", "(?i)(AUFNAHMEBEFUND|KLINISCHER\\sBEFUND|AUFNAHMESTATUS)(.*?)(?=MEDIK|VOR)"
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
                "adm_meds_not", "(?i)(medikamente:?|medikation:?)(.*?)(?=(kurz)?anamnese)"
        ) %>% mutate(varDef_cat = "NEUR-NOT__")
        
        v3 <- tribble(
                ~varDef_name, ~varDef_str,
                # NECHO | String definitions for NECHO necho variables
                # variables to use
                "necho_date", "(?<=de/neuro/ )(.*?)(?= /)",
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
                "adm_meds_stwbr", "(?i)(?<=medikament.?:?|medikation:?)(.*?)(?=(AUFNAHME)?BEFUND|NEURORAD)",
                "adm_diag_stwbr", "(DIAGNOSE.?:?|stationären Behandlung befand.)(.*?)((?i)ANAMNESE|Oberarzt)",
                "final_diag_stwbr", "(DIAGNOSE.?:?|stationären Behandlung befand.)(.*?)((?i)Vor(erkrankung|diagnose))",
                "prev_diag_stwbr", "(?i)Vor(erkrankung|diagnose)(.*?)(AUFNAHMEBEFUND|BEFUND)",
                "adm_epi_stwbr", "(?i)(VERLAUF UND BEURTEILUNG|Epikrise)(.*?)(?=(UniversitätsKlinikum Heidelberg|verbleiben mit freundlich))",
                "adm_nexam_stwbr", "(?i)(AUFNAHMEBEFUND|BEFUND)(.*?)(?=UNTERSUCH)"
                ) %>% mutate(varDef_cat = "NEUR-STWBR")
        
        v5 <- tribble(
            ~varDef_name, ~varDef_str,
            "adm_meds_int", "(?i)(?<=medikament.?:?|medikation:?)(.*?)(?=(AUFNAHME)?BEFUND|NEURORAD)",
            "adm_diag_int", "(DIAGNOSE.?:?|stationären Behandlung befand.)(.*?)((?i)ANAMNESE|Oberarzt)",
            "final_diag_int", "(DIAGNOSE.?:?|stationären Behandlung befand.)(.*?)((?i)Vor(erkrankung|diagnose))",
            "prev_diag_int", "(?i)Vor(erkrankung|diagnose)(.*?)(AUFNAHMEBEFUND|BEFUND)",
            "adm_epi_int", "(?i)(VERLAUF UND BEURTEILUNG|Epikrise)(.*?)(?=(UniversitätsKlinikum Heidelberg|verbleiben mit freundlich))",
            "adm_nexam_int", "(?i)(AUFNAHMEBEFUND|BEFUND)(.*?)(?=UNTERSUCH)"
        ) %>% mutate(varDef_cat = "NEUR-INT__")
        
        v6 <- tribble(
                ~varDef_name, ~varDef_str,
                "adm_meds_stabr", "(?i)(?<=medikament.?:?|medikation:?)(.*?)(?=(AUFNAHME)?BEFUND|NEURORAD)",
                "adm_diag_stabr", "(DIAGNOSE.?:?|stationären Behandlung befand.)(.*?)((?i)ANAMNESE|Oberarzt)",
                "final_diag_stabr", "(DIAGNOSE.?:?|stationären Behandlung befand.)(.*?)((?i)Vor(erkrankung|diagnose))",
                "prev_diag_stabr", "(?i)Vor(erkrankung|diagnose)(.*?)(AUFNAHMEBEFUND|BEFUND)",
                "adm_epi_stabr", "(?i)(VERLAUF UND BEURTEILUNG|Epikrise)(.*?)(?=(UniversitätsKlinikum Heidelberg|verbleiben mit freundlich))",
                "adm_nexam_stabr", "(?i)(AUFNAHMEBEFUND|BEFUND)(.*?)(?=UNTERSUCH)"
        ) %>% mutate(varDef_cat = "NEUR-STABR")
        
        
        use <- bind_rows(v, v2, v3, v4, v5, v6)
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


# retrieve Medikations and hierarchy groups by ChatGPT - no identifier information sent on request
# m$new <-


# Stepwise process:
#best
#         prompt1 <- "Act as an eclectic information collector. Please extract information on medication from the provided text using the following specified format. Format: 'medication | dose | N'.
#         If no medication is found or the string is empty, provide the character 'NA'. If no medication was taken, state 'no prior medication'. Please separate each group-pair with a new line ('\n') and provide lowercase text. For 'medication' only include the term itself.  'dose' represents the amount of substance per tablet e.g. 'mg' so dose would refer to '100' or '2,5'.  'N' represents the total daily dose, which should be calculated from the scheme per day e.g. '1-0-0' (representing 1 tablet in the morning) multiplicated by the dose.
#         Avoid including any unnecessary line breaks or extra text that is not medication related, be extremely precise. Avoid using 'medication', 'dose', 'N' as result. Instead of e.g. '10mg' write '10'."
#         #best
#         prompt1 <- "Act in the role of a meticulous data extractor focused on medical information. From the provided text, your task is to extract specifics about medications, following the outlined format below:
#                 
#                 Format: 'medication | dose | N'
#         
#         Guidelines:
#                 
#                 'medication': Only include the name of the medication.
#         'dose': This refers to the quantity of substance per tablet/capsule, typically represented in 'mg' or similar units. Please only mention the numeric value. For instance, if the dosage is '10mg', your output should be '10'.
#         'N': This represents the total daily dosage, calculated from the prescribed daily regimen. For example, for a regimen of '1-0-0' (indicating 1 tablet in the morning), multiply this by the dose to get 'N'.
#         If the text does not contain any medication information, write 'NA'. If it's clear that no medication was taken, respond with 'no prior medication'.
# 
# Each medication, dose, and N should be separated by a new line ('\n'), and the text should be presented in lowercase.
# 
# Ensure precision by omitting any irrelevant text or unnecessary line breaks. The extracted data should strictly pertain to medications. Please note, you should not use the terms 'medication', 'dose', or 'N' as your result."
#         
#       
#         
#         
#         
#         df2 <- tail(df_meds_prior,20)
#         df2 <- df2 %>% drop_na(meds)
#         s <- Sys.time()
#         df2$new <-
#                 sapply(
#                         df2$meds,
#                         simplify = TRUE,
#                         USE.NAMES = FALSE,
#                         FUN =  function(x) {
#                                 ask_chatgpt(
#                                         paste(
#                                                 prompt1,
#                                                 "Report:", x
# 
#                                         ), temp = 0.1
#                                 )
#                         }
#                 )
# 
#         Sys.time()-s
# 
#         # Split the strings in the 'm' column using "?;\\n" and preserve the 'fallnummer' identifier
#         r2 <- df2 %>%
#                 mutate(m = str_split(new, pattern = "\\n")) %>%
#                 unnest(m) %>%
#                 separate(
#                         m,
#                         into = c("drug",  "dosage", "total"),
#                         sep = " \\| ",
#                         extra = "merge"
#                 ) %>%
#                 mutate(
#                         drug = str_trim(drug),
#                         # substance = str_trim(substance),
#                         # class = str_trim(class),
#                         dosage = str_trim(dosage),
#                         total = str_trim(total)
#                 )
# 
#         r2 <- r2 %>% mutate(drug = gsub("ass", ignore.case = TRUE, "acetylsalicylic acid",x = drug))
#         r2 %>% select(-meds, -new) %>% View()
#  
#         Sys.time()-s
#         
#         prompt2 <- "Act as an eclectic pharma expert. please provide substance names and class of the medication (provided in the text) in the following format. Format: 'substance name | class' . If 'no prior medication' then simply state 'NA'.  If the text is already a substance name, then simply repeat. As for 'class' choose the most relevant class for cerebrovascular diseases."
#  
#         prompt2 <- "Assume the role of a pharmaceutical specialist with a focus on cerebrovascular diseases. Your task is to extract and classify information about medications provided in the text. Follow the outlined format below:
# 
# Format: 'substance name | class'
# 
# Guidelines:
# 
# 'substance name': Identify and provide the name of the substance or medication from the text. If the text is already a substance name, simply replicate it.
# 'class': Determine the most relevant class of the substance for cerebrovascular diseases and include this in your response.
# In cases where it's clear that 'no prior medication' was taken, simply respond with 'NA'.
# 
# The goal is to provide a clear, concise summary of the substance and its relevance to cerebrovascular diseases."
#         s <- Sys.time()
#         r2$sub_class <-
#                 sapply(
#                         r2$drug,
#                         simplify = TRUE,
#                         USE.NAMES = FALSE,
#                         FUN =  function(x) {
#                                 ask_chatgpt(
#                                         paste(
#                                                 prompt2, 
#                                                 "Drug:",
#                                                 x
#                                         ), temp = 0.1
#                                 )
#                         }
#                 )
# 
#         r2 <- r2 %>%
#                 mutate(m = str_split(sub_class, pattern = "\\n")) %>%
#                 unnest(m) %>%
#                 separate(
#                         m,
#                         into = c("substance",  "class"),
#                         sep = " \\| ",
#                         extra = "merge"
#                 ) %>%
#                 mutate(
#                         substance = str_trim(substance),
#                         class = str_trim(class),
#                 )
#         r2 %>% select(-meds, -new) %>% View()
#         Sys.time()-s


extractAphasiaWithGPT <- function(df){

        prompt_aphasia <- "#CONTEXT#
The task involves extracting and analyzing details from medical records to identify and classify instances of aphasia in patients.
#OBJECTIVE#
Carefully extract relevant information that pertains strictly to aphasia, such as word-finding difficulties, paraphasias, etc. Classify the extracted information according to a predefined aphasia grading scale: 'no aphasia,' 'minor aphasia,' 'moderate/severe aphasia,' and 'global aphasia.' Exclude any information related to dysarthria or other conditions not indicative of aphasia. Pay special attention to any details suggesting the absence of aphasia, like phrases indicating no evidence or presence of aphasia.
If the text involves more than one examination - e.g. at different time points 'AUFNAHMEBEFUND: [first examination with no aphasia], Stroke Unit: [second examination with aphasia]' choose the first instance. If the text describes states pre- and post Intervention, choose the examination pre intervention for classification.
Definitions for classification:
No Aphasia: No evidence of language comprehension, speech, reading, or writing difficulties.
Minor Aphasia: Some loss of fluency or comprehension without significant limitation in expressing or understanding ideas. Conversations may be challenging.
Moderate/Severe Aphasia: Communication is significantly impaired, with fragmented expression. The listener must infer or guess much of the communication.
Global Aphasia: Complete absence of speech and understanding, with no ability to communicate effectively.

#FORMAT OF RESPONSE#
Respond with the identified level of aphasia ('no aphasia,' 'minor aphasia,' 'moderate/severe aphasia,' 'global aphasia'). Omit extra explanations and commenting.

#INPUT#"
        
        # s <- Sys.time()
        df$new <-
                sapply(
                        # paste0(df$adm_nexam_aufnb, df$adm_nexam_stwbr),
                        paste0(df$adm_nexam),
                        simplify = TRUE,
                        USE.NAMES = FALSE,
                        FUN =  function(x) {
                                ask_chatgpt(
                                        paste(
                                                prompt_aphasia,
                                                x
                                        ), temp = 0.1
                                )
                        }
                )
        # Sys.time()-s 
        
        return(df)
}

# Gets clinical symptom 'aphasia' only from documents sources available at admission 
getAphasiaAdmission <- function(case_id, df_neur_not, df_neur_aufnb, df_neur_stwbr, df_neur_int){
        
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% drop_na()
        
        adm_nexam_not_str <- "(?i)(Klinischer Befund\\s?:)(.*?)(?=Zusatzdiagnostik)"
        df_neur_not$adm_nexam_not <- str_extract(pattern=adm_nexam_not_str, df_neur_not$txt)
        
        df_neur_not <- df_neur_not %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_not)
        df_neur_aufnb <- df_neur_aufnb %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_aufnb)
        df_neur_stwbr <- df_neur_stwbr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_stwbr)
        df_neur_int <- df_neur_int %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_int)
        
        a <- case_id %>% 
                left_join(df_neur_aufnb, by = c("case_id" = "case_id")) %>% 
                left_join(df_neur_not, by = c("case_id" = "case_id")) %>% 
                left_join(df_neur_int, by = c("case_id" = "case_id")) %>% 
                left_join(df_neur_stwbr, by = c("case_id" = "case_id")) %>% 
                mutate(adm_nexam = coalesce(adm_nexam_aufnb, adm_nexam_not, adm_nexam_int, adm_nexam_stwbr)) %>% 
                select(case_id, adm_nexam)
        
        
        # a <- df_neur_aufnb %>% select(fallnummer, adm_nexam_aufnb) %>% left_join(df_neur_stwbr %>% select(fallnummer, adm_nexam_stwbr), by = c("fallnummer" = "fallnummer"))
        # a <- a %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% drop_na()
        # aj <- anti_join(j, a, by = c("case_id")) %>% drop_na(adm_nexam) %>% select(case_id, adm_nexam)
        # a_list_A <- a_list[1:100]
        # a_list_A <- map(a_list_A, ~mutate(.x, case_id=parse_number(as.character(fallnummer))) %>% select(case_id, new))
        # a_list_B <- map(a_list[101:125], ~select(.x, case_id, new))
        # a_list 
        # extractAphasiaWithGPT
        
        file_path <- "output/aphasia_adm.rds"
        
        if (!file.exists(file_path)) {
                # (a)phasia list
                a_list <- list()
                a_chunks <- split(a, 1:nrow(a) %% 100)
                # c <- 1
                for (c in seq(1, 100)) {
                        d <- a_chunks[[c]]
                        a_list[[c]] <- extractAphasiaWithGPT(d)
                        # write.csv2(as_tibble(do.call(rbind, m_list)), "output/prior_meds.csv")
                        saveRDS(a_list, 'output/aphasia_adm.rds', compress = 'xz')
                        print(paste0(c, "..done"))
                }
        } else {
                a_list <- readRDS(file_path)
        }
        
        d <- do.call(rbind, a_list) %>% 
                as_tibble() %>% 
                mutate(new = as.character(new)) %>% 
                mutate(var_aphasia_adm = ifelse (new %in% c("no aphasia", "minor aphasia", "moderate/severe aphasia", "global aphasia"), new, NA_character_)) %>% 
                select(-new) %>% 
                mutate(var_aphasia_adm = factor(var_aphasia_adm, c("no aphasia", "minor aphasia", "moderate/severe aphasia", "global aphasia"), ordered = TRUE))
        return(d)
}
        
getMicroangiopathie <- function(case_id, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% drop_na()
        
        # Ausgeprägte Makro- und Mikroangiopathie / lakunäre Defekte
        df_neur_aufnb <- df_neur_aufnb %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt)
        df_neur_stwbr <- df_neur_stwbr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt)
        df_neur_int <- df_neur_int %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt)
        df_neur_stabr <- df_neur_stabr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt)
        
        
        ma_str <- "((\\b\\w+\\b.{1,3}){1,6})?([M|m]i[ck]roa.gioph?ath?i|S[A|a][E|e]|Leu[ck]en[cz]ephal)"
        ma_neur_aufnb <- df_neur_aufnb %>% select(case_id, txt) %>% 
                mutate(ma_aufnb = str_extract(txt, pattern=ma_str))
        ma_neur_stwbr <- df_neur_stwbr %>% select(case_id, txt) %>% 
                mutate(ma_stwbr = str_extract(txt, pattern=ma_str))
        ma_neur_int <- df_neur_int %>% select(case_id, txt) %>% 
                mutate(ma_int = str_extract(txt, pattern=ma_str))
        ma_neur_stabr <- df_neur_stabr %>% select(case_id, txt) %>% 
                mutate(ma_stabr = str_extract(txt, pattern=ma_str))
        
        
        ma_str2 <- "[D|d]eutlich|[F|f]ortgeschritt|[A|a]usgepr|s[ch]wer|[H|h]ochgr|[H|h]öchstgr"
        ma_str3 <- "[M|m]ä[ß|sz|s]ig|[M|m]ittelgrad|[M|m]oderat"
        ma_str4 <- "[G|g]ering|[L|l]eicht|[B|b]eginnend|[D|d]iskret|[D|d]ezent|[M|m]i[ck]roa.gioph?ath?i|S[A|a][E|e]|Leu[ck]en[cz]ephal"
        # ma_str4 <- "[L|l]eicht|[G|g]ering|[M|m]inimal"
        ma_neur_aufnb <- ma_neur_aufnb %>% mutate(ma_neur_aufnb = case_when(
                str_detect(ma_aufnb, pattern=ma_str2)~"severe",
                str_detect(ma_aufnb, pattern=ma_str3)~"moderate",
                str_detect(ma_aufnb, pattern=ma_str4)~"minor",
                TRUE~"no",
        )) %>% select(case_id, ma_neur_aufnb)
        ma_neur_stwbr <- ma_neur_stwbr %>% mutate(ma_neur_stwbr = case_when(
                str_detect(ma_stwbr, pattern=ma_str2)~"severe",
                str_detect(ma_stwbr, pattern=ma_str3)~"moderate",
                str_detect(ma_stwbr, pattern=ma_str4)~"minor",
                TRUE~"no",
        )) %>% select(case_id, ma_neur_stwbr)
        ma_neur_stabr <- ma_neur_stabr %>% mutate(ma_neur_stabr = case_when(
                str_detect(ma_stabr, pattern=ma_str2)~"severe",
                str_detect(ma_stabr, pattern=ma_str3)~"moderate",
                str_detect(ma_stabr, pattern=ma_str4)~"minor",
                TRUE~"no",
        )) %>% select(case_id, ma_neur_stabr)
        ma_neur_int <- ma_neur_int %>% mutate(ma_neur_int = case_when(
                str_detect(ma_int, pattern=ma_str2)~"severe",
                str_detect(ma_int, pattern=ma_str3)~"moderate",
                str_detect(ma_int, pattern=ma_str4)~"minor",
                TRUE~"no",
        )) %>% select(case_id, ma_neur_int)
        
        ma <- case_id %>% 
                mutate(var_microangio = NA_character_) %>% 
                left_join(ma_neur_aufnb, by = c("case_id")) %>% 
                left_join(ma_neur_stwbr, by = c("case_id")) %>% 
                left_join(ma_neur_stabr, by = c("case_id")) %>% 
                left_join(ma_neur_int, by = c("case_id")) %>% 
        mutate(var_microangio = coalesce(ma_neur_stwbr, ma_neur_stabr, ma_neur_int, ma_neur_aufnb)) %>% 
                select(case_id, var_microangio) %>% 
                drop_na(case_id) %>% 
        mutate(var_microangio = ordered(var_microangio, levels = c("no","minor","moderate","severe", NA)))
        return(ma)
}


getVigilanceAdmission <- function(case_id, df_neur_not, df_neur_aufnb, df_neur_int, df_neur_stwbr, df_neur_stabr){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% drop_na()
        
        df_neur_not <- df_neur_not %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt)
        df_neur_aufnb <- df_neur_aufnb %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_aufnb)
        df_neur_stwbr <- df_neur_stwbr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_stwbr)
        df_neur_int <- df_neur_int %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_int)
        df_neur_stabr <- df_neur_stabr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_stabr)
        
        #REFINE
        df_neur_not <- df_neur_not %>% mutate(var_vigil_not = case_when(
                str_detect(txt, pattern = "[K|k]omatös") ~ "coma",
                str_detect(txt, pattern = "[S|s]oporös") ~ "sopor",
                str_detect(txt, pattern = "[S|s]omnol") ~ "somnolence",
                str_detect(txt, pattern = "[W|w]ach") ~ "awake"
        ))
        df_neur_aufnb <- df_neur_aufnb %>% mutate(var_vigil_aufnb = case_when(
                str_detect(adm_nexam_aufnb, pattern = "[K|k]omatös") ~ "coma",
                str_detect(adm_nexam_aufnb, pattern = "[S|s]oporös") ~ "sopor",
                str_detect(adm_nexam_aufnb, pattern = "[S|s]omnol") ~ "somnolence",
                str_detect(adm_nexam_aufnb, pattern = "[W|w]ach") ~ "awake"
        ))
        df_neur_stwbr <- df_neur_stwbr %>% mutate(var_vigil_stwbr = case_when(
                str_detect(adm_nexam_stwbr, pattern = "[K|k]omatös") ~ "coma",
                str_detect(adm_nexam_stwbr, pattern = "[S|s]oporös") ~ "sopor",
                str_detect(adm_nexam_stwbr, pattern = "[S|s]omnol") ~ "somnolence",
                str_detect(adm_nexam_stwbr, pattern = "[W|w]ach") ~ "awake"
        ))
        df_neur_int <- df_neur_int %>% mutate(var_vigil_int = case_when(
                str_detect(adm_nexam_int, pattern = "[K|k]omatös") ~ "coma",
                str_detect(adm_nexam_int, pattern = "[S|s]oporös") ~ "sopor",
                str_detect(adm_nexam_int, pattern = "[S|s]omnol") ~ "somnolence",
                str_detect(adm_nexam_int, pattern = "[W|w]ach") ~ "awake"
        ))
        df_neur_stabr <- df_neur_stabr %>% mutate(var_vigil_stabr = case_when(
                str_detect(adm_nexam_stabr, pattern = "[K|k]omatös") ~ "coma",
                str_detect(adm_nexam_stabr, pattern = "[S|s]oporös") ~ "sopor",
                str_detect(adm_nexam_stabr, pattern = "[S|s]omnol") ~ "somnolence",
                str_detect(adm_nexam_stabr, pattern = "[W|w]ach") ~ "awake"
        ))
        
        dfx <- case_id %>% 
                left_join(df_neur_aufnb, by = c("case_id")) %>% 
                left_join(df_neur_stwbr, by = c("case_id")) %>% 
                left_join(df_neur_not, by = c("case_id")) %>% 
                left_join(df_neur_int, by = c("case_id")) %>% 
                left_join(df_neur_stabr, by = c("case_id")) %>% 
                mutate(var_vigil = coalesce(var_vigil_aufnb, var_vigil_stwbr, var_vigil_not, var_vigil_int, var_vigil_stabr)) %>% 
                mutate(var_vigil = ordered(var_vigil, levels = c("awake","somnolence","sopor","coma", NA))) %>% 
                select(case_id, var_vigil)
        return(dfx)
}

# getEyeDeviationAdmission <- function(case_id, df_neur_not, df_neur_aufnb, df_neur_int, df_neur_stwbr, df_neur_stabr){
#         case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% drop_na()
#         
#         df_neur_not <- df_neur_not %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt)
#         df_neur_aufnb <- df_neur_aufnb %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_aufnb)
#         df_neur_stwbr <- df_neur_stwbr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_stwbr)
#         df_neur_int <- df_neur_int %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_int)
#         df_neur_stabr <- df_neur_stabr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_stabr)
#         
#         # ed_str <- "[K|k]opf.{1,30}wendung|[B|b]lickwend|[B|b]lickpräf"
#         ed_str <- "(?<!\\bkeine\\s)[Kk]opf.{1,30}wendung|(?<!\\bkeine\\s)[Bb]lickwend|(?<!\\bkeine\\s)[Bb]lickpräf"
#         
#         # (?<![K|k]eine )
#         df_neur_not <- df_neur_not %>% mutate(var_eyedev_not = case_when(
#                 str_detect(txt, pattern = ed_str) ~ 1,
#                 TRUE~0))
#         df_neur_aufnb <- df_neur_aufnb %>% mutate(var_eyedev_aufnb = case_when(
#                 str_detect(adm_nexam_aufnb, pattern = ed_str) ~ 1,
#                 TRUE~0))
#         df_neur_stwbr <- df_neur_stwbr %>% mutate(var_eyedev_stwbr = case_when(
#                 str_detect(adm_nexam_stwbr, pattern = ed_str) ~ 1,
#                 TRUE~0))
#         df_neur_int <- df_neur_int %>% mutate(var_eyedev_int = case_when(
#                 str_detect(adm_nexam_int, pattern = ed_str) ~ 1,
#                 TRUE~0))
#         df_neur_stabr <- df_neur_stabr %>% mutate(var_eyedev_stabr = case_when(
#                 str_detect(adm_nexam_stabr, pattern = ed_str) ~ 1,
#                 TRUE~0))
#         
#         dfx <- case_id %>% 
#                 left_join(df_neur_aufnb, by = c("case_id")) %>% 
#                 left_join(df_neur_stwbr, by = c("case_id")) %>% 
#                 left_join(df_neur_not, by = c("case_id")) %>% 
#                 left_join(df_neur_int, by = c("case_id")) %>% 
#                 left_join(df_neur_stabr, by = c("case_id")) %>%
#                 mutate(var_eyedev = coalesce(var_eyedev_not, var_eyedev_aufnb, var_eyedev_stwbr, var_eyedev_int, var_eyedev_stabr)) %>% 
#                 select(case_id, var_eyedev)
#         
#         return(dfx)
#                 
# }



getEyeDeviationAdmission <- function(case_id, df_neur_not, df_neur_aufnb, df_neur_int, df_neur_stwbr, df_neur_stabr) {
        library(dplyr)
        library(stringr)
        
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% drop_na()
        
        df_neur_not <- df_neur_not %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt)
        df_neur_aufnb <- df_neur_aufnb %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_aufnb)
        df_neur_stwbr <- df_neur_stwbr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_stwbr)
        df_neur_int <- df_neur_int %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_int)
        df_neur_stabr <- df_neur_stabr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, adm_nexam_stabr)
        
        # Define keyword patterns for categorization
        forced_pattern <- "\\b(unüberwindbar|fixiert|forciert|nicht überwindbar|Kopf- und Blick)\\b"
        non_forced_pattern <- "\\b(überwindbar|Blickpräferenz)\\b"
        
        # Eye deviation pattern
        ed_str <- "(?<!\\bkeine\\s)[Kk]opf.{1,30}wendung|(?<!\\bkeine\\s)[Bb]lickwend|(?<!\\bkeine\\s)[Bb]lickpräf"
        
        # Function to categorize text
        categorize_eyedeviation <- function(text) {
                case_when(
                        str_detect(text, forced_pattern) ~ "forced",
                        str_detect(text, non_forced_pattern) | str_detect(text, ed_str) ~ "non-forced",
                        TRUE ~ "none"
                )
        }
        
        # Apply categorization to each dataset
        df_neur_not <- df_neur_not %>% mutate(var_eyedev_not = categorize_eyedeviation(txt))
        df_neur_aufnb <- df_neur_aufnb %>% mutate(var_eyedev_aufnb = categorize_eyedeviation(adm_nexam_aufnb))
        df_neur_stwbr <- df_neur_stwbr %>% mutate(var_eyedev_stwbr = categorize_eyedeviation(adm_nexam_stwbr))
        df_neur_int <- df_neur_int %>% mutate(var_eyedev_int = categorize_eyedeviation(adm_nexam_int))
        df_neur_stabr <- df_neur_stabr %>% mutate(var_eyedev_stabr = categorize_eyedeviation(adm_nexam_stabr))
        
        dfx <- case_id %>% 
                left_join(df_neur_aufnb, by = c("case_id")) %>% 
                left_join(df_neur_stwbr, by = c("case_id")) %>% 
                left_join(df_neur_not, by = c("case_id")) %>% 
                left_join(df_neur_int, by = c("case_id")) %>% 
                left_join(df_neur_stabr, by = c("case_id")) %>%
                mutate(var_eyedev = coalesce(var_eyedev_not, var_eyedev_aufnb, var_eyedev_stwbr, var_eyedev_int, var_eyedev_stabr)) %>% 
                select(case_id, var_eyedev)
        
        return(dfx)
}



# revised version
extractMedsWithGPT_v2 <- function(df){
        # retrieve Medikations and hierarchy groups by ChatGPT - no identifier information sent on request
        
                
prompt1 <- "#CONTEXT#
I want to extract information from medical records concerning medications take by the patients.

#OBJECTIVE#
Extract all medications, classify each according to the ATC classification system in „SUBSTANCES (=ATC level 5)“, „SUBGROUPS (=ATC level 4)“, and „CLASSES (ATC level 2)“.
Ensure precision by omitting any irrelevant text or unnecessary line breaks. The extracted data should strictly pertain to medications. In case of a medications that contain two or more SUBSTANCES, please make two or more TARGETS, respectively. 


#FORMAT of RESPONSE#
Respond with identified TARGET, ATC code, SUBSTANCES, SUBGROUPS, CLASSES, PRESCRIBED. Use information such as '(Pause)' or 'pausiert' or similar to indicate PRESCRIBED=0. Follow the following format: E.g. 'Marcumar' would be a found TARGET. The RESPONSE would be: '''Marcumar; B01AA04; Phenprocoumon; Vitamin K antagonists; Antithrombotic agents; 0'''. Please skip commenting, and only return desired information. If there are hints for no prior medication' such as 'keine' or 'Vormedikation: keine' or similar, please indicate so - '''no prior medication; no prior medication; no prior medication; no prior medication; no prior medication; 0'''.
Separate each responses using ';;;' without introducing whitespace. In case of NA - leave 'NA'. Remember that each response contains six items. When a substance was found reliably, there can no 'no prior medication'. When you cannot identify a substance for a TARGET, use a web search to identify the SUBSTANCE (e.g. 'Substance of [TARGET]' -  e.g. 'Substance of Godamed' and go from there. When you cannot find a TARGET stick to 'NA'. When the text apears to be recommendation from discharge rather admission medication stick to 'NA'.

#INPUT#
"
# s <- Sys.time()
df$new <-
        sapply(
                df$meds,
                simplify = TRUE,
                USE.NAMES = FALSE,
                FUN =  function(x) {
                        ask_chatgpt(
                                paste(
                                        prompt1,
                                        x
                                ), temp = 0.1
                        )
                }
        )
# Sys.time()-s

df2 <- df %>%
        mutate(new = str_split(new, pattern = ";;;")) %>%
        unnest(new) %>%
        separate(
                new,
                into = c("target",  "atc", "substance", "subgroup", "class", "prescribed"),
                sep = ";",
                extra = "merge"
        ) %>%
        mutate(
                target = str_trim(target),
                atc = str_trim(atc),
                substance = str_trim(substance),
                subgroup = str_trim(subgroup),
                class = str_trim(class),
                prescribed = str_trim(prescribed)
        )
return(df2)
}

#DEPRECATED#
extractMedsWithGPT <- function(df){
        # retrieve Medikations and hierarchy groups by ChatGPT - no identifier information sent on request
        

        # Stepwise process:
                # prompt1 <- "Act as an eclectic information collector. Please extract information on medication from the provided text using the following specified format. Format: 'medication | dose | N'.
                # If no medication is found or the string is empty, provide the character 'NA'. If no medication was taken, state 'no prior medication'. Please separate each group-pair with a new line ('\n') and provide lowercase text. For 'medication' only include the term itself.  'dose' represents the amount of substance per tablet e.g. 'mg' so dose would refer to '100' or '2,5'.  'N' represents the total daily dose, which should be calculated from the scheme per day e.g. '1-0-0' (representing 1 tablet in the morning) multiplicated by the dose.
                # Avoid including any unnecessary line breaks or extra text that is not medication related, be extremely precise. Avoid using 'medication', 'dose', 'N' as result. Instead of e.g. '10mg' write '10'."
        
        # #best prompt
                prompt1 <- "Act in the role of a meticulous data extractor focused on medical information. From the provided text, your task is to extract specifics about medications, following the outlined format below:

                        Format: 'medication | dose | N'

                Guidelines:

                        'medication': Only include the name of the medication.
                'dose': This refers to the quantity of substance per tablet/capsule, typically represented in 'mg' or similar units. Please only mention the numeric value. For instance, if the dosage is '10mg', your output should be '10'.
                'N': This represents the total daily dosage, calculated from the prescribed daily regimen. For example, for a regimen of '1-0-0' (indicating 1 tablet in the morning), multiply this by the dose to get 'N'.
                If the text does not contain any medication information, write 'NA'. If it's clear that no medication was taken, respond with 'no prior medication'.

        Each medication, dose, and N should be separated by a new line ('\n'), and the text should be presented in lowercase.

        Ensure precision by omitting any irrelevant text or unnecessary line breaks. The extracted data should strictly pertain to medications. Please note, you should not use the terms 'medication', 'dose', or 'N' as your result."


                # df2 <- tail(df_meds_prior,20)
                # df2 <- df2 %>% drop_na(meds)
                # s <- Sys.time()
                df$new <-
                        sapply(
                                df$meds,
                                simplify = TRUE,
                                USE.NAMES = FALSE,
                                FUN =  function(x) {
                                        ask_chatgpt(
                                                paste(
                                                        prompt1,
                                                        "Report:", x

                                                ), temp = 0.1
                                        )
                                }
                        )

                # Sys.time()-s

                # Split the strings in the 'm' column using "?;\\n" and preserve the 'fallnummer' identifier
                df2 <- df %>%
                        mutate(m = str_split(new, pattern = "\\n")) %>%
                        unnest(m) %>%
                        separate(
                                m,
                                into = c("drug",  "dosage", "total"),
                                sep = " \\| ",
                                extra = "merge"
                        ) %>%
                        mutate(
                                drug = str_trim(drug),
                                # substance = str_trim(substance),
                                # class = str_trim(class),
                                dosage = str_trim(dosage),
                                total = str_trim(total)
                        )

                # df2 <- df2 %>% mutate(drug = gsub("ass", ignore.case = TRUE, "acetylsalicylic acid",x = drug))
                # df2 %>% select(-meds, -new) %>% View()

                # Sys.time()-s

                prompt2 <- "Act as an eclectic pharma expert. please provide substance names and class of the medication (provided in the text) in the following format. Format: 'substance name | class' . If 'no prior medication' then simply state 'NA'.  If the text is already a substance name, then simply repeat. As for 'class' choose the most relevant class for cerebrovascular diseases."

        #         prompt2 <- "Assume the role of a pharmaceutical specialist with a focus on cerebrovascular diseases. Your task is to extract and classify information about medications provided in the text. Follow the outlined format below:
        # 
        # Format: 'substance name | class'
        # 
        # Guidelines:
        # 
        # 'substance name': Identify and provide the name of the substance or medication from the text. If the text is already a substance name, simply replicate it.
        # 'class': Determine the most relevant class of the substance for cerebrovascular diseases and include this in your response.
        # In cases where it's clear that 'no prior medication' was taken, simply respond with 'NA'.
        # 
        # The goal is to provide a clear, concise summary of the substance and its relevance to cerebrovascular diseases."
                # s <- Sys.time()
                df2$sub_class <-
                        sapply(
                                df2$drug,
                                simplify = TRUE,
                                USE.NAMES = FALSE,
                                FUN =  function(x) {
                                        ask_chatgpt(
                                                paste(
                                                        prompt2,
                                                        "Drug:",
                                                        x
                                                ), temp = 0.1
                                        )
                                }
                        )

                df2 <- df2 %>%
                        mutate(m = str_split(sub_class, pattern = "\\n")) %>%
                        unnest(m) %>%
                        separate(
                                m,
                                into = c("substance",  "class"),
                                sep = " \\| ",
                                extra = "merge"
                        ) %>%
                        mutate(
                                substance = str_trim(substance),
                                class = str_trim(class),
                        )
                # df2 %>% select(-meds, -new) %>% View()
                # Sys.time()-s
                return(df2)
}

# Removing sensitive information that could identify patient or relatives 
# removePossIdentifier <- function(m){
#         str1 <- "([K|k]ontakt|[B|b]evollmächtigt).*?(\\z)"
#         str2 <- "([S|s]ohn|[T|t]ochter|[M|m]utter|[V|v]ater|[B|b]ruder|[S|s]chwester|Schwiegertochter|Schwiegermutter|Schwager|Tante|Onkel|Enkel|Neffe|Nichte|[M|m]ann|[F|f]rau) \\S+ \\S+"
#         str3 <- "([T|t]elefon|[H|h]andy|nummer|[A|a]adresse).*?(\\z)"
#         str4 <- "\\b\\d{4,5}-\\d{4-8}\\b"
#         m2 <- m %>% 
#                 mutate(meds = gsub(str1, "[removed]", meds, perl=TRUE)) %>% 
#                 mutate(meds = gsub(str2, "[removed]", meds, perl=TRUE)) %>% 
#                 mutate(meds = gsub(str3, "[removed]", meds, perl=TRUE)) %>% 
#                 mutate(meds = gsub(str4, "[removed]", meds, perl=TRUE))
#         
#         
# }
# Removing sensitive information that could identify patient or relatives 
removePossIdentifier <- function(column){
        dateRegex <- "\\b\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}\\b"
        str1 <- "([K|k]ontakt|[B|b]evollmächtigt).*?(\\z)"
        str2 <- "([S|s]ohn|[T|t]ochter|[M|m]utter|[V|v]ater|[B|b]ruder|[S|s]chwester|Schwiegertochter|Schwiegermutter|Schwager|Tante|Onkel|Enkel|Neffe|Nichte|[M|m]ann|[H|h]err|[F|f]rau) \\S+ \\S+"
        str3 <- "([T|t]elefon|[H|h]andy|nummer|[A|a]adresse).*?(\\z)"
        str4 <- "\\b\\d{4,5}-\\d{4-8}\\b"
        
        # Apply gsub directly to the input column
        # Remove dates
        column <- gsub(dateRegex, "[date removed]", column, perl=TRUE)
        column <- gsub(str1, "[removed]", column, perl=TRUE)
        column <- gsub(str2, "[removed]", column, perl=TRUE)
        column <- gsub(str3, "[removed]", column, perl=TRUE)
        column <- gsub(str4, "[removed]", column, perl=TRUE)
        
        phraseBegin <- "in unserer stationären Behandlung befand."
        pattern <- paste0(".*?(?=", phraseBegin, ")")
        column <- sub(pattern, "", column, perl=TRUE)
        
        phraseEnd <- "(Wir bedanken uns für).*"
        column <- gsub(phraseEnd, "\\1", column, perl=TRUE)
        
        return(column)
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
                         df_neur_stwbr,
                         df_neur_int,
                         atc) { # mmi,
        df_neur_not <- df_neur_not %>% select(fallnummer, adm_meds_not)
        df_neur_aufnb <- df_neur_aufnb %>% select(fallnummer, adm_meds_aufnb)
        df_neur_stwbr <- df_neur_stwbr %>% select(fallnummer, adm_meds_stwbr)
        df_neur_int <- df_neur_int %>% select(fallnummer, adm_meds_int)
        
        m <- full_join(
                full_join(
                    full_join(df_neur_not,
                          df_neur_aufnb,
                          by = c("fallnummer" = "fallnummer")),
                df_neur_stwbr,
                by = c("fallnummer" = "fallnummer")),
                df_neur_int,
                by = c("fallnummer" = "fallnummer"))
        
        m <- m  %>%  mutate(meds = coalesce(adm_meds_stwbr,
                                       adm_meds_aufnb,
                                       adm_meds_not, adm_meds_int)) %>%
                select(-c(adm_meds_stwbr, adm_meds_aufnb, adm_meds_not, adm_meds_int))

        m$meds <- str_replace_all(m$meds, "- ","-")
        m$meds <- str_replace_all(m$meds, " -","-")
        m$meds <- str_replace_all(m$meds, "½","0,5")
        # DeIdentification
        m <- m %>% mutate(meds = removePossIdentifier(meds))
        
        # run once only - 8.5$ per run over OpenAi-API 05/2023 ~ 8hours - GPT3.5
        # run once only - 57$ per run over OpenAi-API 02/2024 ~ 10hours - GPT 4 Turbo
        file_path <- "output/prior_meds.rds"
        
        if (!file.exists(file_path)) {
                m_list <- list()
                m_chunks <- split(m, 1:nrow(m) %% 100)
                # c <- 1
                for (c in seq(1, 100)) {
                        d <- m_chunks[[c]]
                        m_list[[c]] <- extractMedsWithGPT_v2(d)
                        # write.csv2(as_tibble(do.call(rbind, m_list)), "output/prior_meds.csv")
                        saveRDS(m_list, 'output/prior_meds.rds', compress = 'xz')
                        print(paste0(c, "..done"))
                }
        } else {
                m_list <- readRDS(file_path)
        }
        
        d <- do.call(rbind, m_list) %>% 
                as_tibble() %>% 
                drop_na(atc) %>% 
                filter(atc!="NA") %>% 
                select(-subgroup, -class) %>% # Classification by GPT4.T in subgroups and class was good but needs manual assignment therefore drop
                left_join(atc %>% select(-ddd,-uom,-adm_r,-note) %>% unique(), by = c("atc"="atc"))
        
        # DEPRECATED
        #
        # # Part was used when GPT3.5 was only available and classification was very weak
        # # after GPT4.Turbo manual classifcation with ask_mmi() was inferior to GPT.
        #
        # Correction of some "no prior medication" classifications
        # if unknown -> NA
        # No prior medication only if stated 
        # d2 bkp
        # d <- do.call(rbind, m_list) %>% as_tibble() %>% 
        #         mutate(meds = trimws(gsub("(?i)[M|m]edi[k|c]ation:?|[M|m]edikamente?: ?", "", meds, perl=TRUE))) %>% 
        #         mutate(
        #                 drug = case_when(
        #                         meds == "" ~ NA_character_,
        #                         is.na(meds) ~ NA_character_,
        #                         drug != "NA" ~ drug,
        #                         str_detect(meds, "(?i)keine|nicht") ~ "no prior medication"
        #                         # is.na(drug) && str_detect(meds, "(?i)Liste|Kurve|Brief|siehe|unklar|regelmäßig ein") ~ NA_character_
        #                 )
        #         )
        # d$drug <- gsub("(?<!no prior )[M|m]edi[k|c]ation|[M|m]edikamente?:?", "", d$drug, perl=TRUE)
        # 
        # 
        # Classify each substance or drug following ATC system | function ask_mmi()
        # file_path2 <- "output/prior_meds_classes.csv"
        # 
        # if (!file.exists(file_path2)) {
        #         s <- Sys.time()
        #         d$sub <-
        #                 sapply(
        #                         d$drug,
        #                         simplify = TRUE,
        #                         USE.NAMES = FALSE,
        #                         FUN =  function(x) {
        #                                 print(x)
        #                                 ask_mmi(mmi, x)
        #                         }
        #                 )
        #         Sys.time() - s
        
        #
        #         d2 <-
        # d %>% select(fallnummer, drug, sub) %>% unnest(sub) %>% unique()
        
        
        file_path2 <- "output/prior_meds_classes.csv"
        
        if (!file.exists(file_path2)) {
        d2 <- d
        # Build df with ATC-classes
                d_class <- d2 %>%
                        group_by(fallnummer, class_e) %>%
                        summarise(drug_class_usage = ifelse(any(!is.na(class_e)), 1, 0), .groups = 'drop') %>%
                        pivot_wider(names_from = class_e,
                                    values_from = drug_class_usage,
                                    values_fill = 0)
                saveRDS(d_class, 'output/prior_meds_classes.rds', compress = 'xz')
                write.csv2(d_class, 'output/prior_meds_classes.csv')
                
                # Build df with ATC-Groups
                d_grp <- d2 %>%
                        group_by(fallnummer, grp_e) %>%
                        summarise(drug_class_usage = ifelse(any(!is.na(grp_e)), 1, 0), .groups = 'drop') %>%
                        pivot_wider(names_from = grp_e,
                                    values_from = drug_class_usage,
                                    values_fill = 0)
                saveRDS(d_grp, 'output/prior_meds_groups.rds', compress = 'xz')
                write.csv2(d_grp, 'output/prior_meds_groups.csv')
                
                # Build df with substances
                d_sub <- d2 %>%
                        group_by(fallnummer, name_e) %>%
                        summarise(drug_class_usage = ifelse(any(!is.na(name_e)), 1, 0), .groups = 'drop') %>%
                        pivot_wider(names_from = name_e,
                                    values_from = drug_class_usage,
                                    values_fill = 0)
                saveRDS(d_sub, 'output/prior_meds_substances.rds', compress = 'xz')
                write.csv2(d_sub, 'output/prior_meds_substances.csv')
        } else {
                d_class <- readRDS('output/prior_meds_classes.rds')
                d_grp <- readRDS('output/prior_meds_groups.rds')
                d_sub <- readRDS('output/prior_meds_substances.rds')
        }
        
        l <- list(CLASS = d_class, GROUP = d_grp, SUBSTANCE = d_sub)
        l <- map(l, ~rename(.x, case_id = fallnummer) %>% mutate(case_id = parse_number(as.character(case_id))))
        
        names(l$CLASS) <- gsub("\\.", "_", make.names(names(l$CLASS)))
        names(l$CLASS) <- gsub("__", "_", make.names(names(l$CLASS)))
        names(l$CLASS) <- gsub("__", "_", make.names(names(l$CLASS)))
        names(l$CLASS) <- str_replace(names(l$CLASS), pattern = "_$", "")
        
        names(l$GROUP) <- gsub("\\.", "_", make.names(names(l$GROUP)))
        names(l$GROUP) <- gsub("__", "_", make.names(names(l$GROUP)))
        names(l$GROUP) <- gsub("__", "_", make.names(names(l$GROUP)))
        names(l$GROUP) <- str_replace(names(l$GROUP), pattern = "_$", "")
        
        names(l$SUBSTANCE) <- gsub("\\.", "_", make.names(names(l$SUBSTANCE)))
        names(l$SUBSTANCE) <- gsub("__", "_", make.names(names(l$SUBSTANCE)))
        names(l$SUBSTANCE) <- gsub("__", "_", make.names(names(l$SUBSTANCE)))
        names(l$SUBSTANCE) <- str_replace(names(l$SUBSTANCE), pattern = "_$", "")
       
        
        # Manual correction for acetylsalicylic_acid in category "dihydrocodeine_and_acetylsalicylic_acid"
        # l$SUBSTANCE <- l$SUBSTANCE %>% mutate(acetylsalicylic_acid = case_when(dihydrocodeine_and_acetylsalicylic_acid==1~1, acetylsalicylic_acid==1~1, TRUE~0))
        
        ### Trying parallel ---- 
        # BUT appear to be slow ...
        # require(parallel)
        # 
        # # Get the number of cores
        # no_of_cores <- detectCores() - 1
        # 
        # # Create a cluster
        # cl <- makeCluster(no_of_cores)
        # 
        # # Export your function to each node of your cluster
        # clusterExport(cl, "ask_mmi")
        # clusterExport(cl, "mmi")
        # clusterExport(cl, "d")
        # clusterExport(cl, "atc")
        # clusterEvalQ(cl, library(tidyverse))
        # clusterEvalQ(cl, library(stringr))
        # clusterEvalQ(cl, library(stringdist))
        # 
        # 
        # # Use parLapply to apply the function over the cluster
        # d$sub <- parSapply(cl, d$drug, 
        #                      simplify = TRUE,
        #                      USE.NAMES = FALSE, 
        #                      FUN = function(x) {ask_mmi(mmi, x)})
        # 
        # # Remember to stop the cluster after you're done
        # stopCluster(cl)
        # 
        # saveRDS(d, "output/med_substances_mmi.rds", compress='xz')
        
        return(l)
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
        
        
        m <- df_meds_prior %>%
                mutate(meds = str_remove(meds, "medikation: |Medikation vor Verlegung: |MEDIKAMENTE: ")) %>%
                separate_rows(meds, sep = ", ") %>%
                mutate(drug_name = tolower(str_extract(meds, "^[a-zA-Z]+( [a-zA-Z]+)*"))) %>%
                select(case_id = fallnummer, drug_name) %>% 
                mutate(drug_name = str_extract(drug_name, "^[^\\s]+")) %>%
                filter(str_length(drug_name) >= 3) %>% 
                drop_na() 
        # 
        mdx2 <- mdx %>% mutate(drug = subst1)
        mdx <- rbind(mdx,mdx2) %>% 
                mutate(drug = str_extract(drug, "^[^\\s-]+")) %>% 
                mutate(subst1 = str_extract(subst1, "^[^\\s-]+")) %>% 
                filter(str_length(drug) >= 3) %>% 
                filter(str_length(subst1) >= 3) %>% 
                filter(!is.na(drug))
        
        # m2 <- m %>% left_join(mdx, by = c("drug_name"="drug")) %>% 
        #         group_by(case_id, drug_name) %>% 
        #         arrange(subst2) %>% 
        #         slice(1L)

        # left_join with stringdist approximation        
        m2 <- m %>% stringdist_left_join(mdx, by = c("drug_name"="drug"), max_dist = 1.5) %>% 
                group_by(case_id, drug_name) %>% 
                arrange(subst2) %>% 
                slice(1L) %>% 
                filter(!is.na(drug))
        
        # reference drug-category by substances
        m3 <-
                m2 %>% pivot_longer(
                        cols = c(subst1, subst2, subst3),
                        names_to = "subst_n",
                        values_to = "subst"
                ) %>% 
                filter(!is.na(subst)) %>% 
                mutate(subst = sub("-|\\(|\\_| ", "", subst)) %>%
                mutate(subst6 = substr(subst, 1, 6)) %>% 
                left_join(atc_drugs %>% 
                                  mutate(subst1 = tolower(coalesce(drug_name, name))) %>%
                                  mutate(subst1 = substr(subst1, 1, 6)) %>% 
                                  select(grp_name, subst1), by = c("subst6"="subst1")) %>% 
                unique()
        
        string ("-|\\(|\\_| ")
        
        
        atc_drugs <- atc_drugs %>%
                mutate(drug_name = tolower(drug_name))
        
        m <- m %>% left_join(atc_drugs, by = "drug_name")
        
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
        # df$Name <- as.character(df$Name)
        # df$name <- as.character(df$name)
        df <- df %>% mutate(name = str_squish(as.character(name)))
        
        
        
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
        join$case_id <- coalesce(join$Fallnummer.x, as.character(join$Fallnummer.y), as.character(join$FallNr))
        
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


#' @title renameLysePat
#'
#' @description renames variables from table "LysePatienten" from DB Heireka
#' @param df A dataframe "LysePatienten" from Heireka DB
#' @import tidyverse
#' @return renamed variables in df
renameLysePat <- function(df){
        var <- tibble(heireka_var = colnames(df))
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
        var <- var %>% mutate(var = colnames(df))
        saveRDS(var, 'variables_labels.rds', compress='xz')
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
#' @import hms
#' @return df
cleanDateTime <- function(df, kenn){
        df <- 
                df %>% 
                # lazy_dt() %>%
                mutate(birth_date = as.Date(parse_date_time2(birth_date, orders = "mdy HMS", cutoff_2000 = 10))) %>%
                mutate(date_therapy = str_replace_all(as.character(date_therapy), "\\(|\\)", "")) %>% 
                mutate(time_onset = str_replace_all(as.character(time_onset), "\\(|\\)", "")) %>% 
                mutate(time_admission = str_replace_all(as.character(time_admission), "\\(|\\)", "")) %>% 
                mutate(onset2door = str_replace_all(as.character(onset2door), "\\(|\\)", "")) %>% 
                mutate(time_iv = str_replace_all(as.character(time_iv), "\\(|\\)", "")) %>% 
                mutate(time_ia = str_replace_all(as.character(time_ia), "\\(|\\)", "")) %>% 
                mutate(time_window = str_replace_all(as.character(time_window), "\\(|\\)", "")) %>% 
                mutate(door_to_needle = str_replace_all(as.character(door_to_needle), "\\(|\\)", "")) %>% 
                mutate(door_to_vessel = str_replace_all(as.character(door_to_vessel), "\\(|\\)", "")) %>% 
                mutate(time_lab_adm = str_replace_all(as.character(time_lab_adm), "\\(|\\)", "")) %>% 
                mutate(time_window = str_replace_all(as.character(time_window), "\\(|\\)", "")) %>% 
                mutate(time_onset = str_replace_all(as.character(time_onset), "\\(|\\)", "")) %>% 
                
                mutate(date_therapy = mdy(as.character(str_extract(pattern = ".*\\s", string = date_therapy)))) %>% 
                mutate(date_therapy_year = year(date_therapy)) %>% 
                mutate(date_therapy_month = month(date_therapy)) %>% 
                mutate(date_therapy_day = day(date_therapy)) %>% 
                mutate(time_admission = ymd_hms(paste(as.character(date_therapy), str_extract(pattern = "(?<= ).{8}", string = time_admission)))) %>% 
                mutate(date_time_iv = ymd_hms(paste(as.character(date_therapy), str_extract(pattern = "(?<= ).{8}", string = time_iv)))) %>% 
                mutate(date_time_ia = ymd_hms(paste(as.character(date_therapy), str_extract(pattern = "(?<= ).{8}", string = time_ia)))) %>% 
                # mutate(time_window_man = lubridate::hms(substr(as.character(time_window), 10,17))) %>% 
                mutate(time_onset = ymd_hms(paste(as.character(date_therapy), str_extract(pattern = "(?<= ).{8}", string = time_onset)))) %>% 
                mutate(door_to_needle = lubridate::hms(substr(as.character(door_to_needle), 10,17))) %>%
                # mutate(door_to_needle_man = hour(door_to_needle)*60+minute(door_to_needle)) %>% 
                mutate(time_lab_adm = mdy_hms(as.character(time_lab_adm))) %>% 
                mutate(onset_to_door = difftime(time_admission, time_onset, units = "mins")) %>% 
                mutate(onset_to_needle = difftime(date_time_iv, time_onset, units = "mins")) %>% 
                mutate(door_to_needle = difftime(date_time_iv, time_admission, units = "mins"))
                
                # mutate(door_to_needle_calc = lubridate::dminutes(time_onset - date_time_iv))
                # mutate(date_time_therapy = parse_date_time2(date_therapy, orders = "mdy HMS", cutoff_2000 = 97)) %>% 
                # mutate(time_iv = as_hms(parse_date_time2(time_iv, orders = "mdy HMS"))) %>% 
                # mutate(time_ia = mdy_hms(time_ia)) %>% 
                # mutate(time_admission = mdy_hms(df$time_admission))
        # Selecting all times 
        # times <- colnames(select_if(df, function(col)
        #                 is.POSIXct(col) | is.Date(col)))
        return(df)
}




#' @title cleanDf
#'
#' @description performs specific variable cleaning an preparation
#' @import haven
#' @param df
#' @import tidyverse 
cleanDfHeireka <- function(df){
    df <- df %>% as_tibble() %>% 
            
            # BASE ----
            # age, sex, nihss_d0, side_stroke, therapy, date_therapy_year
            mutate(across(c(age, nihss_d0), ~parse_number(as.character(.))),
                   sex = recode_factor(sex, "w" = "female", "m" = "male"),
                   side_stroke = recode_factor(side_stroke, "links" = "left", "rechts" = "right", "Vertebrobasilär" = "posterior"),
                   therapy = as_factor(therapy)) %>% 
            filter(age<120,
                   !(sex %in% "")) %>% 
            droplevels() %>%
            
            # TIME ----
            # time window
            mutate(time_window2= mdy_hms(time_window),
                   hours = hour(time_window2),
                   minutes = minute(time_window2),
                   time_window = hours + minutes / 60) %>% 
    
            
            # VITALS ----
            # vitals_bb_systolic, vitals_bb_diastolic
            mutate(across(c("vitals_bb_systolic", "vitals_bb_diastolic"), ~ifelse(.=="-1", NA, as.character(.)))) %>%
            mutate_at(vars(contains("vitals_")), ~ parse_number(as.character(.))) %>% 
            
            # LOGISTICS ----
            # Doktor, TeleLyse, wake_up, offlabel, offlabel_reason
            mutate(across(c(wake_up, TeleLyse), ~ parse_number(as.character(.))),
                   offlabel_reason = recode_factor(offlabel_reason, "74" = "Other", "75" = "Other" , "81" = "Other")) %>%  
            
            # IMAGING ----
            # tici, img_d0_mri, img_d0_ct, img_d0_aspects, img_d0_cta, img_vessel, VesselStatus.Verlauf
            mutate(across(c("tici", "offlabel_reason", "etiology_toast", "img_d0_aspects", "img_ich_hbc"), ~ifelse(.=="", NA, as.character(.))),
                   tici = recode_factor(tici, "nein" = "NA", "unklar" = "NA", "-" = "NA", "ja" = "NA", "TICI 0" = "0", "TICI 1" = "1", "TICI 2a" = "2a", "partiell" = "2a", "TICI 2b" = "2b", "TICI 2c" = "2c", "TICI 3" = "3", .ordered = TRUE),
                   img_d0_aspects = recode_factor(img_d0_aspects, "-" = "NA"),
                   img_d0_aspects = ordered(parse_number(as.character(img_d0_aspects))),
                   img_d0_vessel = recode_factor(img_vessel, "ACI isoliert" = "Carotid-I", "ACI und ACM" = "Carotid-L", "Anterior" = "ACA", "Basilaris" = "BAO", "Carotis-T" = "Carotid-T", "M1" = "MCA (M1 Segment)", "M2" = "MCA (M2 Segment)", "M3" = "MCA (M3 Segment)", "nicht vorliegend" = "NA", "offen" = "patent first angiogram", "Posterior" = "PCA", "sonstig" = "other", "unklar" = "other", "Vertebralis" = "VA", .default = NULL, .missing = NULL),
                   img_d1_vessel = recode_factor(VesselStatus.Verlauf, "ACI isoliert" = "Carotid-I", "ACI und ACM" = "Carotid-L", "Anterior" = "ACA", "Basilaris" = "BAO", "Carotis-T" = "Carotid-T", "M1" = "MCA (M1 Segment)", "M2" = "MCA (M2 Segment)", "M3" = "MCA (M3 Segment)", "nicht vorliegend" = NA_character_, "offen" = "patent first angiogram", "Posterior" = "PCA", "sonstig" = "other", "unklar" = "other", "Vertebralis" = "VA", .default = NULL, .missing = NULL),
                   across(c(img_d0_cta, img_d0_mri), ~ parse_number(as.character(.)))) %>%  
            
            # THERAPY ---- 
            # therapy_intracranial, therapy_extracranial, sedation_type, therapy_add_gbIIbIIIa
            mutate(therapy_intracranial = recode_factor(therapy_intracranial, "nachgucken" = "other", "nicht möglich" = "other", "nicht nötig" = "other", "nichts" = "no device", "sonstiges" = "other", "Stent, passager" = "Stent (transient)", "Stent, permanent" = "Stent (permanent)", "unklar" = "other")) %>%           
            mutate(sedation_type = recode_factor(sedation_type, "ITN" = "endotracheal anaesthesia", "Nichts" = "no sedation", "Sedierung" = "(conscious) sedation", "Wechsel" = "conversion", .default = NULL, .missing = NULL)) %>% 
            mutate(therapy_gbIIbIIIa = parse_number(as.character(therapy_add_gbIIbIIIa))) %>% 
            
            # RISK FACTOR ----
            # risk_factor_htn, risk_factor_dm, risk_factor_hchol, risk_factor_current_smoker, risk_factor_prev_stroke, risk_factor_khd, risk_factor_pavk, risk_factor_af, risk_factor_dialysis)
            mutate(across(contains("risk_factor"), ~ifelse(.=="-1", NA, as.character(.))),
                   risk_factor_af_known = (risk_factor_af == "vorbekannt")/1,
                   across(starts_with("risk_factor"), ~as.factor(.))) %>% 
    
            # PRIOR MEDICTAION ----
            # pre_med_apt, pre_med_oak, pre_med_statin)
           mutate(pre_med_apt = as.character(pre_med_apt),
                  pre_med_apt = ifelse(pre_med_apt == "-1", NA, pre_med_apt),
                  pre_med_oak = as.factor(pre_med_oak),
                  pre_med_statin = as.factor(pre_med_statin)) %>%
            
            # LABS ----
              mutate(across(c("lab_d0_glu", "lab_d0_thr", "lab_d0_crea", "lab_d0_uric_acid", "lab_d0_inr", "lab_d0_hba1c", "lab_d0_chol", "lab_d0_ldl", "lab_d0_hdl", "lab_d0_trigl"), ~ifelse(.=="-1", NA, as.character(.)))) %>%
              mutate_at(vars(contains("lab_d0")), as.numeric) %>% 
            #  
           
            # ETIOLOGY ---- 
            # etiology_toast, risk_factor_af_known, etio_afib_new, etio_mtoast, etio_dissection, etio_stroke_mimic
            mutate(etio_afib_new = risk_factor_af == "NeuDx",
                   etio_mtoast = recode_factor(etiology_toast, "-" = "NA", "ungeklärt" = "Stroke of undetermined etiology (SUD)", "kardiogen" = "Cardioembolism (CE)", "artherothrombotisch" = "Atherothrombosis (AT)", "Dissektion" = "Stroke of other determined etiology (SOD)", "lokal thrombotisch" = "Atherothrombosis (AT)", "mikroangiopathisch" = "Small artery disease (SAD)", "konkurrierende Ursachen" = "More than two causes identified (SUDm)", "andere Ursache" = "Stroke of undetermined etiology (SUD)", "sonstiges" = "Uncertain determination (SUDu)", "Gerinnungsstörung" = "Stroke of other determined etiology (SOD)", "psychogen" = "Uncertain determination (SUDu)", "kein Schlaganfall" = "Uncertain determination (SUDu)", "-" = "Incomplete evaluation (SUDi)", "iatrogen" = "Stroke of other determined etiology (SOD)"),
                   etio_dissection = (etiology_toast == "Dissektion")/1,
                   etio_stroke_mimic = (etiology_toast %in% c("kein Schlaganfall", "psychogen"))/1) %>% 
        
            # OUTCOME ----
            # img_d1_infsize_onethird, img_ich, img_ich_ecass2, img_ich_hbc, outcome_fatal_ich, outcome_nihss_discharge, outcome_ihd, discharge_towhere, outcome_mrsd90)
            mutate(img_ich_hbc = recode_factor(img_ich_hbc, "nein" = "no hemorrhagic transformation", "neinn" = "no hemorrhagic transformation", "-" = "NA", "sonstiges" = "NA", "1a/HI" = "1a/HI1"),
              # mutate(img_ich_hbc = ordered(img_ich_hbc, levels =  c("no hemorrhagic transformation","1a/HI1", "1b/HI2", "1c/PH1", "2/PH2","3a/rPH1", "3a/rPH2", "3b/intraventrikulär", "3c/SAB", "3d/SDH"))) %>% 
              outcome_mrsd90 = recode_factor(as.character(outcome_mrsd90), "0" = "0", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "8" = NA_character_, .ordered = TRUE),
              outcome_fatal_ich = parse_number(as.character(outcome_fatal_ich)),
              outcome_nihss_discharge = parse_number(as.character(outcome_nihss_discharge))) %>% 
            
            # ALL ----
        droplevels() %>% 
        mutate_all(~ {attr(., "labels") <- NULL; .})
            
    return(df)
}
 
chunkDf_Hei <- function(df) {
     
        df <- df %>% mutate(case_id = parse_number(as.character(case_id)))
        
     # base
     base_tibble <- df %>% select(case_id, age, sex, nihss_d0, side_stroke, therapy, date_therapy_year)
     
     # vitals
     vitals_tibble <- df %>% select(case_id, vitals_bb_systolic, vitals_bb_diastolic)
     
     # logistics
     logistics_tibble <- df %>% select(case_id, Doktor, TeleLyse, wake_up, offlabel, offlabel_reason)
     
     # imaging
     imaging_tibble <- df %>% select(case_id, tici, img_d0_mri, img_d0_ct, img_d0_aspects, img_d0_cta, img_d0_vessel, img_d1_vessel)
     
     # date_time (old)
     date_time_old_tibble <- df %>% select(case_id, time_window, date_therapy, time_onset, OnsetTimeType, time_admission, onset2door, time_iv, time_ia,  door_to_needle, door_to_vessel)
     
     # date_time (new)
     date_time_new_tibble <- df %>% select(case_id, date_therapy_year, date_therapy_month, date_therapy_day, date_time_iv, date_time_ia, onset_to_door, onset_to_needle, adm_day)
     
     # therapy
     therapy_tibble <- df %>% select(case_id, therapy_intracranial, therapy_extracranial, sedation_type, therapy_add_gbIIbIIIa)
     
     # risk factor
     risk_factor_tibble <- df %>% select(case_id, risk_factor_htn, risk_factor_dm, risk_factor_hchol, risk_factor_prev_stroke, risk_factor_khd, risk_factor_pavk, risk_factor_af_known, risk_factor_dialysis)
     # risk_factor_current_smoker excluded since new variable
     
     # prior medication
     prior_medication_tibble <- df %>% select(case_id, pre_med_apt, pre_med_oak, pre_med_statin)
     
     # labs
     labs_tibble <- df %>% select(case_id, lab_ext, lab_d0_glu, lab_d0_leu, lab_d0_thr, lab_d0_crea, lab_d0_urea, lab_d0_uric_acid, lab_d0_inr, lab_d0_hba1c, lab_d0_chol, lab_d0_ldl, lab_d0_hdl, lab_d0_trigl)
     
     # etiology
     etiology_tibble <- df %>% select(case_id, etiology_toast, risk_factor_af_known, etio_afib_new, etio_mtoast, etio_dissection, etio_stroke_mimic)
     
     # outcome
     outcome_tibble <- df %>% select(case_id, img_d1_infsize_onethird, img_ich, img_ich_ecass2, img_ich_hbc, outcome_fatal_ich, outcome_nihss_discharge, outcome_ihd, discharge_towhere, outcome_mrsd90) %>% 
        mutate(outcome_mortality = (outcome_mrsd90>5)/1,
               outcome_mrs01 = (outcome_mrsd90<2)/1,
               outcome_mrs02 = (outcome_mrsd90<3)/1)     
     
     # Creating a list of tibbles
     list_of_tibbles <- list("base" = base_tibble,
                             "vitals" = vitals_tibble,
                             "logistics" = logistics_tibble,
                             "imaging" = imaging_tibble,
                             "date_time_old" = date_time_old_tibble,
                             "date_time_new" = date_time_new_tibble,
                             "therapy" = therapy_tibble,
                             "risk_factor" = risk_factor_tibble,
                             "prior_medication_HEI" = prior_medication_tibble,
                             "labs_HEI" = labs_tibble,
                             "etiology" = etiology_tibble,
                             "outcome" = outcome_tibble)
     
     
     
}
     
     
# 
# 
# df
# # base
# case_id, age, sex, nihss_d0, side_stroke, therapy, 
# # vitals
# vitals_bb_systolic, vitals_bb_diastolic,
# # logistics
# Doktor, TeleLyse, wake_up, offlabel, offlabel_reason
# # imaging
# tici, img_d0_mri, img_d0_ct, img_d0_aspects, img_d0_cta, img_vessel, VesselStatus.Verlauf,
# # date_time (old)
# time_window, date_therapy, time_onset, OnsetTimeType, time_admission, onset2door, time_iv, time_ia,  door_to_needle, door_to_vessel,
# # date_time (new)
# date_therapy_year, date_therapy_month, date_therapy_day, date_time_iv, date_time_ia, onset_to_door, onset_to_needle, adm_day
# #therapy
# therapy_intracranial, therapy_extracranial, sedation_type, therapy_add_gbIIbIIIa, 
# # risk factor
# risk_factor_htn, risk_factor_dm, risk_factor_hchol, risk_factor_current_smoker, risk_factor_prev_stroke, risk_factor_khd, risk_factor_pavk, risk_factor_af, risk_factor_dialysis,
# # prior medication
# pre_med_apt, pre_med_oak, pre_med_statin,
# # labs
# lab_ext, lab_d0_glu, lab_d0_leu, lab_d0_thr, lab_d0_crea, lab_d0_urea, lab_d0_uric_acid, lab_d0_inr, lab_d0_hba1c, lab_d0_chol, lab_d0_ldl, lab_d0_hdl, lab_d0_trigl,
# # etiology
# etiology_toast, risk_factor_af_known, etio_afib_new, etio_mtoast, etio_dissection, etio_stroke_mimic
# # outcome
# img_d1_infsize_onethird, img_ich, img_ich_ecass2, img_ich_hbc, outcome_fatal_ich, outcome_nihss_discharge, outcome_ihd, discharge_towhere, outcome_mrsd90


     



#' @title transformHeirekaDB
#'
#' @description performs the following functions: 1) renameLysePat, 2) cleanDateTime 3)joinDemog on Heireka "LysePatienten" DF (input is whole DB)
#' 
#' @param file
#' @import tidyverse
#' @import lazyData
transformHeirekaDB <- function(file, demog = demog){
        df <- file['LysePatienten'][[1]]
        df <- df %>% 
                renameLysePat() %>%
                cleanDateTime(kenn = kenn) %>% 
                as_tibble %>%
                joinDemog(demog = demog)  %>% 
                cleanDfHeireka()
                # mutate(case_id = parse_number(as.character(case_id)))
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

#' @title getPatientIdentifier - Function to retrieve patient identifiers such as case_ids patient ids, name, birth_date
#' @description Retrieves information from the following sources: df_hei, demog, kenn, validates names and birth_dates
#' @param df_hei Heireka df
#' @param demog Demography file
#' @param dok_nr List of documents with document numbers
#' @param year_min select years to be extracted - lower bound - standard is 2015
#' @param year_max select years to be extracted - upper bound - standard is present year
#' @import tidyverse
#' @import stringdist
#' @import lubridate
#' @return a tibble
#' @export
#'
#' @examples id <- getPatientIdentifier(df_hei, demog = FALSE, removeNA = TRUE)
getPatientIdentifier <- function(df_hei = df_hei, demog = demog, dok_nr = dok_nr, year_min="2015", year_max=year(today())) 
{
        df1 <- df_hei %>% 
                mutate(case_id = parse_number(as.character(df_hei$case_id))) %>% 
                # filter(date_therapy_year<2022) %>% 
                select(LfdNr, case_id, name.x, birth_date, Patient)
        
        df2 <- demog %>% 
                mutate(case_id = parse_number(as.character(Fall))) %>% 
                mutate(birth_date2 = ymd(as.character(`Geb.`))) %>% 
                select(case_id, Name, birth_date2, Patient)
        
        j <- left_join(df1, df2, by = "case_id") %>% unique() %>% 
                mutate(check_name = name.x == Name) %>% 
                mutate(check_birthdate = birth_date == birth_date2) %>%
                mutate(dist_name = stringdist(name.x, Name)) %>% 
                mutate(dist_birthdate = stringdist(birth_date, birth_date2)) %>% 
                mutate(pat_id = Patient.y) %>% 
                group_by(LfdNr) %>% mutate(dupe = n()>1) %>% ungroup() %>% 
                filter(!(dupe==TRUE & is.na(`Patient.y`)))
        
        # pat_id = NA
        pat_id_na <- j %>% filter(is.na(pat_id))
        demog$`Geb.` <- ymd(as.character(demog$`Geb.`))
        ## search via exact birth_date then matching closest name
        
        j2 <- left_join(pat_id_na, demog, by=c("birth_date"="Geb.")) %>% 
                mutate(dist_name2 = stringdist(name.x, Name.y)) %>% 
                select(LfdNr, case_id, name.x, birth_date, Name.y, dist_name2, Patient, Fall) %>% 
                group_by(LfdNr) %>% slice_min(dist_name2) %>% ungroup() %>% filter(dist_name2<5) %>% 
                group_by(LfdNr) %>% 
                fill(Patient, .direction = "down") %>%
                fill(Patient, .direction = "up") %>%
                ungroup() %>% 
                unique()
        ## search via name then matching closest birth_date
        j3 <- left_join(pat_id_na, demog, by=c("name.x"="Name")) %>% 
                mutate(dist_birthdate2 = stringdist(birth_date, `Geb.`)) %>% 
                select(LfdNr, case_id, name.x, birth_date, `Geb.`, dist_birthdate2, Patient, Fall) %>% 
                group_by(LfdNr) %>% slice_min(dist_birthdate2) %>% ungroup() %>% filter(dist_birthdate2<3) %>% 
                group_by(LfdNr) %>% 
                fill(Patient, .direction = "down") %>%
                fill(Patient, .direction = "up") %>%
                ungroup() %>% 
                unique()
        # Pat_id von j3 kann auf jeden Fall übernommen werden, manche Pat mit 2 Fällen -> müssen händisch recherchiert werden
        
        
        jx <- left_join(
                left_join(j,j2, by="LfdNr"),
                j3, by = "LfdNr") %>% 
                mutate(pat_id2 = coalesce(Patient.x.x, Patient.y.y)) %>%
                mutate(case_id_c = coalesce(Fall.x, Fall.y)) %>%
                mutate(name_c = coalesce(Name.y, name.x, name.x.y)) %>% 
                mutate(birth_date_c = `Geb.`) %>% 
                # rename(check_name = check_name.x) %>%
                # rename(check_birthdate = check_birthdate.x) %>%
                # rename(Name = Name.x) %>% 
                # rename(birth_date2 = birth_date2.x) %>% 
                # filter(is.na(pat_id)) %>%
                mutate(name = case_when(
                        check_name==FALSE & check_birthdate==TRUE ~ Name,
                        check_name==TRUE & check_birthdate==TRUE ~ Name,
                        check_name==FALSE & check_birthdate==FALSE ~ Name,
                        check_name==TRUE & check_birthdate==FALSE ~ Name,
                )) %>% 
                mutate(birth_date_c = case_when(
                        check_name==TRUE & check_birthdate==FALSE ~ birth_date2,
                        check_name==TRUE & check_birthdate==TRUE ~ birth_date2,
                        check_name==FALSE & check_birthdate==TRUE ~ birth_date2,
                        check_name==FALSE & check_birthdate==FALSE ~ birth_date2, 
                )) %>% 
                mutate(pat_id_false = str_detect(pat_id, "[:alpha:]")/1 | nchar(pat_id)>10 | nchar(pat_id)<4 | str_detect(pat_id, "-")) 
        
        # group_by(case_id) %>% drop_na(`Patient.y`) %>% ungroup()
        
        # demog file sometimes lists wrong patient ids -> detect and get the right one from other source (dok_nr)
        wrong_pat_id <- jx %>% filter(pat_id_false==TRUE) %>% select(LfdNr, case_id.x)
        right_pat_id <- dok_nr %>% filter(FALNR %in% wrong_pat_id$case_id.x) %>% select(FALNR, PATNR) %>% unique() %>% mutate(case_id = parse_number(as.character(FALNR)))
        
        jx2 <- left_join(jx, right_pat_id, by = c("case_id.x" = "case_id")) %>% mutate(pat_id = coalesce(PATNR, pat_id)) %>% 
                select(LfdNr, pat_id, pat_id2, name_c, birth_date_c, case_id, case_id_c) %>% 
                mutate(pat_id = coalesce(pat_id2, pat_id))
        
        
        df <- left_join(df_hei,
                        jx2 %>% select(LfdNr, case_id_c, name_c, birth_date_c, pat_id, pat_id2), 
                        by = "LfdNr")
        
        
        
        # NRAD HD (Möhlenbruch) benötigt nicht Fallnummer sondern Patientennummer als ID
        year_min <- parse_number(as.character(year_min))-1
        year_max <- parse_number(as.character(year_max))+1
        nrad_hd_pat_id <- 
                df %>%
                filter(date_therapy_year > year_min) %>%
                filter(date_therapy_year < year_max) %>%
                filter(therapy %in% c("EST","IVT/EST")) %>% 
                # select(LfdNr, contains("name"), contains("case"), contains("birth"), contains("pat"), date_therapy) %>% 
                select(LfdNr, name.x, case_id, birth_date, pat_id, date_therapy) %>% 
                mutate(case_id = parse_number(as.character(case_id))) %>%
                mutate(pat_id = parse_number(as.character(pat_id))) %>% 
                unique()
        return(nrad_hd_pat_id)
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

# classifies each lab value as being "high", "low" or within normal ranges based on the normal ranges given by the labaratory
classifyLab <- function(lab) {
        
        # determine the lab limits based on ">","<" and "-"(range)
        parse_normal <- function(normal) {
                if(grepl("-", normal)) {
                        limits <- as.numeric(unlist(strsplit(normal, "-")))
                        return(list(lower = limits[1], upper = limits[2]))
                } else if(grepl("<", normal)) {
                        limit <- as.numeric(gsub("<", "", normal))
                        return(list(lower = NA, upper = limit))
                } else if(grepl(">", normal)) {
                        limit <- as.numeric(gsub(">", "", normal))
                        return(list(lower = limit, upper = NA))
                } else {
                        return(list(lower = NA, upper = NA))
                }
        }
        
        lab <- lab %>%
                group_by(Text) %>% 
                mutate(Percentile99 = quantile(Wert, 0.999, na.rm = TRUE)) %>% 
                mutate(Percentile01 = quantile(Wert, 0.01, na.rm = TRUE)) %>% 
                ungroup() %>% 
                mutate(Normal_parsed = map(Normal, parse_normal)) %>% 
                unnest_wider(Normal_parsed) %>% 
                mutate(Status = case_when(
                        is.na(lower) & Wert > upper ~ ifelse(Wert>Percentile99, "elevated (>99.9th)", "elevated"),
                        is.na(lower) & Wert <= upper ~ "normal",
                        is.na(upper) & Wert < lower ~ ifelse(Wert<Percentile01,"decreased (<0.1th)", "decreased"),
                        is.na(upper) & Wert >= lower ~ "normal",
                        Wert >= lower & Wert <= upper ~ "normal",
                        Wert < lower ~ ifelse(Wert<Percentile01,"decreased (<0.1th)", "decreased"),
                        Wert > upper ~ ifelse(Wert>Percentile99, "elevated (>99.9th)", "elevated"),
                        TRUE ~ "not determined"
                ))
}


#' @title extractLab
#'
#' @description to extract Lab Values in long data format
#' @param labs Raw labs file (ISH) should be provided
#' @param case_id Case Ids 
#' @param lab_select desired lab value e.g. "Natrium" or multiple
#' @param kenn "Kennzahlen" file 
#' @param timeCutStart Start point for lab time interval since admission; standard is "-20" (for in house stroke and admission time errors)
#' @param timeCutEnd End point for lab time interval; standard is 60' (for lab values within 1h after admission)
#' @param slicing TRUE / FALSE; if TRUE value with lowest timeDiff is extracted
#' @param median Calculates median value of patient for each lab parameter chose in lab_select
#' @param min Calculates min value of patient for each lab parameter chose in lab_select
#' @param max Calculates max value of patient for each lab parameter chose in lab_select
#' @param q25 Calculates q25 value of patient for each lab parameter chose in lab_select
#' @param q75 Calculates q75 value of patient for each lab parameter chose in lab_select
#' @return A tibble
#' @export
#'
#' @examples
extractLab <- function(labs = labs,
                       case_id = case_id,
                       lab_select,
                       kenn = kenn,
                       timeCut = FALSE,
                       timeCutStart = "-20",
                       timeCutEnd = "60",
                       slicing = TRUE,
                       classify = FALSE,
                       limit = "1",
                       N = TRUE,
                       median = FALSE,
                       min = FALSE,
                       max = FALSE,
                       q25 = FALSE,
                       q75 = FALSE)
{
    # Filtering double value, case_ids and lab_selection
    status <- names(labs[str_detect(names(labs), "Status")])[1]
    case_id <- case_id %>% mutate(case_id = parse_number(case_id))
    labs <- labs %>%
        mutate(Fall = parse_number(Fall)) %>% 
        filter(Fall %in% case_id[[1]]) %>% 
        filter(Text %in% lab_select) %>%
        #fitler({{status}} != "HI") %>% 
        filter(is.na(Status...25)) %>%
        distinct() %>% 
        mutate(labDateTime = ymd_hms(paste(DokDatum, DokZeit)))
    
    # Need Kennzahl for hospital admission times
    kenn <- kenn %>% 
        mutate(Fall = parse_number(Fall)) %>% 
        mutate(hospDateTime = ymd_hms(paste(Aufn.Datum, Aufn.Zeit))) %>% 
        select(-Einheit)
    
    # Join
    dfLab <-        
        left_join(labs, kenn, by = "Fall") %>%
        # Calculate Difference between hospital admission and lab Time
        mutate(timeDiff = difftime(labDateTime, hospDateTime, units = "mins")) %>% 
        # remove "<"
        mutate(Wert = str_replace_all(Wert, "<", "")) %>% 
        mutate(Normal = gsub(x = Normal, pattern = " ", replacement = "", perl=TRUE)) %>% 
        select(Fall, Text, Wert, Einheit, Normal, timeDiff, labDateTime, hospDateTime) %>%
        distinct() %>%
            # convert those lab values that have meaningful string values but are no numbers
            mutate(Wert = str_replace_all(Wert, "^.*pos.*$", "1")) %>% 
            mutate(Wert = str_replace_all(Wert, "^.*neg.*$", "0")) %>% 
            mutate(Wert = parse_number(Wert)) %>% 
            drop_na(Wert)
   
    # get number of lab values per case from desired lab value - if N=TRUE
    if (N) {
            dfLab <- dfLab %>% group_by(Fall, Text) %>% 
                    summarise(n = n()) %>% 
                    ungroup() %>% 
                    right_join(dfLab, by = c("Fall", "Text"))
    }
    # if timeCut is specified timeCutStart and timeCutEnd is filtered
    if (timeCut) {
            dfLab <- dfLab %>% group_by(Fall, Text) %>%
                    arrange(hospDateTime, timeDiff) %>%
                    filter(timeDiff > as.numeric(timeCutStart)) %>%
                    filter(timeDiff < as.numeric(timeCutEnd)) %>%
                    ungroup()
    }
    # if median = TRUE, the median value of a available values will be calculated
    if (median) {
            dfLab <- dfLab %>%
                    group_by(Fall, Text) %>%
                    mutate(median = median(Wert))
    }
    # if min = TRUE, the min value of a available values will be calculated
    if (min) {
            dfLab <- dfLab %>%
                    group_by(Fall, Text) %>%
                    # slice_min(Wert)
                    mutate(min = min(Wert))
    }
    # if max = TRUE, the max value of a available values will be calculated
    if (max) {
            dfLab <- dfLab %>%
                    group_by(Fall, Text) %>%
                    # slice_min(Wert)
                    mutate(max = max(Wert))
    }
    
    if (q25) {
            dfLab <- dfLab %>%
                    group_by(Fall, Text) %>%
                    # slice_min(Wert)
                    mutate(q25 = quantile(Wert)[[2]])
    }
     
    if (q75) {
            dfLab <- dfLab %>%
                    group_by(Fall, Text) %>%
                    # slice_min(Wert)
                    mutate(q75 = quantile(Wert)[[4]])
    }       
    # if slicing is specified then the value with the lowest time difference will be preserved  
    if (slicing) {
           dfLab <- dfLab %>% 
                   group_by(Fall, Text) %>%
                    arrange(timeDiff) %>%
                    slice(1L) %>% 
                   ungroup()
    }
    
    if (classify) {
            dfLab <- classifyLab(dfLab)
    }
    
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
            df_hei %>% select(LfdNr, name.x, birth_date, case_id, date_therapy, date_therapy_year),
            df_stamm %>% select(name, PID, birth_date, echo_date),
            by = c("name.x" = "name", "birth_date" = "birth_date")
        ) %>% unique() %>%
        mutate(PID = as.numeric(as.character(PID))) 
        # mutate(check = format(echo_date, format = "%Y") == date_therapy_year)
    
    
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
                    select(UID, PID, Jahrgang, Untersuchungszeit) %>%
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

#' @title combineNechoSources - Simple Wrapper function
#' @description combineNechoSources - Simple Wrapper function for 
#' @param necho_17ff 
#' @param necho_17ff_txt
#' @param necho_16pre
#' @param necho_02_db
#' @param df_hei
#' @param df_hei_stamm
#'
#' @return A tibble
#' @export
combineNechoSources <- function(necho_17ff, necho_17ff_txt, necho_16pre, necho_02_db, df_hei, df_hei_stamm) {
    # n1 = NECHO from digital Source after 2017 - can be matched via case_id
    n1 <- necho_17ff %>%
        left_join(
            .,
            necho_17ff_txt %>% select(REFKEY, necho_finding_txt = CONTENT),
            by = c("HS_BEFTXT" = "REFKEY")
        ) %>%
        # left_join(
        #     .,
        #     necho_17ff_txt %>% select(REFKEY, necho_intpret_txt = CONTENT),
        #     by = c("HS_BEURTXT" = "REFKEY")
        # ) %>% 
        rename(
            "fallnummer" = "HS_FALLNR",
            "necho_year" = "HS_AUFDAT",
            "necho_date" = "HS_BEFDAT",
            "doc_type" = "NE_BE_MODI",
            "doc_version" = "DOKVR",
            "doc_number" = "DOKNR",
            "txt" = "necho_finding_txt",
            "necho_pic_quality" = "NE_BE_BILQ",
            "necho_aortic_root" = "NE_BE_AORT",
            "necho_left_atrium" = "NE_BE_LVOR",
            "necho_septum"  = "NE_BE_SEPT",
            "necho_hinterwand" = "NE_BE_HINT",
            "necho_lv_enddia" = "NE_BE_LVED",
            "necho_ef" = "NE_BE_EF",
            "necho_rv" = "NE_BE_RV",
            "necho_vci" = "NE_BE_VENA",
            "necho_aortic_sten"  = "NE_BE_STEN",
            "necho_aortic_pht" = "NE_BE_PHT0",
            "necho_aortic_dez" = "NE_BE_DEZ",
            "necho_aortic_insuf" = "NE_BE_INSU",
            "necho_aortic_ppg" = "NE_BE_PPG0",
            "necho_aortic_mpg" = "NE_BE_MPG",
            "necho_aortic_kof" = "NE_BE_KOFD",
            "necho_mitral_insuf" = "NE_BE_INS1",
            "necho_mitral_sten" = "NE_BE_STE1",
            "necho_mitral_ppg" = "NE_BE_PPGM",
            "necho_mitral_mpg" = "NE_BE_MPG1",
            "necho_mitral_kof" = "NE_BE_KOF1",
            "necho_mitral_padr" = "NE_BE_PADR",
            "necho_tric_valve" = "NE_BE_STE2",
            "necho_tric_insuf" = "NE_BE_INS2",
            "necho_pulm_valve" = "NE_BE_STE3",
            "necho_heart_rhythm" = "NE_BE_HERZ"
        ) %>% 
        select(fallnummer, necho_date, doc_type, txt, necho_pic_quality, necho_aortic_root, necho_left_atrium, necho_septum, necho_hinterwand, necho_lv_enddia, necho_vci,
               # necho_ef, necho_rv, necho_aortic_sten, necho_aortic_pht, necho_aortic_dez, necho_aortic_insuf, necho_aortic_ppg, necho_aortic_mpg, necho_aortic_kof, necho_mitral_insuf, necho_mitral_sten, necho_mitral_ppg, necho_mitral_mpg, necho_mitral_kof, necho_mitral_padr, necho_tric_valve, necho_tric_insuf, necho_pulm_valve, necho_heart_rhythm, date_adm_hosp
               ) %>% 
        mutate(necho_date = dmy(necho_date)) %>% 
               unique()
 
    
    # n2 <- necho_16pre
    n2 <- necho_16pre %>% select(fallnummer, necho_date, doc_type, txt, necho_pic_quality, necho_aortic_root, necho_left_atrium, necho_septum, necho_hinterwand, necho_lv_enddia, necho_vci, 
                                 # necho_ef, necho_rv, necho_aortic_sten, necho_aortic_pht, necho_aortic_dez, necho_aortic_insuf, necho_aortic_ppg, necho_aortic_mpg, necho_aortic_kof, necho_mitral_insuf, necho_mitral_sten, necho_mitral_ppg, necho_mitral_mpg, necho_mitral_kof, necho_mitral_padr, necho_tric_valve, necho_tric_insuf, necho_pulm_valve, necho_heart_rhythm
                                 ) %>% 
        mutate(necho_date = dmy(necho_date)) %>% 
        unique()
    
    n <-
        rbind(n1, n2)  
        # %>% mutate(LfdNr = NA, .before = fallnummer)
        # %>% left_join(
        #     .,
        #     df_hei_stamm %>% select(LfdNr, case_id),
        #     by = c("fallnummer" = "case_id")) %>% 
        # relocate(LfdNr, .before = fallnummer)
        # 
    
    # n3 <- necho_2002_db ...
    
    
    df_hei_stamm$PID <- parse_number(as.character(df_hei_stamm$PID))
    necho_02_db$EchoTab$PID  <- parse_number(as.character(necho_02_db$EchoTab$PID))
    n3 <- df_hei_stamm %>%
        left_join(.,
                  necho_02_db$EchoTab,
                  by = c("PID" = "PID")) %>%
        rename(
            "fallnummer" = "case_id",
            "necho_date" = "echo_date",
            "doc_type" = "UTyp",
            "txt" = "Beurteilung",
            "necho_pic_quality" = "Bemerkung",
            "necho_aortic_root" = "Aorten.D",
            "necho_left_atrium" = "LA.D",
            "necho_septum"  = "Septum.D",
            "necho_hinterwand" = "HW.D",
            "necho_lv_enddia" = "LV.D.dias",
            "necho_vci" = "VCI.D"
        ) %>%
        select(
            fallnummer,
            necho_date,
            doc_type,
            txt,
            necho_pic_quality,
            necho_aortic_root,
            necho_left_atrium,
            necho_septum,
            necho_hinterwand,
            necho_lv_enddia,
            necho_vci
        ) %>%
        # ungroup() %>%
        # select(-LfdNr) %>%
        unique()
        # 
    # n3$fallnummer <- parse_number(as.character(n3$fallnummer))
    
    df_necho_tte <- rbind(n, n3) %>% 
        mutate(fallnummer = parse_number(as.character(fallnummer))) %>% 
        unique() %>% 
        drop_na(necho_left_atrium)
    
     
    df_help <- 
        df_hei %>% 
        select(case_id, Aufn.Datum) %>% 
        mutate(case_id = parse_number(as.character(case_id)))
        # ungroup() %>% 
        # select(-LfdNr)

    j <- df_necho_tte %>% 
        left_join(.,df_help, by = c("fallnummer" = "case_id")) %>% 
        mutate(necho_diff_days = parse_number(as.character(necho_date - Aufn.Datum))) %>% 
        filter(necho_diff_days>-9) %>% 
        filter(necho_diff_days<30) %>% 
        mutate(na_sum = rowSums(is.na(.))) %>% 
        group_by(fallnummer) %>%
        arrange(-desc(na_sum)) %>% 
        slice(1L)
       
      return(j)  
}    

#' @title extractDopValues - Extracts "Untersuchungstiefe", "Sys", "Dias" in 
#' @description combineNechoSources - Simple Wrapper function for 
#' @param df A tibble generated by function extractFromDPDFs() 
#' @param col Desired columns to apply function on
#'
#' @return A tibble including three new columns "_UT","_SYS","_DIAS"
#' @export
#' @examples e <- extractDopValues(dop2, "GTFE_TCD_ACMDISTAL_RECHTS"))
extractDopValues <- function(df, col){
    var <- paste0(col, "_UT")
    var2 <- paste0(col, "_SYS")
    var3 <- paste0(col, "_DIAS")
    
    df <- df %>% 
        mutate({{var}} := str_extract(!!sym(paste0(col)), "..(?=mm)")) %>% 
        mutate({{var2}} := parse_number(as.character(
            str_replace_all(
                str_extract(!!sym(paste0(col)), ".[:punct:].(?=\\s*/)"),
                ",",
                ".")))
        ) %>% 
        mutate({{var3}} := parse_number(as.character(
            str_replace_all(
                str_extract(!!sym(paste0(col)), "(?<=/)\\s*.[:punct:]."),
                ",",
                ".")))
        )
    return(df)   
}





combineDopSources <- function(dop, dop2, df_hei){
    # dop from Doppler DB
    d1 <- dop$DuplexTab %>%
        drop_na(LfdNr) %>% 
            mutate(duplex_date = as.Date(parse_date_time2(Untersuchungszeit, orders = "mdy HMS", cutoff_2000 = 80))) %>% 
            mutate(check_duplex = abs(duplex_date - date_therapy)<14) %>%
            mutate(time_diff_duplex = duplex_date - date_therapy) %>% 
        filter(check_duplex == TRUE) %>%
        select(
            LfdNr,
            case_id,
            duplex_date,
            time_diff_duplex,
            Kal.ACC.li,
            Imd.ACCli,
            Kal.ACC.re,
            Imd.ACCre,
            Vsys.ACIprox.li,
            Vsys.ACIdis.li,
            Vsys.ACIprox.re,
            Vsys.ACIdis.re
        ) %>%
        rename_with(.cols = "Kal.ACC.li":"Vsys.ACIdis.re", 
                    ~ tolower(
                        paste0("dup_", .x)
                        )
                    ) %>% 
            mutate(case_id = parse_number(as.character(case_id))) %>% 
            unique() %>% 
            group_by(case_id) %>% 
            arrange(-desc(time_diff_duplex)) %>% 
            slice(1L) %>% 
            mutate(across(everything(), as.character)) %>% 
            select(-time_diff_duplex)
    
    # d2 <- dop$TCDTab %>%
    #     drop_na(LfdNr) %>%
    #     filter(check == TRUE) %>%
    #     mutate(dop_date = echo_date) %>%
    #     select(
    #         case_id,
    #         dop_date,
    #         M50.li.sys,
    #         M50.li.dias,
    #         M50.li.Qual,
    #         M50.re.sys,
    #         M50.re.dias,
    #         M50.re.Qual,
    #         Siph65.li.sys,
    #         Siph65.li.dias,
    #         Siph65.li.Qual,
    #         Siph65.re.sys,
    #         Siph65.re.dias,
    #         Siph65.re.Qual,
    #         A70.li.sys,
    #         A70.li.dias,
    #         A70.li.Qual,
    #         A70.re.sys,
    #         A70.re.dias,
    #         A70.re.Qual
    #     ) %>%
    #     rename_with(.cols = "M50.li.sys":"A70.re.Qual",
    #                 ~ tolower(
    #                     paste0("tcd_", .x)
    #                     )
    #                 )
    # 
    # d3 <- dop$TCDupTab %>%
    #     drop_na(LfdNr) %>%
    #     filter(check == TRUE) %>%
    #     mutate(dop_date = echo_date) %>%
    #     select(case_id,
    #            M1.li.sys,
    #            M1.li.dias,
    #            M1.li.Qual,
    #            M1.li.sys,
    #            M1.li.dias,
    #            M1.li.Qual,
    #            M1.re.sys,
    #            M1.re.dias,
    #            M1.re.Qual,
    #            M1.re.sys,
    #            M1.re.dias,
    #            M1.re.Qual,
    #            Siph1.li.sys,
    #            Siph1.li.dias,
    #            Siph1.li.Qual,
    #            Siph1.re.sys,
    #            Siph1.re.dias,
    #            Siph1.re.Qual,
    #            A1.li.sys,
    #            A1.li.dias,
    #            A1.li.Qual,
    #            A1.re.sys,
    #            A1.re.dias,
    #            A1.re.Qual,
    #            P1.li.sys,
    #            P1.li.dias,
    #            P1.li.Qual,
    #            P1.re.sys,
    #            P1.re.dias,
    #            P1.re.Qual,
    #            AV1.li.sys,
    #            AV1.li.dias,
    #            AV1.li.Qual,
    #            AV1.re.sys,
    #            AV1.re.dias,
    #            AV1.re.Qual,
    #            Bas1.sys,
    #            Bas1.dias,
    #            Bas1.Qual,
    #            Bas2.sys,
    #            Bas2.dias,
    #            Bas2.Qual,
    #            Bas3.sys,
    #            Bas3.dias,
    #            Bas3.Qual) %>%
    #     rename_with(.cols = "M1.li.sys":"Bas3.Qual",
    #                 ~ tolower(
    #                     paste0("tccd_", .x)
    #                     )
    #                 )
    #     
    
    # d0 <- left_join(
    #         left_join(d1, d2, by = "case_id"),
    #         d3, by = "case_id") %>% 
    #     unique() %>% 
    # d0 <- d0 %>% mutate(na_sum = rowSums(is.na(.))) %>% 
    #     group_by(case_id) %>%
    #     arrange(-desc(na_sum)) %>% 
    #     slice(1L)
    # 
    # Frequenz Vs Geschwindigkeit problem - manche Werte in cm/s
        
    # names_d0 <- names(d0)
    # paste0(names_d0, collapse = ',')
    # 
    
    ############################################ ###
    # Dop2 from DPDF ############################################ ###
    # names_dop2 <- names(dop2)
    # paste0(names_dop2, collapse = ", ")
     
    df <- dop2 %>% 
            select(FALNR, GDUZ_UNTERSUCHUNGSTAG, GDEZ_ECDUP_KALIBERACC_LINKS, GDEZ_ECDUP_IMTACC_LINKS, GDEZ_ECDUP_KALIBERACC_RECHTS, GDEZ_ECDUP_IMTACC_RECHTS, GDEZ_ECDUP_VMAXACIPROXIMAL_LINKS, GDEZ_ECDUP_VMAXACIDISTAL_LINKS, GDEZ_ECDUP_VMAXACIPROXIMAL_RECHTS, GDEZ_ECDUP_VMAXACIDISTAL_RECHTS) %>% 
            mutate(FALNR = parse_number(FALNR)) %>%
            mutate(GDUZ_UNTERSUCHUNGSTAG = dmy(dop2$GDUZ_UNTERSUCHUNGSTAG)) %>% 
            set_names(names(d1)[2:length(names(d1))])
    df2 <- df_hei %>% select(LfdNr, case_id, date_therapy) %>% mutate(case_id = parse_number(as.character(case_id)))
    
    j <- left_join(df2, df, by = "case_id") %>% 
            mutate(check_duplex = abs(duplex_date - date_therapy)<14) %>% 
            filter(check_duplex==TRUE) %>% 
            mutate(time_diff_duplex = duplex_date - date_therapy) %>% 
            unique() %>% 
            group_by(case_id) %>% 
            arrange(-desc(time_diff_duplex)) %>% 
            slice(1L) %>% 
            select(-check_duplex, -date_therapy) %>% 
            mutate(across(everything(), as.character))
    
    dfx <- bind_rows(d1, j) %>% 
            mutate(LfdNr = parse_number(as.character(LfdNr)))
    dfx <- dfx %>% 
            group_by(LfdNr) %>% 
            fill(everything(), .direction = "up") %>% 
            fill(everything(), .direction = "down") %>% 
            slice_min(duplex_date) %>% 
            unique() %>% 
            mutate(across(starts_with("dup_"), ~parse_number(as.character(.))))
            
    return(dfx)
    # var_names <- deframe(
    #                 tibble(
    #                     "new_names" = c("LfdNr", "case_id", "duplex_date", "dup_kal.acc.li", "dup_imd.accli", "dup_kal.acc.re", "dup_imd.accre", "dup_vsys.aciprox.li", "dup_vsys.acidis.li", "dup_vsys.aciprox.re", "dup_vsys.acidis.re", "dop_date", "tcd_m50.li.sys", "tcd_m50.li.dias", "tcd_m50.li.qual", "tcd_m50.re.sys", "tcd_m50.re.dias", "tcd_m50.re.qual", "tcd_siph65.li.sys", "tcd_siph65.li.dias", "tcd_siph65.li.qual", "tcd_siph65.re.sys", "tcd_siph65.re.dias", "tcd_siph65.re.qual", "tcd_a70.li.sys", "tcd_a70.li.dias", "tcd_a70.li.qual", "tcd_a70.re.sys", "tcd_a70.re.dias", "tcd_a70.re.qual", "tccd_m1.li.sys", "tccd_m1.li.dias", "tccd_m1.li.qual", "tccd_m1.re.sys", "tccd_m1.re.dias", "tccd_m1.re.qual", "tccd_siph1.li.sys", "tccd_siph1.li.dias", "tccd_siph1.li.qual", "tccd_siph1.re.sys", "tccd_siph1.re.dias", "tccd_siph1.re.qual", "tccd_a1.li.sys", "tccd_a1.li.dias", "tccd_a1.li.qual", "tccd_a1.re.sys", "tccd_a1.re.dias", "tccd_a1.re.qual", "tccd_p1.li.sys", "tccd_p1.li.dias", "tccd_p1.li.qual", "tccd_p1.re.sys", "tccd_p1.re.dias", "tccd_p1.re.qual", "tccd_av1.li.sys", "tccd_av1.li.dias", "tccd_av1.li.qual", "tccd_av1.re.sys", "tccd_av1.re.dias", "tccd_av1.re.qual", "tccd_bas1.sys", "tccd_bas1.dias", "tccd_bas1.qual", "tccd_bas2.sys", "tccd_bas2.dias", "tccd_bas2.qual", "tccd_bas3.sys", "tccd_bas3.dias", "tccd_bas3.qual", "na_sum"),
    #                     "old_names" = c("LfdNr", "FALNR", "GDUZ_UNTERSUCHUNGSTAG", "GDEZ_ECDUP_KALIBERACC_LINKS", "GDEZ_ECDUP_IMTACC_LINKS", "GDEZ_ECDUP_KALIBERACC_RECHTS", "GDEZ_ECDUP_IMTACC_RECHTS", "GDEZ_ECDUP_VMAXACIPROXIMAL_LINKS", "GDEZ_ECDUP_VMAXACIDISTAL_LINKS", "GDEZ_ECDUP_VMAXACIPROXIMAL_RECHTS", "GDEZ_ECDUP_VMAXACIDISTAL_RECHTS", "dop_date", "GTFE_TCD_ACMPROXIMAL_LINKS", "tcd_m50.li.dias", "tcd_m50.li.qual", "GTFE_TCD_ACMPROXIMAL_RECHTS", "tcd_m50.re.dias", "tcd_m50.re.qual", "GTFE_TCD_ACMDISTAL_LINKS", "tcd_siph65.li.dias", "tcd_siph65.li.qual", "GTFE_TCD_ACMDISTAL_RECHTS", "tcd_siph65.re.dias", "tcd_siph65.re.qual", "GTFE_TCD_ACA_LINKS", "tcd_a70.li.dias", "tcd_a70.li.qual", "GTFE_TCD_ACA_RECHTS", "tcd_a70.re.dias", "tcd_a70.re.qual", "GTFE_TCDUP_ACMPROXIMAL_LINKS", "tccd_m1.li.dias", "tccd_m1.li.qual", "GTFE_TCDUP_ACMPROXIMAL_RECHTS", "tccd_m1.re.dias", "tccd_m1.re.qual", "GTFE_TCDUP_ACI_LINKS", "tccd_siph1.li.dias", "tccd_siph1.li.qual", "GTFE_TCDUP_ACI_RECHTS", "tccd_siph1.re.dias", "tccd_siph1.re.qual", "GTFE_TCDUP_ACA_LINKS", "tccd_a1.li.dias", "tccd_a1.li.qual", "GTFE_TCDUP_ACA_RECHTS", "tccd_a1.re.dias", "tccd_a1.re.qual", "tccd_p1.li.sys", "tccd_p1.li.dias", "tccd_p1.li.qual", "tccd_p1.re.sys", "tccd_p1.re.dias", "tccd_p1.re.qual", "tccd_av1.li.sys", "tccd_av1.li.dias", "tccd_av1.li.qual", "tccd_av1.re.sys", "tccd_av1.re.dias", "tccd_av1.re.qual", "tccd_bas1.sys", "tccd_bas1.dias", "tccd_bas1.qual", "tccd_bas2.sys", "tccd_bas2.dias", "tccd_bas2.qual", "tccd_bas3.sys", "tccd_bas3.dias", "tccd_bas3.qual", "na_sum")
    #                  )
    #                 )
    # 
    
    # t <- View(dop2$GTFE_TCD_ACMDISTAL_LINKS)
    # dop2$TCD_UT <- str_extract(dop2$GTFE_TCD_ACMDISTAL_LINKS, "..(?=mm)")
    # dop2$TCD_ACM_SYS_LEFT <- str_extract(dop2$GTFE_TCD_ACMDISTAL_LINKS, ".[:punct:].(?=\\s*/)")
    # dop2$TCD_ACM_DIAS_LEFT <- str_extract(dop2$GTFE_TCD_ACMDISTAL_LINKS, "(?<=/)\\s*.[:punct:].")
    # 
    # 
    # test <- dop2 %>%
    #     mutate(TCD_UT = parse_number(as.character(
    #         str_extract(GTFE_TCD_ACMDISTAL_LINKS, "..(?=mm)")
    #     ))) %>%
    #     # mutate(TCD_ACM_SYS_LEFT = parse_number(as.character(str_replace_all(
    #         str_extract(GTFE_TCD_ACMDISTAL_LINKS, ".[:punct:].(?=\\s*/)"),
    #         ",",
    #         "."
    #     )))) %>%         mutate(TCD_ACM_DIAS_LEFT = str_extract(GTFE_TCD_ACMDISTAL_LINKS, "(?<=/)\\s*.[:punct:]."))
    # 


# v <- c("GTFE_TCD_ACMDISTAL_LINKS", 
#        "GTFE_TCD_ACMDISTAL_RECHTS", 
#        "GTFE_TCD_ACMPROXIMAL_LINKS", 
#        "GTFE_TCD_ACMPROXIMAL_RECHTS")
# 
# test <- map_df(v, ~extractDopValues(dop2, .x))


               
               
               }


#' @title getLengthHospStay - 
#' @description Get New Variable from File "Kennzahlen" that determines length of Hopsital Stay in days
#' @param case_id
#' @param kenn
#'
#' @return A tibble
getLengthHospStay <- function(case_id, kenn){
    # case_id <- parse_number(as.character(case_id))
    kenn$Fall <-  parse_number(as.character(kenn$Fall))
    
    d <- case_id %>% 
        mutate(case_id = parse_number(as.character(case_id))) %>% 
        left_join(., kenn,by = c("case_id" = "Fall")) %>% 
        mutate(outcome_lengthHospStay = parse_number(as.character(`Verweild.`))) %>% 
        select(case_id, outcome_lengthHospStay) %>% 
            group_by(case_id) %>% 
            slice_max(outcome_lengthHospStay) %>% 
            unique()
        
    return(d)
}

#' @title getVentilationDays 
#' @description Get New Variable from File "Kennzahlen" that determines ventilation Days
#' @param case_id
#' @param kenn
#'
#' @return A tibble
getVentilationDays <- function(case_id, kenn){
    kenn$Fall <-  parse_number(as.character(kenn$Fall))
    d <- case_id %>% 
        mutate(case_id = parse_number(as.character(case_id))) %>% 
        left_join(., kenn,by = c("case_id" = "Fall")) %>% 
        mutate(outcome_ventilationDays = parse_number(as.character(`Beatmung`))) %>% 
        select(case_id, outcome_ventilationDays) %>% 
            group_by(case_id) %>% 
            slice_max(outcome_ventilationDays) %>% 
            unique()
    return(d)
}


#' @title extractFromDia 
#' @description Extracts Diagnosis codes by searching for desired string - from 1 source: 1.) diagnosis codes
#' @param dia
#' @import tidyverse
#' @return A tibble
extractFromDia <- function(var, var_short, prefix = "outcome", dia=dia, code=FALSE){
        var1 <- paste0(prefix, "_", var_short)
        var2 <- paste0(prefix, "_", var_short,"_code")
        var3 <- paste0(prefix, "_", var_short,"_detail")
        
        if (code) {
        df <- dia %>% 
                filter(str_detect(!!var, string = `RefDiagn.`)) %>%
                mutate(case_id = parse_number(Fall)) %>%
                mutate(!!var1 := 1) %>%
                mutate(!!var3 := str_split_i(`RefDiagn.`, "\\.", 2)) %>% 
                select(case_id, !!var1, !!var3) %>% 
                unique()
        } else {
        df <- dia %>%
                mutate(dx_txt = paste0(`Diagnose...15`, `Diagnose...16`, " | ", `Diagnose...18`)) %>%
                filter(str_detect(!!var, string = dx_txt)) %>%
                mutate(case_id = parse_number(Fall)) %>%
                mutate(!!var1 := 1) %>%
                mutate(!!var2 := `RefDiagn.`) %>% 
                mutate(!!var3 := dx_txt) %>% 
                select(case_id, !!var1, !!var2, !!var3) %>% 
                unique()
        }
        return(df)
}

#' @title extractFromOps 
#' @description Extracts OPS codes by searching for desired string - from 1 source: 1.) ops codes
#' @param var search string (code=FALSE) or code 
#' @param var_short custom short naming of the variable
#' @param prefix desired prefix for variable name - e.g. "outcome", "var", "dx"
#' @param description@param ops
#' @code standard = FALSE, if TRUE explicit code search is done 
#' @import tidyverse
#' @return A tibble
extractFromOps <- function(var, var_short, prefix = "outcome", ops=ops, code=FALSE){
        var1 <- paste0(prefix, "_", var_short)
        var2 <- paste0(prefix, "_", var_short,"_code")
        var3 <- paste0(prefix, "_", var_short,"_detail")
        
        if (code) {
                df <- ops %>% 
                        filter(str_detect(!!var, string = `OP-Code`)) %>%
                        mutate(case_id = parse_number(Fall)) %>%
                        mutate(!!var1 := 1) %>%
                        mutate(!!var3 := str_split_i(`OP-Code`, "\\.", 2)) %>% 
                        select(case_id, !!var1, !!var3) %>% 
                        unique()
        } else {
                df <- ops %>%
                        mutate(dx_txt = paste0(`LstBez.`, `LstBez.2`, " | ", `LstBez.3`)) %>%
                        filter(str_detect(!!var, string = dx_txt)) %>%
                        mutate(case_id = parse_number(Fall)) %>%
                        mutate(!!var1 := 1) %>%
                        mutate(!!var2 := `OP-Code`) %>% 
                        mutate(!!var3 := dx_txt) %>% 
                        select(case_id, !!var1, !!var2, !!var3) %>% 
                        unique()
        }
        return(df)
}


#' @title extractPathogenFromLetters - 
#' @description Retrieves information about text information in  letter about pathogens detected (e.g. Streptococcus, Staph. aureus)
#' @param var - specifies which letter information to retrieve c("aufnb","stwbr","int","stabr")
#' @param df - c(df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
#' @return A tibble with case_id and variable handedness
extractPathogenFromLetters <- function(var = c("aufnb","stwbr","int","stabr"), df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr) {
        var_name <- paste0("var_path_", var)
        if (var %in% c("aufnb","stwbr", "int", "stabr")) {
                df <-
                        eval(as.name(paste0("df_neur_", var))) %>%
                        mutate(
                                var_hand =
                                        str_extract(
                                                txt,
                                                "(?i)Acinetobacter|Streptococcus pneumoniae|Streptococcus pygens|Streptococcus agalactiae|Staphylococcus aureus|Peptostreptococcus|Neisseria meningitidis|Bacillus anthracis|Pseudomonas aeruginosa|Klebsiella pneumoniae|Moraxella catarrhalis|Klebsiella pneumoniae|E(scherichia).{2}coli|Bordetella pertussis|Burkholderia|Yersinia|Francisella|Hemophilus influenzae|Bacteroides melaninogenicus|Strepto[c|k]o|Straphylo[c|k]o|respiratory syncytial|Influenza|Parainfluenza|Adenovir|Rhinovir|Metapneumo|Bocavir|H1N1"
                                                )
                        ) %>% 
                        mutate(case_id = parse_number(as.character(fallnummer))) %>%
                        mutate(!!var_name := var_hand) %>% 
                        select(case_id, !!var_name)
                
        } else return("Error: Please supply on of the following 'stwbr','int', 'stabr'")
        return(df)
}



#' @title getPneumonia 
#' @description Get New Variable "Pneumonia during hospital stay" - from 2 sources: 1.) from File "ZIQS80_1.xls" that determines ventilation Days 2.) diagnosis codes
#' @param case_id
#' @param qs
#' @param dia
#'
#' @return A tibble
getPneumonia <- function(case_id, qs, dia, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr){
    case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
    # Pneumonie source from QS documents
    df1 <- qs %>%
        select(case_id = FALNR, contains("PNEU")) %>%
        mutate(p = coalesce(PNEUINFEKT, PNEUINFEK8, PNEUINFECB, PNEUINFEC1)) %>%
        mutate(p = recode(p, "X" = "1", "NA" = "0")) %>% 
        mutate(case_id = parse_number(case_id)) %>%
        select(case_id, p) %>% 
        unique()
    df1$p[is.na(df1$p)] <- "0"

    # Pneumonia source from Diagnosis Codes
    df2 <- extractFromDia(var = "Pneumonie", var_short = "pneumonia", prefix = "outcome", dia)
    
    
    df3 <- left_join(case_id, df1, by = "case_id")
    df4 <- left_join(case_id, df2, by = "case_id")
    
    # Extraction from diagnosis gives more false positives  despite some improvement in true positives.
    # pneu_str <- "(?<!Z\\.n\\.\\s.+)(Aspirations|Stauungs)[P|p]neumonie|(?<!Z\\.n\\.\\s.+)Lungenentzündung|(?<!Z\\.n\\.\\s+)Pneumonie"
    # df5 <- rbind(df_neur_stwbr %>% select(fallnummer, adm_diag = adm_diag_stwbr), df_neur_int %>% select(fallnummer, adm_diag = adm_diag_int), df_neur_stabr %>% select(fallnummer, adm_diag = adm_diag_stabr)) %>% 
    #     mutate(outcome_pneu = str_detect(pattern=pneu_str, adm_diag)/1) %>% 
    #         mutate(case_id = parse_number(as.character(fallnummer))) 
    #
    # #                        
    # dfx <- df %>% left_join(df5, by = "case_id")
    # 
    pathogens <- tibble(
            term = c("Pneumonie durch Escherichia coli",
                        "Pneumonie durch Staphylokokken",
                        "Pneumonie durch Haemophilus influenzae",
                        "Pneumonie durch Klebsiella pneumoniae",
                        "Pneumonie durch sonstige Streptokokken",
                        "Pneumonie durch Streptococcus pneumoniae",
                        "Pneumonie durch Pseudomonas",
                        "Pneumonie durch andere aerobe gramnegative Bakterien",
                        "Pneumonie durch Streptokokken der Gruppe B",
                        "Legionellose mit Pneumonie",
                        "Pneumonie durch Mycoplasma pneumoniae",
                        "Varizellen-Pneumonie",
                        "Pneumonie durch Herpesviren"),
            Latin = c("Escherichia coli",
                                        "Staphylococcus aureus",
                                        "Haemophilus influenzae",
                                        "Klebsiella pneumoniae",
                                        "other Streptococci",
                                        "Streptococcus pneumoniae",
                                        "Pseudomonas aeruginosa",
                                        "other Gram-negative (Serratia marcescens)",
                                        "Group B streptococci",
                                        "Legionella",
                                        "Mycoplasma pneumoniae",
                                        "Varicella",
                                        "Herpesviridae")
    )
    
    df <- left_join(df4, df3, by = "case_id") %>% 
            mutate(outcome_pneumonia = coalesce(as.character(outcome_pneumonia), p)) %>% 
            mutate(outcome_pneumonia = case_when(outcome_pneumonia==1~1, TRUE~0)) %>%
            mutate(outcome_pneumonia = as.factor(outcome_pneumonia)) %>% 
        select(-p) %>% 
        unique() %>% 
            mutate(outcome_pneu_hosp_aquired = case_when(str_detect(outcome_pneumonia_detail, "(?i)(im Krankenhaus erworben)")~1,TRUE~0)) %>%
            mutate(outcome_pneu_aspiration = case_when(str_detect(outcome_pneumonia_detail, "(?i)Nahrung oder Erbrochenes")~1,TRUE~0)) %>% 
            mutate(outcome_pneu_gram_pos = case_when(str_detect(outcome_pneumonia_detail, "(?i)Strepto|Staphyl|Entero[k|c]|Lister|(andere\\s)?grampositiv")~1,TRUE~0)) %>%
            mutate(outcome_pneu_gram_neg = case_when(str_detect(outcome_pneumonia_detail, "(?i)Pseudomonas|Klebsiella|(andere\\s)?gramnegativ")~1,TRUE~0)) %>% 
            mutate(outcome_pneu_viral = case_when(str_detect(outcome_pneumonia_detail, "(?i)viral|Haemophilus|Influenza|Varizellen|Herpes")~1,TRUE~0)) %>% 
            mutate(outcome_pneu_various = str_to_lower(str_extract(outcome_pneumonia_detail, "(?i)(Streptococcus pneumoniae|Pseudomonas|Haemophilus influenzae|Influenzaviren|Varziellen-Pneumonie|Klebsiella pneumoniae|Staphylokokken|Streptokokken|hypostatisch|Nahrung oder Erbrochenes|atypisch|bakteriell|(andere )?(gramnegativ|grampositiv))"))) %>% 
            group_by(case_id) %>% 
            mutate(outcome_pneu_hosp_aquired = max(outcome_pneu_hosp_aquired)) %>% 
            mutate(outcome_pneu_aspiration = max(outcome_pneu_aspiration)) %>% 
            mutate(outcome_pneu_gram_pos = max(outcome_pneu_gram_pos)) %>% 
            mutate(outcome_pneu_gram_neg = max(outcome_pneu_gram_neg)) %>% 
            mutate(outcome_pneu_viral = max(outcome_pneu_viral)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            select(-outcome_pneumonia_detail)
    
     
    # not used yet | Pathogens are correct but correlation to pneumonia cannot be made this way
    # df5a <- extractPathogenFromLetters("int", df_neur_aufnb = df_neur_aufnb, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr)
    # df5b <- extractPathogenFromLetters("stwbr", df_neur_aufnb = df_neur_aufnb, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr)
    # df5c <- extractPathogenFromLetters("stabr", df_neur_aufnb = df_neur_aufnb, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr)
    # dfx <- left_join(
    #             left_join(
    #                     left_join(df, df5a, by = "case_id"),
    #                     df5b,
    #                     by = "case_id"),
    #             df5c,
    #             by = "case_id")
    # not use end
    
    return(df)
}

#' @title getUrinaryTractInfection 
#' @description Get New Variable "Urinary Tract Infection" - from 1 source: 1.) diagnosis codes 2) medical records diagnosis list 3) lab value Urin status (e.g. Nitrit positive)
#' @param case_id
#' @param dia
#' @param df_neur_stwbr - discharge letter STWA
#' @param df_neur_int - discharge letter INT
#' @return A tibble
getUrinaryTractInfection <- function(case_id, dia, df_neur_stwbr, df_neur_int, df_neur_stabr, lab_ustix){
    
    # 1. dia Source diagnosis codes
    df2 <- extractFromDia(var = "Harnwegsinfekt", var_short = "uti", prefix = "outcome", dia) %>% select(-outcome_uti_code, -outcome_uti_detail) %>% 
            group_by(case_id) %>% mutate(outcome_uti = max(outcome_uti)) %>% slice(1L) %>% ungroup() %>% unique()
    
    # Source discharge letters diagnosis
    df3 <- rbind(df_neur_stwbr %>% select(fallnummer, diag = adm_diag_stwbr), df_neur_int %>% select(fallnummer,  diag = adm_diag_int), df_neur_stabr %>% select(fallnummer,  diag = adm_diag_stabr)) %>% 
            mutate(case_id = parse_number(fallnummer)) %>% 
            mutate(uti = (str_detect(diag, "(?<!Z\\.n\\.\\s.{1,20})[H|h]arnweg|(?<!Z\\.n\\.\\s.{1,20})HWI"))/1) %>% 
            mutate(uti = case_when(uti==1~1,TRUE~0)) %>% 
            group_by(case_id) %>% mutate(outcome_uti = max(uti)) %>%
            ungroup() %>%   select(case_id, outcome_uti) 
                    
            
    # Source UStix from lab values 
    df4 <- lab_ustix %>% 
            mutate(names = paste0("lab_urin_",str_trim(str_extract(pattern = ".+(?=\\/)", Text)),"_max")) %>% 
            mutate(case_id = Fall) %>% 
            group_by(case_id, names) %>%
            mutate(Wert=max(Wert)) %>% 
            slice(1L) %>% 
            pivot_wider(names_from=names, values_from = Wert) %>% 
            fill(contains("lab_urin_") , .direction = c("updown")) %>% 
            slice(1L) %>% 
            ungroup() %>% 
            select(case_id, contains("lab_urin_"))
    
    case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% drop_na(case_id)
    # all cases have digital diagnosis so information is available in all cases 
    # after joining with case_id all other cases are given zero
    dfx <- case_id %>% 
            left_join(df2, by = "case_id") %>% mutate(outcome_uti = case_when(outcome_uti==1~1,TRUE~0)) %>% unique() %>% 
            left_join(df3, by = "case_id") %>% mutate(outcome_uti = coalesce(outcome_uti.x, outcome_uti.y)) %>% unique() %>% 
            left_join(df4, by = "case_id") %>% select(case_id, outcome_uti, contains("urin_"))
            
    # return Dataframe with urinary tract infections
    return(dfx)
        
}


getPrevThyr <- function(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr, lab_thyr, labs, kenn) {
        
        prev_thy_str <- "(\\b\\w+\\b.{1,3})?(([H|h]yp..?)?thyreose|[S|s]childd?rüsen?|[S|s]childdrüsenunterfunktion|[S|s]truma|[T|t]hyroidektomie|Autoimmunthyreoiditis|Hashimoto|Thyreoiditis)(.{1,3}\\b\\w+\\b)?"
        prev_thy_hypo_str <- "([H|h]ypothyreose|[S|s]childd?rüsenunterfunktion|[S|s]truma|[T|t]hyroidektomie)"
        prev_thy_hyper_str <- "([H|h]yperthyreose|[S|s]childd?rüsenüberfunktion)"
        prev_thy_hash_str <- "Autoimmunthyreoiditis|Hashimoto|Thyreoiditis"
        
        # 2. admission medical records  
        df2 <- rbind(
                df_neur_aufnb %>% select(fallnummer, adm_diag = adm_diag_aufnb),
                df_neur_stwbr %>% select(fallnummer, adm_diag = adm_diag_stwbr),
                df_neur_stabr %>% select(fallnummer, adm_diag = adm_diag_stabr),
                df_neur_int   %>% select(fallnummer, adm_diag = adm_diag_int)
        ) %>% 
                mutate(dx_prev_thy = (str_extract(pattern = prev_thy_str, adm_diag))) %>% 
                mutate(dx_prev_thy_hypo = (str_detect(pattern = prev_thy_hypo_str, dx_prev_thy))/1) %>% 
                mutate(dx_prev_thy_hyper = (str_detect(pattern = prev_thy_hyper_str, dx_prev_thy))/1) %>% 
                mutate(dx_prev_thy_hash = (str_detect(pattern = prev_thy_hash_str, dx_prev_thy))/1) %>% 
                distinct(fallnummer, dx_prev_thy, .keep_all = TRUE) %>% 
                mutate(case_id = parse_number(as.character(fallnummer)))
        
        
        
        # 6. source - admission medication
        # Schilddrüsenhormonersatztherapie: Levothyroxin (L-Thyroxin), Euthyrox, L-Thyroxin, Eltroxin, Levothroid, Levoxyl, Synthroid, Tirosint, Liothyronin, L-Triiodthyronin, Cytomel, Liothyronin, Thyroxin-Liotyr, 
        # Thyreostatika (Schilddrüsenfunktionshemmer): Thiamazol, Methimazol, Thiamazol, Carbimazol, Carbimazol, Neo-Mercazole, Propylthiouracil, Propycil, Propylthiouracil
        
        prev_thyr_subst_str <- "[L|l]evothyroxin|L?-?[T|t]hyroxin|[E|e]uthyrox|[E|e]ltroxin|[L|l]evothroid|[L|l]evoxyl|[S|s]ynthroid|[T|t]irosint|[L|l]iothyronin|L?-?[T|t]riiodthyronin|[C|c]ytomel|[L|l]iothyronin|Thyroxin-Liotyr"
        prev_thyr_static_str <- "[T|t]hiamazol|[M|m]ethimazol|[T|t]hiamazol|[C|c]arbimazol|[C|c]arbimazol|[M|m]ercazole|[P|p]ropylthiouracil|[P|p]ropycil|[P|p]ropylthiouracil"
        
        df6 <-
                rbind(
                        df_neur_aufnb %>% select(fallnummer, adm_meds = adm_meds_aufnb),
                        df_neur_int %>% select(fallnummer, adm_meds = adm_meds_int)
                ) %>%
                mutate(prev_thyr_subst = str_detect(pattern = prev_thyr_subst_str, adm_meds) /
                               1) %>%
                mutate(prev_thyr_subst_detail = str_extract(pattern = prev_thyr_subst_str, adm_meds)) %>%
                mutate(prev_thyr_static = str_detect(pattern = prev_thyr_static_str, adm_meds) /
                               1) %>%
                mutate(prev_thyr_static_detail = str_extract(pattern = prev_thyr_static_str, adm_meds)) %>%
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, contains("prev_thyr"))
        

        
        lab_thyr_str <- c("TSH", "freies Trijodthyronin (fT3)", "freies T3", "FT3", "reies Thyroxin (fT4)", "freies T4", "FT4")
        # alc_lab <- rbind(lab_liver, lab_haem) %>% filter(Text %in% string_sel)
        # Lab selection from d0 and d1 as admission values
        df7 <-
                getLabs(
                        labs = labs,
                        case_id = case_id,
                        lab_vars_names = lab_thyr_str,
                        kenn = kenn
                ) %>%
                reduce(left_join, by = "Fall") %>%
                select(c(Fall, contains(c("d0","d1")))) %>%
                mutate(case_id = Fall) %>% select(-Fall) %>% 
                mutate(lab_ft4 = coalesce(lab_FT4_d0, lab_fre_d0, lab_FT4_d1, lab_fre_d1)) %>% 
                mutate(lab_ft3 = coalesce(lab_FT3_d0, lab_fre_d0.x, lab_fre_d0.y, lab_FT3_d1, lab_fre_d1.x, lab_fre_d1.y)) %>% 
                mutate(lab_tsh = lab_TSH_d0, lab_TSH_d1) %>% 
                select(case_id, lab_tsh, lab_ft3, lab_ft4) %>% 
                unique()
        # lab selection from all available values 
        l <-
                lab_thyr %>% pivot_wider(names_from = "Text", values_from = "Wert") %>%
                mutate(
                        lab_ft3 = coalesce(FT3, `freies T3`, `freies Trijodthyronin (fT3)`),
                        .keep = "unused"
                ) %>%
                mutate(lab_ft4 = coalesce(FT4, `freies T4`), .keep = "unused") %>%
                mutate(lab_tsh = TSH, .keep = "unused") %>%
                mutate(case_id = Fall, .keep = "unused") %>%
                select(case_id, lab_tsh, lab_ft3, lab_ft4, timeDiff) %>%
                group_by(case_id) %>%
                fill(lab_ft3, lab_ft4, , lab_tsh, .direction = "down") %>%
                fill(lab_ft3, lab_ft4, , lab_tsh, .direction = "up") %>%
                slice(1L) %>% 
                ungroup() %>%
                unique()
        
        
        
        
        # JOIN
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        df <- case_id %>% 
                # left_join(df1, by = "case_id") %>% unique() %>% 
                left_join(df2, by = "case_id") %>% unique() %>% 
                # left_join(df3, by = "case_id") %>% unique() %>% 
                # left_join(df4, by = "case_id") %>% unique() %>% 
                # left_join(df5, by = "case_id") %>% unique() %>%
                left_join(df6, by = "case_id") %>% unique() %>%
                left_join(df7, by = "case_id") %>% unique() %>% 
                group_by(case_id) %>% 
                arrange(-desc(dx_prev_thy)) %>% 
                slice(1L) %>% 
                ungroup() %>% select(-fallnummer, -Fall)

        
        df <- df  %>% mutate(var_thyr = case_when(
                # (dx_prev_thy_hypo==1 | dx_prev_thy_hash==1) & prev_thyr_subst==1    ~ "hypo (+Tx)",
                (dx_prev_thy_hypo==1 | dx_prev_thy_hash==1) & prev_thyr_subst==1    ~ "hypo",
                # prev_thyr_subst==1 ~ "hypo (+Tx)",
                prev_thyr_subst==1 ~ "hypo",
                (dx_prev_thy_hypo==1 | dx_prev_thy_hash==1) & prev_thyr_subst==0    ~ "hypo",
                # prev_thyr_static==1 ~ "hyper (+Tx)",
                prev_thyr_static==1 ~ "hyper",
                # (dx_prev_thy_hyper==1                        & prev_thyr_static==1) ~ "hyper (+Tx)",
                (dx_prev_thy_hyper==1                        & prev_thyr_static==1) ~ "hyper",
                (dx_prev_thy_hyper==1                        & prev_thyr_static==0) ~ "hyper",
                is.na(adm_diag) ~ NA_character_,
                TRUE~'euthyroidism')
        ) %>% select(case_id, var_thyr) %>% 
                mutate(var_prev_thyr = factor(var_thyr))
        return(df)
}

concatenate_row <- function(df) {
        # Apply the concatenation function to each row
        concatenated_strings <- apply(df, 1, function(row) {
                paste(names(df), row, sep = ": ", collapse = ", ")
        })
        
        # Return the concatenated strings
        return(concatenated_strings)
}

# classifyThyroidWithGPT <- function(df){
#         library(tidyverse)
#         prompt1 <- "# CONTEXT # 
# Classification of thyroid disease in german medical documents
# 
# # OBJECTIVE #
# classify given text, preclassified strucutes and lab values. Classify every case to either 1) hypothyroidism 2) euthyroidism 3) hyperthyroidism 4) NA. please be precise.
# 
# # TONE #
# scientific/neutral
# 
# # RESPONSE #
# only respond with the facts in the following format
# 
# #FORMAT#
# FINAL RESPONSE should stick to following FORMAT and just give back the classfication: 
# e.g. 'hypothyroidism' or 'euthyroidism' or 'hyperthyroidism.'
# 
# If 'adm_diag' is NA then respond with 'NA'
# If there are no hints for hypothyroidism or hyperthyroidism then respond with 'euthyroidism'
# Recheck if prev_thyr_subst_details - e.g. 'L-Thyroxin' and lab values (if present) is in line with this decision 
# 
# #TEXT#
# "
#         df$new <- concatenate_row(df %>% select(-case_id))
#         
#         # s <- Sys.time()
#         df$var_thy <-
#                 sapply(
#                         df$new,
#                         simplify = TRUE,
#                         USE.NAMES = FALSE,
#                         FUN =  function(x) {
#                                 ask_chatgpt(
#                                         paste(
#                                                 prompt1,
#                                                 x
#                                         ), temp = 0.1
#                                 )
#                         }
#                 )
#         # Sys.time()-s
#         return(df)
# }


#' @title getMi 
#' @description Get New Variable "Myokardial Infarction" - from sources: 1.) diagnosis codes
#' @param case_id
#' @param dia
#' @param df_neur_stwbr - discharge letter STWA
#' @param df_neur_int - discharge letter INT
#' @return A tibble
getMi <- function(case_id, dia) {
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% 
                drop_na(case_id)
        df1 <- extractFromDia(var = "I21", var_short = "mi", prefix = "outcome", dia, code=TRUE)
        
        #JOIN
        dfx <- case_id %>% left_join(df1, by = c("case_id" = "case_id")) %>% unique() %>% 
                mutate(outcome_mi = as.factor(ifelse(is.na(outcome_mi), 0, outcome_mi)))
}

#' @title getPrevMi
#' @description Get New Variable "Previous Myokardial infarction" - from sources: 1.) dia 2.) docs 
#' @param case_id
#' @param dia
#' @param df_neur_aufnb
#' @param df_neur_stwbr
#' @param df_neur_int
#' @param df_neur_stabr
#' @return A tibble
getPrevMi <- function(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr) {
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        # 1.) Dia
        df1 <- extractFromDia(var = "I25.2", var_short = "prev_mi", prefix = "rf", dia, code=TRUE)
        
        #2.) Str/extr from documents
        
        
        # prev_mi_str <- "(?:([Z|z]\\.?\\s?n\\.?\\s?)|[A|a]lter?\\s?|(Vorerk|VORERK|Vordiagn|VORDIAGN).{1,350})(?:(?!Hirn|Milz|Leber)(Herz|Myokard|Vorderwand|Hinterwand|RIVA)(infarkt|ischämie|.{1,5}STEMI))(?!\\: 1 nein)"
        prev_mi_str <- "(?:([Z|z]\\.?\\s?n\\.?\\s?)|[A|a]lter?\\s?|(Vorerk|VORERK|Vordiagn|VORDIAGN).{1,350})(?:(?!Hirn|Milz|Leber)(Herz(infarkt|ischämie)|Myokard(infarkt|ischämie)|Vorderwand(infarkt|ischämie)|Hinterwand(infarkt|ischämie)|RIVA|NSTEMI))(?!\\: 1 nein)"
        
        # 2. source - df_neur_aufnb
        df2 <- df_neur_aufnb %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                mutate(dx_prev_mi_2 = (str_detect(pattern = prev_mi_str, txt))/1) %>% 
                select(case_id, dx_prev_mi_2)
        
        # 3. source - df_neur_stwbr
        df3 <- df_neur_stwbr %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                mutate(dx_prev_mi_3 = (str_detect(pattern = prev_mi_str, adm_diag_stwbr))/1) %>% 
                select(case_id, dx_prev_mi_3)
        
        # 4. source - df_neur_int
        df4 <- df_neur_int %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                mutate(dx_prev_mi_4 = (str_detect(pattern = prev_mi_str, adm_diag_int))/1) %>% 
                select(case_id, dx_prev_mi_4)
        
        # 5. source - df_neur_stabr
        df5 <- df_neur_stabr %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                mutate(dx_prev_mi_5 = (str_detect(pattern = prev_mi_str, adm_diag_stabr))/1) %>% 
                select(case_id, dx_prev_mi_5)
        
        df <- case_id %>% 
                left_join(df1, by = "case_id") %>% unique() %>% 
                left_join(df2, by = "case_id") %>% unique() %>% 
                left_join(df3, by = "case_id") %>% unique() %>% 
                left_join(df4, by = "case_id") %>% unique() %>% 
                left_join(df5, by = "case_id") %>% unique() %>%
                mutate(rf_prev_mi = coalesce(pmax(rf_prev_mi, dx_prev_mi_2, dx_prev_mi_3, dx_prev_mi_4, dx_prev_mi_5, na.rm = TRUE)), .keep = "unused") %>% 
                drop_na(case_id)
        
        
}


extractDelirWithGPT <- function(df){
        
        prompt1 <- "# CONTEXT # 
Classification of delirium in german medical text

# OBJECTIVE #
classify given text whether there is evidence of delirium or not. If there is delirium, please classify the subtype and identify possible medications. please be precise and consider temporal aspects of information.

# TONE #
scientific/neutral

# RESPONSE #
only respond with the facts in the following format

#FORMAT#
FINAL RESPONSE should stick to following FORMAT : 
'0; NA; NA' or 
'1; postoperative; risperdal' or 
'1; dementia; haldol'.

The first item should relate to delirum: '0' or '1'. Please be precise - only if 'Delir' or 'Durchgangssyndrom' or similar is mentioned explicitely, delirium may be classified as '1'. 

The second item (after ';' ) should relate to the subgroup:
In case of Delirium = '0' respond with 'NA'.
In case of Delirium = '1' classify according to the following subgroups: 'dementia', 'withdrawl (alcohol)', 'withdrawl (opioids)', 'seizure-related', 'postoperative', 'infection', 'multifactorial', 'not classified'.
Only use these classification terms if indicated.

The third item (after the second ';') should relate to medication:
In case of Delirium = '0' respond with 'NA'.
In case of delirium = '1' extract delirium related medications. If more than one medication is identified, separate the medications with ','.


#TEXT#
"
        # s <- Sys.time()
        df$new <-
                sapply(
                        df$txt,
                        simplify = TRUE,
                        USE.NAMES = FALSE,
                        FUN =  function(x) {
                                ask_chatgpt(
                                        paste(
                                                prompt1,
                                                x
                                        ), temp = 0.1
                                )
                        }
                )
        # Sys.time()-s
        return(df)
}

#' @title getDelir
#' @description Get New Variable "Delir" - from various sources: 1.) dia 2.) docs 3.) labs (sodium) 4.) diverse risk factors such as count of operations or length of stay
#' @param case_id
#' @param dia
#' @param df_neur_stwbr
#' @param df_neur_int
#' @param df_neur_stabr
#' @return A tibble
getDelir <- function(case_id, dia, df_neur_stwbr, df_neur_int, df_neur_stabr, labs, kenn) {

        #1 Dia
        df1 <- extractFromDia(var = "Delir", var_short = "delir", prefix = "outcome", dia) %>% 
                mutate(outcome_delir_alc = case_when(str_detect(pattern="Alkohol", outcome_delir_detail)~1,TRUE~0)) %>% 
                mutate(outcome_delir_dem = case_when(str_detect(pattern="(?<!ohne\\s)Demenz", outcome_delir_detail)~1,TRUE~0)) %>% 
                mutate(outcome_delir_postop = case_when(str_detect(pattern="[P|p]ostop", outcome_delir_detail)~1,TRUE~0)) %>% 
                mutate(outcome_delir_seiz = case_when(str_detect(pattern="Dämmer|epilept", outcome_delir_detail)~1,TRUE~0)) %>% 
                mutate(m = outcome_delir_alc+outcome_delir_dem+outcome_delir_postop+outcome_delir_seiz) %>% 
                mutate(outcome_delir_multi = case_when( (str_detect(pattern="gemischt", outcome_delir_detail) | m>1)~1,TRUE~0)) %>% 
                group_by(case_id) %>%
                mutate(outcome_delir = pmax(outcome_delir)) %>% 
                mutate(outcome_delir_alc = pmax(outcome_delir_alc)) %>% 
                mutate(outcome_delir_dem = pmax(outcome_delir_dem)) %>% 
                mutate(outcome_delir_postop = pmax(outcome_delir_postop)) %>% 
                mutate(outcome_delir_seiz = pmax(outcome_delir_seiz)) %>% 
                mutate(outcome_delir_multi = pmax(outcome_delir_multi)) %>% 
                slice(1L) %>% select(case_id, contains("outcome_delir"), -outcome_delir_code, -outcome_delir_detail)
                
                
        #2 medical notes
        delir_str <- "(?<!Z\\.n\\.\\s.{1,20})[D|d]elir"
        delir_alc_str <- "(?<!Z\\.n\\.\\s.{0,20})([A|a]lko?ho?l|C2.?)([E|e]ntzun?g)?s?.?[D|d]elir|(?<!Z\\.n\\.\\s.{0,20})[E|e]ntzugs(delir|syndrom)|(?<!Z\\.n\\.\\s.{0,20})[D|d]elir.{1,15}(Alkohol|C2)"
        delir_dem_str <- "[D|d]elir.{0,30}[D|d]emen[z|t]|[D|d]emen[t|z].{1,50}Delir"
        delir_inf_str <- "[D|d]elir.{0,30}([I|i]nfekt|[F|f]ieber|[P|p]neumonie|[E|e]ntzündung)|([I|i]nfekt|[F|f]ieber|[P|p]neumonie|[E|e]ntzündung).{1,50}Delir"
        df2 <- rbind(df_neur_stwbr %>% select(fallnummer, diag = adm_diag_stwbr), df_neur_int %>% select(fallnummer,  diag = adm_diag_int), df_neur_stabr %>% select(fallnummer,  diag = adm_diag_stabr)) %>% 
                mutate(outcome_delir = str_detect(pattern = delir_str, diag)/1) %>% 
                mutate(outcome_delir_alc = str_detect(pattern = delir_alc_str, diag)/1) %>% 
                mutate(outcome_delir_dem = str_detect(pattern = delir_dem_str, diag)/1) %>% 
                mutate(outcome_delir_inf = str_detect(pattern = delir_inf_str, diag)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                group_by(case_id) %>%
                mutate(outcome_delir = pmax(outcome_delir)) %>% 
                mutate(outcome_delir_alc = pmax(outcome_delir_alc)) %>% 
                mutate(outcome_delir_dem = pmax(outcome_delir_dem)) %>% 
                mutate(outcome_delir_inf = pmax(outcome_delir_inf)) %>% 
                slice(1L) %>% select(case_id, contains("outcome_delir"))
                
        # - Elektrolytstörung
        na1 <- getLabs(labs, case_id, lab_vars_names = "Natrium", kenn) 
        na2 <- getLabs(labs, case_id, lab_vars_names = "Natrium(ISE)", kenn) 
        df3 <- rbind(na1[[1]], na2[[1]]) %>% 
                group_by(Fall) %>% arrange(-desc(Text)) %>% slice(1L) %>% 
                select(case_id = Fall, lab_Nat_min)
                
        # - Operation
        df4 <- kenn %>% select(Fall, Anz.ND, `OP-Beweg.`, Prozedur., Verweild., Beatmung) %>%
                mutate(case_id = parse_number(as.character(Fall))) %>% 
                mutate(outcome_delir_diag_count = parse_number(as.character(Anz.ND))) %>%
                mutate(outcome_delir_op_count = parse_number(as.character(`OP-Beweg.`))) %>%
                mutate(outcome_delir_proc_count = parse_number(as.character(Prozedur.))) %>%
                mutate(outcome_delir_lohs_days = parse_number(as.character(Verweild.))) %>%
                mutate(outcome_delir_vent_days = parse_number(as.character(Beatmung))) %>% 
                select(case_id, contains("outcome_delir")) %>% 
                group_by(case_id) %>% arrange(desc(outcome_delir_lohs_days)) %>% slice(1L)
        
        # - Abteilungswechsel
        # ggf. 
        # NBEW_NEUR
        # LFDNR für Anzahl der Bewegungen
        # 
        # BEKAT RL oder PRI / PRIS
        
        # Join 
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% drop_na(case_id)
        dfx <- case_id %>% left_join(df1, by = "case_id") %>% 
                left_join(df2, by = "case_id") %>% 
                left_join(df3, by = "case_id") %>% 
                left_join(df4, by = "case_id") %>%
                mutate(outcome_delir.z = 0) %>%
                mutate(outcome_delir_alc.z = 0) %>%
                mutate(outcome_delir_dem.z = 0) %>%
                mutate(outcome_delir_postop.z = 0) %>%
                mutate(outcome_delir_seiz.z = 0) %>%
                mutate(outcome_delir_inf.z = 0) %>%
                mutate(outcome_delir_multi.z = 0) %>% 
                
                mutate(outcome_delir = coalesce(outcome_delir.x, outcome_delir.y, outcome_delir.z), .keep = "unused") %>% 
                mutate(outcome_delir_alc = coalesce(outcome_delir_alc.x, outcome_delir_alc.y, outcome_delir_alc.z), .keep = "unused") %>% 
                mutate(outcome_delir_dem = coalesce(outcome_delir_dem.x, outcome_delir_dem.y, outcome_delir_dem.z), .keep = "unused") %>% 
                mutate(outcome_delir_postop = coalesce(outcome_delir_postop, outcome_delir_postop.z), .keep = "unused") %>% 
                mutate(outcome_delir_seiz = coalesce(outcome_delir_seiz, outcome_delir_seiz.z), .keep = "unused") %>% 
                mutate(outcome_delir_inf = coalesce(outcome_delir_inf, outcome_delir_inf.z), .keep = "unused") %>% 
                mutate(outcome_delir_multi = coalesce(outcome_delir_multi, outcome_delir_multi.z), .keep = "unused") %>% 
                mutate(outcome_delir_cat = case_when(outcome_delir_multi==1~"multifactorial",
                                                     outcome_delir_alc==1~"alcohol-related",
                                                     outcome_delir_dem==1~"dementia-related",
                                                     outcome_delir_postop==1~"postoperative",
                                                     outcome_delir_seiz==1~"seizure-related",
                                                     outcome_delir_inf==1~"infection-related",
                                                     outcome_delir==1~"unspecified",
                                                     TRUE~"no delirium")) %>% 
                mutate(outcome_delir_cat <- factor(outcome_delir_cat, levels = outcome_delir_cat %>% unique()))
                
                
        
        # Additionally consider results from ChatGPT4 and medication 
        #
        # case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id))) %>% drop_na()
        # 
        # df_stwbr <- df_neur_stwbr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt_stwbr = txt)
        # df_stabr <- df_neur_stabr %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt_stabr = txt)
        # df_int <- df_neur_int %>% mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, txt_int = txt)
        # 
        # d <- case_id %>% 
        #         left_join(df_stwbr, by = c("case_id" = "case_id")) %>% 
        #         left_join(df_stabr, by = c("case_id" = "case_id")) %>% 
        #         left_join(df_int, by = c("case_id" = "case_id")) %>% 
        #         mutate(txt = coalesce(txt_stwbr, txt_stabr, txt_int)) %>% 
        #         select(case_id, txt) %>% 
        #         drop_na(txt) %>% 
        #         mutate(txt = removePossIdentifier(txt))
        # 
        # 
        # file_path <- "output/delir.rds"
        # 
        # if (!file.exists(file_path)) {
        #         # (d)elir
        #         d_list <- list()
        #         d_chunks <- split(d, 1:nrow(d) %% 100)
        #         # c <- 1
        #         for (c in seq(1, 100)) {
        #                 b <- d_chunks[[c]]
        #                 d_list[[c]] <- extractDelirWithGPT(b)
        #                 saveRDS(d_list, 'output/delir.rds', compress = 'xz')
        #                 print(paste0(c, "..done"))
        #         }
        # } else {
        #         d_list <- readRDS(file_path)
        # }
        # 
        # df5 <- do.call(rbind, d_list) %>% 
        #         as_tibble() 
        #         
        
        
        
        
        
        
        
        
        return(dfx)
}

#' @title getStrokeCare
#' @description Get New Variables "Stroke Care" crediting I) psycho-social interventions done with the patient and hours invested. These intervention can be 1) Nachsorgeorganisation 2) Sozialrechtliche Beratung 3)SupportiveTherapie or 4) Familien-,Paar- und Erziehungsberatung. II) variable that deal with discharge to home or to rehabilitation and if rehabilitation was recommended
#' @param case_id
#' @param ops 
#' @param fakar
#' @return A tibble
getStrokeCare <- function(case_id = case_id, ops = ops, fakar = fakar, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        df1 <-
                extractFromOps(var = "Psychosoziale Interventionen",
                               var_short = "stroke_care",
                               prefix = "var",
                               ops) %>%
                mutate(
                        var_stroke_care_hours = case_when(
                                var_stroke_care_detail == "Psychosoziale Interventionen: Nachsorgeorganisation: Mehr als 2 Stunden bis 4 St | unden" ~ 2,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Nachsorgeorganisation: Mindestens 50 Minuten bis 2 | Stunden" ~ 1,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Sozialrechtliche Beratung: Mindestens 50 Minuten b | is 2 Stunden" ~ 1,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Sozialrechtliche Beratung: Mehr als 2 Stunden bis | 4 Stunden" ~ 2,
                                var_stroke_care_detail == "Psychosoziale Interventionen: SupportiveTherapie: Mindestens 50 Minuten bis 2 S | tunden"  ~ 1,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Nachsorgeorganisation: Mehr als 4 Stunden bis 6 St | unden" ~ 3,
                                var_stroke_care_detail == "Psychosoziale Interventionen: SupportiveTherapie: Mehr als 2 Stunden bis 4 Stun | den" ~ 2,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Nachsorgeorganisation: Mehr als 6 Stunden | NA" ~ 4,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Familien-,Paar- und Erziehungsberatung: Mehr als | 2 Stunden bis 4 Stunden" ~ 2,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Familien-,Paar- und Erziehungsberatung: Mehr als | 4 Stunden" ~ 3,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Familien-,Paar- und Erziehungsberatung: Mindesten | s 50 Minuten bis 2 Stunden" ~ 1,
                                var_stroke_care_detail == "Psychosoziale Interventionen: SupportiveTherapie: Mehr als 4 Stunden | NA" ~ 3,
                                var_stroke_care_detail == "Psychosoziale Interventionen: Sozialrechtliche Beratung: Mehr als 4 Stunden | NA" ~ 3,
                                TRUE~0
                        )
                ) %>% group_by(case_id) %>% mutate(var_stroke_care_h = sum(var_stroke_care_hours)) %>% slice(1L) %>% select(case_id, var_stroke_care, var_stroke_care_h)
        
        # Fakar discharge to rehabilitation
        df2 <- fakar %>% 
                mutate(case_id = parse_number(as.character(Fall))) %>% 
                group_by(case_id) %>% drop_na(BewegArt) %>% 
                select(case_id, BewArt, BewegArt, Name...16, Geschl) %>% 
                mutate(outcome_discharge_home = case_when(BewegArt == 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(outcome_discharge_tohosp = case_when(BewegArt == 2 ~ 1, TRUE ~ 0)) %>% 
                mutate(outcome_discharge_rehab = case_when(BewegArt == 8 ~ 1, TRUE ~ 0)) %>% 
                mutate(outcome_discharge_rehab2 = case_when(BewegArt == 8 | BewegArt == 2 ~ 1, TRUE ~ 0)) %>% 
                mutate(outcome_discharge_loc = case_when(BewegArt == 1 | BewegArt == 4 ~ "home", 
                                                         BewegArt == 3~"death in hospital",
                                                         TRUE~Name...16)) %>% 
                mutate(outcome_discharge_loc = fct_lump_n(as.factor(outcome_discharge_loc), n = 10, other_level = "Other")) %>%
                mutate(outcome_discharge_loc = fct_infreq(outcome_discharge_loc)) %>% 
                group_by(case_id) %>% slice(1L)
                
        # Discharge recommendation for rehabilitation
        df3 <- rbind(df_neur_stwbr %>% select(fallnummer, epi = adm_epi_stwbr), df_neur_int %>% select(fallnummer,  epi = adm_epi_int), df_neur_stabr %>% select(fallnummer,  epi = adm_epi_stabr)) %>%
                mutate(var_rehab_recomm = str_detect(pattern = "[R|r]ehabilitation", epi)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% select(case_id, var_rehab_recomm) %>% unique() %>% 
                group_by(case_id) %>% arrange(desc(var_rehab_recomm)) %>% slice(1L) %>% drop_na(var_rehab_recomm)
        
        # cross section of 1) discharge home + 2) recommendation for rehab
        dfx <- case_id %>% drop_na(case_id) %>% left_join(df3, by = "case_id") %>% unique() %>% 
                left_join(df2, by = "case_id") %>% 
                mutate(outcome_discharge_home_rehab = case_when(
                        outcome_discharge_home==1 & var_rehab_recomm==1 ~ 1,
                        TRUE ~ 0
                )) %>% 
                left_join(df1, by = "case_id") %>% 
                mutate(var_stroke_care_h = ifelse(is.na(var_stroke_care_h),0,var_stroke_care_h)) %>% 
                mutate(var_stroke_care = ifelse(is.na(var_stroke_care),0,var_stroke_care)) %>% 
                select(-"Name...16", -BewArt, -BewegArt, -Geschl)
        
        return(dfx)
}



generateRegexString <- function(data, column, lowercase = TRUE) {
        column_values <- data %>% pull({{column}})
        unique_values <- unique(column_values)
        
        regex_string <- paste(unique_values, collapse = "|")
        if (lowercase) {
                regex_string <- paste0("(?i)(", regex_string, ")")
        } else {
                regex_string <- paste0("(", regex_string, ")")
        }
        return(regex_string)
}


getSepsis <- function(case_id = case_id, dia = dia, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr){
        require(tidyverse)
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        # get Sepsis vom official diagnosis list
        df1 <- extractFromDia(var = "[S|s]ep[s|t]is",
                               var_short = "sepsis",
                               prefix = "outcome",
                               dia)
        
        # build pathogen list and latin nomenclature
        pathogens <- tibble(
                Term = c("Staphylokokken", "Staphylococcus aureus", "Aspergillus", "E. coli", "Pilzsepsis", "Meningokokken", "Streptokokken", "Streptokokken Gruppe D", "Escherichia coli [E. coli]", "Pseudomonas", "Streptococcus pneumoniae", "Candida", "Listerien", "Meningokokken", "Salmonellen", "Neugeborenensepsis", "Propionibakt.", "Streptokokken Gruppe B", "Enterokokken", "Enterobakter", "Haemophilus influenzae", "Sepsis tuberculosa acutissima (Landouzy)", "Streptokokken", "C.glabrata", "gramnegativ", "Corynebakterien", "koagulasenegative Staphylokokken", "Streptokokken, Gruppe D, und Enterokokken", "Pneumokokken", "Candida", "Koagulase-negative Staphylokokken", "Anaerobier", "Klebsiella", "Enterobacter", "Proteus", "MSSA", "MRSA"),
                Latin = c("Staphylococcus spp.", "Staphylococcus aureus", "Aspergillus spp.", "Escherichia coli", "Fungal sepsis", "Neisseria meningitidis", "Streptococcus spp.", "Streptococcus bovis", "Escherichia coli", "Pseudomonas spp.", "Streptococcus pneumoniae", "Candida spp.", "Listeria spp.", "Neisseria meningitidis", "Salmonella spp.", "Neonatal sepsis", "Propionibacterium spp.", "Streptococcus agalactiae", "Enterococcus spp.", "Enterobacter spp.", "Haemophilus influenzae", "Mycobacterium tuberculosis", "Streptococcus spp.", "Candida glabrata", "Gram-negative", "Corynebacterium spp.", "Coagulase-negative Staphylococci", "Streptococcus bovis and Enterococcus spp.", "Streptococcus pneumoniae", "Candida spp.", "Coagulase-negative Staphylococci", "Anaerobes", "Klebsiella spp.", "Enterobacter spp.", "Proteus spp.", "Methicillin-Sensitive Staphylococcus aureus", "Methicillin-Resistant Staphylococcus aureus")
        )
        
        #build regex strings
        pathogen_str <- generateRegexString(pathogens, Term)
        pathogen_latin_str <- generateRegexString(pathogens, Latin)
        
        # join
        dfx <- case_id %>% drop_na(case_id) %>% left_join(df1, by = "case_id") %>% 
                mutate(outcome_sepsis_pathogen = str_extract(pattern = pathogen_str, outcome_sepsis_detail)) %>%
                left_join(pathogens, by = c("outcome_sepsis_pathogen" = "Term")) %>% 
                mutate(outcome_sepsis_path = case_when(
                str_detect(pattern = pathogen_str, outcome_sepsis_detail) ~ Latin,
                outcome_sepsis==1 ~ "unknown pathogen",
                TRUE ~ NA)) %>% 
                mutate(outcome_sepsis = case_when(outcome_sepsis==1~1,TRUE~0)) %>% 
                mutate(outcome_sepsis = as.factor(outcome_sepsis)) %>% 
                select(case_id, outcome_sepsis, outcome_sepsis_path)
}

getTvt <- function(case_id = case_id, dia = dia){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        df1 <- extractFromDia(var = "I80", var_short ="tvt", prefix = "outcome", dia, code=TRUE)
        dfx <- case_id %>% drop_na(case_id) %>% left_join(df1, by = "case_id") %>% 
                mutate(outcome_tvt = case_when(outcome_tvt==1~1,TRUE~0)) %>% group_by(case_id) %>% arrange(-desc(outcome_tvt_detail)) %>% slice(1L)
        dfx <- dfx %>% mutate(outcome_tvt = as.factor(outcome_tvt))
}
getLae <- function(case_id = case_id, dia = dia){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        df1 <- extractFromDia(var = "I26", var_short ="lae", prefix = "outcome", dia, code=TRUE)
        dfx <- case_id %>% drop_na(case_id) %>% left_join(df1, by = "case_id") %>% 
                mutate(outcome_lae = case_when(outcome_lae==1~1,TRUE~0)) %>% group_by(case_id) %>% arrange(-desc(outcome_lae_detail)) %>% slice(1L)
        dfx <- dfx %>% mutate(outcome_lae = as.factor(outcome_lae))
}

getIhd <- function(case_id, pat_neur, demog, df_hei){
        df1 <- pat_neur %>% 
                drop_na(TODKZ) %>% 
                select(case_id = FALNR, GSCHL, outcome_ihd = TODKZ, outcome_ihd_date = TODDT, outcome_ihd_time = TODZT) %>% 
                mutate(ihd_date_time = dmy_hms(paste(outcome_ihd_date, outcome_ihd_time))) %>% 
                mutate(case_id = parse_number(as.character(case_id))) %>% 
                mutate(outcome_ihd = 1) %>% 
                select(-outcome_ihd_date, -outcome_ihd_time)
                
        
# 1.         # check with df_hei
# 2.         # get kenn
        
        df3 <- demog %>% 
                mutate(hosp_adm_datetime = ymd_hms(paste(Aufn.Datum, Aufn.Zeit))) %>% 
                mutate(hosp_dis_datetime = ymd_hms(paste(Entl.Datum, Entl.Zeit))) %>% 
                mutate(case_id = parse_number(as.character(Fall))) %>% 
                select(case_id, hosp_adm_datetime, hosp_dis_datetime) %>% unique()
        
        df4 <- df_hei %>% 
                select(case_id, sex, outcome_ihd, outcome_mrsd90, age, nihss_d0) %>% 
                mutate(case_id = parse_number(as.character(case_id))) %>% 
                mutate(outcome_ihd = parse_number(as.character(outcome_ihd)))
        
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        dfx <- case_id %>% drop_na() %>% 
                left_join(df1, by = "case_id") %>% 
                mutate(outcome_ihd = ifelse(outcome_ihd==1, 1, 0)) %>% 
                left_join(df3, by = "case_id") %>% unique() %>% 
                left_join(df4, by = "case_id") %>% unique() %>% 
                mutate(dif = hosp_dis_datetime - hosp_adm_datetime) %>% 
                mutate(dif2 = as.numeric(dif, units = "days")) %>% 
                mutate(outcome_ihd = coalesce(outcome_ihd.y, outcome_ihd.x)) %>% 
                mutate(time = case_when(outcome_ihd==1 ~ dif2,
                                        TRUE ~ 90))
        dfx <- dfx %>% mutate(outcome_ihd = as.factor(outcome_ihd))
        # library(survival)
        # library(survminer)
        # cox_fit <- coxph(Surv(time, outcome_ihd) ~ sex + age, data = data)
        # fit <- survfit(Surv(time, outcome_ihd) ~ sex, data = data)
        # 
        # ggsurvplot(fit, data = data, risk.table = TRUE)
        # 
        
                
        
        
}

#' @title getPrevStroke 
#' @description Get New Variable "Previous Stroke" - from sources: 1.) QS (until 2017) 2.) docs (all)
#' @param case_id
#' @param qs
#' @param df_neur_aufnb
#' @param df_neur_stwbr
#' @param df_neur_int
#' @param df_neur_stabr
#' @param df_hei
#' @return A tibble
getPrevStroke <- function(case_id, qs, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr, df_hei) {
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        # 1. source - qs documents until 2017
        df1 <- qs %>% select(FALNR, REINSULT) %>% 
                mutate(case_id = parse_number(as.character(FALNR))) %>% 
                mutate(dx_prev_stroke_1 = REINSULT) %>% 
                select(-FALNR, -REINSULT)
        
        # 2. source - df_neur_aufnb
        df2 <- df_neur_aufnb %>% select(fallnummer, dx_prev_stroke_2=dx_prev_stroke) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% select(-fallnummer)
        
        # optimise for not detecting Myokard
        # prev_stroke_str <- "(?:[Z|z]\\.?n\\.?\\s?)(Media+.{1,5}?infarkt|.+[I|i]schämie)"
        # prev_stroke_str1 <- "(?i)(?:([Z|z]\\.?\\s?n\\.?\\s?)|[A|a]lter?\\s?)(?:(?!Myokard|Herz|Milz|Leber)(Media)?(teil)?infarkt|.{1,15}[I|i]schämie|[I|i]nfarkt\\s?(im Stromgebiet der A. cerebri)?|.{1,4}[S|s]chlaganfall|[A|a]poplex|(Hirn)?insult)"
        prev_stroke_str <- "(?:([Z|z]\\.?\\s?n\\.?\\s?)|[A|a]lter?\\s?|(VORERKRANK.{1,50}))(?:(?!Myokard|Herz|Milz|Leber)(Media)?(teil)?infarkt|.{1,15}[I|i]schämie|[I|i]nfarkt\\s?(im Stromgebiet der A. cerebri)?|.{1,4}[S|s]chlaganfall|[A|a]poplex|(Hirn)?insult|TIA|[T|t]ransitorisch ischämische Attacke)"
        
        # 3. source - df_neur_stwbr
        df3 <- df_neur_stwbr %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                mutate(dx_prev_stroke_3 = (str_detect(pattern = prev_stroke_str, adm_diag_stwbr))/1) %>% 
                select(case_id, dx_prev_stroke_3)
                
        # 4. source - df_neur_int
        df4 <- df_neur_int %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                mutate(dx_prev_stroke_4 = (str_detect(pattern = prev_stroke_str, adm_diag_int))/1) %>% 
                select(case_id, dx_prev_stroke_4)
        
        # 5. source - df_neur_stabr
        df5 <- df_neur_stabr %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                mutate(dx_prev_stroke_5 = (str_detect(pattern = prev_stroke_str, adm_diag_stabr))/1) %>% 
                select(case_id, dx_prev_stroke_5)
        
        
        df <- df_hei %>% 
                mutate(case_id = parse_number(as.character(case_id))) %>% 
                select(case_id, risk_factor_prev_stroke) %>% 
                left_join(df1, by = "case_id") %>% unique() %>% 
                left_join(df2, by = "case_id") %>% unique() %>% 
                left_join(df3, by = "case_id") %>% unique() %>% 
                left_join(df4, by = "case_id") %>% unique() %>% 
                left_join(df5, by = "case_id") %>% unique() %>%
                mutate(dx_prev_stroke = coalesce(pmax(risk_factor_prev_stroke, dx_prev_stroke_1, dx_prev_stroke_2, dx_prev_stroke_3, dx_prev_stroke_4, dx_prev_stroke_5, na.rm = TRUE)), .keep = "unused") %>% 
                drop_na(case_id)
        
        # t <- df_hei %>% mutate(case_id = parse_number(as.character(case_id))) %>% select(case_id, risk_factor_prev_stroke) %>% left_join(df, by = "case_id")
        # t <- t %>% mutate(new10 = ifelse(risk_factor_prev_stroke==1 & dx_prev_stroke==0, 1, 0))
        # t <- t %>% mutate(new01 = ifelse(risk_factor_prev_stroke==0 & dx_prev_stroke==1, 1, 0))
}


#' @title getEndocarditis 
#' @description Get information on Endocarditis during hospital stay from source: 1.) diagnosis codes
#' @param case_id
#' @param dia - diagnosis codes 
#'
#' @return A tibble
getEndocarditis <- function (case_id = case_id, dia = dia){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        # source diagnosis codes 
        df1 <- extractFromDia(var = "Endokarditis", var_short = "endocarditis", prefix = "var", dia)
        
        # source discharge letter
        # prompt1 <- "Act as an eclectic medical documentar. Please extract information on 'endocarditis' from the strings provided. 
        # please differentiate between the following states:
        # If there is no mentioning of endocarditis, simply state 'no'.
        # If endocarditis was one of the diagnosis indicate 'definite'.
        # If endocardtiis was suspected but not a definite diagnosis - for example 'V.a. Endokarditis' - indicate 'suspected'. 
        # if the text string is empty provide 'NA'.
        # Text: "
        # # df_neur_stwbr, df_neur_int, df_neur_stwbr
        # df2 <- rbind(df_neur_int %>% select(fallnummer, txt), df_neur_stabr %>% select(fallnummer, txt), df_neur_stwbr %>% select(fallnummer, txt))
        # # df2 <- head(df2, 50)
        # s <- Sys.time()
        # df2$new <-
        #         sapply(
        #                 df2$txt,
        #                 simplify = TRUE,
        #                 USE.NAMES = FALSE,
        #                 FUN =  function(x) {
        #                         ask_chatgpt(
        #                                 paste(
        #                                         prompt1,
        #                                         x 
        #                                 ), temp = 0.1
        #                         )
        #                 }
        #         )
        # Sys.time()-s
        
        
        # JOIN
        dfx <- left_join(case_id, df1, by = "case_id")
        return(dfx)
}


getPfo <- function (case_id, dia = dia){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        # source diagnosis codes 
        df1 <- extractFromDia(var = "Foramen ovale", var_short = "pfo", prefix = "etio", dia)
        
        # JOIN
        dfx <- left_join(case_id, df1, by = "case_id") %>% 
                mutate(etio_pfo = case_when(
                        etio_pfo>0~1,
                        TRUE~0
                ))
        
        return(dfx)
}

#' @title getCarotidStenosis 
#' @description Get information on Carotid Stenosis during hospital stay from source: 1.) diagnosis codes I63.0
#' @param case_id
#' @param dia - diagnosis codes 
#'
#' @return A tibble
getCarotidStenosis <- function(case_id, dia){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        # source diagnosis codes 
        df1 <- dia %>% mutate(case_id = parse_number(as.character(`Fall`))) %>% 
                mutate(etio_cs_code = `RefDiagn.`) %>% 
                mutate(etio_cs_detail = paste0(`Diagnose...15`, `Diagnose...16`, " | ", `Diagnose...18`)) %>% 
                select(case_id, etio_cs_code, etio_cs_detail) %>% 
                filter(etio_cs_code == "I63.0") %>% 
                mutate(etio_cs = 1) %>% 
                unique()
                
        
        # JOIN
        dfx <- left_join(case_id, df1, by = "case_id")
        dfx$etio_cs[is.na(dfx$etio_cs)] <- 0
        return(dfx)
}

#' @title getPrevAlc 
#' @description Get information on C2/Alcohol taking into account source: 1.) ICD10 diagnosis 2.) medical records 3.) lab values GGT, AST, ALT, MCV, thromobzytes,
#' @param case_id
#' @param dia - diagnosis codes of patients 
#' @param df_neur_aufnb - medical records from admission on stroke unit
#' @param df_neur_stwbr - medical records from official letter stroke unit
#' @param df_neur_int - medical records from official letter intensive care unit
#' @param df_neur_stabr - medical records from official letter normal ward
#' @import tidyverse
#' @return A tibble
getPrevAlc <- function(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr, labs, kenn) {
        require(tidyverse)
        
        # Grading 
        # (1) EVIDENCE OF ALCOHOL INTAKE
        # (1) T50.9       akute Intox
        # (1) F10.0       akuter Rausch
        # (1) R78.0       Nachweis von Alkohol im Blut
        
        # (2) EVIDENCE OF MODERATE ORGAN DAMAGE
        # (2) F10.1       Schädlicher Gebrauch
        # (2) G62.1       PNP
        # (2) K86.0       chronische Pankreatitis
        # (2) K29.2       Gastritis
        # (2) K70.2       Fibrose
        # (2) K70.0       Alkoholische Fettleber
        # (2) K70.1       Alkholische Hepatitis
        
        # (3) EVIDENCE OF SEVERE ORGAN DAMAGE
        # (3) F10.2       Abhängigkeit 
        # (3) I42.6     Alkohlische Kardiomyopathie
        # (3) F10.3       Entzugssyndrom
        # (3) K70.3       Zirrhose
        # (3) E51.2       Wernicke Encephalopathie
        # (3) F10.6       Korsakow Syndrom
        
        
        # 1. ICD10 Diagnosis 
        df1 <- extractFromDia(var = "Alkoh", var_short = "alc", prefix = "var", dia) %>% 
                mutate(dx_rf_alc_grade = case_when(var_alc_code %in% c("T50.9","F10.0") ~ 1, 
                                                   var_alc_code %in% c("K70.0", "K70.1", "K70.2", "K29.2", "K86.0", "G62.1", "F10.1") ~ 2,
                                                   var_alc_code %in% c("F10.2", "I42.6", "F10.3", "E51.2", "K70.3", "K72.7", "F10.4", "F10.6") ~ 3)) 
        
        # Medical records
        # 2. df_neur_aufnb from Yes/No String -> dx_rf_alc_str
        df2 <- df_neur_aufnb %>% select(fallnummer, dx_rf_alc_str) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% select(-fallnummer)
        # 3 df_neur_stwbr from diagnosis 
        df3 <- df_neur_stwbr %>%
                mutate(dx_rf_alc_severe3 = ifelse(
                        str_detect(pattern = "[A|a]lkohol|C2", adm_diag_stwbr) / 1 == 1,
                        2,
                        0
                ))  %>%
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                               select(case_id, dx_rf_alc_severe3)
        # 4 df_neur_int from diagnosis
        df4 <- df_neur_int %>% 
                mutate(dx_rf_alc_severe4 = ifelse(str_detect(pattern = "[A|a]lkohol|C2", adm_diag_int)/1==1,2,0)) %>%
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_rf_alc_severe4)
        # 5 df_neur_stabr from diagnosis
        df5 <- df_neur_stabr %>% 
                mutate(dx_rf_alc_severe5 = ifelse(str_detect(pattern = "[A|a]lkohol|C2", adm_diag_stabr)/1==1,2,0)) %>%
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                               select(case_id, dx_rf_alc_severe5)
                                        
                                        
        # 6. Lab values
        string_sel <- c("Albumin quant.", "GGT", "GOT/AST", "GPT/ALT", "LDH", "CHE", "Ges.Bilirubin", "Dir.Bilirubin", "MCH", "MCV", "Thrombozyten")
        # alc_lab <- rbind(lab_liver, lab_haem) %>% filter(Text %in% string_sel)
        df6 <-
                getLabs(
                        labs = labs,
                        case_id = case_id,
                        lab_vars_names = string_sel,
                        kenn = kenn
                ) %>%
                reduce(left_join, by = "Fall") %>%
                select(c(Fall, contains("median"))) %>%
                mutate(case_id = Fall) %>% select(-Fall)
        
        # JOIN
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        dfx <- case_id %>% drop_na(case_id) %>% left_join(df1, by = "case_id") %>% 
                mutate(dx_rf_alc_grade = ifelse(is.na(dx_rf_alc_grade), 0, dx_rf_alc_grade)) %>% 
                left_join(df2, by = "case_id") %>% 
                        left_join(df3, by = "case_id") %>% 
                        left_join(df4, by = "case_id") %>% 
                        left_join(df5, by = "case_id") %>% 
                mutate(dx_rf_alc = coalesce(pmax(dx_rf_alc_grade, dx_rf_alc_str, dx_rf_alc_severe3, dx_rf_alc_severe4, dx_rf_alc_severe5, na.rm = TRUE)), .keep = "unused") %>% 
                select(-var_alc, -var_alc_code, -var_alc_detail) %>% 
                left_join(df6, by = c("case_id" = "Fall"))
        
        dfx <- dfx %>% mutate(dx_rf_alc = recode(dx_rf_alc, "0" = "not present", "1" = "mild", "2" = "moderate", "3" = "severe")) %>% 
                select(-case_id.y)
        return(dfx)
}

getAlcoholAdmission <- function(case_id, df_neur_aufnb){
        
        df2 <- df_neur_aufnb %>% select(fallnummer, dx_rf_alc_str) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% select(-fallnummer) %>% 
                mutate(
                        risk_factor_alc_adm = case_when(
                                is.na(dx_rf_alc_str) ~ "not determined",
                                dx_rf_alc_str == 0 ~ "no/mild",
                                dx_rf_alc_str == 1 ~ "moderate/severe"
                        )) %>%
                mutate(risk_factor_alc_adm = ordered(risk_factor_alc_adm, levels = c("no/mild", "moderate/severe", "not determined"))) %>% 
                select(-dx_rf_alc_str)
        
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        dfx <- case_id %>% drop_na(case_id) %>% 
                left_join(df2, by = "case_id")
        return(dfx)
                
}


#' @title getDxDementia 
#' @description Get information on Dementia taking into account source: 1.) ICD10 diagnosis 2.) admission medications 3.) medical records 
#' @param case_id
#' @param dia - diagnosis codes of patients 
#' @param df_neur_aufnb - medical records from admission on stroke unit
#' @param df_neur_stwbr - medical records from official letter stroke unit
#' @param df_neur_int - medical records from official letter intensive care unit
#' @param df_neur_stabr - medical records from official letter normal ward
#' @import tidyverse
#' @return A tibble
getDxDementia <- function(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr){
        # 1. ICD10 Diagnosis 
        df1 <- extractFromDia(var = "Demenz", var_short = "prev_dementia", prefix = "dx", dia)
        
        # 2. Extract from Aufnahme-Medikation
        # search for Acetylcholinesterase-Hemmer: Donepezil, Aricept, Rivastigmin, Exelon, Galantamin, Reminyl, 
        # search for N-Methyl-D-Aspartat (NMDA)-Rezeptor-Antagonist: Memantin, Ebixa, Axura
        # search for Kombination: Donepezil und Memantin (Handelsname: Namzaric)
        
        search_str_mdx <- "Acetylcholinesterase|Donepezil|Aricept|Rivastigmin|Exelon|Galantamin|Reminyl|Memantin|Ebixa|Axura|Namzaric"
        search_str <- "[D|d]emenz"
        
        df2 <- df_neur_aufnb %>% 
                mutate(dx_prev_dementia_meds = str_detect(pattern = search_str_mdx, adm_meds_aufnb)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_dementia_meds)
        
        # 3. Extract from diagnosis
        df3 <- df_neur_aufnb %>% 
                mutate(dx_prev_dementia3 = str_detect(pattern = search_str, adm_diag_aufnb)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_dementia3)
        
        # 4 Extract from diagnosis df_neur_int
        df4 <- df_neur_int %>% 
                mutate(dx_prev_dementia4 = str_detect(pattern = search_str, adm_diag_int)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_dementia4)
           
        # 5 Extract from diagnosis df_neur_stabr
        df5 <- df_neur_stabr %>% 
                mutate(dx_prev_dementia5 = str_detect(pattern = search_str, adm_diag_stabr)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_dementia5)
        
        
        # JOIN
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        dfx <- case_id %>% drop_na(case_id) %>% left_join(df1, by = "case_id") %>% 
                mutate(dx_prev_dementia = ifelse(is.na(dx_prev_dementia), 0, dx_prev_dementia)) %>% 
                left_join(df2, by = "case_id") %>% 
                left_join(df3, by = "case_id") %>% 
                left_join(df4, by = "case_id") %>% 
                left_join(df5, by = "case_id") %>% 
                mutate(dx_prev_dementia_mdx = ifelse(is.na(dx_prev_dementia_meds), 0, dx_prev_dementia_meds)) %>% 
                mutate(dx_prev_dementia = coalesce(pmax(dx_prev_dementia, dx_prev_dementia_meds, dx_prev_dementia3, dx_prev_dementia4, dx_prev_dementia5, na.rm = TRUE)), .keep = "unused") %>% 
                select(-dx_prev_dementia_code, -dx_prev_dementia_detail)
        return(dfx)
}
  
#' @title getPrevDementia 
#' @description Get INFORMATION THAT ARE PRESENT ONLY AT PRESENTATION of patient 1.) admission medications 3.) medical records stating previous diagnosis (EXPLICITELY THIS VARIABLE DOES NOT INCLUDE DIAGNOSIS MADE DURING THE HOSPITAL STAY)
#' @param case_id
#' @param df_neur_aufnb - medical records from admission on stroke unit
#' @param df_neur_int - medical records from official letter intensive care unit
#' @import tidyverse
#' @return A tibble
getPrevDementia <- function(case_id, df_neur_aufnb, df_neur_not, df_neur_int){
        
        search_str_mdx <- "Acetylcholinesterase|Donepezil|Aricept|Rivastigmin|Exelon|Galantamin|Reminyl|Memantin|Ebixa|Axura|Namzaric"
        search_str <- "[D|d]emenz"
        
        df1 <- df_neur_not %>% 
                mutate(dx_prev_dementia1 = str_detect(pattern = search_str_mdx, txt)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_dementia1)
        
        
        df2 <- df_neur_aufnb %>% 
                mutate(dx_prev_dementia2 = str_detect(pattern = search_str_mdx, adm_meds_aufnb)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_dementia2)
        
        #  Diagnosis at admission
        df3 <- df_neur_aufnb %>% 
                mutate(dx_prev_dementia3 = str_detect(pattern = search_str, adm_diag_aufnb)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_dementia3)
        
        # 4 Extract from diagnosis df_neur_int
        df4 <- df_neur_int %>% 
                mutate(dx_prev_dementia4 = str_detect(pattern = search_str_mdx, txt)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_dementia4)
        

        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        dfx <- case_id %>% drop_na(case_id) %>% 
                left_join(df1, by = "case_id") %>% 
                left_join(df2, by = "case_id") %>% 
                left_join(df3, by = "case_id") %>% 
                left_join(df4, by = "case_id") %>% 
                mutate(var_prev_dementia = coalesce(pmax(dx_prev_dementia1, dx_prev_dementia2, dx_prev_dementia3, dx_prev_dementia4, na.rm = TRUE)), .keep = "unused") %>%
        return(dfx)
}

                                 

#' @title getPrevCOPD 
#' @description Get information on chronic obstructive pulmonary disease (COPD) taking into account source: 1.) ICD10 diagnosis 2.) admission medications 3.) medical records 
#' @param case_id
#' @param dia - diagnosis codes of patients 
#' @param df_neur_aufnb - medical records from admission on stroke unit
#' @param df_neur_stwbr - medical records from official letter stroke unit
#' @param df_neur_int - medical records from official letter intensive care unit
#' @param df_neur_stabr - medical records from official letter normal ward
#' @import tidyverse
#' @return A tibble
getPrevCOPD <- function(case_id, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr){
        # 1. ICD10 Diagnosis 
        df1 <- extractFromDia(var = "Chronische obstruktive Lungen", var_short = "prev_copd", prefix = "dx", dia)
        
        # 2. Extract from Aufnahme-medikation
        # search for Kurzwirksame Beta-2-Agonisten (SABAs): Salbutamol, Ventolin, Fenoterol, Berotec
        # search for Langwirksame Beta-2-Agonisten (LABAs): Salmeterol Serevent, Formoterol Foradil, Oxis, Indacaterol Onbrez, Olodaterol Striverdi
        # search for Kurzwirksame Anticholinergika (SAMAs): Ipratropiumbromid, Atrovent, 
        # search for Langwirksame Anticholinergika (LAMAs): Tiotropiumbromid, Spiriva, Aclidiniumbromid Eklira, Tudorza, Umeclidiniumbromid, Incruse, Glycopyrroniumbromid, Seebri
        search_str <- "[S|s]albutamol|[V|v]entolin|[F|f]enoterol|[B|b]erotec|[S|s]almeterol|[S|s]erevent|[F|f]ormoterol|[F|f]oradil|[O|o]xis|[I|i]ndacaterol|[O|o]nbrez|[O|o]lodaterol|[S|s]triverdi|[I|i]pratropiumbromid|[A|a]trovent|[T|t]iotropiumbromid|[S|s]piriva|[A|a]clidiniumbromid|[E|e]klira|[T|t]udorza|[U|u]meclidiniumbromid|[I|i]ncruse|[G|g]lycopyrroniumbromid|[S|s]eebri"
        df2 <- df_neur_aufnb %>% 
                mutate(dx_prev_copd2 = str_detect(pattern = search_str, adm_meds_aufnb)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_copd2)
        
        # 3. Extract from diagnosis
        df3 <- df_neur_aufnb %>% 
                mutate(dx_prev_copd3 = str_detect(pattern = "COPD|[C|c]hronisch obstruktive|Lungenerkrankung|[C|c]hronisches Astma", adm_diag_aufnb)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_copd3)
        
        # 4 Extract from diagnosis df_neur_int
        df4 <- df_neur_int %>% 
                mutate(dx_prev_copd4 = str_detect(pattern = search_str, adm_diag_int)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_copd4)
        
        # 5 Extract from diagnosis df_neur_stabr
        df5 <- df_neur_stabr %>% 
                mutate(dx_prev_copd5 = str_detect(pattern = search_str, adm_diag_stabr)/1) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                select(case_id, dx_prev_copd5)
        
        
        # JOIN
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        dfx <- case_id %>% drop_na(case_id) %>% left_join(df1, by = "case_id") %>% 
                mutate(dx_prev_copd = ifelse(is.na(dx_prev_copd), 0, dx_prev_copd)) %>% 
                left_join(df2, by = "case_id") %>% 
                left_join(df3, by = "case_id") %>% 
                left_join(df4, by = "case_id") %>% 
                left_join(df5, by = "case_id") %>% 
                mutate(dx_prev_copd = coalesce(pmax(dx_prev_copd, dx_prev_copd2, dx_prev_copd3, dx_prev_copd4, dx_prev_copd5, na.rm = TRUE)), .keep = "unused") %>% 
                select(-dx_prev_copd_code, -dx_prev_copd_detail)
        return(dfx)
}


getPrevSmoke <- function(case_id, df_hei, dia, df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr){
        
        # 1. Dia 
        df1 <- extractFromDia(var = "F17", var_short = "smoke", prefix = "dx_rf", dia = dia, code = TRUE) %>% 
                group_by(case_id) %>% arrange(desc(dx_rf_smoke_detail)) %>% slice(1L) %>% unique() %>% 
                select(case_id, dx_rf_smoke_active = dx_rf_smoke)
        
        # 2. Admission medical records - semistructured content
        df2 <- df_neur_aufnb %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                mutate(dx_rf_smoke_py = parse_number(dx_rf_smoke_py)) %>% 
                mutate(dx_rf_smoke_active = parse_number(dx_rf_smoke_str)) %>% 
                select(case_id, dx_rf_smoke_active, dx_rf_smoke_py) 
                
                
                
        # 3. from all medical - unstructured content
                py_str <- "(?:\\b(\\d+)\\s?(?:[P|p]ack[-\\s]?[Y|y]ears?|PY|Pack[-\\s]?years|Zigaretten\\s?\\/\\s?d|[P|p][Y|y])\\b)"
                prev_smoke_str <- "(?:[E|e]hemal(iger)?|[V|v]ormals?.|[A|a]bstinen.?).{1,20}([R|r]aucher|[N|n]ikotin)|(?:Z\\.?n\\.?)\\s([R|r]aucher|[N|n]ikotin)"
                active_smoke_str <- "(?:[A|a]ktiver?).{1,5}[R|r]aucher|(?<!Z\\.n\\.\\s.{0,20})(?<!([E|e]hemal|[V|v]ormal|Ex-?).{0,8}\\s?)[N|n]ikotin(abusus|konsum)?(?!\\sbis)"
                
                df3 <-
                rbind(
                        df_neur_aufnb %>% select(fallnummer, txt),
                        df_neur_stwbr %>% select(fallnummer, txt),
                        df_neur_int %>% select(fallnummer, txt),
                        df_neur_stabr %>% select(fallnummer, txt)
                ) %>%
                mutate(py = parse_number(str_extract(pattern = py_str, txt))) %>% 
                mutate(prev_smoke = str_detect(pattern = prev_smoke_str, txt)/1) %>%
                mutate(active_smoke = str_detect(pattern = active_smoke_str, txt)/1) %>%
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                group_by(case_id) %>% 
                mutate(py = max(py)) %>% 
                        mutate(prev_smoke = max(prev_smoke)) %>% 
                        mutate(active_smoke=case_when(prev_smoke==1~0,TRUE~max(active_smoke))) %>%
                        slice(1L) %>% ungroup() %>% 
                        select(case_id, dx_rf_smoke_py = py, risk_factor_prev_smoker = prev_smoke, dx_rf_smoke_active = active_smoke)
                
        # 4. Heireka db
                df4 <- df_hei %>% select(case_id, risk_factor_current_smoker) %>% 
                        mutate(case_id = parse_number(as.character(case_id))) %>% drop_na(case_id) %>% 
                        mutate(dx_rf_smoke_active = parse_number(as.character(risk_factor_current_smoker))) %>% select(-risk_factor_current_smoker)
                        
        # Join
                case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
                
                dfx <- case_id %>% drop_na(case_id) %>% left_join(df1, by = "case_id") %>% unique() %>% 
                        mutate(dx_rf_smoke_active = case_when(dx_rf_smoke_active==1~1,TRUE~0)) %>% 
                        left_join(df2, by = "case_id") %>% unique() %>% 
                        left_join(df3, by = "case_id") %>% unique() %>% 
                        left_join(df4, by = "case_id") %>% unique() %>% 
                        mutate(risk_factor_current_smoker = coalesce(pmax(dx_rf_smoke_active.y.y, dx_rf_smoke_active.x.x, dx_rf_smoke_active.x, dx_rf_smoke_active.y, na.rm = TRUE)), .keep = "unused") %>% 
                        mutate(dx_rf_smoke_py = coalesce(pmax(dx_rf_smoke_py.x, dx_rf_smoke_py.y)), .keep = "unused") %>% 
                        mutate(risk_factor_smoker_py = case_when(risk_factor_current_smoker==1~dx_rf_smoke_py, risk_factor_prev_smoker==1~dx_rf_smoke_py, TRUE~0)) %>% 
                        select(-dx_rf_smoke_py)
                
                dfx <- dfx %>% mutate(risk_factor_smoking = factor(case_when(risk_factor_prev_smoker==1~"previous",
                                                                      risk_factor_current_smoker==1~"current",
                                                                      TRUE~"no")))
        # return        
                return(dfx)
}


#' @title getPalliativeCare 
#' @description Get information on PALLIATIVE CARE during hospital stay from source: 1.) QS documents
#' @param case_id
#' @param QS
#'
#' @return A tibble
getPalliativeCare <- function(case_id, qs){
   
        df <- qs %>%
        select(case_id = FALNR, contains("PALL")) %>% 
        # filter(PALLIATIV == 1) %>% 
        select(case_id, 
               outcome_palliative_care = "PALLIATIV", 
               outcome_pallitive_date = "PALLIATIVD", 
               outcome_palliative_care_reason = "PALLIATIVG") %>%  # Reason for Palliative case: 1) no medical indciation; 2) presumed patients will
                mutate(case_id = parse_number(as.character(case_id)))
}


#' @title getBarthelHospDischarge 
#' @description Get information on admission and discharge BARTHEL INDEX source: 1.) QS Dokuments
#' @param case_id
#' @param QS
#'
#' @return A tibble
getBarthelHospDischarge <- function(case_id, qs){
    df <- qs %>%
        select(case_id = FALNR,
               adm_prestroke_care = VERSORGVOR, # Wert 1: unabhängig, # Wert 2: Pflege zu Hause, # Wert 3: Pflege in Instituation
               # SCHLUCKVER,
               adm_dysphagia = SCHLUCKST2,
               # adm_barthel_index = AUFNBARTB1,
               adm_bi_eat = ABNA111,
               adm_bi_sit = ABBS11,
               adm_bi_wash = ABKP11,
               adm_bi_toilet = ABTB11,
               adm_bi_bath = ABBD11,
               adm_bi_urin = ABHK11,
               adm_bi_stair = ABTS11,
               adm_bi_dress = ABAK11,
               adm_bi_bowel = ABSK11,
               adm_bi_walk = ABGE11,
               adm_barthel_index = AB01,
               # PATVERSTO2,
               # ENTLBARTB1,
               dis_bi_eat = EBNA111,
               dis_bi_sit = EBBS11,
               dis_bi_wash = EBKP11, 
               dis_bi_toilet = EBTB11,
               dis_bi_bath = EBBD11,
               dis_bi_urin = EBGE11,
               dis_bi_stair = EBTS11,
               dis_bi_dress = EBAK11,
               dis_bi_bowel = EBSK11,
               dis_bi_walk = EBHK11,
               EB01) %>% 
        mutate(outcome_dis_barthel_index = parse_number(as.character(EB01))) %>% 
            unique() %>% 
        mutate(case_id = parse_number(as.character(case_id))) %>% 
            drop_na(outcome_dis_barthel_index) %>% 
            group_by(case_id) %>% 
            arrange(-desc(outcome_dis_barthel_index)) %>% 
            slice(1L) %>% 
            ungroup()
        return(df)
    
    # BARTHEL INDEX COMMENT ####### - #
    # ############################# - #
    # D1: Versorgungssituation vor dem Akutereignis?:	[USER_PD:PRIMTAB-VERSORGVOR]
    # D2: Wurde ein Schluckversuch durchgef¸hrt?:	[USER_PD:PRIMTAB-SCHLUCKVER]
    # D3: Wenn ja, Schluckstˆrung:	[USER_PD:PRIMTAB-SCHLUCKST2]
    # D4: Barthel-Index bei Aufnahme bestimmt?:	[USER_PD:PRIMTAB-AUFNBARTB1]
    # D5: Essen:	[USER_PD:PRIMTAB-ABNA111]
    # D6: Auf- & Umsetzen:	[USER_PD:PRIMTAB-ABBS11]
    # D7: Sich Waschen:	[USER_PD:PRIMTAB-ABKP11]
    # D8: Toilettenbenutzung:	[USER_PD:PRIMTAB-ABTB11]
    # D9: Baden/Duschen:	[USER_PD:PRIMTAB-ABBD11]
    # D14: Harnkontrolle:	[USER_PD:PRIMTAB-ABHK11]
    # D11: Treppensteigen:	[USER_PD:PRIMTAB-ABTS11]
    # D12: An- & Auskleiden:	[USER_PD:PRIMTAB-ABAK11]
    # D13: Stuhlkontrolle:	[USER_PD:PRIMTAB-ABSK11]
    # D10: Aufstehen & Gehen:	[USER_PD:PRIMTAB-ABGE11]
    # D15: Barthel-Index-Wert bei Aufnahme:	[USER_PD:PRIMTAB-AB01]
    # D16: Patient verstorben:	[USER_PD:PRIMTAB-PATVERSTO2]
    # D17: Wurde ein Pflege¸berleitungsbogen erstellt:	[USER_PD:PRIMTAB-EHAUSV1]
    # D18: Barthel-Index bei Entlassung bestimmt?:	[USER_PD:PRIMTAB-ENTLBARTB1]
    # D19: Essen:	[USER_PD:PRIMTAB-EBNA111]
    # D20: Auf- & Umsetzen:	[USER_PD:PRIMTAB-EBBS11]
    # D21: Sich Waschen:	[USER_PD:PRIMTAB-EBKP11]
    # D22: Toilettenbenutzung:	[USER_PD:PRIMTAB-EBTB11]
    # D23: Baden/Duschen:	[USER_PD:PRIMTAB-EBBD11]
    # D24: Aufstehen & Gehen:	[USER_PD:PRIMTAB-EBGE11]
    # D25: Treppensteigen:	[USER_PD:PRIMTAB-EBTS11]
    # D26: An- & Auskleiden:	[USER_PD:PRIMTAB-EBAK11]
    # D27: Stuhlkontrolle:	[USER_PD:PRIMTAB-EBSK11]
    # D28: Harnkontrolle:	[USER_PD:PRIMTAB-EBHK11]
    # D29: Barthel-Index-Wert bei Entlassung:	[USER_PD:PRIMTAB-EB01]
}


#' @title extractHandednessFromLetters - 
#' @description Retrieves information about text information in  letter about handedness (right, left, ambi)
#' @param var - specifies which letter information to retrieve c("aufnb","stwbr","int","stabr")
#' @param df - c(df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
#' @return A tibble with case_id and variable handedness
extractHandednessFromLetters <- function(var = c("aufnb","stwbr","int","stabr"), df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr) {
        var_name <- paste0("var_hand_", var)
        if (var %in% c("aufnb","stwbr", "int", "stabr")) {
                df <-
                        eval(as.name(paste0("df_neur_", var))) %>%
                        mutate(
                                var_hand =
                                        str_extract(
                                                txt,
                                                "(?i)(Rechts|Links|Bi)h[äae]ndl?e?r?|Ambidex"
                                        )
                        ) %>% 
                        mutate(case_id = parse_number(as.character(fallnummer))) %>%
                        mutate(!!var_name := var_hand) %>% 
                        select(case_id, !!var_name)
                
        } else return("Error: Please supply one of the following 'stwbr','int', 'stabr'")
        return(df)
}

#' @title getHandedness - anamnestic information about handedness of the patient 
#' @description Handedness self reported by the patient 1) right, 2) left, 3) ambidextrous; Extracted information from the tab-forms of "Aufnahmebogen" - Information available from patients of Stroke-Unit, not ICU
#' @param df_neur_aufnb
#' @param case_id
#'
#' @return A tibble
#'
#' @examples getHandedness(df_neur_aufnb, case_id)
getHandedness <- function(df_neur_aufnb = df_neur_aufnb, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr, case_id = case_id){
        
        # source tab-forms adm/stroke-unit
        df <- df_neur_aufnb
        df1 <- df %>% 
                mutate(handed_left = na_if(handed_left, "0")) %>% 
                mutate(handed_left = recode_factor(handed_left, "1" = "left")) %>% 
                mutate(handed_right = na_if(handed_right, "0")) %>% 
                mutate(handed_right = recode_factor(handed_right, "1" = "right")) %>%
                mutate(handed_ambidextrous = case_when(handed_right=="right" & handed_left=="left" ~ "ambidextrous")) %>% 
                mutate(handedness = coalesce(handed_ambidextrous, handed_left, handed_right)) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                select(case_id, "handedness")

        # source letters
        df2a <- extractHandednessFromLetters("aufnb", df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
        df2b <- extractHandednessFromLetters("stwbr", df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
        df2c <- extractHandednessFromLetters("int", df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
        df2d <- extractHandednessFromLetters("stabr", df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
        
        # JOINING 
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        dfx <- left_join(
                left_join(
                        left_join(
                                left_join(
                                        left_join(
                                                case_id, 
                                                df1, 
                                                by = "case_id"),
                                        df2a, 
                                        by = "case_id"),
                                df2b, 
                                by = "case_id"),
                        df2c, 
                        by = "case_id"),
                df2d, 
                by = "case_id") %>% 
                mutate(across(everything(), as.character))
        
        dfx <- dfx %>% mutate(var_handed = case_when(
                str_detect(handedness, "ambidextrous") ~ handedness,
                str_detect(var_hand_aufnb, "Linkshänder") ~ "left",
                str_detect(var_hand_stwbr, "Linkshänder") ~ "left",
                str_detect(var_hand_int, "Linkshänder") ~ "left",
                str_detect(var_hand_stabr, "Linkshänder") ~ "left",
                str_detect(handedness, "left") ~ handedness,
                TRUE ~ handedness
                )) %>% 
                mutate(var_handed = as_factor(var_handed)) %>% 
                select(case_id, var_handed) %>% 
                mutate(case_id = parse_number(as.character(case_id)))
        
        methods <- "Using the 'getHandedness' function, new variables are derived, particularly for identifying self-reported left-handedness, right-handedness, and ambidextrousness. Handedness information is extracted from different letter sources - 'aufnb', 'stwbr', 'int', and 'stabr' - using the function 'extractHandednessFromLetters'. A join operation and post-join transformation using 'case_id' as key brings together all extracted information. The final 'var_handed' variable is converted into a factor."
        return(dfx)
}


getSymptCas <- function(case_id, qs){
    # source QS documents
    df <- qs %>%
        select(case_id = FALNR, contains("STENOS")) %>% 
        mutate(etio_symp_cas = coalesce(STENOSE,STENOSE1,STENOSE112)) %>% 
        mutate(etio_symp_cas = recode(etio_symp_cas, "0" = "no", "1" = "<50%", "2" = "50%-69%", "3" = "70%-99%", "4" = "occlusion")) %>% 
        select(-STENOSE,-STENOSE1,-STENOSE112)
   
    # source discharge documents
    
}


# Deprecated, since adding to source df_hei/TOAST/dissection classification from PR does not add much.
# getDissection <- function(df_hei, case_id, dia){
#         # from Heireka
#         df1 <- df_hei %>% select(case_id, etiology_dissection)
#         
#         # source dia
#         df2 <- extractFromDia(var = "Dissektion", var_short = "diss", prefix = "etio", dia = dia)
#         
#         dfx <- left_join(df1, df2, by = "case_id")
# }


#' @title extractAphasieFromLetters - 
#' @description Retrieves information about text information in discharge letter about Aphasia
#' @param var - specifies which letter information to retrieve c("stwbr","int","stabr")
#' @param df - c(df_neur_stwbr, df_neur_int, df_neur_stabr)
#' @return A tibble with case_id and two variables aphasia grade
#'
#' @examples extractAphasiagetAphasiaFromLetters(df_neur_stwbr)
extractAphasiaFromLetters <- function(var = c("stwbr","int","stabr"), df_neur_stwbr, df_neur_int, df_neur_stabr) {
        if (var %in% c("stwbr", "int", "stabr")) {
                df <-
                        eval(as.name(paste0("df_neur_", var))) %>%
                        mutate(
                                aph_diag =
                                        str_extract(
                                                eval(as.name(paste0(
                                                        "adm_diag_", var
                                                ))),
                                                "(?i)(schwere?n?r?|globale?n?r?|ausgeprägte?n?r?|mäßigen?|moderate?n?r?|deutliche?n?r?|leichte?n?r?|geringe?n?r?|minimale?n?r?|eingeschränkte?n?|reduzierte?[n|r]?verminderte?|fehlende?)?\\s?(Aphasi|Wortfind|Sprachproduktion|Sprachstörung|Sprachverständnis|Paraphras)"
                                        )
                        ) %>%
                        mutate(
                                aph_epi = str_extract(
                                        eval(as.name(paste0(
                                                "adm_epi_", var
                                        ))),
                                        "(?i)(schwere?n?r?|globale?n?r?|ausgeprägte?n?r?|mäßigen?|moderate?n?r?|deutliche?n?r?|leichte?n?r?|geringe?n?r?|minimale?n?r?|eingeschränkte?n?|reduzierte?[n|r]?verminderte?|fehlende?)?\\W{1,8}?Aphasi|Wortfind|Sprachproduktion|Sprachstörung|Sprachverständnis|Paraphras"
                                )
                        ) %>%
                        mutate(
                                aph_epi_no = str_extract(
                                        eval(as.name(paste0(
                                                "adm_epi_", var
                                        ))),
                                        "(?i)([K|k]eine?)\\s?(Dysarthrie oder\\s|Anhalt für\\s)?(Aphasi|Sprachstörung?|Paraphras)"
                                )
                        ) %>%
                        mutate(
                                aph_exam_no = str_extract(
                                        eval(as.name(paste0(
                                                "adm_nexam_", var
                                        ))),
                                        "(?i)([K|k]eine?)\\s?(Dysarthrie oder\\s|Anhalt für\\s)?(Aphasi|Sprachstörung|Paraphras)"
                                )
                        ) %>%
                        mutate(
                                aph_exam = str_extract(
                                        eval(as.name(paste0(
                                                "adm_nexam_", var
                                        ))),
                                        "(?i)(schwere?n?r?|globale?n?r?|ausgeprägte?n?r?|mäßigen?|moderate?n?r?|deutliche?n?r?|leichte?n?r?|geringe?n?r?|minimale?n?r?|eingeschränkte?n?|reduzierte?[n|r]?verminderte?|fehlende?)?\\W{1,8}?Aphasi|Wortfind|Sprachproduktion|Sprachstörung|Sprachverständnis|Paraphras"
                                )
                        )
        } else
                return("Error: Please supply on of the following 'stwbr','int', 'stabr'"
                )
        
        var_name <- paste0("var_aphasia_", var)
        
        df <- df %>% 
                mutate(
                        var_aphasia = str_to_lower(
                                case_when(
                                        !is.na(aph_diag) ~ 
                                                coalesce(
                                                        str_extract(aph_diag, "(?i)(schwer|global|ausgeprägte?|mäßige?|moderate?|deutliche?|leicht|gering|minimal|vermindert|fehlend)"),
                                                        str_extract(aph_epi, "(?i)(schwer|global|ausgeprägte?|mäßige?|moderate?|deutliche?|leicht|gering|minimal|vermindert|fehlend)"),
                                                        str_extract(aph_exam, "(?i)(schwer|global|ausgeprägte?|mäßige?|moderate?|deutliche?|leicht|gering|minimal|vermindert|fehlend)"),
                                                        paste0(str_extract(aph_diag, ".*"))
                                                ),
                                        is.na(aph_diag) & !is.na(aph_exam) & !is.na(aph_exam_no) ~ "no aphasia",
                                        is.na(aph_diag) & is.na(aph_exam) & is.na(aph_epi) ~ "no aphasia",
                                        is.na(aph_diag) &
                                                !is.na(aph_exam) & !is.na(aph_epi) ~ 
                                                coalesce(
                                                        str_extract(aph_diag, "(?i)(schwer|global|ausgeprägte?|mäßige?|moderate?|deutliche?|leicht|gering|minimal|vermindert|fehlend)"),
                                                        str_extract(aph_epi, "(?i)(schwer|global|ausgeprägte?|mäßige?|moderate?|deutliche?|leicht|gering|minimal|vermindert|fehlend)"),
                                                        str_extract(aph_exam, "(?i)(schwer|global|ausgeprägte?|mäßige?|moderate?|deutliche?|leicht|gering|minimal|vermindert|fehlend)"),
                                                        paste0(str_extract(aph_epi, ".*"))
                                                ),
                                        is.na(aph_diag) & !is.na(aph_exam) & is.na(aph_exam_no) ~ 
                                                coalesce(
                                                        str_extract(aph_exam, "(?i)(schwer|global|ausgeprägte?|mäßige?|moderate?|deutliche?|leicht|gering|minimal|vermindert|fehlend)"),
                                                        paste0(str_extract(aph_exam, ".*"))
                                                ),
                                        is.na(aph_diag) & !is.na(aph_epi) & is.na(aph_epi_no) ~ 
                                                coalesce(
                                                        str_extract(aph_epi, "(?i)(schwer|global|ausgeprägte?|mäßige?|moderate?|deutliche?|leicht|gering|minimal|vermindert|fehlend)"),
                                                        paste0(str_extract(aph_epi, ".*"))
                                                ),
                                ))
                ) %>%
                mutate(var_aphasia = str_replace_all(var_aphasia, "[\\.,\\(\\):]", "")) %>% 
                mutate(var_aphasia = str_replace(var_aphasia, "^\\s", "")) %>% 
                mutate(var_aphasia = recode(var_aphasia, "schwer" = "severe", "leicht" = "minor", "minimal" = "minor", "gering" = "minor", "aphasi" = "moderate", "mäßige" = "moderate", "deutlich" = "severe", "ausgeprägte" = "severe", "deutliche" = "moderate", "vermindert" = "minor", "fehlend" = "severe", "wortfind" = "minor", "sprachproduktion" = "moderate", "sprachverständnis" = "moderate", "paraphras" = "minor", "eingeschränkte sprachproduktion" = "moderate", "sprachstörung" = "moderate")) %>% 
                # mutate(var_aphasia = ordered(var_aphasia, levels = c("no aphasia", "minor", "moderate", "severe", "global", NA))) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                mutate(!!var_name := var_aphasia) %>% 
                select(case_id, !!var_name)
        return(df)
}      



#' @title getAphasie - Retrieves information about Aphasia being present as symptom
#' @description Information of Aphasia is taken from 3 different sources: 1) Heireka (SymptomTab), 2) QS Dokumentation, 3) Diagnosis codes
#' @param db_heireka
#' @param df_hei
#' @param dia_neur
#' @param case_id
#'
#' @return A tibble with case_id and two variables 1) aphasia grade (limited patients) 2) aphasia present (yes/no)
#'
#' @examples 
getAphasia <- function(db_heireka = db_heireka, df_hei = df_hei, qs = qs, dia = dia, case_id = case_id, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr){
        # - ################# - #
        # source db_heireka
        df1 <- left_join(df_hei %>% select(case_id, LfdNr),
                         db_heireka$SymptomsTab %>% select(LfdNr, Aphasie),
                         by = "LfdNr") %>% 
                mutate(case_id = parse_number(as.character(case_id))) %>% 
                mutate(var_aphasia_grade = as.character(Aphasie)) %>% 
                select(case_id, var_aphasia_grade)
        
        # - ######### - #
        # source qs
        cols <- qs %>% select(contains("ENTLSPRAC")) %>% colnames()
        df2 <- qs %>% select(FALNR, !!!cols) %>% 
                mutate(var_aphasia_grade = coalesce(ENTLSPRACH, ENTLSPRAC1, ENTLSPRAC2)) %>% 
                select(case_id = FALNR, var_aphasia_grade) %>% 
                mutate(case_id = parse_number(as.character(case_id)))
        
        
        df12 <- bind_rows(df1, df2) %>% 
                select(case_id, var_aphasia_grade) %>% 
                mutate(var_aphasia_grade = parse_number(as.character(var_aphasia_grade)))
        
        # - ########## - #
        # source dia
        df3 <- dia %>% 
                mutate(dx_txt = paste0(`Diagnose...15`, `Diagnose...16`," | ",`Diagnose...18`)) %>% 
                filter(str_detect("(?i)Aphasi", string = dx_txt)) %>% 
                mutate(case_id = parse_number(as.character(Fall))) %>% 
                mutate(var_aphasia_yes = 1) %>% 
                select(case_id, var_aphasia_yes)
        
        # - ##################################### - #
        # source diagnosis of discharge letters
        df4a <- extractAphasiaFromLetters("stwbr", df_neur_stwbr, df_neur_int, df_neur_stabr)
        df4b <- extractAphasiaFromLetters("int", df_neur_stwbr, df_neur_int, df_neur_stabr)
        df4c <- extractAphasiaFromLetters("stabr", df_neur_stwbr, df_neur_int, df_neur_stabr)
        
        # - ######### - #
        # JOINING #
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        df12_j <- left_join(case_id, df12, by = "case_id") %>%
                group_by(case_id) %>%
                slice(which.max(!is.na(var_aphasia_grade)))
        df123 <-
                left_join(df12_j, df3, by = "case_id") %>% 
                mutate(var_aphasia = coalesce(var_aphasia_grade, var_aphasia_yes)) %>% 
                mutate(var_aphasia = (var_aphasia>0)/1) %>% select(-var_aphasia_yes)
        
        
        dfx <- left_join(left_join(left_join(df123,
                                             df4a,
                                             by = "case_id"),
                                   df4b,
                                   by = "case_id"),
                         df4c,
                         by = "case_id") %>% 
                mutate(across(everything(), as.character))
        
        # CAVE :::: FOR CASE_WHEN NECESSARY THAT ALL ARE SAME TYP !!! ~4h
        # CONDENSING
         dfx <- dfx %>% 
                mutate(var_aphasia_c = coalesce(var_aphasia_stwbr, var_aphasia_int, var_aphasia_stabr)) %>%
                mutate(var_aphasia_grade2 =
                               case_when(
                                       is.na(var_aphasia) ~ var_aphasia_c,
                                       var_aphasia == 1 & var_aphasia_c != "no aphasia" ~ var_aphasia_c,
                                       var_aphasia == 0 & var_aphasia_c == "no aphasia" ~ var_aphasia_c,
                                       var_aphasia == 0 & is.na(var_aphasia_c) ~ "no aphasia",
                                       var_aphasia == 1 & var_aphasia_c == "no aphasia" ~ "no aphasia",
                                       var_aphasia == 0 & !is.na(var_aphasia_c) ~ var_aphasia_c,
                                       var_aphasia == 1 & is.na(var_aphasia_c) & !is.na(var_aphasia_grade) ~ var_aphasia_grade,
                                       var_aphasia == 1 & is.na(var_aphasia_c) & is.na(var_aphasia_grade) ~ "moderate"
                               )
                       ) %>% 
                select(case_id, var_aphasia_grade2) %>% 
                mutate(var_aphasia_grade2 = recode(var_aphasia_grade2, "1" = "minor", "2" = "moderate", "3" = "severe")) %>% 
                mutate(var_aphasia_grade2 = ordered(var_aphasia_grade2, levels = c("no aphasia", "minor", "moderate", "severe", "global", NA)))
   return(dfx)
}

#' @title extractNeglectFromLetters - Retrieves information about Neglect / Extinction phenomena being present as symptom
#' @description Information of Neglect is taken from differen sources: 1) letters stroke-unit 2) icu 3) normal ward 
#' @param var - specifies the desired letter information to extract e.g. "stwbr" for stroke-unit, "int" for icu, "stabr" for normal ward
#' @param case_id
#'
#' @return A tibble with case_id and two variables 1) aphasia grade (limited patients) 2) aphasia present (yes/no)
extractNeglectFromLetters <- function(var = c("aufnb","stwbr","int","stabr"), df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr) {
        
        neglect_yes_str <- "(?i)(schwere?n?r?|multimodale?r?n?|ausgeprägte?n?r?|visuelle?r?n?|taktile?r?n?|sensible?r?n?|mäßigen?|moderate?n?r?|deutliche?n?r?|leichte?n?r?|geringe?n?r?|minimale?n?r?|eingeschränkte?n?|reduzierte?[n|r]?verminderte?|fehlende?)?\\s?(Ne[gk]le[c|k]t|Anosognos)"
        neglect_no_str <- "(?i)([K|k]eine?n?)\\s?(Ne[gk]le[c|k]t|Vernachlässigung|Anosognos)"
        neglect_grade_str <- "(?i)(schwer|multimodal|ausgeprägt|visuell|taktil|sensib|mäßig|moderate?|deutlich|leicht|gering|minimal|vermindert|fehlend)"
        
        if (var %in% c("stwbr", "int", "stabr")) {
                df <-
                        eval(as.name(paste0("df_neur_", var))) %>%
                        mutate(
                                neglect_diag =
                                        str_extract(
                                                eval(as.name(paste0(
                                                        "adm_diag_", var
                                                ))),
                                                neglect_yes_str
                                        )
                        ) %>%
                        mutate(
                                neglect_epi = str_extract(
                                        eval(as.name(paste0(
                                                "adm_epi_", var
                                        ))),
                                        neglect_yes_str
                                )
                        ) %>%
                        mutate(
                                neglect_epi_no = str_extract(
                                        eval(as.name(paste0(
                                                "adm_epi_", var
                                        ))),
                                        neglect_no_str
                                )
                        ) %>%
                        mutate(
                                neglect_exam_no = str_extract(
                                        eval(as.name(paste0(
                                                "adm_nexam_", var
                                        ))),
                                        neglect_no_str
                                )
                        ) %>%
                        mutate(
                                neglect_exam = str_extract(
                                        eval(as.name(paste0(
                                                "adm_nexam_", var
                                        ))),
                                        neglect_yes_str
                                )
                        )
        } else if (var %in% c("aufnb")) {
                df <-
                        eval(as.name(paste0("df_neur_", var))) %>%
                        mutate(neglect_exam_no = str_extract(eval(as.name(
                                paste0("adm_nexam_", var)
                        )),
                        neglect_no_str)) %>%
                        mutate(neglect_exam = str_extract(eval(as.name(
                                paste0("adm_nexam_", var)
                        )),
                        neglect_yes_str)
                        )
        } else return("Error: Please supply on of the following 'aufnb', stwbr','int', 'stabr'")
        
        var_name <- paste0("var_neglect_", var)
        
        if (var %in% c("stwbr", "int", "stabr")) {
                df <- df %>% 
                mutate(
                        var_neglect = str_to_lower(
                                case_when(
                                        !is.na(neglect_diag) ~ 
                                                coalesce(
                                                        str_extract(neglect_diag, neglect_grade_str),
                                                        str_extract(neglect_epi, neglect_grade_str),
                                                        str_extract(neglect_exam, neglect_grade_str),
                                                        paste0(str_extract(neglect_diag, ".*"))
                                                ),
                                        is.na(neglect_diag) & !is.na(neglect_exam) & !is.na(neglect_exam_no) ~ "no neglect",
                                        is.na(neglect_diag) & !is.na(neglect_epi) & !is.na(neglect_epi_no) ~ "no neglect",
                                        is.na(neglect_diag) & is.na(neglect_exam) & is.na(neglect_epi) & !is.na(eval(as.name(paste0("adm_nexam_", var)))) ~ "no neglect",
                                        is.na(neglect_diag) &
                                                !is.na(neglect_exam) & !is.na(neglect_epi) ~ 
                                                coalesce(
                                                        str_extract(neglect_diag, neglect_grade_str),
                                                        str_extract(neglect_epi, neglect_grade_str),
                                                        str_extract(neglect_exam, neglect_grade_str),
                                                        paste0(str_extract(neglect_epi, ".*"))
                                                ),
                                        is.na(neglect_diag) & !is.na(neglect_exam) & is.na(neglect_exam_no) ~ 
                                                coalesce(
                                                        str_extract(neglect_exam, neglect_grade_str),
                                                        paste0(str_extract(neglect_exam, ".*"))
                                                ),
                                        is.na(neglect_diag) & !is.na(neglect_epi) & is.na(neglect_epi_no) ~ 
                                                coalesce(
                                                        str_extract(neglect_epi, neglect_grade_str),
                                                        paste0(str_extract(neglect_epi, ".*"))
                                                ),
                                ))
                ) 
        } else if (var %in% c("aufnb")) {
                df <- df %>% 
                        mutate(
                                var_neglect = str_to_lower(
                                        case_when(
                                                is.na(neglect_exam)  & !is.na(neglect_exam_no) ~ "no neglect",
                                                is.na(neglect_exam)  & is.na(neglect_exam_no) ~ "no neglect",
                                                !is.na(neglect_exam) & !is.na(neglect_exam_no) ~ "no neglect",
                                                !is.na(neglect_exam) & is.na(neglect_exam_no) ~ str_extract(neglect_exam, neglect_grade_str)
                                                        ),
                                        )
                        )
        }
        df <- df %>%
                mutate(var_neglect = str_replace_all(var_neglect, "[\\.,\\(\\):]", "")) %>% 
                mutate(var_neglect = str_replace(var_neglect, "^\\s", "")) %>% 
                mutate(var_neglect = recode(var_neglect, "schwer" = "multimodal", "leicht" = "minor (not specified)", "neglekt" = "neglect (not specified)", "neglect" = "neglect (not specified)", "visuell" = "visual", "ausgeprägt" = "multimodal", "taktil" = "sensible/tactile", "deutlich" = "multimodal", "sensib" = "sensible/tactile", "anosog" = "anosognosia")) %>% 
                # mutate(var_aphasia = ordered(var_aphasia, levels = c("no aphasia", "minor", "moderate", "severe", "global", NA))) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>%
                mutate(!!var_name := var_neglect) %>% 
                select(case_id, !!var_name)
        return(df)
}      

#' @title getNeglect - Retrieves information about Neglect/Extinction being present as symptom
#' @description Information of Aphasia is taken from different sources of documentation: 1) ) Diagnosis codes 2) stroke-unit discharge letter 3) admission documents stroke-unit 4) icu 5) normal ward
#' @param dia - Diagnosis codes list
#' @param df_neur_stwbr - data frame including information on discharge letters of stroke unit
#' @param df_neur_int - discharge letter intensive care unit
#' @param df_neur_aufnb - admission documents stroke-unit
#' @param df_neur_stabr - normal ward
#' @param case_id - desired case id list
#'
#' @return A tibble with case_id and variable 1) neglect graded when possible
getNeglect <- function(case_id = case_id, dia = dia, df_neur_aufnb = df_neur_aufnb, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr) {
        
        ################ - ###
        # source dia
        df1 <- extractFromDia(var = "Neglect", var_short = "neglect", prefix = "var", dia)
        
        ################ - ###
        # source discharge letters
        df2a <- extractNeglectFromLetters("aufnb", df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
        df2b <- extractNeglectFromLetters("stwbr", df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
        df2c <- extractNeglectFromLetters("int", df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
        df2d <- extractNeglectFromLetters("stabr", df_neur_aufnb, df_neur_stwbr, df_neur_int, df_neur_stabr)
        
        
        # JOINING
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        dfx <- left_join(
                        left_join(
                                left_join(
                                        left_join(
                                                left_join(
                                                        case_id, 
                                                        df1, 
                                                        by = "case_id"),
                                                df2a, 
                                                by = "case_id"),
                                        df2b, 
                                        by = "case_id"),
                                df2c, 
                                by = "case_id"),
                        df2d, 
                        by = "case_id") %>% 
                mutate(across(everything(), as.character))
                
        # CAVE :::: FOR CASE_WHEN NECESSARY THAT ALL ARE SAME TYP !!! ~4h
        # CONDENSING
        
        dfx <- dfx %>% mutate(var_neglect_grade = case_when(
                str_detect(var_neglect_stwbr, "multimodal") ~ "multimodal",
                str_detect(var_neglect_aufnb, "multimodal") ~ "multimodal",
                str_detect(var_neglect_stabr, "multimodal") ~ "multimodal",
                str_detect(var_neglect_int, "multimodal") ~ "multimodal",
                str_detect(var_neglect_stwbr, "sensible/tactile") ~ "sensible/tactile",
                str_detect(var_neglect_aufnb, "sensible/tactile") ~ "sensible/tactile",
                str_detect(var_neglect_stabr, "sensible/tactile") ~ "sensible/tactile",
                str_detect(var_neglect_int, "sensible/tactile") ~ "sensible/tactile",
                str_detect(var_neglect_stwbr, "visual") ~ "visual",
                str_detect(var_neglect_aufnb, "visual") ~ "visual",
                str_detect(var_neglect_stabr, "visual") ~ "visual",
                str_detect(var_neglect_int, "visual") ~ "visual",
                str_detect(var_neglect_stwbr, "minor\\s\\(not\\sspecified\\)") ~ "minor/unspecified",
                str_detect(var_neglect_aufnb, "minor\\s\\(not\\sspecified\\)") ~ "minor/unspecified",
                str_detect(var_neglect_stabr, "minor\\s\\(not\\sspecified\\)") ~ "minor/unspecified",
                str_detect(var_neglect_int, "minor\\s\\(not\\sspecified\\)") ~ "minor/unspecified",
                str_detect(var_neglect_stwbr, "neglect\\s\\(not\\sspecified\\)") ~ "minor/unspecified",
                str_detect(var_neglect_aufnb, "neglect\\s\\(not\\sspecified\\)") ~ "minor/unspecified",
                str_detect(var_neglect_stabr, "neglect\\s\\(not\\sspecified\\)")  ~ "minor/unspecified",
                str_detect(var_neglect_int, "neglect\\s\\(not\\sspecified\\)")  ~ "minor/unspecified",
                str_detect(var_neglect_stwbr, "no\\sneglect") & is.na(var_neglect) ~ "no neglect",
                str_detect(var_neglect_aufnb, "no\\sneglect") & is.na(var_neglect) ~ "no neglect",
                str_detect(var_neglect_stabr, "no\\sneglect") & is.na(var_neglect) ~ "no neglect",
                str_detect(var_neglect_int, "no\\sneglect") & is.na(var_neglect) ~ "no neglect",
                TRUE ~ "NA"
                )
        ) %>% 
                select(case_id, var_neglect_grade) %>% 
                mutate(var_neglect_grade = ordered(var_neglect_grade, levels = c("no neglect", "minor/unspecified", "visual", "sensible/tactile", "multimodal"))) %>% 
                mutate(case_id = parse_number(as.character(case_id)))
        return(dfx)
}


#' @title getNechoNewVar - Retrieves information about Echocardiography from respective reports
#' @description New variables are extracted from echocardiography exam reports
#' @param df_necho_c - data frame that combines several echocardiography sources
#' @import tidyverse
#' @return A tibble with case_id and new variables 1) left ventricular function grades as a) normal b) mild dysfunction c) moderate dysfunction d) severe dysfunction e) hyperdynam and 2.) presence of tako-tsubo 
getNechoNewVar <- function(df_necho_c=df_necho_c) {
        
        # Left Ventricular function (Pumpfunktion)
        necho_lvfkt_str = "(?i)(\\b.{3,50}){2}(Pumpfunktion|Funktion)"
        necho_lvfkt_grade_str = "(?i)(leicht-(\\s?bis\\s?)?mittelgradig|leicht(gradig)?|mäßig|mittelgradig|mittel-(\\s?bis\\s?)?hoch(gradig)?|hochgradig|höchstgradig|hyperdynam)?\\s(reduziert|ei.?n?.?g.?eschr?)|global (gut|ein.?g.?schr?än.?k?.?t?|hyperdynam|mittel|mäßig)|noch gut|gut|(mäßig|mittel)gradig|normale?|reduziert|ein.?g.?schr?än.?k?.?t?|ohne (höhergradige)? Einschränkung|erhaltene longitudinal|hyperdynam"
        df <- df_necho_c %>%
                mutate(lvfkt = str_extract(txt, necho_lvfkt_str)) %>%
                mutate(lvfkt_grade = str_extract(string = lvfkt, pattern = necho_lvfkt_grade_str)) %>%
                mutate(
                        lvfkt_grade2 = case_when(
                                str_detect(lvfkt_grade, "(?i)normal|ohne|gut") ~ "normal function",
                                str_detect(lvfkt_grade, "(?i)h[o|ö]chs?t?gradig") ~ "severe dysfunction",
                                str_detect(lvfkt_grade, "(?i)mittel|mäßig") ~ "moderate dysfunction",
                                str_detect(lvfkt_grade, "(?i)leicht|ge?schrä?n?k?t?|longitudinal|reduziert") ~ "mild dysfunction",
                                str_detect(lvfkt_grade, "(?i)hyperdynam") ~ "hyperdynam"
                        )
                ) %>% 
                mutate(necho_lvfkt_grade = ordered(lvfkt_grade2, levels = c("normal function", "mild dysfunction", "moderate dysfunction", "severe dysfunction", "hyperdynam")))
        
        # Tako Tsubo
        necho_tako_tsubo ="(?i)tako.?tsubo|tonnenförmig"
        df <- df %>%
                mutate(tako = case_when(
                        str_detect(string=txt, pattern=necho_tako_tsubo) ~ 1,
                        TRUE ~ 0
                       )) %>% 
                rename(case_id = fallnummer)
        
        # - ############### - #
        # Endocarditis not reasonable, because mostly not specified in txt
        # necho_endocarditis = "(?i)(\\b.{3,50}){2}Endo[c|k]arditis(.{3,50}){2}"
        # df <- df_necho %>%
        #         mutate(endo = str_extract(txt, necho_endocarditis))
        cols_to_convert <- c("necho_aortic_root", "necho_left_atrium", "necho_septum", "necho_hinterwand", "necho_lv_enddia", "necho_vci")
        
        # use mutate_at to apply as.numeric across multiple columns
        df <- df %>%
                mutate_at(vars(cols_to_convert), as.numeric)
        return(df)
}


#' @title getPrämorbidRankin - Retrieves information about prämorbid disability measured by Rankin scale
#' @description Information of pRS is taken from different sources of documentation: 1) ) QS dokuments 2) df_hei
#' @param df_hei 
#' @param qs 
#'
#' @return A tibble with LfdNr, case_id and combined variable pRS
getPraemorbidRankin <- function(df_hei, qs){
        #source df_hei
        df1 <- df_hei %>% select(case_id, pRS) %>% 
                mutate(pRS = as.character(pRS)) %>% 
                mutate(case_id = parse_number(as.character(case_id)))
        
        # source qs
        cols <- qs %>% select(contains("RANKINVOR")) %>% colnames()
        df2 <- qs %>% select(FALNR, !!!cols) %>% 
                mutate(pRS = RANKINVOR) %>% 
                select(case_id = FALNR, pRS) %>% 
                mutate(case_id = parse_number(as.character(case_id))) %>%
                group_by(case_id) %>% 
                arrange(desc((pRS))) %>%
                slice(1)
        
        # JOIN
        dfx <- left_join(df1, df2, by = "case_id") %>% 
                mutate(pRS = coalesce(pRS.x, pRS.y)) %>% 
                select(case_id, pRS) %>% 
                unique() %>% 
                mutate(pRS = ordered(pRS, levels = c("0","1","2","3","4","5"))) %>% 
                drop_na(case_id)
        return(dfx)
}

#' @title getNihssDischarge - Retrieves information about NIHSS Scores at discharge
#' @description Information of NIHSS is taken from different sources : 1.) letter (as validation) 2.) Heireka DB
#' @param df_hei 
#' @param df_neur_stwbr
#' @param df_neur_int
#' @param df_neur_stabr
#'
#' @return A tibble with LfdNr, case_id and combined variable pRS
getNihssDischarge <- function(case_id = case_id, df_neur_stwbr = df_neur_stwbr, df_neur_int = df_neur_int, df_neur_stabr = df_neur_stabr, df_hei = df_hei){
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        nihss_dis_str <- "(?i)NIH\\s?S?S?S?.?(\\sbei\\sEntlassung\\s?)?.?\\s?\\d\\d?"
        
        df1 <- df_neur_stwbr %>% 
                mutate(outcome_nihss_discharge_stwbr = str_extract(adm_epi_stwbr, nihss_dis_str)) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                select(case_id, outcome_nihss_discharge_stwbr)
        df2 <- df_neur_int %>% 
                mutate(outcome_nihss_discharge_int = str_extract(adm_epi_int, nihss_dis_str)) %>% 
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                select(case_id, outcome_nihss_discharge_int)
        df3 <- df_neur_stabr %>% 
                mutate(outcome_nihss_discharge_stabr = str_extract(adm_epi_stabr, nihss_dis_str)) %>%
                mutate(case_id = parse_number(as.character(fallnummer))) %>% 
                select(case_id, outcome_nihss_discharge_stabr)
        
        df4 <- df_hei %>% select(case_id, outcome_nihss_discharge) %>%
                mutate(case_id = parse_number(as.character(case_id))) %>% 
                mutate(outcome_nihss_discharge = as.character(outcome_nihss_discharge))
        
        df <- left_join(
                        left_join(
                                left_join(
                                        left_join(case_id, df1, by = "case_id"),
                                        df2,
                                        by = "case_id"),
                                df3,
                                by = "case_id"),
                        df4,
                        by = "case_id")
        
        dfx <- df %>% 
                mutate(outcome_nihss_discharge_j = parse_number(
                        coalesce(outcome_nihss_discharge, outcome_nihss_discharge_stabr, outcome_nihss_discharge_stwbr, outcome_nihss_discharge_int))) %>% 
                select(case_id, outcome_nihss_discharge = outcome_nihss_discharge_j) %>% unique()
                        
        return(dfx)
}

#' @title prepLab - Helper Function for 'prepLabs'-Function 
#' @description This function takes a dataframe of lab test results and pivots it into a wide format with columns for each lab test and rows for each patient. The values for each lab test can be specified as either the median, minimum, maximum, 25th percentile, or 75th percentile.
#' @param lab a dataframe of lab test results with columns for Fall (patient ID), Text (name of lab test), and Value (numeric value of lab test)
#' @param values a character vector specifying which lab test values to use for pivoting, with options for "median", "min", "max", "q25", and "q75"
#' @return a pivoted dataframe with columns for each lab test and rows for each patient
#'
#' @examples
#' data(lab_results)
#'
#' # Prepare lab data with median values
#' prepLab(lab_results, values = "median")
#'
#' # Prepare lab data with median and minimum values
#' prepLab(lab_results, values = c("median", "min"))
prepLab <- function(lab, values = c("median","min","max","q25","q75")){
        labs_w <- lab %>% pivot_wider(id_cols = Fall,
                                      names_from = Text,
                                      values_from = values)
        
}


#' @title prepLabs - Prepare lab data for analysis
#' @description 
#' @param lab_data 
#' @param lab_type 
#' @return a pivoted dataframe with columns for each lab test and rows for each patient
#'
#' @exam
prepLabs <- function(lab_data, lab_type){
        # prep labs
        labs <- prepLab(lab_data, values = "Wert")
        
        # individual transforming steps | transforming | combining | cols can change here 
        labs <- switch(lab_type,
                       "lipid" = {
                               labs %>%
                                       mutate(`LDL-Cholesterin` = coalesce(`LDL-Cholesterin`, `LDL-Chol.(berechn.)`), .keep = "unused") 
                       },
                       "HbA1c" = {
                               labs %>%
                                       mutate(hba1c_ifcc = coalesce(`HBA1IFCC`, `HbA1c (IFCC)`), .keep = "unused")
                       },
                       "glucose" = labs %>% select("Fall", `Glucose nue.`),
                       "cardio" = { labs %>% select(-TNT, -`NT-BNP`)},
                       "haem" = labs,
                       "nephr" = { 
                               labs %>% rowwise() %>%
                                       mutate(lab_gfr_epi = coalesce(c_across(5), c_across(7))) %>%
                                       ungroup() %>%
                                       mutate(lab_gfr_c = .[[6]]) %>% 
                                       select(Fall, Harnstoff, Kreatinin, Harnsaeure, lab_gfr_epi, lab_gfr_c)
                               },
                       "thyr" = {
                               labs %>% 
                                       mutate(`Trijodthyronin (fT3)` = coalesce(`freies Trijodthyronin (fT3)`, `freies T3`, `FT3`), .keep = "unused") %>%
                                       mutate(`Tetrajodthyronin (fT4)` = coalesce(`freies T4`, `FT4`), .keep = "unused")
                               },
                       "infl" = {
                               labs %>% mutate(Procalcitonin = coalesce(`Procalcitonin-ST`, `PCTST`, `PCT sens.`, `Procalcitonin sens.`), .keep = "unused") 
                       },
                       "liver" = labs,
                       "coag" = {
                               labs %>% 
                                       mutate(`International Normalized Ratio (INR)` = coalesce(`INR`, `INR - ber.`), .keep = "unused")
                               }
        )
        
        # "adm" = {
        #         labs %>% 
        #                 mutate(`International Normalized Ratio (INR)` = coalesce(`INR`, `INR - ber.`), .keep = "unused") %>% 
        #                 mutate(lab_gfr_epi = coalesce(`GFR n. CKD-EPI`, `GFR, CKD-EPI`), .keep = "unused") %>% 
        #                 mutate(Procalcitonin = coalesce(`Procalcitonin-ST`, `PCTST`, `PCT sens.`, `Procalcitonin sens.`), .keep = "unused") %>% 
        #                 mutate(`Trijodthyronin (fT3)` = coalesce(`freies Trijodthyronin (fT3)`, `freies T3`, `FT3`), .keep = "unused") %>% 
        #                 mutate(`Tetrajodthyronin (fT4)` = coalesce(`freies T4`, `FT4`), .keep = "unused")
        # }
        
        labs <- labs %>%
                select(Fall, sort(setdiff(names(.), "Fall"))) # getting columns in alphabetical order 
        
        # Get Labels for DataDic    
        df <- labs %>% names() %>% as_tibble() %>% rename(label = value) %>% filter(label!="Fall")
        
        # 2. individual transforming step to establish variable names 
        labs <- switch(lab_type,
                       "lipid" = {
                               labs %>%
                                       rename(lab_chol = Cholesterin,
                                              lab_trig = Triglyceride,
                                              lab_hdl = `HDL-Cholesterin`,
                                              lab_ldl = `LDL-Cholesterin`,
                                              lab_vldl = `VLDL-Cholesterin`)
                               
                       },
                       "HbA1c" = {
                               labs %>%
                                       mutate(lab_hba1c = HbA1c, .keep = "unused")
                                       
                       },
                       "glucose" = {
                               labs %>% rename(lab_glu = `Glucose nue.`)
                       },
                       "cardio" = {
                               labs %>% mutate(lab_ckmb = `CK-MB`, .keep = "unused") %>% 
                               mutate(lab_hstnt = `hs-TroponinT`, .keep = "unused") %>% 
                               mutate(lab_ntprobnp = `NT-ProBNP`, .keep = "unused") %>% 
                                       select(Fall, lab_ckmb, lab_hstnt, lab_ntprobnp)},
                       "haem" = {
                               labs %>% rename(
                                       lab_hkt = Haematokrit,
                                       lab_hb = Haemoglobin,
                                       lab_mch = MCH,
                                       lab_mcv = MCV,
                                       lab_mchc = MCHC)
                       },
                       "nephr" = {
                               labs %>% 
                                       rename(lab_urea = Harnstoff,
                                              lab_crea = Kreatinin,
                                              lab_uricacid = Harnsaeure)
                       },
                       "thyr" = {
                               labs %>% rename(lab_tsh = TSH,
                                               lab_fT3 = `Trijodthyronin (fT3)`, 
                                               lab_fT4 = `Tetrajodthyronin (fT4)`)
                       },
                       "infl" = { 
                               labs %>% 
                                       rename(lab_pct = Procalcitonin,
                                              lab_fib = `Fibrinogen`,
                                              lab_leu = `Leukozyten`,
                                              lab_il6 = `IL-6`)
                       },
                       "liver" = {
                               labs %>% 
                                       rename(lab_alb = `Albumin quant.`,
                                              lab_ggt = `GGT`,
                                              lab_ast = `GOT/AST`,
                                              lab_alt = `GPT/ALT`,
                                              lab_ldh = `LDH`,
                                              lab_che = `CHE`,
                                              lab_bil = `Ges.Bilirubin`,
                                              lab_bil_dir = `Dir.Bilirubin`,
                                              lab_amm = `Ammoniak`)
                               },
                       "coag" = {
                               labs %>% 
                                       rename(lab_inr = `International Normalized Ratio (INR)`,
                                              lab_aptt = `aPTT`,
                                              lab_tt = `Thrombinzeit`
                                              )
                       }
        )
        
        # Get Variable Names for DataDic     
        df <- df %>% 
                mutate(name = labs %>% ungroup() %>% names() %>% setdiff("Fall")) %>% 
                select(name, label) %>% 
                mutate(units = sapply(label, function(x) {lab_data %>% filter(x %in% Text) %>% pull(Einheit) %>% unique() %>% first()})) %>% 
                mutate(description = NA_character_) %>% 
                mutate(footnote = NA_character_)
        
        # Read-In DataDic
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        
        # Integrate new data into data dictionary 
        dfx <- full_join(data_dic, df, by = "name") %>%
                mutate(
                        label = coalesce(label.x, label.y),
                        units = coalesce(units.x, units.y),
                        description = coalesce(description.x, description.y),
                        footnote = coalesce(footnote.x, footnote.y)
                ) %>%
                select(name, label, units, description, footnote)
        write.csv2(dfx, "data_dic.csv")
        return(labs)
}


# df to be update
# data_dic supply data_dic
# rows ... if rows of a specific column instead of cols should be updated 
# if rows=TRUE ... update_rows_of_col ... specify the cols name (default = "name") 
updateLabels <- function(df, data_dic, rows=FALSE, update_rows_of_col="name", updateInteraction=TRUE) {
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        is_matrix <- is.matrix(df)
        
        
        if (rows==TRUE) {
                
                for (i in seq_len(nrow(data_dic))) {
                        df[[update_rows_of_col]] <- str_replace_all(df[[update_rows_of_col]], pattern = paste0("\\b", data_dic$name[i], "\\b"), data_dic$label[i])
                }
                
                if (updateInteraction){ df[[update_rows_of_col]] <- sapply(df[[update_rows_of_col]], wrap_interaction_format) }
                # df <- df %>% mutate(!!update_rows_of_col:= sapply(.[[update_rows_of_col]], wrap_interaction_format)) 
                
                
                # Function to convert text to desired format for Labels
                convert_text_format <- function(text) {
                        # Convert to lowercase, replace underscores, and capitalize first letter of each word
                        str_replace_all(str_to_title(str_replace_all(tolower(text), "_", " ")), " Ii", " II")
                }
                
                # Replacing the 'NA' values in 'label' with the transformed values from `update_rows_of_col`
                
                indices_to_replace <- which(is.na(df[[update_rows_of_col]]))
                if (length(indices_to_replace)!=0) {
                        df <- left_join(df, data_dic, by = setNames("name", eval(as.character(update_rows_of_col))))
                        df$label[indices_to_replace] <- sapply(df[indices_to_replace, update_rows_of_col], convert_text_format)
                }
                
                
        } else {
                for (i in seq_len(nrow(data_dic))) {
                        if (data_dic$name[i] %in% names(df)) {
                                attr(df[[data_dic$name[i]]], "label") <- data_dic$label[i]
                                attr(df[[data_dic$name[i]]], "units") <-
                                        data_dic$units[i]
                                attr(df[[data_dic$name[i]]], "footnote") <-
                                        data_dic$footnote[i]
                        }
                }
        }
        if (is_matrix) {
                colnames(df) <- ifelse(colnames(df) %in% data_dic$name, data_dic$label[match(colnames(df), data_dic$name)], colnames(df))
                rownames(df) <- ifelse(rownames(df) %in% data_dic$name, data_dic$label[match(rownames(df), data_dic$name)], rownames(df))
        }
        return(df)
}




getAttr <- function(df, attr=c("label", "units","footnote")) {
        attributes <- sapply(names(df), function(x) attr(df[[x]], attr))
        return(attributes)
}


joinPacks <- function(dfs){
        # JOIN the pack
        dfs <- reduce(dfs, left_join, by = 'case_id')
        # Read-In DataDic
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        # ADD Labels
        dfs <- updateLabels(dfs, data_dic)
        return(dfs)
}


prepDf <- function (lab_lipid, lab_HbA1c, lab_glu, lab_cardio, lab_haem, lab_nephr, lab_thyr, lab_infl, lab_liver, lab_coag, lab_adm,
                    df_base, df_necho, df_dop, pRS, var_handedness, var_vigil_adm, var_aphasia, var_aphasia_adm, var_neglect, var_eyedev_adm, var_prev_stroke, var_prev_mi,
                    var_prev_thyr, var_prev_alc, var_microangio, risk_factor_alc_adm, var_prev_dementia, dx_prev_copd, var_stroke_care, dx_rf_smoke, etio_pfo,
                    etio_endocarditis, etio_cs, outcome_delir, outcome_mi, outcome_nihss_discharge, outcome_lengthHospStay,
                    outcome_ventilationDays, outcome_pneumonia, outcome_uti, outcome_sepsis, df_barthel_index,
                    outcome_palliativeCare,outcome_tvt, outcome_lae,
                    df_meds_prior,
                    print=FALSE,
                    caseid=FALSE)
{
        # List of lab packs
        lab_packs <- list("lipid", "HbA1c", "glucose", "cardio", "haem", "nephr", "thyr", "infl", "liver", "coag")
        # Corresponding list of lab data frames
        lab_dfs <- lst(lab_lipid, lab_HbA1c, lab_glu, lab_cardio, lab_haem, lab_nephr, lab_thyr, lab_infl, lab_liver, lab_coag)
        # Apply prepLabs function to each data frame with its corresponding name
        labs <- map2(lab_dfs, lab_packs, ~prepLabs(.x,.y))
        
        # Admission labs (all)
        lab_adm <- lab_adm %>% 
                prepLab(values = "Wert")
        lab_adm <- lab_adm %>% 
                select(-Fall) %>%
                janitor::clean_names() %>%
                rename_with(~ paste0("lab_d0_", .x)) %>%
                bind_cols(Fall = lab_adm$Fall)
                
        
        # Change to case_id
        # labs <- map(labs, ~rename(.x, case_id = Fall))
        
        # WHAT DATAFRAMES TO BE JOINED
        # df_pack <- df_base[1] %>%
        #         append(df_base["risk_factor"]) %>%
        #         append(df_base["outcome"]) %>%
        #         append(df_base["logistics"]) %>%
        #         append(df_base["imaging"]) %>%
        #         append(df_base["date_time_old"]) %>% # select(case_id, time_window) %>% list())
        #         append(df_base["therapy"]) %>% # select(case_id, sedation_type) %>% list()) %>%
        #         append(df_base["etiology"]) %>%

        # base - incorparate pRS
        df_base$base <- list(df_base$base %>% drop_na(case_id), pRS = pRS) %>%
                reduce(., left_join, by = c("case_id")) 
        
        # clinic - include new
        clinic <- list(var_handedness = var_handedness %>% drop_na(case_id), 
                       var_aphasia = var_aphasia %>% drop_na(case_id),
                       var_aphasia_adm = var_aphasia_adm %>% drop_na(case_id), 
                       var_neglect = var_neglect %>% drop_na(case_id), 
                       var_vigil_adm = var_vigil_adm %>% drop_na(case_id),
                       var_eyedev_adm = var_eyedev_adm %>% drop_na(case_id)) %>% 
                reduce(., left_join, by = c("case_id"))
        
        # risk factors - include and update
        df_base$risk_factor <- list(rf_HEI = df_base$risk_factor %>% drop_na(case_id), 
                                    var_prev_stroke = var_prev_stroke %>% drop_na(case_id),
                                    var_prev_thyr = var_prev_thyr %>% drop_na(case_id), 
                                    var_prev_alc = var_prev_alc %>% drop_na(case_id), 
                                    var_prev_dementia = var_prev_dementia %>% drop_na(case_id), 
                                    dx_prev_copd = dx_prev_copd %>% drop_na(case_id),
                                    dx_rf_smoke = dx_rf_smoke %>% drop_na(case_id), 
                                    var_prev_mi = var_prev_mi %>% drop_na(case_id),
                                    var_microangio = var_microangio %>% drop_na(case_id),
                risk_factor_alc_adm = risk_factor_alc_adm %>% drop_na(case_id)) %>% 
                reduce(., left_join, by = c("case_id")) %>% 
                # update some columns with new data 1. prev stroke, 2. smoke
                mutate(risk_factor_prev_stroke = coalesce(risk_factor_prev_stroke, dx_prev_stroke)) %>% 
                select(-dx_prev_stroke) 
                
        
        # etio
        df_base$etiology <- list(etio_HEI = df_base$etiology %>% drop_na(case_id), 
                                 etio_pfo = etio_pfo %>% drop_na(case_id),
                                 etio_endocarditis = etio_endocarditis %>% drop_na(case_id), 
                                 etio_cs = etio_cs %>% drop_na(case_id)) %>% 
                reduce(., left_join, by = c("case_id"))
        
        # therapy
        df_base$therapy <- df_base$therapy %>% mutate(sedation_type = case_when(is.na(sedation_type)|sedation_type==""~"no sedation", TRUE~sedation_type))
        df_base$therapy <- list(df_base$therapy %>% select(case_id, sedation_type) %>% drop_na(case_id))
        
        #outcome
        df_base$outcome$outcome_nihss_discharge <- NULL 
        df_base$outcome$discharge_towhere <- NULL
        
        df_base$outcome <- list(outcome_HEI = df_base$outcome %>% drop_na(case_id),
                                outcome_delir = outcome_delir %>% drop_na(case_id), 
                                outcome_nihss_discharge = outcome_nihss_discharge %>% drop_na(case_id), 
                                df_barthel_index = df_barthel_index %>% drop_na(case_id),
                                outcome_lengthHospStay = outcome_lengthHospStay %>% drop_na(case_id),
                                outcome_ventilationDays = outcome_ventilationDays %>% drop_na(case_id), 
                                outcome_mi = outcome_mi %>% drop_na(case_id), 
                                outcome_pneumonia = outcome_pneumonia %>% drop_na(case_id), 
                                outcome_uti = outcome_uti %>% drop_na(case_id), 
                                outcome_tvt = outcome_tvt %>% drop_na(case_id), 
                                outcome_lae = outcome_lae %>% drop_na(case_id), 
                                outcome_sepsis = outcome_sepsis %>% drop_na(case_id), 
                                outcome_palliativeCare = outcome_palliativeCare %>% drop_na(case_id),
                                outcome_stroke_care = var_stroke_care %>% drop_na(case_id))
        
        df_pack <- df_base %>% 
                append(list(df_necho = df_necho, 
                            df_dop = df_dop,  
                            clinic = clinic,
                            meds_prior_SUBSTANCE = df_meds_prior[["SUBSTANCE"]],
                            meds_prior_GROUP = df_meds_prior[["GROUP"]],
                            meds_prior_CLASS = df_meds_prior[["CLASS"]],
                            lab_packs = labs,
                            lab_adm = lab_adm))
            
        # small function to rename recursively in a mixed list
        rename_distinct_no_na <- function(x) {
                if (is_tibble(x)) {
                        # Rename the column, remove NA in case_id, and filter for distinct case_id
                        x <- x %>%
                                rename_with(~ if_else(. == "Fall", "case_id", .)) %>%  # Rename the column
                                filter(!is.na(case_id)) %>%  # Remove rows where case_id is NA
                                distinct(case_id, .keep_all = TRUE)  # Ensure distinct case_id
                } else if (is.list(x)) {
                        # Apply the function recursively to each list element
                        x <- map(x, rename_distinct_no_na)
                }
                x
        }
        df_pack <- rename_distinct_no_na(df_pack)
         
         return(df_pack)

# #### Transfer to other function
        # check unique
        # df_pack <- map(df_pack, ~ distinct(.x, case_id, .keep_all = TRUE))
        # drop NA in case_id's
        # df_pack <- map(df_pack, ~ drop_na(.x, case_id))
        
        # Read-In DataDic
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        # ADD Labels
        # Hmisc upData did not work - df_label <- map(df, ~upData(., labels = setNames(data_dic$label, data_dic$name)))
        # therefore custom function
        df <- updateLabels(df, data_dic)
        saveRDS(df, 'df.rds', compress='xz')
# #### Transfer to other function
        
        
}


#' @title filterDf - # The main selection steps for this study 
#' @description ...
#' @param df - data frame to be filtered 
#' @import tidyverse
#' @import Hmisc
#' @import rms
#' @return A tibble with 
filterDf <- function(df){
        df <- df %>% filter(therapy!="IVT24" & side_stroke!="spinal" & side_stroke!="retinal")
        df <- df %>% mutate(therapy = recode(therapy, "IVT0.6" = "IVT")) %>% droplevels()
        # remove when nearly only NA and constant in whole column (= all zero)
        df <- df %>%
                select_if(~sum(!is.na(.)) >= 2 && length(unique(.[!is.na(.)])) > 1)
        library(rms)
        dd <- datadist(df)
        options(datadist = 'dd')
        options(contrasts=c("contr.treatment", "contr.treatment"))
        return(df)
}

# prepare for analysis of relevant factor at admission
# prepDfAdm <- function(df_pack) {
#         df_adm <- 
#                 
#                 
#         df_adm <- updateLabels(df_adm, data_dic)
#         saveRDS(df_adm, 'df_adm.rds', compress='xz')
#                 
# }


#' @title clearForAnalysis
#' @description This function recodes factor variables in a data.frame as numbers starting from 0, creates a data dictionary that maps each level of each factor variable to its corresponding number, and saves the data dictionary as an RDS and CSV file. The function returns the recoded data.frame as output.
#' @param df A data.frame to be analyzed
#' @return A data.frame with factor variables recoded as numbers starting from 0. It also saves a data dictionary as an RDS and CSV file.
#' @examples 
#' clearForAnalysis(df)
clearForAnalysis <- function (df = df) {
        
        # Determine factor variables and their levels
        factor_cols <- sapply(df, is.factor)
        factor_levels <- lapply(df[, factor_cols], unique)
        
        # Recode factor variables as numbers starting from 0
        df[, factor_cols] <- lapply(df[, factor_cols], function(x) {
                ifelse(is.na(x), NA, as.integer(factor(x, levels = unique(x))) - 1)
        })
        
        # Build a data dictionary
        data_dict <- list()
        for (i in names(factor_levels)) {
                data_dict[[i]] <- setNames(as.list(0:(length(factor_levels[[i]]) - 1)), factor_levels[[i]])
        }
        saveRDS(data_dict, "data_dict_categorical.rds")
        # Convert data_dict to a data frame
        data_dict_df <- stack(data_dict)
        data_dict_df$levels <- labels(unlist(data_dict))
        
        # Save data_dict_df as a csv
        write.csv(data_dict_df, "data_dict.csv", row.names = FALSE)
        
        df <- df %>% select(-case_id)
        saveRDS(df, 'heireka_label_clean.rds', compress='xz')
        return(df)
}

#' @title getDataDic - Generate a Data Dictionary for a Data Frame in R
#' @description Generates a data dictionary for a given data frame. The data dictionary includes information about the variable names, labels, units, class, number of levels, levels, missing values, and storage type.
#' @param df A data frame for which a data dictionary is to be generated.
#' @return A tibble containing information about the variables in the input data frame.
#' @examples 
getDataDic <- function(df) {
        require(Hmisc)
        require(purrr)
        var_names <- names(df)
        # var_label <- label(df)
        var_label <- getAttr(df, "label")
        var_classes <- sapply(df, class)
        var_classes_concat <-
                sapply(var_classes, function(x)
                        paste(x, collapse = ", "))
        var_classes_concat <-
                sapply(var_classes_concat, function(x) {
                        paste(gsub("labelled,", "", x), collapse = ",")
                })
        
        # var_units <- contents(df)$contents$Units
        var_units <- getAttr(df, "units")
        var_nlevels <- sapply(df, function(x) {
                if (is.numeric(x) ||
                    is.character(x) || is.logical(x) || is.factor(x)) {
                        length(unique(na.omit(x)))
                } else {
                        ""
                }
        })
        var_levels <- sapply(df, function(x) {
                if (is.factor(x) || is.ordered(x)) {
                        paste(levels(x), collapse = ", ")
                } else {
                        ""
                }
        })
        var_storage <- sapply(df, typeof)
        var_missing <- sapply(df, function(x)
                sum(is.na(x)))
        
        # create data dictionary
        tibble(
                "Variable" = var_names,
                "Label" = var_label,
                "Units" = var_units,
                "Class" = var_classes_concat,
                "N° levels" = var_nlevels,
                "Levels" = var_levels,
                "NA" = var_missing,
                "Type" = var_storage
        )
}


















getFunctionInfo <- function(func) {
        # Get function name
        func_name <- deparse(substitute(func))
        
        # Get function arguments
        input_params <- names(formals(func))
        
        # Extract output type from comments
        func_comments <- capture.output(print(func))
        output_pattern <- "[@|#]return A\\s+(\\w+)"
        output_type <- regmatches(func_comments, regexpr(output_pattern, func_comments, perl = TRUE))
        if (length(output_type) > 0) {
                output_type <- regmatches(output_type[[1]], regexec(output_pattern, output_type[[1]]))[[1]][2]
        } else {
                output_type <- "Unknown"
        }
        
        # Return function information
        return(list(
                func_name = func_name,
                input_params = input_params,
                output_type = output_type
        ))
}

getMermaidCode <- function(func_name, input_params, output_type) {
        # Create input parameter nodes
        input_nodes <- paste0("    ", LETTERS[1:length(input_params)], "[", input_params, "] --> ", func_name, "\n")
        
        # Create function node
        func_node <- paste0("    ", func_name, "[", func_name, "] --> Output\n")
        
        # Create output node
        output_node <- paste0("    Output[", output_type, "]")
        
        # Combine all nodes
        mermaid_code <- paste0("flowchart LR\n", paste(input_nodes, collapse = ""), func_node, output_node)
        
        return(mermaid_code)
        # function working, but could not call inside quarto mermaid code chunk
        # makermaid funtion #qreport does the job
        # still to implement https://hbiostat.org/rflow/doverview.html#fig-doverview-consort
}


# DEPRECATED ----

#' @title extractLab OLD / DEPRECATED
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
extractLab_OLD <- function(labs = labs, case_id = case_id, lab_select, kenn = kenn) {
        # Filtering double value, case_ids and lab_selection
        status <- names(labs[str_detect(names(labs), "Status")])[1]
        case_id <- case_id %>% mutate(case_id = parse_number(case_id))
        labs <- labs %>%
                mutate(Fall = parse_number(Fall)) %>% 
                filter(Fall %in% case_id[[1]]) %>% 
                filter(Text %in% lab_select) %>%
                filter({{status}} != "HI") %>%
                distinct() %>% 
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
                select(Fall, Text, Wert, Einheit, {{lab_norm}}, Normal, timeDiff, labDateTime, hospDateTime) %>%
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


# DEPRECATED - integrated in 'prepLabs'     
prepLipids <- function(lab_lipid){
        # prep labs
        labs_lipid <- prepLab(lab_lipid, values = "Wert") %>% 
                mutate(`LDL-Cholesterin` = coalesce(`LDL-Cholesterin`, `LDL-Chol.(berechn.)`), .keep = "unused") %>% 
                select(sort(setdiff(names(.), "Fall"))) # getting columns in alphabetical order 
        
        # Get Labels for DataDic    
        df <- labs_lipid %>% names() %>% as_tibble() %>% rename(label = value) %>% filter(label!="Fall")
        
        # Process labs        
        labs_lipid <- labs_lipid %>% 
                rename(lab_chol = Cholesterin,
                       lab_trig = Triglyceride,
                       lab_hdl = `HDL-Cholesterin`,
                       lab_ldl = `LDL-Cholesterin`,
                       lab_vldl = `VLDL-Cholesterin`)
        
        # Get Variable Names for DataDic     
        df <- df %>% 
                mutate(name = labs_lipid %>% ungroup() %>% select(-Fall) %>% names()) %>% 
                select(name, label) %>% 
                mutate(units = sapply(label, function(x) {lab_lipid %>% filter(x %in% Text) %>% pull(Einheit) %>% unique() %>% first()})) %>% 
                mutate(description = NA_character_) %>% 
                mutate(footnote = NA_character_)
        
        # Read-In DataDic
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        
        # Integrate new data into data dictionary 
        dfx <- full_join(data_dic, df, by = "name") %>%
                mutate(
                        label = coalesce(label.x, label.y),
                        units = coalesce(units.x, units.y),
                        description = coalesce(description.x, description.y),
                        footnote = coalesce(footnote.x, footnote.y)
                ) %>%
                select(name, label, units, description, footnote)
        write.csv2(dfx, "data_dic.csv")
        return(labs_lipid)
}

# INCOMPLETE
extractBrainTumorCases <- function() {
        df1 <- extractFromDia(var = "Hirntumor", var_short = "bt", prefix = "var", dia)
        df2 <- extractFromDia(var = "Bösartige Neubildung", var_short = "bt2", prefix = "var", dia)
        case_id <- case_id %>% mutate(case_id = parse_number(as.character(case_id)))
        
        
        df_hei
}

# MAYBE IMPLEMENT
extractYesNoTabValues <- function(df = df_neur_aufnb, col = c("dx_prev_stroke","dx_prev","dx_rf")
){
        
        # col <- "dx_rf_migraine_str"
        col_name <- str_extract(as_string(col), "(?<=dx_rf_)\\w+(?=_)")
        col <- ensym(col)
        col_name <- ensym(col_name)
        
        # t <- str_extract_all(df_neur_aufnb$dx_rf_migraine_str, "(?<=1\\s)\\w+")
        # bind_rows(t %>% map(., ~as_tibble_row(., .name_repair = "unique"))) %>% View()
        
        df <- df %>% 
                mutate(temp := str_extract_all(!!col, "(?<=1\\s)\\w+")) %>%
                mutate(one := map(temp, ~nth(., n =1, default = NA))) %>% 
                mutate(two := map(temp, ~nth(., n =2, default = NA))) %>% 
                mutate(!!col_name := unlist(coalesce(two, one))) %>% 
                select(case_id = fallnummer, !!col_name)
        return(df)
}
