# EXTRACTING text and data from medical letters (e.g. discharge letter, necho letters)
# Last modified: 24.03.2021
# First version: 05.02.2021
# CH
# Comment: For tryout of txt/doc/docx import (see Heireka_Briefe.R)

# Set Working Directory to Project Directory
# setwd("~/Dropbox/My Mac (Christians-MacBook-Pro.local)/Documents/heireka")
Sys.setlocale(category = "LC_ALL", "de_DE.UTF-8") # on MAC

source("R/load.R")

# LIB ----
if (!require("sinew")) install.packages("sinew")
if (!require("readtext")) install.packages("readtext")
if (!require("tidyverse")) install.packages("tidyverse")

library(sinew)
library(readtext)
library(tidyverse)
# library(gsubfn)

# Custom String Functions -----

left <- function(text, num_char) {
  substr(text, 1, num_char)
}

mid <- function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right <- function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# READ IN TXT FILES ----

readTxtFiles <- function(path = "input/DOCS/TXT/",keepAll = FALSE) {
  filenames = dir(path, pattern = "*.txt")
  dfDoc <- data.frame()
  for (i in 1:length(filenames)) {
    # Extract Fallnummer
    dfDoc[i, "fallnummer"] <- fnr <- substr(filenames[i], 14, 21)
    # Extract Dokumententyp
    dfDoc[i, "doc_type"] <- substr(filenames[i], 23, 32)
    # Extract Dokumentenversion
    if (substr(filenames[i], 34, 36) != "AA") {
      dfDoc[i, "doc_version"] <- substr(filenames[i], 64, 65)
      dfDoc[i, "doc_number"] <- substr(filenames[i], 52, 62)
    } else {
      # Korrektur f??r Telekonsil-Files
      dfDoc[i, "doc_version"] <- substr(filenames[i], 63, 64)
      dfDoc[i, "doc_number"] <- substr(filenames[i], 51, 61)
    }
    # read file & clean newlines and whitespace
    dfDoc[i, "txt"] <-
      gsub("\a", " ", str_squish(read_file(file = paste0(path,filenames[i]))))
    # dfDoc[i, "txt"] <- read_file(filenames[i])
    message( (i / length(filenames) * 100) )
  }
  if (keepAll == FALSE) {
    dfDoc <- dfDoc %>% as_tibble %>% 
      # only keep latest version of document
      group_by(fallnummer) %>%
      filter(doc_version == max(doc_version))  
  }
  return(dfDoc)
}
# dfDoc$fallnummer <- sapply(filenames, function(x) substr(x, 14, 21))
# dfDoc$doc_type <- sapply(filenames, function(x) substr(x, 23, 32))

# Since each document type is different Lists are created to store docs of each in its respective category
# STORE in LISTS of DOCTYPES ----
# One way

transformToDocList <- function(df, doc_type = doc_type) {
  docType <- unique(df$doc_type)
  docs <- list()
  for (i in 1:length(docType)) {
    docs[[{
      {
        docType[i]
      }
    }]] <- df %>% filter(doc_type == docType[i])
  }
  return(docs)
}
# Second way using PURRR
# t <- dfDoc %>% group_by(doc_type) %>% nest()







getNECHO <- function(df) {
  # NECHO | String definitions for NECHO necho variables
  # variables to use
  necho_pic_quality = "(?<=Bildqualität:)(.*?)(?=Nummer)"
  necho_aortic_root = "(?<=Aortenwurzel\\W)\\d{1,3}"
  necho_left_atrium = "(?<=Linker Vorhof\\W)\\d{1,3}"
  necho_septum = "(?<=Septum\\W)\\d{1,3}"
  necho_hinterwand = "(?<=Hinterwand\\W)\\d{1,3}"
  necho_lv_enddia = "(?<=LV enddiastol\\.\\W)\\d{1,3}"
  necho_ef = "(?<=EF\\W)\\d{1,3}"
  necho_rv = "(?<=RV\\W)\\d{1,3}"
  necho_vci = "(?<=Vena cava inf\\W)\\d{1,3}"
  necho_pa_pressure = "(?<=PA\\WDruck\\W)\\d{1,3}"
  necho_heart_rhythm = "(?<=Herzrhythmus\\W)\\w"
  necho_aortic_valve = "(?<=Aortenklappe)(.+?)(?=Mitralklappe I)"
  necho_mitral_valve = "(?<=Mitralklappe)(.+?)(?=Tricuspidalklappe I)"
  necho_tric_valve =  "(?<=Tricuspidalklappe)(.+?)(?=Pulmonalklappe I)"
  necho_pulm_valve =  "(?<=Pulmonalklappe)(.+?)(?=Befund)"
  necho_question = "(?<=Fragestellung:)(.+?)(?=Medikation)"
  necho_search_endok = "(?<=Diagnose)(.+?)Endokarditis(.+?)(?=Medikation)"
  necho_befund = "(?<=Befund:).+"
  
  df <-
    df %>%
    mutate(!!paste0(substitute(necho_pic_quality)) := stringr::str_extract(txt, necho_pic_quality)) %>%
    mutate(!!paste0(substitute(necho_aortic_root)) := stringr::str_extract(txt, necho_aortic_root)) %>%
    mutate(!!paste0(substitute(necho_left_atrium)) := stringr::str_extract(txt, necho_left_atrium)) %>%
    mutate(!!paste0(substitute(necho_septum)) := stringr::str_extract(txt, necho_septum)) %>%
    mutate(!!paste0(substitute(necho_hinterwand)) := stringr::str_extract(txt, necho_hinterwand)) %>%
    mutate(!!paste0(substitute(necho_lv_enddia)) := stringr::str_extract(txt, necho_lv_enddia)) %>%
    mutate(!!paste0(substitute(necho_aortic_valve)) := stringr::str_extract(txt, necho_aortic_valve)) %>%
    mutate(!!paste0(substitute(necho_mitral_valve)) := stringr::str_extract(txt, necho_mitral_valve)) %>%
    mutate(!!paste0(substitute(necho_tric_valve)) := stringr::str_extract(txt, necho_tric_valve)) %>%
    mutate(!!paste0(substitute(necho_pulm_valve)) := stringr::str_extract(txt, necho_pulm_valve)) %>%
    mutate(!!paste0(substitute(necho_question)) := stringr::str_extract(txt, necho_question)) %>%
    mutate(!!paste0(substitute(necho_search_endok)) := as.character(str_detect(necho_question, necho_search_endok) /
                                                                      1)) %>%
    mutate(!!paste0(substitute(necho_befund)) := stringr::str_extract(txt, necho_befund))
  
  
  # Second run for search in preselected variables (e.g. aortic valve)
  # Aortic Valve
  necho_aortic_insuf = "(?<=Insuff)(.*?)(?=PHT)"
  necho_aortic_pht = "(?<=PHT)(.*?)(?=Dez.)"
  necho_aortic_dez = "(?<=Dez.)(.*?)(?=Stenose)"
  necho_aortic_sten = "(?<=Stenose)(.*?)(?=PPG)"
  necho_aortic_ppg = "(?<=PPG)(.*?)(?=MPG)"
  necho_aortic_mpg = "(?<=MPG)(.*?)(?=KÖF)"
  necho_aortic_kof = "(?<=KÖF)(.*?)(?=Pulmon)"
  
  df <- df %>%
    mutate(
      !!paste0(substitute(necho_aortic_insuf)) := stringr::str_extract(necho_aortic_valve, necho_aortic_insuf)
    ) %>%
    mutate(
      !!paste0(substitute(necho_aortic_pht)) := stringr::str_extract(necho_aortic_valve, necho_aortic_pht)
    ) %>%
    mutate(
      !!paste0(substitute(necho_aortic_dez)) := stringr::str_extract(necho_aortic_valve, necho_aortic_dez)
    ) %>%
    mutate(
      !!paste0(substitute(necho_aortic_sten)) := stringr::str_extract(necho_aortic_valve, necho_aortic_sten)
    ) %>%
    mutate(
      !!paste0(substitute(necho_aortic_ppg)) := stringr::str_extract(necho_aortic_valve, necho_aortic_ppg)
    ) %>%
    mutate(
      !!paste0(substitute(necho_aortic_mpg)) := stringr::str_extract(necho_aortic_valve, necho_aortic_mpg)
    ) %>%
    mutate(
      !!paste0(substitute(necho_aortic_kof)) := stringr::str_extract(necho_aortic_valve, necho_aortic_kof)
    )
  
  # Mitral Valve
  necho_mitral_insuf = "(?<=Insuff)(.*?)(?=Stenose)"
  necho_mitral_sten = "(?<=Stenose)(.*?)(?=PPG)"
  necho_mitral_ppg = "(?<=PPG)(.*?)(?=MPG)"
  necho_mitral_mpg = "(?<=MPG)(.*?)(?=KÖF)"
  necho_mitral_kof = "(?<=KÖF)(.*?)(?=PHT)"
  necho_mitral_pht = "(?<=PHT)(.*?)(?=Tricus)"
  
  df <- df %>% 
    mutate(
      !!paste0(substitute(necho_mitral_insuf)) := stringr::str_extract(necho_mitral_valve, necho_mitral_insuf)
    ) %>%
    mutate(
      !!paste0(substitute(necho_mitral_sten)) := stringr::str_extract(necho_mitral_valve, necho_mitral_sten)
    ) %>%
    mutate(
      !!paste0(substitute(necho_mitral_ppg)) := stringr::str_extract(necho_mitral_valve, necho_mitral_ppg)
    ) %>%
    mutate(
      !!paste0(substitute(necho_mitral_mpg)) := stringr::str_extract(necho_mitral_valve, necho_mitral_mpg)
    ) %>%
    mutate(
      !!paste0(substitute(necho_mitral_kof)) := stringr::str_extract(necho_mitral_valve, necho_mitral_kof)
    ) %>%
    mutate(
      !!paste0(substitute(necho_mitral_pht)) := stringr::str_extract(necho_mitral_valve, necho_mitral_pht)
    )
  
  # Tricuspid Valve
  necho_tric_insuf = "(?<=Insuff)(.*?)(?=PA)"
  necho_tric_pa = "(?<=PA-Druck)(.*?)(?=Stenose)"
  necho_tric_sten = "(?<=Stenose)(.*?)(?=PPG)"
  necho_tric_ppg = "(?<=PPG)(.*?)(?=MPG)"
  necho_tric_mpg = "(?<=MPG)(.*?)(?=KÖF)"
  necho_tric_kof = "(?<=KÖF)(.*?)(?=Pulm)"
  
  df <- df %>% 
    mutate(
      !!paste0(substitute(necho_tric_insuf)) := stringr::str_extract(necho_tric_valve, necho_tric_insuf)
    ) %>%
    mutate(!!paste0(substitute(necho_tric_pa)) := stringr::str_extract(necho_tric_valve, necho_tric_pa)) %>%
    mutate(
      !!paste0(substitute(necho_tric_sten)) := stringr::str_extract(necho_tric_valve, necho_tric_sten)
    ) %>%
    mutate(!!paste0(substitute(necho_tric_ppg)) := stringr::str_extract(necho_tric_valve, necho_tric_ppg)) %>%
    mutate(!!paste0(substitute(necho_tric_mpg)) := stringr::str_extract(necho_tric_valve, necho_tric_mpg)) %>%
    mutate(!!paste0(substitute(necho_tric_kof)) := stringr::str_extract(necho_tric_valve, necho_tric_kof))
  
} 
getAUFNAHMEBOGEN <- function(df) {
    
# AUFNAHMEBOGEN | String definitions for Aufnahmebögen ----

  
  # separate chunks
  # adm_dx <- "(?<=AUFNAHME-DIAGNOSEN)"
  # adm_befund <- 
  adm_meds <- "(?<=MEDIKAMENTE)(.+?)(?=Händigkeit)"
  # zusätzlich rechtshänder
  
  adm_nihss_validation <- "(?<=NIHS{1,3}.{0,4})\\d{1,2}"
  adm_pRS_validation <- "(?<=(([p|P][m|M]?[r|R][s|S])| pr[e|ä](morbid)?).{0,10})\\d"
  #ggf. "Vorzustand" ergänzen
  handed_right <- "(?<=Händigkeit:\\W)\\d"
  handed_left <- "(?<=Händigkeit: \\d\\Wrechts\\W)\\d(?=\\Wlinks)"
  dx_prev_allergy <- "(?<=Allergien:)(.*?)(?=VOR) | (Allergie)  "
  
  dx_prev_stroke_str <- "(?<=Frühere Infarkte:\\W)(.*?)(?=KHK)"
  dx_prev_chd_str <- "(?<=KHK/Herzinfarkt:\\W)(.*?)(?=pAVK)"
  dx_prev_pad_str <- "(?<=pAVK:\\W)(.*?)(?=Krebs)"
  dx_prev_cancer_str <- "(?<=Krebs:\\W)(.*?)(?=Augenerkrankungen)"
  dx_prev_opthalm_str <- "(?<=Augenerkrankungen:\\W)(.*?)(?=Nierenerkrankungen)"
  dx_prev_nephr_str <- "(?<=Nierenerkrankungen:\\W)(.*?)(?=SD-Erkrankungen)"
  dx_prev_thyr_str <- "(?<=SD-Erkrankungen:\\W)(.*?)(?=Metall)"
  dx_rf_htn <- "(?<=Hypertonus:\\W)(.*?)(?=Diabetes)"
  dx_rf_dm <- "(?<=Diabetes:\\W)(.*?)(?=Hyper)"
  dx_rf_hch <- "(?<=Hypercholesterinämie\\W{1,2})(.*?)(?=Vorhofflimmern)"
  dx_rf_afib <- "(?<=Vorhofflimmern:\\W)(.*?)(?=Rauchen)"
  dx_rf_smoke_str <- "(?<=Rauchen:\\W)(.*?)(?=Alkohol)"
  dx_rf_smoke_py <- "([p|P](ack)?[Y|y](ears?)?\\W{0,2})\\d{1,2}"
  dx_rf_alc_str <- "(?<=Alkohol:\\W)(.*?)(?=Familie|Pille)"
  dx_rf_hormone_str <- "(?<=Pille:\\W)(.*?)(?=Migräne|Familie)"
  dx_rf_migraine_str <- "(?<=Migräne:\\W)(.*?)(?=Familie|TECHN)"
  dx_rf_family <- "(?<=Familie)(.*?)(?=TECHN)"
  
  
  df <- 
    df %>% 
    mutate(adm_nihss_validation = parse_number(str_extract(txt, adm_nihss_validation))) %>%
    mutate(adm_pRS_validation1 = parse_number(str_extract(txt, adm_pRS_validation))) %>%
    mutate(adm_meds = str_extract(txt, adm_meds)) %>%
    mutate(handed_right = parse_number(str_extract(txt, handed_right))) %>%
    mutate(handed_left = parse_number(str_extract(txt, handed_left))) %>%
    mutate(dx_prev_allergy = str_extract(txt, dx_prev_allergy)) %>% 
    mutate(dx_prev_stroke_str = str_extract(txt, dx_prev_stroke_str)) %>% 
    mutate(dx_prev_chd_str = str_extract(txt, dx_prev_chd_str)) %>% 
    mutate(dx_prev_pad_str = str_extract(txt, dx_prev_pad_str)) %>% 
    mutate(dx_prev_cancer_str = str_extract(txt, dx_prev_cancer_str)) %>% 
    mutate(dx_prev_opthalm_str = str_extract(txt, dx_prev_opthalm_str)) %>% 
    mutate(dx_prev_nephr_str = str_extract(txt, dx_prev_nephr_str)) %>% 
    mutate(dx_prev_thyr_str = str_extract(txt, dx_prev_thyr_str)) %>% 
    mutate(dx_rf_htn = str_extract(txt, dx_rf_htn)) %>% 
    mutate(dx_rf_dm = str_extract(txt, dx_rf_dm)) %>% 
    mutate(dx_rf_hch = str_extract(txt, dx_rf_hch)) %>% 
    mutate(dx_rf_afib = str_extract(txt, dx_rf_afib)) %>% 
    mutate(dx_rf_smoke_str = str_extract(txt, dx_rf_smoke_str)) %>% 
    mutate(dx_rf_smoke_py = str_extract(txt, dx_rf_smoke_py)) %>% 
    mutate(dx_rf_alc_str = str_extract(txt, dx_rf_alc_str)) %>% 
    mutate(dx_rf_hormone_str = str_extract(txt, dx_rf_hormone_str)) %>% 
    mutate(dx_rf_migraine_str = str_extract(txt, dx_rf_migraine_str)) %>% 
    mutate(dx_rf_family = str_extract(txt, dx_rf_family))
    
  return(df)
}    
getNEURNOT <- function(df) {
  
  # NOTAMBULANZBOGEN | String definitions for emergency room docs ----
  
  er_vitals <- "(?<=VITALPARAMETER bei Aufnahme:\\W)(.*?)(?=Relevante V)"
  er_vitals_rr <- "(?<=RR:)(.*?)(?=HF)"
  er_vitals_rrsys <- "\\d{2,3}(?=\\W)"
  er_vitals_rrdia <- "(?<=\\d{2,3}\\W)\\d{2,3}(?=\\W)"
  er_vitals_hr <- "(?<=HF:\\W?)\\d{1,3}(?=\\W)"
  # er_vitals_spo2 <- "(?<=SpO2:)(.*?)(?=T:)"
  er_vitals_spo2 <- "(?<=SpO2:\\W?)\\d{1,3}(?=\\W?%)"
  er_vitals_temp <- "(?<=T:\\W?)(.*?)(?=°C)"
  er_vitals_tempsite_rect <- "(?<=°C\\W?\\W?)\\d(?=rektal)"
  er_vitals_tempsite_tymp <- "(?<=rektal\\W?)\\d(?=tympanal)"
  er_tx_str <- "(?<=Notfalltherapie:)(.*?)(?=Proced)"
  er_physician <- "(?<=Entlassung gegen ärztlichen Rat)(.*?)(?=rzt)"
  
  df <- 
    df %>%
    mutate(er_vitals = str_extract(txt, er_vitals)) %>%
    mutate(er_vitals_rr = str_extract(er_vitals, er_vitals_rr)) %>%
    mutate(er_vitals_rrsys = str_extract(er_vitals_rr, er_vitals_rrsys)) %>%
    mutate(er_vitals_rrdia = str_extract(er_vitals_rr, er_vitals_rrdia)) %>%
    mutate(er_vitals_hr = str_extract(er_vitals, er_vitals_hr)) %>%
    mutate(er_vitals_spo2 = str_extract(er_vitals, er_vitals_spo2)) %>%
    mutate(er_vitals_temp = str_extract(er_vitals, er_vitals_temp)) %>%
    mutate(er_vitals_tempsite_tymp = str_extract(er_vitals, er_vitals_tempsite_tymp)) %>%
    mutate(er_tx_str = str_extract(txt, er_tx_str)) %>%
    mutate(er_physician = str_extract(txt, er_physician)) 
  return(df)
}

getPhysicianList <- function() {
  # get Mitarbeiterliste ----
  physicianList <- read_delim(
    "input/PMD/NEUR/NGPA_NEUR-ABESC+.xls",
    "\t",
    escape_double = FALSE,
    locale = locale(encoding = "ISO-8859-1"),
    trim_ws = TRUE
  ) %>% select(firstName = NAME1, surName = NAME2, sex = GSCHL) %>% as_tibble()
  
  return(physicianList)
  
  #get Vornamen from demog + extern list (http://www.albertmartin.de/vornamen/?name=&f=1&m=1)
  # firstName <-
  #   demog %>% mutate(Vorname = str_squish(str_split(Name, ",", simplify = TRUE)[, 2])) %>% select(Vorname, Geschl)
  # 
  # firstName2 <-
  #   read_delim(
  #     "~/Dropbox/My Mac (Christians-MacBook-Pro.local)/Documents/vornamen.csv",
  #     ";",
  #     escape_double = FALSE,
  #     col_names = colnames(firstName),
  #     trim_ws = TRUE
  #   )
  # fn <- bind_rows(firstName, firstName2) 
  # fn <- fn[!is.na(fn$Geschl),] %>% arrange(Geschl)
  # fn[fn$Geschl=="männlich",] <- "1"
  # fn[fn$Geschl=="weiblich",] <- "2"
  # fn[fn$Geschl=="unspezifisch",] <- "3"
}


# RUN ----
dfDoc <- readTxtFiles(files[1:1000])
docs <- transformToDocList(dfDoc)

dtest <- dfDoc %>% group_by(doc_type) %>% as.list()

docs$`NEUR-NECHO` <- getNECHO(docs$`NEUR-NECHO`)
docs$`NEUR-AUFNB` <- getAUFNAHMEBOGEN(docs$`NEUR-AUFNB`)   
docs$`NEUR-NOT__` <- getNEURNOT(docs$`NEUR-NOT__`)    
 
phys <- getPhysicianList()

# DISCHARGE LETTER | STring definitions for    ----
  
  # TODO NOCH DISCHARGE LETTER VON NORMALSTATION ERGÄNZEN
  # NOCH DISCHARGE LETTER VON NORMALSTATION ERGÄNZEN
  # NOCH DISCHARGE LETTER VON NORMALSTATION ERGÄNZEN
  # NOCH DISCHARGE LETTER VON NORMALSTATION ERGÄNZEN
  
  dis_dx <- "(?<=DIAGNOSEN:)(.*?)(?=ANAMNESE)"
  dis_anamn <- "(?<=AUFNAHMEBEFUND)(.*?)(?=NEURORAD)"
  dis_nrad <- "(?<=NEURORADIOLOGISCHE UNTERSUCHUNGEN)(.*?)(?=NEUROSONO)"
  dis_nsono <- "(?<=NEUROSONOLOGISCHE  UNTERSUCHUNGEN)(.*?)(?=KONSIL)"
  dis_misc <- "(?<=WEITERE KONSILIARUNTERSUCHUNGEN)(.*?)(?=KONSIL)" 
  
  
  docs$`NEUR-STWBR` <-
    docs$`NEUR-STWBR` %>% 
    mutate(dis_dx = str_extract(txt, dis_dx)) %>%
    mutate(dis_anamn = str_extract(txt, dis_anamn)) %>%
    mutate(dis_nrad = str_extract(txt, dis_nrad)) %>%
    mutate(dis_nsono = str_extract(txt, dis_nsono)) %>%
    mutate(dis_misc = str_extract(dis_anamn, dis_misc))
    
  
# MEDIKAMENTE DRUGS prior to stroke ----  
  # search in admission letters, discharge letters and emergency documents
  # adm_meds1 <- "(?i)medi[s|k](ament|ation)?e?(.*?)(?=Händig|AUFNAHMEBEFUND)"
  adm_meds1 <- "(?i)(medikamente:|medikation:)(.*?)(?=Händig|AUFNAHMEBEFUND)"
  m1 <- docs$`NEUR-AUFNB` %>% mutate(adm_meds_aufnb = str_extract(txt, adm_meds1)) %>% select(fallnummer, adm_meds_aufnb)
  # adm_meds2 <- "(?i)medi[s|k](ament|ation)?e?(.*?)(?=AUFNAHMEBEFUND|NEURORAD)"
  adm_meds2 <- "(?i)(medikamente:|medikation:)(.*?)(?=AUFNAHMEBEFUND|NEURORAD)"
  m2 <- docs$`NEUR-STWBR` %>% mutate(adm_meds_stwbr = str_extract(txt, adm_meds2)) %>% select(fallnummer, adm_meds_stwbr)
  # adm_meds3 <- "(?i)medi[s|k](ament|ation)?e?(.*?)(?=(kurz)?anamnese)"
  adm_meds3 <- "(?i)(medikamente:|medikation:)(.*?)(?=(kurz)?anamnese)"
  m3 <- docs$`NEUR-NOT__` %>% mutate(adm_meds_not = str_extract(txt, adm_meds3)) %>% select(fallnummer, adm_meds_not)
  
  m <-
    full_join(full_join(m1,
                        m2,
                        by = c("fallnummer" = "fallnummer")),
              m3,
              by = c("fallnummer" = "fallnummer"))
  
  
  
# # IMPORT drugs lists from DIMDI / BFARM
#   # library(readr)
#   # Source "https://www.dimdi.de/dynamic/.downloads/arzneimittel/stoffbezeichnungen/stoffbezeichnungen-dateien-20210115.zip"
#   bezvo <-
#     read_delim(
#       "~/Dropbox/My Mac (Christians-MacBook-Pro.local)/Documents/Arzneimittelliste/bezvo.csv",
#       ";",
#       escape_double = FALSE,
#       locale = locale(encoding = "WINDOWS-1252"),
#       trim_ws = TRUE
#     )
#   drug_list <- as_tibble(unique(bezvo$HBEZ1))
#
#   # dauert zu lange -> reduced list of medications
#   for (i in 1:length(unique(drug_list$short))){
#     if
#     m <-
#       m %>%
#       mutate(!!paste0(unique(drug_list$short)[i]) := str_detect(adm_meds_aufnb, unique(drug_list$short)[i]))
#     print(i/length(unique(drug_list$short))*100)
#   }
#
#   t <- paste(m$adm_meds_aufnb, collapse = "")
# 
#   # custom list of drugs and groups
#   arznei_gruppen <-
#     read_delim(
#       "~/Dropbox/My Mac (Christians-MacBook-Pro.local)/Documents/Arzneimittelliste/arznei_gruppen.csv",
#       ";",
#       escape_double = FALSE,
#       trim_ws = TRUE
#     )

# DRUGS AND SUBSTANCES | ATC | Classification ----
  # ###########################
  # I # ATC Drugs to select by ATC code ----
  # atc readin
  ############################
  atc <-
    read_delim(
      "input/Arzneimittelliste/atc_codes.csv",
      ";",
      escape_double = FALSE,
      trim_ws = TRUE
    ) %>% 
    as_tibble 
    atc <- atc %>% 
    mutate(cat = nchar(ATC)) %>%
    mutate(grp = substr(ATC, 1, 5)) %>%
    left_join(atc, atc %>% select(ATC, atc_txt), by = c("grp" = "ATC"))
    atc <- atc %>% rename(name = "atc_txt.x", grp_name = "atc_txt.y")
  
  # ########################################
  # II #  GET drug names in addition to pharmacological substances -----
  # 
  # Download of "Erweiterte_Arzneimittelliste HAM.xlsx"
  # Source: https://swissmedic.ch/swissmedic/de/home/services/listen_neu.html#-257211596
  # ########################################
    if (!require("readxl")) install.packages("readxl")
    library(readxl)
  drugs <-
    read_excel(
      "input/Arzneimittelliste/Erweiterte_Arzneimittelliste HAM.xlsx",
      skip = 5
    ) %>% select(3, 9, 15, 18, 19, 21, 4)
  
  colnames(drugs) <- c("name_long","atc_code","substance","use","use_dose","dose_insulin","company")
 drugs <- drugs %>% 
   separate(name_long, ",\\W", into = c("name","route_adm")) %>% 
   separate(name, "\\W", into = c("drug_name","drugs_dose")) %>% 
   filter(nchar(drug_name)>4) %>% 
   mutate(substance = str_replace_all(substance, "um\\b","")) %>% 
   mutate(substance = str_replace_all(substance, "i\\b","")) %>% 
   separate(substance, ",\\W", into = c("subst1","subst2","subst3","subst4")) %>% 
   mutate(subst1 = str_extract(subst1, "^([^\\s]+)(?=\\s?)")) %>% 
   mutate(subst2 = str_extract(subst2, "^([^\\s]+)(?=\\s?)")) %>% 
   mutate(subst3 = str_extract(subst3, "^([^\\s]+)(?=\\s?)")) %>% 
   mutate(subst4 = str_extract(subst4, "^([^\\s]+)(?=\\s?)")) %>% 
   filter(nchar(subst1)>4)
    
 # Join I+II (substances and drugs) ------
 atc_drugs <- left_join(atc, drugs %>%  select(atc_code, drug_name, subst1), by=c("ATC" = "atc_code"))
 
 # ###########################
 # III # ATC Level - selection ----
 # e.g. Level 3 selection "C03 - Diuretika" lists all diuretics
 # e.g. Level 4 selection "G03D - Gestagene" lists all gestagen and their parent groups
 # e.g. Level 4 selection "G02AD - Prostaglandine" lists all prostaglandines 
 # ###########################
 getATCLevel <- function(grp_level, elem_level, atc){
   atc_list <- list()
   for (i in 1:length(which(atc$cat == grp_level))) {
     atc_grp_lev <- atc[atc$cat == grp_level, 4][[1]][[i]]
     atc_list[[{
       {
         paste0(atc_grp_lev,
                " - ",
                atc[atc$cat == grp_level, 5][[1]][[i]])
       }
     }]] <- atc %>% filter(str_detect(grp, atc_grp_lev), cat == elem_level)
   }
   return(atc_list)
 }
 
 atc_level1 <- getATCLevel(1,7, atc_drugs)
 atc_level3 <- getATCLevel(3,7,  atc_drugs)
 atc_level4 <- getATCLevel(4,7, atc_drugs)
 atc_level5 <- getATCLevel(5,7,  atc_drugs)
 atcs <- append(atc_level1, atc_level3) %>% append(atc_level4) %>% append(atc_level5)
 
  
  
  
  
  
  
  
  
  

    # remove some stuff to make better searchable
  m$adm_meds_aufnb <- str_replace_all(m$adm_meds_aufnb, "- ","-")
  m$adm_meds_aufnb <- str_replace_all(m$adm_meds_aufnb, " -","-")
  m$adm_meds_aufnb <- str_replace_all(m$adm_meds_aufnb, "½","0,5")
  
  
  
getMeds <- function (drug, abr, txt_col, dat) {
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
      mutate({{drug_presc_str }} := str_extract(eval(as.name(paste0(abr,"_str"))), presc))
    
    dat <- dat %>% 
      extract(eval(as.name(txt_col)), 
              c(
                
              )
              )
    drug_str <- enquo(drug_str)
    txt_col <- enquo(txt_col)
    
    
    t <- dat %>% 
      mutate({{drug_str       }} := str_extract_all(eval(as.name(!!txt_col)), drug2, simplify = F)) %>% 
      unnest_wider({{drug_str       }}, names_sep = "_")
    
    
    df <- data.frame()
    df <- str_extract_all(dat$adm_meds_aufnb, drug2, simplify = T) %>% as_tibble()
    df <- str_count(dat$adm_meds_aufnb, drug2)
    
      
    return(dat)
}  



#' creatStrFromATCs String is constructed from 1) Substance Name 2) Drug Name 3) abbreviated version of 1 & 2 with first 6 letters
#' 
#' @param df A data.frame or tibble from getATC_Level function
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
    str_remove_all("kombinationen\\|") 
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






# GET ALL MEDS (~10 min) ----
# select level - e.g. atc_level3, atc_level4

for (i in 1:length(atc_level3)) {
  m <- getMeds(createStrFromATCs(atc_level3[[i]]),
               substr(names(atc_level3)[i], 1, 3),
               "adm_meds_aufnb",
               m_bkp)
  print(i / length(atc_level3) * 100)
}

test <-
  getMeds(createStrFromATCs(atcs$`N03 - Antiepileptika`),
          "aed",
          "adm_meds_aufnb",
          m)

prokin <- paste0("(?i)",atcs$`A03F - Prokinetika`$name, collapse="|")
test2 <- getMeds(prokin, "prokin", "adm_meds_aufnb", m)

antiepil <- paste0("(?i)",atcs$`N03 - Antiepileptika`$name, collapse="|")
test3 <-   getMeds(antiepil, "aed", "adm_meds_aufnb", m)

ppi <- paste0("(?i)",atcs$`A02BC - Protonenpumpenhemmer`$name, collapse="|")
test3 <-   getMeds(ppi, "ppi", "adm_meds_aufnb", m)
  


m <- getMeds(thromb2, "tah", "adm_meds_aufnb", m) 

m1 <- getMeds("(?i)\\b(ASS|Aspirin|Acetylal[y|i]|Godamed)", "ass", "meds", m) 
m2 <- getMeds("(?i)\\b(Clopi|Plavix)", "clopi", "meds", m) 
m <- getMeds("(?i)(Prasu|Efient|prasillt)", "prasu", "adm_meds_aufnb", m) 
m <- getMeds("(?i)(Ticagr|Briliqu)", "ticag", "adm_meds_aufnb", m) 

# blood pressure
     
  # ACE INHIBITORS
ace <-
  paste("(?i)(",
        paste(arznei_gruppen[!is.na(arznei_gruppen$htn_ace_sart), ]$htn_ace_sart, collapse =
                "|"),
        ")",
        sep = "")

  ace <- "(?i)(^rami|delix|enala|xanef|capto|quina|accuzide|lisin|acercomp|perind|bipreterax|benaze)"
  m <- getMeds(ace, "ace", "adm_meds_aufnb", m) 
  
  # BETA BLOCKER
     # CALCIUM ANTAGONISTS
     
     

m <- m $ass_str <- str_extract(m$adm_meds_aufnb, drug2)
m$ass_dose <- 
m$ass_presc <- str_extract(m$ass_str, presc)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# # # <ECHO-2> ----
# usable variables Aortenwurzel, Linker Vorhof, Septum, Hinterwand
# JOIN by Fallnummer / Patientennummer / Name / Dok NR möglich
necho_new <-
  read_delim(
    "~/Dropbox/HEIREKA/PMD_HEIREKA_2013-2019/20190607_ZNEUR00000000000.xls",
    "\t",
    escape_double = FALSE,
    col_types = cols(
      `DOKNR` = col_number(),
      `Bildqualität` = col_character(),
      `Aortenwurzel` = col_number(),
      `Linker Vorhof` = col_number(),
      `Septum` = col_number(),
      `Hinterwand` = col_number(),
      `LV enddiasto` = col_number(),
      `LV endsystol` = col_number(),
      `EF` = col_number(),
      `RV` = col_number(),
      `Vena cava in` = col_number(),
      `PA-Druck` = col_number()
    ),
    locale = locale(encoding = "ISO-8859-1"),
    trim_ws = TRUE
  )
# colnames(necho_new)
necho_new <-
  necho_new %>% select(
    case_id = "FallNr.",
    doc_number = DOKNR,
    necho_pic_quality = Bildqualität,
    necho_aortic_root = Aortenwurzel,
    necho_left_atrium = "Linker Vorhof",
    necho_septum = Septum,
    necho_hinterwand = Hinterwand,
    necho_lv_enddia = "LV enddiasto",
    necho_lv_endsys = "LV endsystol",
    necho_ef = EF,
    necho_rv = RV,
    necho_vci = "Vena cava in",
    necho_pa_pressure = "PA-Druck"
  ) 

# BIND ECHO1 + ECHO2
necho <- bind_rows(necho, necho_new)
necho$fallnummer <- coalesce(necho$fallnummer, necho$case_id)







#######################################
# WERITER - FILTER UND STORE IN LIST
dfNECHO %>% 
  filter(doc_type == "NEUR-NECHO") %>% 
  group_by(fallnummer) %>% 
  filter(doc_version == max(doc_version))







# EXTRACTTEXT FUNCTION ----
extractText <- function(BEG, END, COL) {
  for (i in 1:length(filenames)) {
    txt <- readLines(filenames[i], encoding = "UTF-8", skipNul = TRUE)
    beg <- which(grepl(BEG, txt, perl = TRUE))[1]
    
    if (END == "99999*") {
      end <- length(txt)
    } else
    {
      end <- which(grepl(END, txt, perl = TRUE))[1]
    }
    
    if (!is.na(beg) & !is.na(end)) {
      if ((end - beg) > 1) {
        df[i, COL] <<- paste(txt[seq(from = beg, to = end, by = 1)], collapse = " ")
      } else {
        df[i, COL] <<- paste(txt[beg], txt[beg + 1])
      }
    } # else {
    #   df[i, COL] <<- NA
    # }
  }
}