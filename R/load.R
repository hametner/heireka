#' @title readTxtFiles
#' @description Read in *.txt files of (medical) semistructured documents 
#' @param filenames Provide filenames to be processed
#' @param keepAll Keep all versions of a document - default is FALSE, Default: FALSE
#' @param path Provide path to files, Default: 'input/DOCS/TXT/'
#' @return Returns a tibble 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  files = dir(path = "input/DOCS/TXT/", pattern = "*.txt")
#'  df <- readTxtFiles(files[1:1000])
#'  }
#' }
#' @export
#' @importFrom readr read_file
#' @import tidyverse
#' @rdname readTxtFiles
readTxtFiles <-
        function(path = "input/DOCS/TXT/",
                 keepAll = FALSE) {
                filenames <- list.files(path = path, pattern = "*.txt")
                dfDoc <- data.frame()
                for (i in 1:length(filenames)) {
                        # Extract Fallnummer
                        dfDoc[i, "fallnummer"] <- substr(filenames[i], 14, 21)
                        # Extract Dokumententyp
                        dfDoc[i, "doc_type"] <- substr(filenames[i], 23, 32)
                        # Extract Dokumentenversion
                        if (substr(filenames[i], 34, 36) != "AA") {
                                dfDoc[i, "doc_version"] <- substr(filenames[i], 64, 65)
                                dfDoc[i, "doc_number"] <- substr(filenames[i], 52, 62)
                        } else {
                                # Korrektur f??r Telekonsil-Files
                                dfDoc[i, "doc_version"] <-
                                        substr(filenames[i], 63, 64)
                                dfDoc[i, "doc_number"] <-
                                        substr(filenames[i], 51, 61)
                        }
                        # read file & clean newlines and whitespace
                        dfDoc[i, "txt"] <-
                                gsub("\a", " ", str_squish(read_file(file = paste0(
                                        path, filenames[i]
                                ))))
                        # dfDoc[i, "txt"] <- vroom(paste0(path, filenames[i]), delim = "")
                        message((i / length(filenames) * 100))
                }
                if (keepAll == FALSE) {
                        dfDoc <- dfDoc %>% as_tibble %>%
                                # only keep latest version of document
                                group_by(fallnummer) %>%
                                filter(doc_version == max(doc_version)) %>% 
                                ungroup()
                }
                return(dfDoc)
        }


#' @title transform
#' @description Takes tibble from function "readTxtFiles" and transforms to a list of tibbles based on types of documents ("doc_type" )
#' @param df - input tibble from readTxtFiles 
#' @param doc_type - specifies the type of docs used - if left as is, all doc types are used
#' @return list of tibbles based on doc_type
#' @export
transformToDocTypeList <-
        function(df,
                 doc_type = doc_type) {
                # using for-loop : 2.2 sec.
                # docType <- unique(df$doc_type)
                # docs <- list()
                # for (i in 1:length(docType)) {
                #         docs[[{
                #                 {
                #                         docType[i]
                #                 }
                #         }]] <- df %>% filter(doc_type == docType[i])
                # }
                # return(docs)
                
                # using purrr::nest() : 0.020 sec.
                df <- df  %>% group_by(doc_type) %>% nest() %>% ungroup()
                df$data <- set_names(df$data, df$doc_type)
                
                return(df)
        }

#' @title readPhysicianList
#' @description Load list of physicians from source file
#' @import tidyverse
#' @importFrom readr read_delim
#' @param path - A excplicit path to file has to be provided
#' @return A list
#' @export
readPhysicianList <- function(path) {
        physicianList <- read_delim(
                path,
                "\t",
                escape_double = FALSE,
                locale = locale(encoding = "ISO-8859-1"),
                trim_ws = TRUE
        ) %>% select(firstName = NAME1,
                     surName = NAME2,
                     sex = GSCHL) %>% as_tibble()
        return(physicianList)
}

#' @title readATCClass
#' @description Read the ATC Classification file
#' @import tidyverse
#' @importFrom readr read_delim
#' @param path - A excplicit path to file has to be provided
#' @return A dataframe
#' @source "https://www.dimdi.de/dynamic/.downloads/arzneimittel/stoffbezeichnungen/stoffbezeichnungen-dateien-20210115.zip"
#' @export
readATCClass <- function(path) {
        atc <-
                read_delim(path,
                           ";",
                           escape_double = FALSE,
                           trim_ws = TRUE) %>%
                as_tibble
        atc <- atc %>%
                mutate(cat = nchar(ATC)) %>%
                mutate(grp = substr(ATC, 1, 5)) %>%
                left_join(atc, atc %>% select(ATC, atc_txt), by = c("grp" = "ATC"))
        atc <-
                atc %>% rename(name = "atc_txt.x", grp_name = "atc_txt.y")
        
}


#' @title readDrugList
#' @description Read drug list files / names from (A) Swissmedic or (B) MMI Pharmindex or (C) Drugbank, currently only implemented for A
#' @import tidyverse
#' @importFrom readr read_delim
#' @importFrom readxl read_excel
#' @param path - A explicit path to file has to be provided
#' @return A dataframe
#' @source A https://swissmedic.ch/swissmedic/de/home/services/listen_neu.html#-257211596/Erweiterte_Arzneimittelliste HAM.xlsx
#' @source B 
#' @source C 
#' @export
readDrugList <- function(path){
        # II #  GET drug names in addition to pharmacological substances 
if (!require("readxl")) install.packages("readxl")
library(readxl)
drugs <-
        read_excel(
                path,
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
return(drugs)
}

# Alpha-Version - TODO
# create new columns
# leave choice to remove old shifted columns or not
# shiftCharToRight <- function(df, col_start, col_end, shift_n = 1, shift_to = "right") {
#         vec_colnames <- names(df[ which(colnames(df) == {{col_start}}) : which(colnames(df) == {{col_end}}) ])
#         vec_colnames_old <- unlist(map(vec_colnames, ~paste0({{.x}},"_old")))
#         vec_col <- c(which(colnames(df) == {{col_start}}) : which(colnames(df) == {{col_end}}))
#         
#         
#         df <- df %>% mutate(paste0({{vec_colnames}},"_old") := .x[{{vec_col}}])
#         df <- df %>% map(vec_colnames, ~mutate(old = .x))
#         
#         df <- map(vec, ~df[.x] %>% as.character() %>% str_trunc() %>% )
#         
#         df[11] <- 
#                 as.character(
#                         str_trunc(
#                                 paste0(str_trunc(paste0(df[11-1]), width = 1, side = "left", ellipsis = ""),
#                                        demog$Entl.Zeit), width = 6, side = "right", ellipsis = "")
#                 )
#         
# }

#' @title repairDemog
#'
#' @description repairs shift in imported demography files by judging if more than 90% of "Aufn.Datum" has been imported correctly
#' @param demog file to be check for reparation
#'
#' @return if needed repaired df
#' @export
#' COMMENT: function could be further developed generating generic shift function
repairDemog <- function(demog){
        d <- parse_datetime(as.character(demog['Aufn.Datum'][[1]])) 
        if ( (length(d[!is.na(d)]) / length(d)) < "0.9" ) {
                demog$Entl.Zeit <- 
                        as.character(
                                str_trunc(
                                        paste0(str_trunc(paste0(demog$Entl.Datum), width = 1, side = "left", ellipsis = ""),
                                               demog$Entl.Zeit), width = 6, side = "right", ellipsis = "")
                )
                demog$Entl.Datum <- 
                        as.character(
                                str_trunc(
                                        paste0(str_trunc(paste0(demog$Aufn.Zeit), width = 1, side = "left", ellipsis = ""),
                                               demog$Entl.Datum), width = 8, side = "right", ellipsis = "")
                )
                demog$Aufn.Zeit <- 
                as.character(
                        str_trunc(
                                paste0(str_trunc(paste0(demog$Aufn.Datum), width = 1, side = "left", ellipsis = ""),
                                       demog$Aufn.Zeit), width = 6, side = "right", ellipsis = "")
                )
                demog$Aufn.Datum <- 
                as.character(
                        str_trunc(
                                paste0(str_trunc(paste0(demog$Pfleg.OE), width = 1, side = "left", ellipsis = ""),
                                       demog$Aufn.Datum), width = 8, side = "right", ellipsis = "")
                )
                demog$Geb. <- parse_number(str_trunc(paste0(demog$Geschl, demog$Geb.), width = 8, side = "right", ellipsis = ""))
        }
        demog <- demog %>% mutate_all(as.character)
        return(demog)
}


#' @title readISHFiles
#'
#' @description Searches desired file e.g. "LAB.csv" in project directory and subdirectories, imports all found files and combines them. (LAB.csv, KENNZAHLEN.csv, DIA_PRIM.csv, DIA_SEC.csv, ...)
#' @param file Provide "file" to be searched and imported for (may be several in different subdirectories)
#'
#' @return A tibble
#' 
#' @import tidyverse
#' @import lubridate
#' @import vroom
#' @importFrom readr read_delim
#' @import dtplyr
#'
#' @examples readLabFiles("LAB.csv")
readISHFiles <- function(file, sep = ";") {
        files <- list.files(recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
        ish_files <- files[files %>% str_detect({{file}})]
        enc <- map(ish_files, guess_encoding)
        enc <- unlist(map(enc, function(x) x[[1]][1]))
        
        if (str_detect(file, "ZNEUR")) sep <- "\t"
        df <- map2(ish_files, enc,
                ~vroom(
                        .x,
                        sep,
                        escape_double = FALSE,
                        col_names = TRUE,
                        col_types = cols(.default = "c"), 
                        trim_ws = TRUE,
                        skip = 0,
                        locale = locale(encoding = .y)
                ))
        names(df) <- ish_files        
        
        if (str_detect({{file}}, "LAB")) {
                df[[1]]$ErgebnisID <- as.character(df[[1]]$ErgebnisID)
                df[[2]]$ErgebnisID <- as.character(df[[2]]$ErgebnisID)
        } 
        
        if (str_detect({{file}}, "KENNZAHLEN")) {
                # df[[2]]$Aufn.Datum <- as.character(df[[2]]$Aufn.Datum)
                # df[[2]]$Entl.Datum <- as.character(df[[2]]$Entl.Datum)
        }
        
        if (str_detect({{file}}, "DEMOGR")) {
                df <- map(df, ~repairDemog(.x))
        }
        
        df <- df %>% reduce(bind_rows)
        
        # # Date Correction
        # df$labDateTime <-
        #         ymd_hms(paste(df$DokDatum, df$DokZeit)) 
        # df$Fall <- as.double(df$Fall)
        
        # df$hospDateTime <- 
        #         ymd_hms(paste(df$Aufn.Datum, df$Aufn.Zeit))
# 
        return(df)
}


# demog <- demogr %>% unique()
# demogr$Geb. <- ymd(demogr$Geb.)
# demog$Geb. <- as.Date(demog$Geb., format = "%Y%m%d")
# demog$Fall <- parse_number(demog$Fall)
# demogr$Aufn.Datum <- ymd(demogr$Aufn.Datum)
# demog$Aufn.Datum <- as.Date(demog$Aufn.Datum, format = "%Y%m%d")



#' @title getLatestDBFile
#'
#' @description search files in directory and subdirs for the desired string. Function automatically selects file created last 
#' @param file 
#' @import Hmisc
#' @import tidyverse
#' @return filename as character string
#' @export
#'
#' @examples getLatestFile("LysePat")
getLatestDBFile <- function(file){
        files <- list.files(recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
        heir_files <- files[files %>% str_detect({{file}})]
        file <- bind_cols(file = heir_files,
                          file.info(heir_files)) %>% 
                as_tibble %>% 
                arrange(desc(mtime)) %>% 
                slice(1L) %>% 
                select(file)
        return(file[[1]])
}

#' @title readDB - Read in mdb-Typ Databases using mdb.get function from Hmisc package
#'
#' @param file 
#' @param latest 
#' @import Hmisc
#' @import tidyverse

#' @return A nested list (table items as list items, tables as tibbles/data.frames)
#' @export
#'
#' @examples heireka <- readDB("LysePat", latest = TRUE) or <- readDB("LysePat2018_bis_01_2021.mdb", latest = FALSE)
readDB <- function(file, latest = TRUE){
        Sys.setlocale(category = "LC_ALL", "de_DE.UTF-8") # on MAC
        if (latest == TRUE) {
                f <- getLatestDBFile({{file}})
        }
        
        tables <- mdb.get(f, tables = TRUE)
        
        if (str_detect({{file}}, "Doppler")) {
                tables <- tables[!str_detect(tables, "Ä")]
        }
                
        
        
        db <- map(tables,
                  ~ mdb.get(f, tables = .x) %>%
                          cleanup.import() %>%
                          as_tibble())
        names(db) <- tables
        return(db)
}


#' @title getFlatXML - 
#'
#' @param x 
#' @tidyverse
#' @XML
#' @return A tibble 
#' @export
#'
#' @examples
getFlatXML <- function(x){
        flat <- x %>% 
                xmlParse() %>% 
                xmlToList() %>% 
                as_tibble() %>% 
                flatten() %>% flatten() %>% flatten() %>% flatten() %>% flatten() %>% 
                flatten() %>% flatten() %>% flatten() %>% flatten() %>% flatten() %>% 
                as_tibble(.name_repair = "universal") %>% 
                select(-contains(c("dataNode","BARCODE")))
        return(flat)
}

readSingleXMLFileWithManyDPDF <- function(file) {
        lines <- readLines(file)
        start   <- grep('<?xml version',lines,fixed=T)
        end     <- c(start[-1]-1,length(lines))
        dpdf <- data.frame()
        for (i in 1:length(start)) {
                txt <- paste(lines[start[i]:end[i]],collapse="\n")
                typeDoc <- str_extract(txt, "(?<=DTID_)(.*?)(?=>)")
                dpdf[i,"txt"] <- txt
                dpdf[i,"docType"] <-  typeDoc
        }
        dT <- unique(dpdf$docType)
        dpdfs <- list()
        for (i in 1:length(dT)) {
                dpdfs[[{{dT[i]}}]] <- as_tibble(dpdf) %>% 
                                filter(docType == dT[i]) %>% 
                                select(txt)
        }
        return(dpdfs)
}

readDPDFs <- function(file, files){
        dpdf_files <- files[files %>% str_detect({{file}})]
        # lines <- map(dpdf_files, readLines)
        # names(lines) <- dpdf_files
        # lines <- lines %>% reduce(bind_rows)
         
        list <- map(dpdf_files,
                   ~readSingleXMLFileWithManyDPDF(.x))
                  
        names(list) <- dpdf_files  
        dpdf <- list %>% 
                reduce(append) %>% 
                enframe(name = "name", value = "value")
        names(dpdf$value) <- dpdf$name
        return(dpdf)
}

#' @import tidyverse
#' @import XML
extractFromDPDFs <- function(file, dpdf, dok_nr) {
        if (file %in% dpdf['name'][[1]]) {
                df <-
                        map_dfr(dpdf['value'][[1]][{{file}}][[1]][['txt']], ~ getFlatXML(.x)) %>%
                        mutate(GTFE_DOKNR = parse_number(GTFE_DOKNR)) %>%
                        left_join({{dok_nr}} %>%
                                select(DOKNR, FALNR) %>%
                                mutate(DOKNR = parse_number(DOKNR)),
                        by = c("GTFE_DOKNR" = "DOKNR"))
        } else {
                message("Please provide one of: ")
                message(paste0(dpdf['name'][[1]], sep = ", "))
        }
        return(df)
}



#' @title readDokNr - Retrieves a dataframe/tibble with a list of available documents including (DOKNR <-> PATNR <-> FALNR)
#'
#' @param file desired file containing "ZVNDOCDRAW"
#' @import vroom
#' @import tidyverse
#' @importFrom stringr str_detect
#' @return A tibble
#' @export
#'
#' @examples doc_nr <- readDokNr("ZVNDOCDRAW")
readDokNr <- function(file, f = files) {
        dok_nr_files <- f[f %>% str_detect({{file}})]
        dok_nr <-
                map(dok_nr_files,
                    ~ vroom(
                            .x,
                            "\t",
                            col_names = TRUE,
                            col_types = cols(.default = "c"),
                            trim_ws = TRUE
                    )) %>% 
                reduce(bind_rows) %>% 
                select(DOKNR, DOKVR, PATNR, FALNR, ORGLA, DODAT, DOTIM, ERDAT, ERTIM)
        return(dok_nr)
}


#' @title readMedxDischarge - Collects dataframe with discharge medications 
#'
#' @description reads in discharge meds, joins case_id/FallNr and provides a accessible structure of all medications
#' @param file - please provide string that identifies file of discharge medication (e.g. "Y0000035")
#' @param files - project files
#' @import vroom
#' @import tidyverse
#' @return A tibble
#' @export
#'
#' @examples mdx <- readMedxDischarge("Y0000035", files)
readMedxDischarge <- function(file, files) {
        meds_dis_files <- 
                files[files %>% str_detect({{file}})]
        meds_dis_data_file <-
                file.info(meds_dis_files) %>%
                arrange(desc(size)) %>%
                slice(1L) %>% 
                row.names()
        meds_dis_stamm_file <- 
                meds_dis_files[nchar(meds_dis_files) == min(nchar(meds_dis_files))]

        meds_dis_list <-
                map(meds_dis_files,
                    ~ vroom(
                            .x,
                            "\t",
                            col_names = TRUE,
                            locale = locale(encoding = "ISO-8859-1"),
                            col_types  = cols(.default = "c")
                    )) %>%
                set_names(meds_dis_files)

        mdx_base <-
                meds_dis_list[[meds_dis_stamm_file]] %>%
                select(DOKNR, DOKVR, ISHMPATNAM, ISHMPATGEB, ISHMPATFAL)
        
        mdx <-
                meds_dis_list[[meds_dis_data_file]] %>%
                select(DOKNR, DOKVR, ME_AI_DAT, ME_AI_PRID, ME_AI_PRNA,ME_AI_DSCH, ME_AI_WIR1, ME_AI_WIR2, ME_AI_WIR3) %>%
                left_join(mdx_base, by = c("DOKNR" = "DOKNR")) %>%
                pivot_longer(
                        cols = c(ME_AI_WIR1, ME_AI_WIR2, ME_AI_WIR3),
                        names_to = "WIRX",
                        values_to = "WIRKSTOFF") %>% 
                drop_na(WIRKSTOFF) %>%
                mutate(WIRKSTOFF = word(str_replace(WIRKSTOFF, "[-,\\[]", " "))) %>% 
                unique() %>%
                pivot_wider(
                        data = .,
                        names_from = WIRKSTOFF,
                        values_from = ME_AI_DSCH) %>%
                group_by(DOKNR) %>%
                fill(everything(), .direction = "down") %>%
                fill(everything(), .direction = "up") %>%
                slice(1) %>%
                select(-ME_AI_PRID, -ME_AI_PRNA, -contains("DOKVR"), -WIRX)

        cols <- mdx %>% keep(is.list) %>% names()
        mdx <- mdx %>% unnest(all_of(cols))
        
        return(mdx)
}

#' @title - readMedxList - Builds a list of substance names and drugs from discharge medications
#' @description The list that is built comprises the most common drugs and substances in stroke patients / neurology, Function has side effect of writing csv file of list when specified with option export = TRUE
#' @param file 
#' @param files 
#' @param export Default is FALSE, if TRUE file csv-File is exported to "output/HEIREKA"
#' @import stringr
#' @import tidyverse
#' @import vroom
#' @return
#' @export
#'
#' @examples meds <- readMedxList("Y0000035", files, export = FALSE)
readMedxList <- function(file, files, export = FALSE) {
        meds_dis_files <- 
                files[files %>% str_detect({{file}})]
        meds_dis_data_file <-
                file.info(meds_dis_files) %>%
                arrange(desc(size)) %>%
                slice(1L) %>% 
                row.names()
        meds_dis_stamm_file <- 
                meds_dis_files[nchar(meds_dis_files) == min(nchar(meds_dis_files))]

        meds_dis_list <- map(meds_dis_files, ~ vroom(.x,
                            "\t",
                            col_names = TRUE,
                            locale = locale(encoding = "ISO-8859-1"),
                            col_types  = cols(.default = "c")
                    ))
        names(meds_dis_list) <- meds_dis_files
        
        
        m <- meds_dis_list[[meds_dis_data_file]] %>%
                pivot_longer(
                        cols = c(ME_AI_WIR1, ME_AI_WIR2, ME_AI_WIR3, ME_AI_PRNA),
                        names_to = "WIRX",
                        values_to = "WIRKSTOFF"
                ) %>%
                mutate(WIRKSTOFF = word(str_replace(WIRKSTOFF, "[-,\\[\\(\\)]", " "))) %>%
                mutate(WIRKSTOFF = str_to_lower(str_replace(WIRKSTOFF, "[®]", " "))) %>%
                select(WIRKSTOFF) %>% 
                drop_na(WIRKSTOFF) %>%
                unique() %>% 
                arrange(WIRKSTOFF)
        
        meds <- m$WIRKSTOFF[str_count(m$WIRKSTOFF) > 2]
        if (export == TRUE) write.csv2(meds, "output/HEIREKA/HEIREKA_medication_list_unique_2021_04_21.csv")                
        return(meds)
}





