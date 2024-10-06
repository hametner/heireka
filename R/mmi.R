# tidyverse version
# Comment: Tried a datatable version - in comparison was not really that faster 
ask_mmi <- function(mmi = mmi, prompt){
        require(stringdist)
        df<-0
        search_str<-0
        MOLECULE <- mmi$MOLECULE %>% as_tibble %>% mutate(NAME = tolower(NAME)) %>% mutate(SORTNAME = tolower(SORTNAME))
        
        if (is.na(prompt)) { return(df <- as_tibble(NA_character_))}
        # check no prior medication
        if (str_detect(pattern = "no prior medication", prompt)) { 
                # df <- NA
                df <- data.frame(name_e = "no prior medication", grp_e = "no prior medication", class_e = "no prior medication") %>% as_tibble()
                return(df)
                # print("[NPM]")
        } else if ((is.character(prompt) & nchar(prompt)>2)) { 
                        prompt <- trimws(gsub("[[:punct:]]", " ", prompt))
                        prompt <- tolower(gsub("\\s\\d+", "", prompt))
                        search_str <- paste0("(?i)^", gsub("\\s", ".{0,20}", prompt, perl = TRUE))
                        n_sub <- length(str_split(prompt, " ")[[1]])
                        # if (n_sub>1) prompt <- str_split(prompt, " ")
                
                if (nrow(MOLECULE %>% filter(str_detect(SORTNAME, paste0("\\b",prompt,"\\b"))))!=0) { # check IS MOLECULE ?
                        df <- MOLECULE %>% 
                                filter(str_detect(SORTNAME, paste0("\\b",prompt,"\\b"))) %>% 
                                # slice(1) %>% 
                                left_join(mmi$COMPOSITIONELEMENT, by = c("ID" = "MOLECULEID"), suffix = c(".MOLECULE", ".CE")) %>%
                                left_join(mmi$ITEM_COMPOSITIONELEMENT, by = c("ID.CE" = "COMPOSITIONELEMENTID"), suffix = c(".CE", ".I_CE")) %>% 
                                left_join(mmi$ITEM, by = c("ITEMID" = "ID"), suffix = c(".I_CE", ".ITEM")) %>%
                                left_join(mmi$ITEM_ATC, by = c("ITEMID" = "ITEMID"), suffix = c(".ITEM", ".I_ATC")) %>% 
                                left_join(mmi$PRODUCT, by = c("PRODUCTID" = "ID"), suffix = c(".ITEM", ".PROD")) %>% 
                                left_join(mmi$PRODUCT_ICD, by = c("PRODUCTID" = "PRODUCTID"), suffix = c(".PROD", ".P_ICD")) %>% 
                                # filter(is.na(UPPERMOLECULEID)) %>% slice(1) %>% 
                                filter(!is.na(ATCCODE)) %>% 
                                filter(ACTIVESUBSTANCECOUNT>0) %>% 
                                mutate(sim = stringdist::stringdist(NAME, search_str, method = "lcs")) %>%
                                arrange(-desc(sim)) %>%
                                slice(1)
                        
                        if ( (nrow(df)==0) || (is.null(nrow(df))) ){
                                # Check IF MOLECULE is matchable by name
                                df <- atc %>%
                                        filter(str_detect(pattern=paste0("(?i)^",substring(prompt, 1,1)),name_g)) %>% 
                                        filter(!is.na(name_e)) %>%
                                        mutate(name_split = str_split(name_g, " |-")) %>% unnest(name_split) %>%
                                        mutate(name_split = tolower(name_split)) %>%
                                        # filter(name_split %in% prompt) %>% 
                                        filter(str_detect(pattern=prompt, name_split)) %>% 
                                        mutate(sim = stringdist::stringdist(name_split, prompt, method="lcs")) %>% 
                                        arrange(-desc(sim)) %>% 
                                        # filter(sim<3) %>% 
                                        slice(1) %>% 
                                        select(atc_code = atc, name_g, name_e, grp, grp_g, grp_e, class, class_g, class_e, name_of_mol = name_g)
                                active_subst <- TRUE
                                # Otherwise no active substance likely
                                if ( (nrow(df)==0) || (is.null(nrow(df))) ){
                                        df <- NA #"ACTIVESUBSTANCECOUNT=0" %>% as_tibble()
                                        active_subst <- FALSE
                                }
                        } else {
                        
                               if (!is.na(df$UPPERMOLECULEID)) {
                                       name_of_mol <- mmi$MOLECULE %>% filter(ID==df$UPPERMOLECULEID) %>% pull(NAME) %>% tolower()
                               } else { name_of_mol <- tolower(df$NAME.I_CE) }
                        
                                if (nrow(atc %>% filter(atc==df$ATCCODE) %>% filter(!is.na(name_e)))!=0) {
                                        df <- df %>% left_join(atc, by = c("ATCCODE" = "atc")) 
                                        # mutate(name_of_mol = case_when(is.na(UPPERMOLECULEID) ~ NAME.I_CE, TRUE ~ name_of_mol)) %>% 
                                        df$name_of_mol <- name_of_mol
                                        df <- df %>% select(atc_code = ATCCODE, name_g, name_e, grp, grp_g, grp_e, class, class_g, class_e, name_of_mol) %>% 
                                                slice(1:n_sub)
                                        active_subst <- TRUE
                                                
                                } else {
                                        df <- atc %>% 
                                                mutate(name_g = tolower(name_g)) %>% 
                                                filter(name_g %in% name_of_mol) %>% 
                                                slice(1:n_sub)
                                        active_subst <- TRUE
                                }
                        }
                        # print(paste0("[MOL]", prompt))
                } else { # IF NO MOLECULE THEN DRUG PRODUCT
                
                        if ( (nrow(mmi$PRODUCT %>% as_tibble() %>% filter(str_detect(NAME, search_str)))!=0) || (nrow(mmi$PRODUCT %>% as_tibble() %>% filter(str_detect(tolower(NAME), tolower(str_extract(pattern="^\\w+", prompt))) ))!=0) ){

                                if (nrow(mmi$PRODUCT %>% as_tibble() %>% filter(str_detect(NAME, search_str)))==0){
                                        search_str <- paste0("(?i)",tolower(str_extract(pattern="^\\w+", prompt)))
                                }
                                
                                df <-
                                        mmi$PRODUCT %>% as_tibble() %>%
                                        mutate(NAME = tolower(NAME)) %>% 
                                        filter(str_detect(NAME, search_str)) %>%
                                        mutate(sim = stringdist::stringdist(NAME, search_str, method = "lcs")) %>% # calculate the stringdistance in NAME
                                        filter(!is.na(ACTIVESUBSTANCECOUNT))
                                
                                if ((nrow(df)==0) || (is.null(nrow(df))) || is.na(max(df$ACTIVESUBSTANCECOUNT))) { # Check if there is an active compound 
                                        
                                        # # MAYBE Check IF PRODUCT is DIETÄRY PRODUCT / Vitamin
                                        
                                        #  print("[No ACTIVESUBSTANCECOUNT]")
                                        df <- NA #"ACTIVESUBSTANCECOUNT=0" %>% as_tibble()
                                        active_subst <- FALSE
                                } else {
                                        n_sub <- df %>% arrange(-desc(sim)) %>% slice(1) %>% pull(ACTIVESUBSTANCECOUNT)
                                        
                                        df <- df %>% 
                                        arrange(-desc(sim)) %>% # Make us of stringdistance
                                        slice(1) %>%
                                        left_join(mmi$ITEM,
                                                  by = c("ID" = "PRODUCTID"),
                                                  suffix = c(".PRODUCT", ".ITEM")) %>%
                                        left_join(
                                                mmi$ITEM_COMPOSITIONELEMENT,
                                                by = c("ID.ITEM" = "ITEMID"),
                                                suffix = c(".ITEM", ".I_CE")
                                        ) %>%
                                        arrange(-desc(SORTNAME)) %>% 
                                                # slice(1:max(ifelse(!is.na(ACTIVESUBSTANCECOUNT), ACTIVESUBSTANCECOUNT, 1))) %>%
                                                slice(1:n_sub) %>% 
                                        left_join(mmi$COMPOSITIONELEMENT, by = c("COMPOSITIONELEMENTID" = "ID")) %>%
                                        left_join(mmi$MOLECULE, by = c("MOLECULEID" = "ID")) %>%
                                        left_join(mmi$ITEM_ATC, by = c("ID.ITEM" = "ITEMID"))
                                        active_subst <- TRUE
                                        # print(paste0(search_str, df %>% unlist %>% paste0()))
                                }
                        } else { # In case the search_str returns 0 results in PRODUCT search ... could be a typo
                    
                                df <-
                                        mmi$PRODUCT_SPLIT %>% as_tibble() %>%
                                        filter(str_detect(pattern=paste0("(?i)^",substring(prompt, 1,1)),NAME)) %>% 
                                        mutate(sim = stringdist::stringdist(NAME_split, prompt, method="lcs")) %>% 
                                        filter(!is.na(ACTIVESUBSTANCECOUNT))
                                                
                                if ((nrow(df)==0) || (is.null(nrow(df))) || is.na(max(df$ACTIVESUBSTANCECOUNT))) {
                                        # print("[No ACTIVESUBSTANCECOUNT]")
                                        df <- NA # "ACTIVESUBSTANCECOUNT=0" %>% as_tibble()
                                        active_subst <- FALSE
                                } else {
                                        df <- df %>% 
                                        arrange(-desc(sim)) %>%
                                        slice(1) %>% 
                                        left_join(mmi$ITEM, by = c("ID" = "PRODUCTID"), suffix = c(".PRODUCT", ".ITEM")) %>% 
                                        left_join(mmi$ITEM_COMPOSITIONELEMENT, by = c("ID.ITEM" = "ITEMID"), suffix = c(".ITEM", ".I_CE")) %>% 
                                        arrange(-desc(SORTNAME)) %>% slice(1:max(ifelse(!is.na(ACTIVESUBSTANCECOUNT),ACTIVESUBSTANCECOUNT,1))) %>% 
                                        left_join(mmi$COMPOSITIONELEMENT, by = c("COMPOSITIONELEMENTID" = "ID")) %>% 
                                        left_join(mmi$MOLECULE, by = c("MOLECULEID" = "ID")) %>%
                                        left_join(mmi$ITEM_ATC, by = c("ID.ITEM" = "ITEMID")) 
                                active_subst <- TRUE
                                # print(paste0("[TYPO | ",df$sim,"] ", prompt))
                                }
                        }
                        
 
                        if (active_subst) {
                                # Get MOLECULE NAMES
                                df <- df %>% 
                                        mutate(name_of_mol = map2(df$UPPERMOLECULEID, df$MOLECULEID, ~{
                                                id <- ifelse(is.na(.x), .y, .x)
                                                res <- mmi$MOLECULE %>% filter(ID == id) %>% pull(NAME)
                                                if(length(res) == 0) {
                                                        res <- df %>% 
                                                                filter(MOLECULEID == id) %>%
                                                                pull(NAME)
                                                }
                                                return(tolower(str_split(res, " ")[[1]][1]))
                                        }) %>% unlist())
                                
                                # IDENTIFY ATC by MOLECULE NAMES
                                nom <- df$name_of_mol
                                class <- atc %>% 
                                        filter(!is.na(name_g)) %>%
                                        mutate(name_split = str_split(name_g, " |-")) %>% unnest(name_split) %>%
                                        mutate(name_split = tolower(name_split)) %>%
                                        filter(name_split %in% nom) %>%
                                        filter(str_detect(pattern=paste0("^",str_sub(df$ATCCODE,1,1)[[1]]), atc))
                                        
                             # MAYBE #  filter(str_detect(pattern=nom, name_split)) %>% 
                                        # drop_na(class_e) %>% 
                                        # # mutate(sim = stringdist::stringdist(name_split, prompt, method="lcs")) %>% 
                                        # arrange(-desc(sim)) %>%
                                        
                                
                                if (nrow(class)==0) {
                                        class <- atc %>% 
                                                filter(!is.na(name_g)) %>%
                                                mutate(name_split = str_split(name_e, " |-")) %>% unnest(name_split) %>%
                                                mutate(name_split = tolower(name_split)) %>% 
                                                filter(name_split %in% nom)
                                        if (nrow(class)==0) {
                                                df <- df %>% 
                                                        mutate(name_of_mol_try = substr(str_split(name_of_mol, "[- ]")[[1]][1],1,10)) 
                                                nom2 <- gsub("[^a-zA-Z ]", "",substr(str_split(df$name_of_mol, "[- ]")[[1]][1],1,10))
                                                if (nom2=="") { nom2 <- str_split(prompt, "[-| ]")[[1]][1] }
                                                class <- atc %>% 
                                                        filter(!is.na(name_g)) %>%
                                                        mutate(name_split = str_split(name_g, " |-")) %>% unnest(name_split) %>%
                                                        mutate(name_split = tolower(name_split)) %>%
                                                        mutate(name_split = gsub("[^a-zA-Z ]", "", name_split)) %>% 
                                                        filter(str_detect(pattern = nom2, name_split)) %>% 
                                                        filter(str_detect(pattern=paste0("^",str_sub(df$ATCCODE,1,1)[[1]]), atc))
                                                df$name_of_mol <- df$name_of_mol_try
                                        }
                                }
                                        
                                if ( class %>% select(name_split) %>% unique() %>% nrow() == 1 ){
                                        class <- class %>% 
                                                arrange(-desc(atc)) %>% slice(1)
                                } else {
                                        class_bkp <- class
                                        class <- class %>% 
                                            mutate(str = str_count(name_g)) %>%
                                                filter(str_detect(pattern=paste0("^",str_sub(df$ATCCODE,1,1)[[1]]), atc)) %>% 
                                                mutate(name_g = tolower(name_g)) %>%
                                                filter(name_g %in% nom) %>% 
                                                distinct(name_g, .keep_all = TRUE)
                                        if ( (nrow(class)==0) && (nom2!=0) ) {
                                                class <- class_bkp %>% 
                                                mutate(str = str_count(name_g)) %>%
                                                        filter(str_detect(pattern=paste0("^",str_sub(df$ATCCODE,1,1)[[1]]), atc)) %>% 
                                                        mutate(name_g = tolower(name_g)) %>%
                                                        filter(name_g %in% nom2) %>% 
                                                        distinct(name_g, .keep_all = TRUE)
                                        }
                                                
                                }
                                            
                               # JOIN 
                               # df <- df %>% select(ATCCODE, name_of_mol) %>% 
                               #         left_join(class, by = c("name_of_mol" = "name_split")) %>%
                               #         select(atc_code = ATCCODE, name_g, name_e, grp, grp_g, grp_e, class, class_g, class_e, name_of_mol)
                               df <- class %>% 
                                       select(atc_code = atc, name_g, name_e, grp, grp_g, grp_e, class, class_g, class_e, name_of_mol = name_g)
                               active_subst <- TRUE
                        }
                }
        } else {
                df <- as_tibble(NA_character_)
                return(df)
                # print(paste0("[NA]", search_str))
        }
        
        # if (active_subst==FALSE) { # Check if prompt is/fits a category 
        #         df <- atc %>% 
        #                 # filter(str_detect(pattern=prompt, tolower(name))) %>% 
        #                 mutate(sim1 = stringdist::stringdist(name, prompt, method = "lcs")) %>%
        #                 mutate(sim2 = stringdist::stringdist(atc_name.x, prompt, method = "lcs")) %>% 
        #                 mutate(sim3 = stringdist::stringdist(atc_name.y, prompt, method = "lcs")) %>%
        #                 drop_na(sim1, sim2, sim3) 
        #         # determine which column has minimum value
        #         i <- which.min(sapply(df %>% select(sim1, sim2, sim3), min, na.rm = TRUE))
        #         df <- df %>% arrange(-desc(!!sym(paste0("sim",i)))) %>% slice(1) %>%
        #                 mutate(class = str_sub(ATC, 1, 3)) %>% 
        #                 mutate(atc_class_engl = atc %>% filter(ATC=="C09") %>% pull(atc_name.y)) %>% 
        #                 mutate(atc_code = NA) %>% 
        #                 mutate(atc_name_engl = NA) %>% 
        #                 mutate(atc_name_ger = NA) %>% 
        #                 select(atc_code, atc_name_engl, atc_name_ger, 
        #                        atc_grp = grp, atc_grp_name_engl = atc_name.y, atc_grp_ger = grp_name, 
        #                        atc_class = class, atc_class_engl, atc_code2 = ATC)
        # }
        
        # Group/Class Check
        df <- df %>% 
                # mutate(class_e = replace_na(class_e, atc %>% filter(class==class) %>% drop_na(class_e) %>% slice(1) %>% pull(class_e))) %>%
                # mutate(grp_e   = replace_na(grp_e, atc %>% filter(grp==grp) %>% drop_na(grp_e) %>% slice(1) %>% pull(grp_e))) %>%
                as_tibble()
        
        # Return
        return(df)
}

