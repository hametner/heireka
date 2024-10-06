#' @title ask_pubmed 
#' @description Perform a Pubmed Search with a standard query
#' @param db Standard with "pubmed"
#' @param abstracts_only Retrieve abstracts only (beta-version)
#' @param dumpfile if TRUE writes out a csv file, instead of returning a data.frame.
#' @param dump_citations if TRUE writes a txt file containing the citations in "bibtex" (=Standard) Format or "apa"
#' @param output_file Standard to "citations.bib" - if alread present text will be appended
#' @param associated_citations if TRUE links lists of a) predecessor and b) successor articles are generated (cave: slow) 
#' @return A tibble
ask_pubmed <-
        function(query,
                 db = "pubmed",
                 abstracts_only = FALSE,
                 dumpfile = FALSE,
                 dump_citations = TRUE,
                 citations_output_file = "citations.bib",
                 associated_citations = FALSE) {
                # Load required libraries
                require(rentrez)
                require(xml2)
                require(xml)
                require(dplyr)
                require(tidyr)
                require(purrr)
                require(tidyverse)
                # query <- "sex differences in stroke patients with renal disease and pregnancy"
                # db <- "pubmed"
                # use_history set to TRUE
                Sys.setenv(ENTREZ_KEY = "c372fb9a5a18c9da03f7ef3f831753413a08")
                
                if (is.character(query)) {
                        search_results <- entrez_search(db = db,
                                                        term = query,
                                                        use_history = TRUE)
                } else
                        return("Error: Please supply a valid query - for example \"sex difference in stroke\"")
                
                # Get the total number of records and web environment variables
                total_records <- search_results$count
                if (total_records<1) return("No citations were found given your query.")
                webenv <- search_results$web_history$WebEnv
                query_key <- search_results$web_history$QueryKey
                
                # Set the desired number of records to fetch and calculate the number of iterations
                retmax <- 20
                iterations <- ceiling(total_records / retmax)
                
                # Initialize a list to store the results
                res <- vector("list", length = iterations)
                
                # Fetch the records in batches using the web history parameters
                for (i in seq_len(iterations)) {
                        retstart <- (i - 1) * retmax
                        res[[i]] <-
                                entrez_fetch(
                                        db = db,
                                        web_history = search_results$web_history,
                                        retstart = retstart,
                                        retmax = retmax,
                                        rettype = "abstract",
                                        retmode = "xml"
                                )
                }
                
                # Extract relevant information and store it in a tibble
                df <- map_df(res, function(y) {
                        articles <- read_xml(y) %>% xml_find_all("//PubmedArticle")
                        
                        map_df(articles, ~ {
                                tibble(
                                        PMID = .x %>% xml_find_first(".//PMID") %>% xml_text(),
                                        year = .x %>% xml_find_first(".//PubDate/Year") %>% xml_text(),
                                        journal = .x %>% xml_find_first(".//Journal/ISOAbbreviation") %>% xml_text(),
                                        volume = .x %>% xml_find_first(".//JournalIssue/Volume") %>% xml_text(),
                                        pages = .x %>% xml_find_first(".//StartPage") %>% xml_text(),
                                        month = .x %>% xml_find_first(".//PubDate/Month") %>% xml_text(),
                                        day = .x %>% xml_find_first(".//PubDate/Day") %>% xml_text(),
                                        title = .x %>% xml_find_first(".//ArticleTitle") %>% xml_text(),
                                        abstract = .x %>% xml_find_all(".//AbstractText") %>% xml_text() %>% paste(collapse = " "),
                                        authors = .x %>% xml_find_all(".//Author") %>%
                                                map_chr(
                                                        ~ paste(
                                                                .x %>% xml_find_first(".//LastName") %>% xml_text(),
                                                                .x %>% xml_find_first(".//ForeName") %>% xml_text()
                                                        )
                                                ) %>% paste(collapse = "; "),
                                        doi = .x %>% xml_find_first(".//ArticleId[@IdType='doi']") %>% xml_text(),
                                        affil = .x %>% xml_find_first(".//Affiliation") %>% xml_text(),
                                        
                                        
                                )
                        })
                })
                
                # append predecessor and successor articles if associated_citations=TRUE
                if ( associated_citations ) {
                df <- df %>%
                        mutate(linked_articles = map(PMID, get_linked_articles)) %>%
                        mutate(predecessor = map(linked_articles, "predecessor"),
                               successor = map(linked_articles, "successor")) %>%
                        select(-linked_articles)
                }
                
                # articles <- sapply(res, function(x) {
                #         articles <- read_xml(x) %>% xml_find_all("//PubmedArticle")
                # })
                # articles <- do.call(c, articles)
                # 
                # articles <- lapply(res, function(x) {
                #         articles <- read_xml(x) %>% xml_find_all("//PubmedArticle")
                # })
                # articles <- unlist(articles, recursive = FALSE)
                # # articles <- do.call(c, articles)
                # 
                # df <- map_df(articles, ~ {
                #                 tibble(
                #                         PMID = .x %>% xml_find_first(".//PMID") %>% xml_text(),
                #                         Year = .x %>% xml_find_first(".//Year") %>% xml_text(),
                #                         Journal = .x %>% xml_find_first(".//ISOAbbreviation") %>% xml_text(),
                #                         ArticleTitle = .x %>% xml_find_first(".//ArticleTitle") %>% xml_text(),
                #                         Abstract = .x %>% xml_find_all(".//AbstractText") %>% xml_text() %>% paste(collapse = " "),
                #                         AuthorNames = .x %>% xml_find_all(".//Author") %>%
                #                                 map_chr(
                #                                         ~ paste(
                #                                                 .x %>% xml_find_first(".//LastName") %>% xml_text(),
                #                                                 .x %>% xml_find_first(".//ForeName") %>% xml_text()
                #                                         )
                #                                 ) %>% paste(collapse = "; "),
                #                         Citation =  get_citation_from_pmid(pmid=articles[[1]] %>% xml_find_first(".//PMID") %>% xml_text(), format = "apa"),
                #                         doi = .x %>% xml_find_first(".//ArticleId[@IdType='doi']") %>% xml_text(),
                #                         affil = .x %>% xml_find_first(".//Affiliation") %>% xml_text()
                #                 )
                #         })
                # # df <- df %>%
                #         mutate(Citations = get_citation_from_pmid(pmid = PMID, format = "apa"))
                # i <- 1
                # c <- list()
                # for (i in seq(1:length(df$PMID))){
                #         c[[i]] <- get_citation_from_pmid(df$PMID[i], format = "apa")
                #         Sys.sleep(5)
                #         print(i)
                # }
                # c <- lapply(df$PMID, function(x) get_citation_from_pmid(pmid=x, format = "apa"))
                # 
                # if_else( !is.na(PMID),
                #          .x %>% xml_find_first(".//Citation") %>% xml_text(),
                #          get_citation_from_pmid(PMID, format = "apa") ),
                # 
                # if (is.na(df$Citation)) {
                #         df <- df %>% mutate(Citation = get_citation_from_pmid(PMID, format="apa"))
                # }
                
                # if bib file is requested (dump_citations=TRUE)
                if (dump_citations){
                        # citation <- sapply(df$PMID, function(x) get_citation_from_pmid(x))
                        # citations <- sapply(df$PMID, get_citation_from_pmid)
                        # citations_text <- paste(citations, collapse = "\n")
                        # # append to file
                        # cat(citations_text, file = citations_output_file, append = TRUE, sep = "\n")
                        # # writeLines(citations_text, output_file, append = TRUE)
                        
                        citations <- get_bibtex2(df)
                        citations_text <- paste(citations, collapse = "\n")
                        cat(citations_text, file = citations_output_file, append = TRUE, sep = "\n")
                }
                
                # in case of abstract_only=TRUE (beta)
                if (abstracts_only) {
                        # Get abstract details
                        abstract_texts <-
                                xml_find_all(read_xml(raw_abs), "//AbstractText")
                        abstract_data <-
                                tibble(Label = character(), Content = character())
                        for (abstract_text in abstract_texts) {
                                label <-
                                        xml_attr(abstract_text, "Label")
                                content <-
                                        abstract_text %>% xml_text()
                                # Add the extracted label and content to the tibble
                                abstract_data <-
                                        add_row(abstract_data,
                                                Label = label,
                                                Content = content)
                        }
                        if (dumpfile) {
                                write.csv2(abstract_data,
                                           paste0("abstracts_only_query_#_", query, ".csv"))
                                message("... file saved.")
                        } else {
                                return (abstract_data)
                        }
                } else {
                        if (dumpfile) {
                                write.csv2(df,
                                           paste0("pubmed_records_query_#_", query, ".csv"))
                                message("... file saved.")
                        } else {
                                return (df)
                        }
                }
                
                
        }


#' @title is_doi 
#' @description R function to check doi format
#' @param input_string String to be assessed if valid doi string
#' @return boolean
# R code for checking if a string is in DOI format
is_doi <- function(input_string) {
        # if (is.null(input_string) || input_string == "0" || trimws(input_string) == "") {
        #         return(FALSE)
        # }
        if (is.null(input_string) || length(input_string) == 0 || input_string == "0" || trimws(input_string) == "") {
                return(FALSE)
        }
        input_string <- gsub("^doi:\\s*", "", input_string, ignore.case = TRUE)
        
        doi_regex <- "^10\\.\\d{4,9}\\/[-._;()/:A-Za-z0-9]+$"
        return(grepl(doi_regex, input_string, perl = TRUE))
}


#' @title get_bibtex 
#' @description R function to extract bibtex format from a single PMID using eutils
#' @param format
#' @param article
#' @param authors
#' @param title
#' @param journal
#' @param year
#' @param volume
#' @param issue
#' @param pages
#' @param doi
#' @return boolean
get_bibtex <-
        function(format,
                 article,
                 authors,
                 title,
                 journal,
                 year,
                 volume,
                 issue,
                 pages,
                 doi,
                 abstract) {
                if (format == "bibtex") {
                        bibtex <- paste0(
                                "@article{",
                                if (is.character(article$authors[[1]][[1]])) {
                                        strsplit(article$authors[[1]][[1]], " ")[[1]][1]
                                } else {
                                        "unknown_author"
                                },
                                year,
                                ",\n",
                                "  author = {",
                                authors,
                                "},\n",
                                "  title = {",
                                title,
                                "},\n",
                                "  journal = {",
                                journal,
                                "},\n",
                                "  year = {",
                                year,
                                "},\n",
                                "  volume = {",
                                volume,
                                "},\n",
                                "  number = {",
                                issue,
                                "},\n",
                                "  pages = {",
                                pages,
                                "},\n",
                                "  doi = {",
                                doi,
                                "}\n",
                                "  abstract = {",
                                abstract,
                                "}"
                        )
                        return(bibtex)
                }
        }

format_citation <- function(authors) {
        # Split the string into individual author names
        author_names <- str_split(authors, "; ")[[1]]
        
        # Extract the last name and initials for each author
        author_parts <- str_split(author_names, " ")
        last_names <- sapply(author_parts, "[", 1)
        initials <- sapply(author_parts, function(x) {
                paste0(substring(x[-1], 1, 1), collapse = "")
        })
        
        # Combine the last names and initials in the desired format
        formatted_authors <- mapply(function(last, init) {
                paste0("{", last, "}, {", init, "}")
        }, last_names, initials, SIMPLIFY = FALSE)
        
        # Combine the formatted author names with "and" separators
        citation <- paste0(formatted_authors, collapse = " and ")
        
        
        return(citation)
}

#' @title get_bibtex 
#' @description R function to extract bibtex format from a single PMID using eutils
#' @param authors
#' @param title
#' @param journal
#' @param year
#' @param volume
#' @param issue
#' @param pages
#' @param doi
#' @return boolean
get_bibtex2 <-
        function(df) {
                bibtex <- df %>%
                        rowwise() %>%
                        mutate(citation = {
                                paste0(
                                        "@article{",str_split(authors, "\\s")[[1]][1], year,
                                        ",\n",
                                        " author = {", format_citation(authors),
                                        "},\n",
                                        "  title = {",
                                        title,
                                        "},\n",
                                        "  journal = {",
                                        journal,
                                        "},\n",
                                        "  year = {",
                                        year,
                                        "},\n",
                                        "  volume = {",
                                        volume,
                                        "},\n",
                                        "  number = {",
                                        issue,
                                        "},\n",
                                        "  pages = {",
                                        pages,
                                        "},\n",
                                        "  doi = {",
                                        doi,
                                        "}\n",
                                        "  abstract = {",
                                        abstract,
                                        "}", 
                                        "\n}"
                                )
                        }) %>%
                        ungroup() %>%
                        pull(citation)
                
                        return(bibtex)
                }










pubmed_search <- function(query=NULL, id=NULL, retmax = 20) {
        if (!is.null(id)) {
                search_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/", "esearch.fcgi?db=pubmed&id=")
                }
        
        search_params <- list(
                db = "pubmed",
                id = id,
                term = query,
                retmax = retmax,
                api_key = "c372fb9a5a18c9da03f7ef3f831753413a08"
        )
        search_response <- GET(search_url, query = search_params)
        search_content <- content(search_response, "parsed")
        return(search_content)
}

# install.packages(c("httr", "jsonlite"))
get_citation_from_pmid <- function(pmid, format="bibtex") {
        require(httr)
        require(jsonlite)
        api_key_ncbi <- "c372fb9a5a18c9da03f7ef3f831753413a08"
        url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=", pmid, "&retmode=json", "&api_key=",paste0(api_key_ncbi))
        # url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=", pmid, "&api_key=",paste0(api_key_ncbi))
        
        response <- GET(url)
        # response <- GET(url, config(handle = custom_handle))
        # custom_handle <- curl::new_handle()
        # curl::handle_setopt(custom_handle, http_version = 1L)
        content <- content(response, "text", encoding = "UTF-8")
        data <- fromJSON(content)
        article <- data$result[[as.character(pmid)]]
        
        # Extract citation information
        title <- sub("\\.$", "", article$title)
        
        if (!is.null(article$authors) &&
            is.character(article$authors[[1]])) {
                authors <- sub("\\.$", "", paste0(article$authors[[1]], collapse = ", "))
        } else {
                authors = "NA"
        }
        
        journal <- sub("\\.$", "", article$fulljournalname)
        
        if (!is.null(article$pubdate) &&
            is.character(article$pubdate[[1]])) {
                year <- strsplit(as.character(article$pubdate), " ")[[1]][1]
        } else {
                year <- "NA"
        }
        
        if (!is.null(article$volume) &&
            is.character(article$volume[[1]])) {
                volume <- sub("\\.$", "", article$volume)
        } else {
                volume <- "NA"
        }
        
        if (!is.null(article$issue) &&
            is.character(article$issue[[1]])) {
                issue <- sub("\\.$", "", article$issue)
        } else {
                issue <- "NA"
        }
        
        if (!is.null(article$pages) &&
            is.character(article$pages[[1]])) {
                pages <- sub("\\.$", "", article$pages)
        } else {
                (pages <- "NA")
        }
        
        if (!is.null(article$elocationid) &&
            is.character(article$elocationid[[1]])) {
                # doi_regex <- "^(doi:\\s*)?10\\.\\d{4,9}\\/[-._;()/:A-Za-z0-9]+$"
                
                if (is_doi(article$elocationid)) {
                        doi <- gsub("^.?doi:\\s*", "", article$elocationid, ignore.case = TRUE)
                } else {
                        doi1 <- paste0(article$articleids %>% filter(idtype == "doi") %>% select(value) %>% unlist())
                        if (is_doi(doi1)) {
                                doi <- doi1
                        }
                }
                
        } else {
                doi <- "NA"
        }
        
        # # if (!is.null(article$pages) &&
        #     is.character(article$pages[[1]])) {
        #         pages <- sub("\\.$", "", article$pages)
        # } else {
        #         (pages <- "NA")
        # }
        pages = ""
        abstract = ""

        
        if (format=="bibtex") {
                bibtex <- get_bibtex(format, article, authors, title, journal, year, volume, issue, pages, doi, abstract)
                return(bibtex)
        }
        
        if (format=="apa") {
                citation <- paste0(
                                paste(authors, paste0("(",year,")"), title, journal, sep = ". "),
                                paste0(volume, "(", issue, "), ", pages, ". ", "doi: ",doi, ".")
                        )
                return(citation)
        }

}

#' @title extract_pubmed_xml 
#' @description Helper function to get_linked_articels 
#' @param url a pubmed url using eutils is provided by the user e.g. 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id='
#' @return list of 'PMID'
extract_pubmed_xml <- function(url){
        require(xml2)
        api_key_ncbi <- "c372fb9a5a18c9da03f7ef3f831753413a08"
        url <- paste0(url, "&api_key=",api_key_ncbi)
        response <- GET(url)
        content <- content(response, "text", encoding = "UTF-8")
        xml <- read_xml(content(response, "text", encoding = "UTF-8"))
        pmids <- xml_find_all(xml, ".//Link/Id") %>% xml_text()
        # Sys.sleep(0.2)
        return(pmids)
}

#' @title get_linked_articles 
#' @description By providing 'PMID' associated articles listed in Pubmed are identified a) successors and b) predecessors 
#' @param pmid a pubmed 'PMID'
#' @return list of two (successor, predecessor)
get_linked_articles <- function(pmid){
        require(httr)
        require(jsonlite)
        require(purrr)
        # Article was 'cited by' (successors)
        successor = paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=", pmid)
        predecessor = paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id=", pmid)
        pmids <-
                map(set_names(list(successor, predecessor), c("successor", "predecessor")), ~ extract_pubmed_xml(.x))
        return(pmids)
}



if (!requireNamespace("rcrossref", quietly = TRUE)) {
        install.packages("rcrossref")
}
if (!requireNamespace("RefManageR", quietly = TRUE)) {
        install.packages("RefManageR")
}
library(rcrossref)
doi <- "10.1161/STROKEAHA.110.592642"
citation_info <- cr_cn(doi, format = "bibtex")







get_pmid <- function(doi) {
        # install and load the 'rentrez' package
        if(!requireNamespace("rentrez", quietly = TRUE)){
                install.packages("rentrez")
        }
        library(rentrez)
        
        # search for the PubMed ID using the 'rentrez' function 'entrez_search'
        my_search <- entrez_search(db="pubmed", term=paste0(doi, "[DOI]"))
        
        return(my_search[["ids"]])
}

# Load required libraries
library(stringr)
library(rcrossref)

# Define the function
replace_citation_numbers <- function(text, doi_list) {
        # Extract citation numbers from the text
        citation_numbers <- unique(str_extract_all(text, "\\d+")[[1]])
        
        # Replace citation numbers with citation keys
        for (i in 1:length(citation_numbers)) {
                # Get the DOI for the current citation number
                doi <- doi_list[as.integer(citation_numbers[i]) - 9] # Adjust the index based on the starting citation number in the text
                
                # Retrieve citation key from CrossRef
                citation_key <- paste0("@", str_extract(cr_cn(doi, format = "bibtex"), "(?<=\\bkey\\s=\\s\\{)\\w+"))
                
                # Replace the citation number with the citation key
                text <- gsub(paste0(citation_numbers[i]), citation_key, text)
        }
        
        return(text)
}

# Example usage
# doi_list <- c("10.1161/STROKEAHA.110.592642")
# text <- 'Epidemiological data on stroke in Germany are 9 scarce and this study is only the second population-based stroke register without any age restriction in our country.10 Diagnosis of stroke was based on the World Health Organization definition and therefore on clinical data. We are aware of recent changes in tissue-based definition of stroke and TIA.11'

# a <- get_linked_articles(get_pmid(doi))




















