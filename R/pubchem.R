require("rvest")
# library(rvest)

# Specify the URL of the webpage containing the table
url <- "https://www.fda.gov/regulatory-information/fdaaa-implementation-chart/usp-therapeutic-categories-model-guidelines"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract all tables from the webpage
tables <- webpage %>% html_nodes("table") %>% html_table(fill = TRUE, header = TRUE, trim = TRUE)
df <- tables[[1]] %>% as_tibble()
df[df == ""] <- NA
df <- df %>% group_by(`Therapeutic Category`) %>% fill(`Pharmacologic Class`, .direction = "down")
df$ID <- seq(1:nrow(df))
df <- df %>% select(ID, cat = `Therapeutic Category`, class = `Pharmacologic Class`, type = `Formulary Key Drug Types`)
write.csv2(df, "output/medication_cat_class_type.csv")







library(httr)
library(jsonlite)

search_pubchem <- function(drug_name) {
        url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", drug_name, "/JSON")
        response <- GET(url)
        
        if (status_code(response) == 200) {
                data <- content(response, "parsed")
                cid <- data$PC_Compounds[[1]]$id$cid
                # Synonyms are typically stored under the 'value' field in the list of properties. However, it can vary depending on the specific compound.
                synonyms <- NULL 
                for (prop in data$PC_Compounds[[1]]$props) {
                        if (prop$urn$name == "Synonyms") {
                                synonyms <- prop$value$sval
                                break
                        }
                }
                return(list(cid = cid, synonyms = synonyms))
        } else {
                print(paste("Error:", status_code(response)))
                return(NULL)
        }
}

result <- search_pubchem('spirobeta')




library(httr)
library(RCurl)
library(jsonlite)

get_compound_info <- function(compound_name) {
        # Define the base URL for the PubChem API
        base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
        
        # Build the URL for the API request
        api_url <- paste0(base_url, "/compound/name/", compound_name, "/JSON")
        
        # Make the API request and store the response
        response <- GET(api_url)
        
        # Check if the request was successful
        if (response$status_code == 200) {
                # Parse the response content as JSON
                content <- content(response, "text")
                json_data <- fromJSON(content)
                
                # Extract the information about the compound
                substance <- json_data$PC_Compounds[[1]]$props[[1]]$value$sval
                synonyms <- json_data$InformationList$Information$Synonym
                
                # Return a list with the substance and synonyms
                return(list("substance" = substance, "synonyms" = synonyms))
        } else {
                # If the request was not successful, return an error message
                return(paste("Error: ", response$status_code))
        }
}

# Test the function
get_compound_info("spironolacton")




library(httr)
library(jsonlite)

get_compound_name <- function(query) {
        # Define the base URL for the NCBI Entrez API
        base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
        
        # Build the URL for the API request
        api_url <- paste0(base_url, "/esearch.fcgi?db=pccompound&term=", query, "&retmode=json")
        
        # Make the API request and store the response
        response <- GET(api_url)
        
        # Check if the request was successful
        if (response$status_code == 200) {
                # Parse the response content as JSON
                content <- content(response, "text")
                json_data <- fromJSON(content)
                
                # Extract the first compound ID from the results
                cid <- json_data$esearchresult$idlist[1]
                
                ...
}

# Test the function
get_compound_name("spirobeta")




library(httr)
library(jsonlite)

get_compound_name <- function(query) {
        # Define the base URL for the NCBI Entrez API
        base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
        
        # Build the URL for the API request
        api_url <- paste0(base_url, "/esearch.fcgi?db=pccompound&term=", query, "&retmode=json")
        
        # Make the API request and store the response
        response <- GET(api_url)
        
        # Check if the request was successful
        if (response$status_code == 200) {
                # Parse the response content as JSON
                content <- content(response, "text")
                json_data <- fromJSON(content)
                
                # Extract the first compound ID from the results
                cid <- json_data$esearchresult$idlist[1]
                
                # Build the URL for the second API request to get the compound's name
                name_api_url <- paste0(base_url, "/esummary.fcgi?db=pccompound&id=", cid, "&retmode=json")
                
                # Make the second API request and store the response
                name_response <- GET(name_api_url)
                
                # Check if the second request was successful
                if (name_response$status_code == 200) {
                        # Parse the second response content as JSON
                        name_content <- content(name_response, "text")
                        name_json_data <- fromJSON(name_content)
                        
                        # Extract the compound's name from the results
                        compound_name <- name_json_data$result[[as.character(cid)]]$meshheadinglist
                        compound_syn <- name_json_data$result[[as.character(cid)]]$synonymlist
                        drug_syn <- name_json_data$result[[as.character(cid)]]$meshtermlist
                        class <- name_json_data$result[[as.character(cid)]]$pharmactionlist
                        
                        # Return the CID and the compound's name
                        return(list(cid = cid, name = compound_name, syn = drug_syn, class = class))
                } else {
                        # If the second request was not successful, return an error message
                        return(paste("Error: ", name_response$status_code))
                }
        } else {
                # If the request was not successful, return an error message
                return(paste("Error: ", response$status_code))
        }
}


get_compound_info <- function(cid) {
        # Define the base URL for the PubChem API
        base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
        
        # Build the URL for the API request
        api_url <- paste0(base_url, "/compound/cid/", cid, "/synonyms/JSON")
        
        # Make the API request and store the response
        response <- GET(api_url)
        
        # Check if the request was successful
        if (response$status_code == 200) {
                # Parse the response content as JSON
                content <- content(response, "text")
                json_data <- fromJSON(content)
                
                # Extract the information about synonyms
                synonyms <- json_data$InformationList$Information$Synonym[[1]]
                
                
                # Return a list with the substance and synonyms
                return(list("cid" = cid, "synonyms" = synonyms))
        } else {
                # If the request was not successful, return an error message
                return(paste("Error: ", response$status_code))
        }
}

# Test the functions
cid <- get_compound_name("spirobeta")
get_compound_info(cid)







library(rvest)

# Define the URL of the medication page on MedlinePlus
# Replace 'drugname' with the actual name of the drug
url <- paste0("https://medlineplus.gov/druginfo/meds/a", drugname, ".html")
url <- paste0("https://www.gelbe-liste.de/suche?term=", drugname)
# Read the HTML of the webpage
webpage <- read_html(url)

# Extract the substance name using the appropriate CSS selector
# Replace 'css_selector' with the actual CSS selector of the substance name on the webpage
substance_name <- webpage %>%
        html_nodes("a") %>%
        html_text()

print(substance_name)












