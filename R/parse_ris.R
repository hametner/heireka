#' @title RIS Format Parser 
#'
#' @param RIS File 
#' @return tibble
#' @examples ris <- parse_ris("references-2024-10-04.ris")
parse_ris <- function(file_path) {
        library(tidyverse)
        # Read the RIS file
        lines <- readLines(file_path, warn = FALSE)
        
        # Initialize variables
        records <- list()
        current_record <- list()
        
        # Regular expression to match RIS lines: TAG  - VALUE
        ris_line_pattern <- "^([A-Z0-9]{2})  - (.*)$"
        
        for (line in lines) {
                # Check for the end of a record
                if (grepl("^ER  -", line)) {
                        records <- append(records, list(current_record))
                        current_record <- list()
                } else if (grepl(ris_line_pattern, line)) {
                        # Extract tag and value using regex
                        matches <- regmatches(line, regexec(ris_line_pattern, line))[[1]]
                        tag <- matches[2]
                        value <- matches[3]
                        
                        # Handle multiple entries for the same tag
                        if (tag %in% names(current_record)) {
                                current_record[[tag]] <- c(current_record[[tag]], value)
                        } else {
                                current_record[[tag]] <- value
                        }
                }
                # Ignore lines that do not match the RIS pattern
        }
        
        # Convert the list of records into a data.frame
        if (length(records) == 0) {
                warning("No records found in the RIS file.")
                return(data.frame())
        }
        
        # Identify all unique tags across all records
        all_tags <- unique(unlist(lapply(records, names)))
        
        # Function to process each record into a named list with all tags
        process_record <- function(rec) {
                sapply(all_tags, function(tag) {
                        if (tag %in% names(rec)) {
                                # If multiple entries exist for a tag, concatenate them with a separator
                                if (length(rec[[tag]]) > 1) {
                                        paste(rec[[tag]], collapse = "; ")
                                } else {
                                        rec[[tag]]
                                }
                        } else {
                                NA  # Assign NA if the tag is missing in the record
                        }
                }, USE.NAMES = FALSE)
        }
        
        # Apply the processing to all records and create a data.frame
        df_matrix <- do.call(rbind, lapply(records, process_record))
        df <- as.data.frame(df_matrix, stringsAsFactors = FALSE)
        
        # Assign column names based on tags
        colnames(df) <- all_tags
        
        # Optional: Clean up specific tags if needed (e.g., remove trailing whitespace)
        df <- data.frame(lapply(df, function(col) {
                if (is.character(col)) trimws(col) else col
        }), stringsAsFactors = FALSE) %>% as.tibble()
        
        
        return(df)
}

