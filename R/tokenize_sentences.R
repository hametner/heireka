# Function to tokenize a txt file into sentences
tokenize_sentences <- function(file_path) {
        # Read the txt file
        txt <- readLines(file_path, encoding = "UTF-8")
        
        # Combine lines into a single string
        txt_combined <- paste(txt, collapse = " ")
        
        # Tokenize the string into sentences
        sentence_pattern <- "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<![A-Z]\\.)(?<=\\.|\\?|\\!)\\s"
        sentences <- str_split(txt_combined, sentence_pattern, simplify = FALSE) %>% 
                enframe(name = NULL, value = "sentences") %>% 
                unnest(cols = c(sentences))
        return(data.frame(sentences = sentences))
        
}

# Example usage
file_name <- "sc_hiv"
file_path <- "input/subchapter/sc_hiv.txt"
sentences <- tokenize_sentences(file_path)
writeLines(sentences, "input/subchapter/sc_hiv_tokenized.txt")
print(sentences)
