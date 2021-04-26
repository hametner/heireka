#' @title left
#' 
#' @description helper function that mimics excel function of left(). Take text as first parameter and number of characters to be truncated from left hand side. Uses substr function from base R
#' @param text 
#' @param num_char 
#'
#' @return truncated string
#' @export
left <- function(text, num_char) {
        substr(text, 1, num_char)
}

#' @title mid
#' @description helper function that mimics excel function mid. Takes text as first parameter and start and end character position as second and third
#' @param text A string to be supplied
#' @param start_num charcter start position 
#' @param num_char character end position
#'
#' @return
#' @export
mid <- function(text, start_num, num_char) {
        substr(text, start_num, start_num + num_char - 1)
}

#' @title right
#'
#' @param text 
#' @param num_char 
#'
#' @return
#' @export
#'
#' @examples
right <- function(text, num_char) {
        substr(text, nchar(text) - (num_char-1), nchar(text))
}

unnest_all <- function(df) {
        list_columns <- df %>% keep(is.list) %>% discard(~any(map_lgl(., is_empty))) %>% names()
        # list_columns <- df %>% keep(is.list) %>% names()
        if (length(list_columns) == 0) {
                return(df)
        }
        
        for (list_column in list_columns) {
                df <-
                        df %>%
                        unnest_wider(list_column, names_sep = "_")
        }
        unnest_all(df)
}
