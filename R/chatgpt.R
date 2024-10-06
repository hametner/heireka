api_key = "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"
#' @title Ask Chatgpt
#' @description  Conntects with OpenAI API to access language models
#' @param prompt Fill in the prompt you would normally 
#' @model gpt-3.5-turbo for speed and gpt-4 for accuracy # this is still 2024 :) 
ask_chatgpt <- function(prompt, model = "gpt-4o", temp = 0) {
        require(httr)
        require(stringr)
        response <- POST(
                url = "https://api.openai.com/v1/chat/completions", 
                add_headers(Authorization = paste("Bearer", api_key)),
                content_type_json(),
                encode = "json",
                body = list(
                        model = model,
                        temperature = temp,
                        messages = list(list(
                                role = "user", 
                                content = prompt
                        ))
                )
        )
        str_trim(content(response)$choices[[1]]$message$content)
}



# gpt-3.5-turbo-0125
# gpt-4-1106-preview. 41.56 sec
# gpt-4-0125-preview
