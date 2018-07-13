
#' Stop PropagHateWithoutPrompt function
#'
#' This function allows you to evaluate if a set of text messages constains hate speech.
#' @param data_frame, is a data.frame containing a column named "text".
#' @param hate_type, is the type of hate you want to measure. It can be general "hate" speech, "sexism" or "racism".
#' @param language, we provide this funtion both for Enlish "en" and Portuguese "pt".
#' @keywords hate speech, racism, sexism
#' @export
#' @examples texts_data_frame <- data.frame(id = c(1,2,3), texts_data_frame <- data.frame(id = c(1,2,3), text = c("Call me sexist but female sports anchors r the worst. Makes me uncomfortable when women know more about sports than I do", "May Allah bless him with 72 virgin pigs.", "Stop sending me anonymous valentines things. I m pretty sure they are")))
#' stopPropagHateWithoutPrompt(texts_data_frame, "hate", "en")
stopPropagHateWithoutPrompt <- function(data_frame, hate_type, language){
  
  # control of inputs
  try(
    if(length(intersect(colnames(data_frame),"text")) == 0){
      stop("No column named text in data_frame argument.")
    } else {
      try(
        if(length(intersect(hate_type, c("hate", "sexism", "racism"))) == 0){
          stop("The hate type is not available. Try with hate, sexism or racism.")
        } else {
          try(
            if(length(intersect(language, c("en", "pt", "racism"))) == 0){
              stop("The language is not available. Try with en (English) or pt (Portuguese).")
            } else {
              
              if (!file.exists("folder_name.txt")){
                
                #download keras files
                filepath <- "keras_files.zip"
                url <- "https://github.com/paulafortuna/hate_speech_deep_learning_models/zipball/master"
                downloader::download(url, destfile = filepath, mode = "wb")
                
                # unzip and get folder name
                folder_name <- unzip("keras_files.zip")
                folder_name <- dirname(folder_name[1])
                
                #save folder name for posterior usage
                write(folder_name, file = "folder_name.txt")
                
              } 
              
              con <- file("folder_name.txt","r")
              folder_name <- readLines(con,n=1)
              close(con)
              
              try(
                if(!dir.exists(folder_name)){
                  stop(paste0("The directory with keras models could not be found. The package can not work. ", folder_name))
                } else {
                  
                  ## define tokenizer and model based on hate_type and language
                  model_filename <- paste(c(folder_name,"/models/", hate_type, "_", language, ".h5"), collapse = '')
                  tokenizer_filename <- paste(c(folder_name,"/tokenizers/tokenizer_", language), collapse = '')
                  
                  #library(keras)
                  ## convert texts_vector to word_embeddings ##
                  #read tokenizer
                  tokenizer <- keras::load_text_tokenizer(tokenizer_filename)
                  
                  #apply tokenizer to the data
                  sequences <- keras::texts_to_sequences(tokenizer, data_frame$text)
                  x_test <- keras::pad_sequences(sequences, maxlen = 100)
                  
                  #library(kerasR)
                  ## apply model ##
                  # read model based on hate_type and language
                  mod <- kerasR::keras_load(model_filename)
                  
                  # apply model to the data
                  classified_data <- kerasR::keras_predict_proba(mod, x_test)
                  
                  # round data to 0 for no hate or to 1 for hate
                  classified_data <- round(classified_data)
                  # return result
                  return(classified_data)
                  
                }
                )
            }
          )
        }
      )
    }
  )
}

#' Stop PropagHate function
#'
#' This function allows you to evaluate if a set of text messages constains hate speech. The usage of the function stopPropagHate requires the download of tokenizer and model files to the working directory. For an automatic download and usage without prompt check the function stopPropagHateWithoutPrompt.
#' @param data_frame, is a data.frame containing a column named "text".
#' @param hate_type, is the type of hate you want to measure. It can be general "hate" speech, "sexism" or "racism".
#' @param language, we provide this funtion both for Enlish "en" and Portuguese "pt".
#' @keywords hate speech, racism, sexism
#' @export
#' @examples texts_data_frame <- data.frame(id = c(1,2,3), texts_data_frame <- data.frame(id = c(1,2,3), text = c("Call me sexist but female sports anchors r the worst. Makes me uncomfortable when women know more about sports than I do", "May Allah bless him with 72 virgin pigs.", "Stop sending me anonymous valentines things. I m pretty sure they are")))
#' stopPropagHate(texts_data_frame, "hate", "en")
stopPropagHate <- function(data_frame, hate_type, language){
  
  if (!file.exists("folder_name.txt")){
    answer <- menu(c("Yes", "No"), title="The usage of the function stopPropagHate requires the download of tokenizer and model files to your working directory. Can we proceed with this? (for an automatic download and usage without prompt check the function stopPropagHateWithoutPrompt.)")
    if(answer == 1){
      stopPropagHateWithoutPrompt(data_frame, hate_type, language)
    }
  
  } else{
    stopPropagHateWithoutPrompt(data_frame, hate_type, language)
  }
}

#' Stop PropagHate function
#'
#' This function allows you to evaluate if a set of text messages constains hate speech. The usage of the function stopPropagHate requires the download of tokenizer and model files to the working directory. For an automatic download and usage without prompt check the function stopPropagHateWithoutPrompt.
#' @param message, is a string with the text to be classified.
#' @param hate_type, is the type of hate you want to measure. It can be general "hate" speech, "sexism" or "racism".
#' @param language, we provide this funtion both for Enlish "en" and Portuguese "pt".
#' @keywords hate speech, racism, sexism
#' @export
#' @examples message <- "Call me sexist but female sports anchors r the worst. Makes me uncomfortable when women know more about sports than I do", "May Allah bless him with 72 virgin pigs."
#' stopPropagHateMessage(message, "hate", "en")
stopPropagHateMessage <- function(message, hate, language){
  texts <- data.frame(id = c(1, 2), text = c(message, ""))
  return(stopPropagHateWithoutPrompt(texts, hate,  language)[1])
}

#' test_package function
#'
#' This function allows you to evaluate if a set of text messages constains hate speech. The usage of the function stopPropagHate requires the download of tokenizer and model files to the working directory. For an automatic download and usage without prompt check the function stopPropagHateWithoutPrompt.
#' @keywords hate speech, racism, sexism
#' @export
#' @examples test_package()
test_package <- function(){
  message <- "Call me sexist but female sports anchors r the worst. Makes me uncomfortable when women know more about sports than I do"
  return(stopPropagHateMessage(message, "hate", "en")[1])
}
