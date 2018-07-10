
#' Stop PropagHate function
#'
#' This function allows you to evaluate if a set of text messages constains hate speech.
#' @param data_frame, is a data.frame containing a column named "text".
#' @param hate_type, is the type of hate you want to measure. It can be general "hate" speech, "sexism" or "racism".
#' @param language, we provide this funtion both for Enlish "en" and Portuguese "pt".
#' @keywords hate speech, racism, sexism
#' @export
#' @examples texts_data_frame <- data.frame(id = c(1,2,3), text = c("Lugar de mulher e na cozinha, isto e a verdade", "gorda e feia", "mais uma mensagem de teste"))
#' stopPropagHate(texts_data_frame, "sexism", "pt")
stopPropagHate <- function(data_frame, hate_type, language){

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

              ## define tokenizer and model based on hate_type and language
              model_filename <- paste(c("./StopPropagHateR/data/models/", hate_type, "_", language, ".h5"), collapse = '')
              tokenizer_filename <- paste(c("./StopPropagHateR/data/tokenizer_", language), collapse = '')

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

