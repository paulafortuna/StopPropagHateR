% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopPropagHate.R
\name{stopPropagHate}
\alias{stopPropagHate}
\title{Stop PropagHate function}
\usage{
stopPropagHate(data_frame, hate_type = "hate", language = "en")
}
\arguments{
\item{data_frame, }{is a data.frame containing a column named "text".}

\item{hate_type, }{is the type of hate you want to measure. It can be general "hate" speech, "sexism" or "racism".}

\item{language, }{we provide this funtion both for Enlish "en" and Portuguese "pt".}
}
\description{
This function allows you to evaluate if a set of text messages constains hate speech. The usage of the function stopPropagHate requires the download of tokenizer and model files to the working directory. For an automatic download and usage without prompt check the function stopPropagHateWithoutPrompt.
}
\examples{
texts_data_frame <- data.frame(id = c(1,2,3), texts_data_frame <- data.frame(id = c(1,2,3), text = c("Call me sexist but female sports anchors r the worst. Makes me uncomfortable when women know more about sports than I do", "May Allah bless him with 72 virgin pigs.", "Stop sending me anonymous valentines things. I m pretty sure they are")))
stopPropagHate(texts_data_frame, "hate", "en")
}
\keyword{hate}
\keyword{racism,}
\keyword{sexism}
\keyword{speech,}
