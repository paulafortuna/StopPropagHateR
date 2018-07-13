# StopPropagHateR

In this package we provide deep learning models for hate speech classification (binary hate speech, sexism and racism). These were build using keras and allow text classification to Portuguese and English. 

The usage of the function stopPropagHate requires the download of tokenizer and model files to the working directory. The user has to give permission for the installation with the function stopPropagHate. For an automatic download and usage without prompt check the function stopPropagHateWithoutPrompt.

To install the package:

library(devtools)

devtools::install_github("paulafortuna/StopPropagHateR")

Apart from these commands proper installation of keras and tensorflow and python is needed.
Namely, for mac OS, the followed command were:

install.packages("keras")

install_keras()

devtools::install_github("paulafortuna/StopPropagHateR")

library(StopPropagHateR)

Additional configuration of python, conda, and tensorflow may be needed.
Also the library(reticulate) is being used to solve some problems related with keras.
