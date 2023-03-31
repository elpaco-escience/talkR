## code to prepare `IFADV` dataset goes here
## "https://www.fon.hum.uva.nl/IFA-SpokenLanguageCorpora/IFADVcorpus/"

# Currently, we are simply loading the existing object that has been
# manipulated elsewhere.
# This code is a placeholder for the workflow to generate this data
# object.
ifadv <- load("data/ifadv.rda")

# attach the data to the package
usethis::use_data(ifadv, overwrite = TRUE)
