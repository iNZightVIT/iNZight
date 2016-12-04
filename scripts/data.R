## Script to create(/update) datasets shipped with iNZight

SURFIncomeSurvey <- read.csv("~/Downloads/SURFIncomeSurvey.csv", header = TRUE, comment.char = "#")
SURFIncomeSurvey$X <- NULL
devtools::use_data(SURFIncomeSurvey, overwrite = TRUE)


devtools::load_all()
devtools::document()
