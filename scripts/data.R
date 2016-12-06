## Script to create(/update) datasets shipped with iNZight

SURFIncomeSurvey_200 <- read.csv("~/Downloads/SURFIncomeSurvey.csv", header = TRUE, comment.char = "#")
SURFIncomeSurvey_200$X <- NULL
devtools::use_data(SURFIncomeSurvey_200, overwrite = TRUE)


devtools::load_all()
devtools::document()
