## Script to create(/update) datasets shipped with iNZight

SURFIncomeSurvey <- read.csv("~/Downloads/SURFIncomeSurvey.csv", header = TRUE, comment.char = "#")
devtools::use_data(SURFIncomeSurvey)
