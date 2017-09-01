source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

needed_packages <- c("mirt")
sapply(needed_packages, load_packages)

item_content <- read.xlsx("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis/Reporting/item_content_from_QBTB-db.xlsx")

responses <- readRDS("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis/Analysis/item-level_RN-2016CP_outcome-scores.rds")

names(responses)[1] <- gsub("ï..", "", names(responses)[1])


responses <- responses %>% 
  select(BookletID, qbtbQuestionID, IsCorrect) %>%
  spread(qbtbQuestionID, IsCorrect)
