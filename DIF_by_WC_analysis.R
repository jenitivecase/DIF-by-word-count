source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

needed_packages <- c("mirt")
sapply(needed_packages, load_packages)

item_content <- read.xlsx("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis/Reporting/item_content_from_QBTB-db.xlsx")

responses <- readRDS("CP2016_responses.rds")

names(responses)[1] <- gsub("ï..", "", names(responses)[1])


responses <- responses %>% 
  select(BookletID, qbtbQuestionID, IsCorrect, LanguageID) %>%
  mutate(LanguageID = case_when(LanguageID == 0 ~ 0,
                                is.null(LanguageID) ~ 2,
                                LanguageID != 0 ~ 1)) %>%
  filter(LanguageID != 2) %>%
  spread(qbtbQuestionID, IsCorrect) %>%
  unique()
