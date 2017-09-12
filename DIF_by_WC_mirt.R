################################################################################
#### SETUP #####################################################################
################################################################################
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

needed_packages <- c("mirt", "stringr")
sapply(needed_packages, load_packages)

item_content <- read.xlsx("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis/Reporting/item_content_from_QBTB-db.xlsx")

responses <- readRDS("CP2016_responses_20170905.rds")

#filter down to one form. comment out when doing multiple forms
responses <- filter(responses, AssessmentID == 143248)

responses <- responses %>% 
  select(BookletID, qbtbQuestionID, IsCorrect, LanguageID) %>%
  mutate(LanguageID = case_when(LanguageID == 0 ~ 0,
                                is.null(LanguageID) ~ 2,
                                LanguageID != 0 ~ 1)) %>%
  filter(LanguageID != 2) %>%
  filter(!is.na(IsCorrect)) %>%
  unique() %>%
  group_by(BookletID, qbtbQuestionID) %>% 
  filter(length(unique(IsCorrect)) == 1) %>% 
  ungroup() %>%
  arrange(qbtbQuestionID)

gc()

nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

item_content <- item_content %>%
  filter(QuestionID %in% responses$qbtbQuestionID) %>%
  select(QuestionID, stem) %>%
  unique() %>%
  mutate(WC = nwords(stem, pseudo = T)) %>%
  arrange(QuestionID)

item_WC <- as.data.frame(unique(as.numeric(as.character(responses$qbtbQuestionID))))
colnames(item_WC) <- "QuestionID"
item_WC <- left_join(item_WC, item_content, by = c("QuestionID" = "QuestionID")) %>%
  arrange(QuestionID) %>%
  mutate(QuestionID = as.numeric(factor(QuestionID))) %>%
  select(QuestionID, WC)

responses <- responses %>% 
  mutate(BookletID = as.numeric(factor(BookletID))) %>%
  mutate(qbtbQuestionID = as.numeric(factor(qbtbQuestionID)))

responses <- spread(responses, key = qbtbQuestionID, value = IsCorrect)


################################################################################
#### ANALYSIS ##################################################################
################################################################################

model <- paste0("F1 = 1-", nrow(item_content))
group <- as.matrix(responses$LanguageID)
names(group) <- "group"
group <- as.character(group)

#stage 1
out <- mixedmirt(data = as.data.frame(responses[, -c(1:2)]), covdata = group, itemdesign = item_WC, 
          model = model, itemtype = "2PL",
          fixed = ~ 0 + group + items,
          verbose = TRUE)

summary(out)
randef(out)
out_coef <- coef(out, IRTpars = TRUE, as.data.frame = TRUE)
out_coef

mirtCluster(2)

out_MG <- multipleGroup(data = as.data.frame(responses[, -c(1:2)]), group = group, 
                        model = model, invariance = c("free_means", "slopes"), itemtype = "2PL")
  
out_MG_DIF <- DIF(out_MG, which.par = "d", plotdif = TRUE, verbose = TRUE, 
                  technical = list(NCYCLES = 1000))
