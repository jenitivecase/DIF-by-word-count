################################################################################
#### SETUP #####################################################################
################################################################################
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

needed_packages <- c("mirt", "rstan", "stringr")
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
  select(QuestionID, stem) %>%
  unique() %>%
  mutate(WC = nwords(stem, pseudo = T)) %>%
  arrange(QuestionID)

item_WC <- as.data.frame(unique(as.numeric(as.character(responses$qbtbQuestionID))))
colnames(item_WC) <- "QuestionID"
item_WC <- left_join(item_WC, item_content, by = c("QuestionID" = "QuestionID")) %>%
  arrange(QuestionID) %>%
  mutate(QuestionID = as.numeric(factor(QuestionID)))

responses <- responses %>% 
  mutate(BookletID = as.numeric(factor(BookletID))) %>%
  mutate(qbtbQuestionID = as.numeric(factor(qbtbQuestionID)))

################################################################################
#### ANALYSIS ##################################################################
################################################################################

precomp <- stanc(file = "explanatory_DIF.stan")
precomp_model <- stan_model(stanc_ret = precomp)

n_people <- length(unique(responses$BookletID))
n_items <- length(unique(responses$qbtbQuestionID))
n_observations <- nrow(responses)
respondentid <- responses$BookletID
itemid <- responses$qbtbQuestionID
response <- responses$IsCorrect
group_long <- responses$LanguageID
group <- unique(responses[, c("BookletID", "LanguageID")])$LanguageID
DIFpredict <- item_WC$WC


dat_long <- list("n_people", "n_items", "n_observations", "respondentid", 
                   "itemid", "response", "group", "group_long",
                   "DIFpredict")

analysis <- sampling(precomp_model, data = dat_long,
                     iter = 12000, warmup = 5000, chains = 2, 
                     verbose = TRUE, cores = 2)

saveRDS(analysis, paste0("DIF-by-WC-stanfit_", date, ".rds"))

params_summary <- summary(analysis, pars = c("a", "b", "D", "beta0", "beta1", "mu", 
                                             "sigma2", "R2", "theta",
                                             "foc_mean"),
                          probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary
