################################################################################
#### SETUP #####################################################################
################################################################################
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

needed_packages <- c("mirt", "rstan", "stringr")
sapply(needed_packages, load_packages)

item_content <- read.xlsx("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis/Reporting/item_content_from_QBTB-db.xlsx")

responses <- readRDS("CP2016_responses_20170905.rds")

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

nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

item_content <- item_content %>%
  group_by(QuestionID) %>%
  mutate(WC = nwords(unique(stem), pseudo = T)) %>%
  ungroup() %>%
  arrange(QuestionID)

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
response <- responsese$IsCorrect
group_long <- responses$LanguageID
group <- unique(responses[, c("BookletID", "LanguageID")])
DIFpredict <- item_content$WC



dat_long <- list("n_people", "n_items", "n_observations", "respondentid", 
                   "itemid", "response", "group", "group_long",
                   "DIFpredict")

analysis <- sampling(precomp_model, data = dat_long,
                     iter = 12000, warmup = 5000, chains = 2, 
                     verbose = TRUE, cores = 2)

params_summary <- summary(analysis, pars = c("a", "b", "D", "beta0", "beta1", "mu", 
                                             "sigma2", "R2", "theta",
                                             "foc_mean"),
                          probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary
