################################################################################
#### SETUP #####################################################################
################################################################################
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

needed_packages <- c("mirt", "stringr", "RODBC")
sapply(needed_packages, load_packages)


db <- odbcDriverConnect('driver={SQL Server};server=asc-prd-sql07;trusted_connection=true')

responses <- sqlQuery(db, "
SELECT book.BookletID, book.QuestionID, book.IsCorrect, book.atiDate, 
batch.AssessmentID, batch.ProgramTypeID,
asmt.AssessmentID, asmt.AssessmentStyleID, asmt.TestTypeID, 
qbtbmap.QuestionID as qbtbQuestionID,
users.RaceID,
users.LanguageID
	FROM asm.BookletAnswer.Bookletanswer (nolock) as book
	LEFT JOIN asm.Config.Batch (nolock) as batch on batch.batchID = book.BatchID
	LEFT JOIN asm.Config.Assessment (nolock) as asmt on asmt.AssessmentID = batch.AssessmentID
	LEFT JOIN contentBuilder.QBTB.TestSectionQuestionASMMappedValue (nolock) as qbtbmap on qbtbmap.ASMMappedValue = book.QuestionID
	LEFT JOIN contentBuilder.QBTB.Question (nolock) as qbtbq on qbtbq.QuestionID = qbtbmap.QuestionID
	LEFT JOIN asm.Stats.Booklet (nolock) as bookinfo on bookinfo.BookletID = book.BookletID
	LEFT JOIN asm.Config.atiUser (nolock) as users on users.UserID = bookinfo.UserID
	WHERE book.IsDemo = 0 AND
	((book.atiDate >= '20161115' AND book.atiDate <= '20161231') OR
	(book.atiDate >= '20170401' AND book.atiDate <= '20170515')) AND
	batch.ProgramTypeID = 2 AND
	asmt.AssessmentStyleID = 2 AND
	asmt.TestTypeID = 3 AND
	asmt.AssessmentID IN (143248, 143436, 143657) AND
	qbtbq.IsValidated = 1
")

item_content <- sqlQuery(db, sprintf("
SELECT q.QuestionID, q.Value as stem, q.UpdateDate, choice.ChoiceID, choice.IsCorrect, cattr.Value as responseopt
  FROM [contentBuilder].[QBTB].[QuestionAttributeValue] as q
                         LEFT JOIN [contentBuilder].[QBTB].[Choice] as choice on choice.QuestionID = q.QuestionID
                         LEFT JOIN [contentBuilder].[QBTB].[ChoiceAttributeValue] as cattr on cattr.ChoiceID = choice.ChoiceID
                         WHERE q.AttributeEntityMappingID = 1056 AND
                         cattr.AttributeEntityMappingID = 1034 AND
                         q.QuestionID IN(%s)
ORDER BY QuestionID, IsCorrect
                         ", paste0(unique(responses$qbtbQuestionID), collapse = ",")))

#filter down to one form. comment out when doing multiple forms
#pick assessment id 143248, 143436, or 143657
formid <- 143436
responses <- filter(responses, AssessmentID == formid)

#pare down to needed variables, eliminate missing responses, and recode language
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

#function to count number of words
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

word_length <- function(string, pseudo = F){
  ifelse( pseudo,
          pattern <- "\\S+",
          pattern <- "[[:alpha:]]+" 
  )
  n_words <- str_count(string, pattern)
  all_lets <- str_count(string, "[[:alpha:]]")
  return(all_lets/n_words)
}

#get item ids, count number of words in stem
item_content <- item_content %>%
  filter(QuestionID %in% responses$qbtbQuestionID) %>%
  select(QuestionID, stem) %>%
  unique() %>%
  mutate(WC = nwords(stem, pseudo = T)) %>%
  mutate(WL = word_length(stem, pseudo = T)) %>%
  arrange(QuestionID)

#creating a factorized version (i.e., sequentially numbered, starting from 1) 
#of the QuestionIDs for the word count info
item_WC <- as.data.frame(unique(as.numeric(as.character(responses$qbtbQuestionID))))
colnames(item_WC) <- "QuestionID"
item_WC <- left_join(item_WC, item_content, by = c("QuestionID" = "QuestionID")) %>%
  arrange(QuestionID) %>%
  mutate(QuestionID = as.numeric(factor(QuestionID))) %>%
  select(QuestionID, WC)

#creating factorized versions of the booklet (aka respondent) and question IDs
responses <- responses %>% 
  mutate(BookletID = as.numeric(factor(BookletID))) %>%
  mutate(qbtbQuestionID = as.numeric(factor(qbtbQuestionID)))

#move to wide format
responses <- spread(responses, key = qbtbQuestionID, value = IsCorrect)


################################################################################
#### ANALYSIS ##################################################################
################################################################################

#set up model to explicitly specify that all items load to a single factor F1
model <- paste0("F1 = 1-", nrow(item_content))
#the grouping variable is language. moved to character for multipleGroup()
group <- as.matrix(responses$LanguageID)
names(group) <- "group"
group <- as.character(group)


mirtCluster(2)

#stage 1 - multiple groups analysis
responses %>%
  group_by(LanguageID) %>%
  summarize(mean(`1`, na.rm = TRUE))

out_MG <- multipleGroup(data = as.data.frame(responses[, -c(1:2)]), group = group, 
                        model = model, invariance = c("free_means", "slopes", c("2", "3", "6", "14", "26")),
                        itemtype = "2PL")
  
out_coef <- coef(out_MG, IRTpars = TRUE, simplify = TRUE)

out_MG_DIF <- DIF(out_MG, which.par = "d", plotdif = TRUE, verbose = TRUE, 
                  technical = list(NCYCLES = 1000))

sig_ind <- t(sapply(out_MG_DIF, FUN = function(x) print(x$p)))
sig_ind <- data.frame(seq(1:nrow(sig_ind)), sig_ind[,2])
names(sig_ind) <- c("Item", "pvalue")

dif_ind <- sig_ind[which(sig_ind$`pvalue` <= .05), "Item"]
DIF_results <- out_MG_DIF[dif_ind]

for(i in dif_ind){
  print(itemplot(out_MG, item = paste0(i), theta_lim = c(-3, 3)))
  Sys.sleep(1)
}

#Stage 2 - so simple! Wow!
dif_amt <- out_coef$`0`$items[, "b"] - out_coef$`1`$items[, "b"]
dif_explain <- lm(dif_amt ~ item_WC$WC)
summary(dif_explain)

out <- list(responses, group, item_WC, out_MG, out_MG_DIF)
names(out) <- c("responses", "group", "item_WC", "out_MG", "out_MG_DIF")

saveRDS(out, paste0("mirt-DIF-by-WC-analysis_form-", formid, "_", date, ".rds"))
