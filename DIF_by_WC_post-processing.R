source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

needed_packages <- c("mirt", "stringr", "RODBC")
sapply(needed_packages, load_packages)

out <- readRDS("mirt-DIF-by-WC-analysis_form-143436_20170918.rds")

for(i in 1:length(out)){
  assign(names(out[i]), out[i])
}

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