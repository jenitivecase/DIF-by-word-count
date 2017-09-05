# responses <- read.table("CP2016_responses_20170901.txt",
#                             sep = ",", header = TRUE, as.is = TRUE,
#                             strip.white = TRUE, fill = TRUE,
#                             blank.lines.skip = TRUE)

responses <- read.csv("CP2016_responses_20170905.csv", stringsAsFactors = FALSE)

names(responses)[1] <- gsub("ï..", "", names(responses)[1])

saveRDS(responses, "CP2016_responses_20170905.rds")
gc()
