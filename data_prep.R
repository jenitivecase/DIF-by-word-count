responses <- read.table("CP2016_responses.txt",
                            sep = ",", header = TRUE, as.is = TRUE,
                            strip.white = TRUE, fill = TRUE,
                            blank.lines.skip = TRUE)

saveRDS(responses, "CP2016_responses.txt.rds")
gc()