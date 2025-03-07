## code to prepare `AZ_A1020_BLM` dataset goes here

AZ_A1020_BLM <- read.csv("data-raw/AZ_A1020_BLM_point_plots.csv")
colnames(AZ_A1020_BLM) <- c("x", "y", "type")

usethis::use_data(AZ_A1020_BLM, overwrite = TRUE)
