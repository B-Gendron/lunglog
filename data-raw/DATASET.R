## code to prepare `DATASET` dataset goes here

patients <- readr::read_csv("survey_lung_cancer.csv")

usethis::use_data(patients, overwrite = TRUE)
