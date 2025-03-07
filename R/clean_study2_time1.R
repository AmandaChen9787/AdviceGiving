library(dplyr)
library(ggplot2)
library(magrittr)

Advisee <- data.table::fread(here::here("Data/Raw Data/S2_time1.csv"),
                             header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Status == 'IP Address',Progress == '100',!is.na(PROLIFIC_PID) ,!is.na(Gender)) %>%
  dplyr::filter(!is.na(ResponseId) ,!is.na(Age) ) %>%
  dplyr::mutate(Guess = coalesce(Guess, Guess_L)) %>%
  dplyr::mutate(Guess = ifelse(Guess == "Compete against the High Performer Group",1,0)) %>%
  dplyr::select(PROLIFIC_PID,Guess,default,Score,Age,Gender, Ethnicity,Education) %>%
  dplyr::rename("ProlificID" = "PROLIFIC_PID") %>%
  dplyr::mutate(Score = as.numeric(Score))


saveRDS(Advisee, file = here::here("Data/Study2_time1.Rds"))
Advisee %>% write.csv("Data/Cleaned Data/S2_time1_clean.csv")


