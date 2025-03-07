library(dplyr)
library(ggplot2)
library(magrittr)

targets <- data.table::fread(here::here("Data/Raw Data/S1_time1.csv"),
                           header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100') %>%
  dplyr::select(ResponseId, PROLIFIC_PID,gender, age, ranking,ethnicity,education, QID7_Id) %>%
  dplyr::mutate(ranking = gsub("st|nd|rd|th| rank| \\(most attractive\\)| \\(least attractive\\)", "", ranking),
                ranking = as.numeric(ranking),age = as.numeric(age)) %>%
  dplyr::filter(!is.na(ranking)) %>%
  dplyr::rename('Guess' = 'ranking')

saveRDS(targets, file = here::here("Data/Study1_time1.Rds"))
targets %>% write.csv("Data/Cleaned Data/S1_time1_clean.csv")

females <- targets %>% dplyr::filter(gender == 'Female')
males <- targets %>% dplyr::filter(gender == 'Male')
females <- females[1:100,]
targets200 <- rbind(females, males)

saveRDS(targets200, file = here::here("Data/Study1_time1_200.Rds"))
targets200 %>% write.csv("Data/Cleaned Data/S1_time1_200_clean.csv")
