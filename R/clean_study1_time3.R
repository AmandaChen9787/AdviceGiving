library(dplyr)
library(ggplot2)
library(magrittr)


Target <- read.csv("Data/Cleaned Data/S1_time1_clean.csv")[-c(1,2)]  %>%
  dplyr::mutate(Guess = as.numeric(Guess))


Receiver <- data.table::fread(here::here("Data/Raw Data/S1_time3.csv"),
                              header = T,na.strings = '')[-c(1,2)]  %>%
  dplyr::filter(Progress == '100')%>%
  dplyr::select(ResponseId, PROLIFIC_PID, bet:sincere, advice.0.finalRank,rank) %>%
  dplyr::mutate(bet = gsub("st|nd|rd|th| rank| \\(most attractive\\)| \\(least attractive\\)", "", bet),
                bet = as.numeric(bet), rank = as.numeric(rank),advice.0.finalRank = as.numeric(advice.0.finalRank) ) %>%
  dplyr::mutate(dplyr::across(c(likable:sincere),
                              ~ dplyr::case_when(grepl("Not at all", .) ~ 1,
                                                 grepl("Slightly", .) ~ 2,
                                                 grepl("Somewhat", .) ~ 3,
                                                 grepl("Very", .) ~ 4,
                                                 grepl("Extremely", .) ~ 5))) %>%
  dplyr::mutate(warmAvg = rowMeans(select(.,c('warm','well-intentioned','friendly','good-natured')))) %>%
  dplyr::mutate(trustAvg = rowMeans(select(.,c('trustworthy','sincere'))))

Receiver <- merge(Receiver,Target,by = 'PROLIFIC_PID') %>% dplyr::rename('Advice' = 'rank', 'Final Rank' = 'advice.0.finalRank')

Advice <- read.csv("Data/Raw Data/S1_Advice200.csv")[-c(1,2)] %>%
  dplyr::select(PROLIFIC_PID,Giver_Id,Condition)


Receiver <- merge(Receiver,Advice,by= 'PROLIFIC_PID')

saveRDS(Receiver, file = here::here("Data/Study1_time3.Rds"))
Receiver %>% write.csv("Data/Cleaned Data/S1_time3_clean.csv")
