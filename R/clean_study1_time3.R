library(dplyr)
library(ggplot2)
library(magrittr)


Target <- read.csv("Data/Cleaned Data/S1_time1_clean.csv")[-c(1,3)]



Receiver <- data.table::fread(here::here("Data/Raw Data/S1_time3.csv"),
                              header = T,na.strings = '')[-c(1,2)]  %>%
dplyr::filter(Progress == '100' & Status == 'IP Address') %>%
  dplyr::rename('ProlificID' = 'Q3', 'Advice' = '0.Advice','GiverID' = '0.GiverID','IntGuess' = 'Guess',
                'GiverCondition' = '0.Condition') %>%
  dplyr::mutate(Bet = case_when(bet == 'Compete against the Low Performer Group' ~ 0,
                                bet == 'Compete against the High Performer Group' ~ 1)) %>%
  dplyr::select(ProlificID,Bet, IntGuess, Advice, GiverID, GiverCondition) %>%
  dplyr::mutate(GiverCondition = case_when(GiverCondition == 'E' ~ 1,
                                           GiverCondition == 'None' ~ 0))

Receiver <- merge(Receiver,Target,by = 'ProlificID')

saveRDS(Receiver, file = here::here("Data/Study1_time3.Rds"))
Receiver %>% write.csv("Data/Cleaned Data/S1_time3_clean.csv")
