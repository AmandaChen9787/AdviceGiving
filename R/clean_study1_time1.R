library(dplyr)
library(ggplot2)
library(magrittr)

Advisee <- data.table::fread(here::here("Data/Raw Data/S1_time1.csv"),
                             header = T,na.strings = '')[-c(1,2)] %>%
  dplyr::filter(Progress == '100' & Status == 'IP Address' & !is.na(QID3) & Gender != 'Other') %>%
  dplyr::rename('ProlificID' = 'QID3') %>%
  dplyr::mutate(Guess = as.numeric(Guess), Age = as.numeric(Age))  %>%
  dplyr::select(ProlificID,ResponseId,Condition, QID10:QID19,Guess,
                Gender, Age, Ethnicity,Education)



# count correct answers
answerList <- c('113 feet','x = -2','x15','13','2.06',
                '-34','6','63°','3','4x2 + 32x + 48')
answer <- Advisee %>% dplyr::select(QID10:QID19)
#count <-  apply(answer, 1, function(x) sum(x == answerList))

count <- apply(answer, 1, function(x) {
  # Check if there are NAs in the list
  if (any(is.na(x))) {
    # If there are NAs, count non-NA elements
    sum(!is.na(x) & x %in% answerList)
  } else {
    # If there are no NAs, count all elements
    sum(x == answerList)
  }
})


count <- as.data.frame(count)
Advisee <- cbind(Advisee,count)

saveRDS(Advisee, file = here::here("Data/Study1_time1All.Rds"))
Advisee %>% write.csv("Data/Cleaned Data/S1_time1All_clean.csv")

#create a gander-balance sample
females <- Advisee %>% dplyr::filter(Gender == "Female")
males <- Advisee %>% dplyr::filter(Gender == "Male")
males <- males[1:500,]

Advisee1000 <- rbind(females, males)

saveRDS(Advisee1000, file = here::here("Data/Study1_time1.Rds"))
Advisee1000 %>% write.csv("Data/Cleaned Data/S1_time1_clean.csv")



