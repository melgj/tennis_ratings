library(tidyverse)
library(lubridate)
library(data.table)
library(doMC)

setwd("~/projects/elo")

df <- fread("wta_results_db_11_21.csv")

colnames(df)

df <- df %>% select(tourney_id, tourney_name, tourney_date, winner_id, winner_name,
                    round, loser_id, loser_name, score, winner_rank, loser_rank)

head(df)

colSums(is.na(df))

unique(df$round)

df$round <- factor(df$round, levels = c("R128", "R64", "R32", "R16", "RR",
                                        "QF", "BR", "SF", "F"), ordered = T)

glimpse(df)
wta <- df %>% 
  filter(winner_rank <= 50 | loser_rank <= 50) %>% 
  mutate(result = 1) %>% 
  drop_na(winner_rank, loser_rank) %>% 
  arrange(tourney_date)

glimpse(wta)

pl <- list(union(wta$winner_name, wta$loser_name))
init_rtng <- c()

for(n in 1:lengths(pl)){
  rtng <- 1500
  names(rtng) <- pl[[1]][[n]]
  init_rtng <- c(init_rtng, rtng)
}

unique((names(init_rtng)))

pl

# players <- init_rtng
# names(players[1])

probP1 <- function(p1_rtng, p2_rtng){
  dij <- p2_rtng - p1_rtng
  return(1 / (1 + 10 ^ (dij/400)))
}

newP1 <- function(m_res, oldP1, oldP2, k){
  return(oldP1 + k * (m_res - (1 / (1 + 10 ^ ((oldP2 - oldP1)/400)))))
}

newP2 <- function(m_res, oldP1, oldP2, k){
  return(oldP2 + k * ((1 - m_res) - (1 / (1 + 10 ^ ((oldP1 - oldP2)/400)))))
}

# test elo probability functions
x1 <- probP1(1500, 1500)
x2 <- 1 - x1
x1
x2

y1 <- newP1(1, 1600, 1400, 30)
y2 <- newP2(1, 1600, 1400, 30)
y1
y2

# create empty vectors to hold probability and rating values

plyr1Prob <- c()
plyr2Prob <- c()
plyr1New <- c()
plyr2New <- c()
plyr1Old <- c()
plyr2Old <- c()

registerDoMC(4)

k <- 36

for(m in seq_along(wta$winner_name)){
  
  plyr1Old <- c(plyr1Old, init_rtng[[wta$winner_name[m]]])
  plyr2Old <- c(plyr2Old, init_rtng[[wta$loser_name[m]]])
  
  p1p <- probP1(init_rtng[[wta$winner_name[m]]], init_rtng[[wta$loser_name[m]]])
  p2p <- 1 - p1p
  
  plyr1Prob <- c(plyr1Prob, round(p1p, 2))
  plyr2Prob <- c(plyr2Prob, round(p2p, 2))
  
  p1new <- newP1(1, init_rtng[[wta$winner_name[m]]], init_rtng[[wta$loser_name[m]]], k)
  p2new <- newP2(1, init_rtng[[wta$winner_name[m]]], init_rtng[[wta$loser_name[m]]], k)
  
  plyr1New <- c(plyr1New, round(p1new, 2))
  plyr2New <- c(plyr2New, round(p2new, 2))
  
  init_rtng[[wta$winner_name[m]]] <- round(p1new, 2)
  init_rtng[[wta$loser_name[m]]] <- round(p2new, 2)
  
}
  
 




# check winner value pos and loser value neg
mean(plyr1New-plyr1Old)
mean(plyr2New-plyr2Old)



wta <- wta %>% 
  mutate(P1_ELO = plyr1Old,
         P2_ELO = plyr2Old,
         P1_Win_Prob = plyr1Prob,
         P2_Win_Prob = plyr2Prob,
         P1_New_ELO = plyr1New,
         P2_New_ELO = plyr2New,
         Player1 = winner_name,
         Player2 = loser_name,
         P1_ELO_Change = P1_New_ELO - P1_ELO,
         P2_ELO_Change = P2_New_ELO - P2_ELO)

summary(wta$P1_ELO_Change)
summary(wta$P2_ELO_Change)

write_csv(wta, "wta_elo_k36.csv")

current_ratings <- as_tibble(enframe(init_rtng)) %>% 
  rename(ELO_Rating = value)

current_ratings %>% 
  arrange(desc(ELO_Rating)) %>% 
  head(30) 



