library(tidyverse)
library(tidymodels)
library(lubridate)
library(data.table)
library(doMC)
library(scoring)

setwd("~/projects/elo")

df <- fread("wta_results_db_11_21.csv")

colnames(df)

df <- df %>% select(tourney_id, tourney_name, tourney_date, winner_id, winner_name,
                    round, loser_id, loser_name, score, winner_rank, loser_rank)

head(df)

colSums(is.na(df))

unique(df$round)

df$round <- factor(df$round, levels = c("R128", "R64", "R32", "R16", "QF",
                                        "RR", "BR", "SF", "F"), ordered = T)

glimpse(df)
wta <- df %>% 
  #filter(winner_rank <= 50 | loser_rank <= 50) %>% 
  mutate(result = 1) %>% 
  drop_na(winner_rank, loser_rank) %>% 
  arrange(tourney_date, round)

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
  return(uij <- 1 / (1 + 10 ^ (dij/400)))
}

newP1 <- function(m_res, oldP1, oldP2, k){
  new <- oldP1 + k * (m_res - (1 / (1 + 10 ^ ((oldP2 - oldP1)/400))))
}

newP2 <- function(m_res, oldP1, oldP2, k){
  new <- oldP2 + k * ((1 - m_res) - (1 / (1 + 10 ^ ((oldP1 - oldP2)/400))))
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

start_k <- 20
end_k <- 100
kk <- seq(start_k, end_k, by = 1)
lk <- length(k)

best_ks <- 0
best_kv <- 0

best_brs <- 1000
best_brk <- 0

ks <- c()
kv <- c()

brs <- c()
brk <- c()

registerDoMC(4)

for(i in kk){
  
  cat("i value is ", i, ", best_ks is ", best_ks, ", best_kv is ", best_kv, "\n")
  cat("i value is ", i, ", best brier score is ", best_brs, ", best brier k is ", best_brk, "\n")
  for(m in seq_along(wta$winner_name)){
    
    plyr1Old <- c(plyr1Old, init_rtng[[wta$winner_name[m]]])
    plyr2Old <- c(plyr2Old, init_rtng[[wta$loser_name[m]]])
    
    p1p <- probP1(init_rtng[[wta$winner_name[m]]], init_rtng[[wta$loser_name[m]]])
    p2p <- 1 - p1p
    
    plyr1Prob <- c(plyr1Prob, round(p1p, 2))
    plyr2Prob <- c(plyr2Prob, round(p2p, 2))
    
    p1new <- newP1(1, init_rtng[[wta$winner_name[m]]], init_rtng[[wta$loser_name[m]]], i)
    p2new <- newP2(1, init_rtng[[wta$winner_name[m]]], init_rtng[[wta$loser_name[m]]], i)
    
    plyr1New <- c(plyr1New, round(p1new, 2))
    plyr2New <- c(plyr2New, round(p2new, 2))
    
    init_rtng[[wta$winner_name[m]]] <- round(p1new, 2)
    init_rtng[[wta$loser_name[m]]] <- round(p2new, 2)
    
  }
  
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
  
  
  df <- wta %>% 
    mutate(Year = lubridate::year(tourney_date)) %>% 
    filter(P1_Win_Prob != P2_Win_Prob, Year >= 2012) %>% 
    arrange(tourney_date, round)
    
  
  df$ELO_Diff <- df$P1_ELO - df$P2_ELO
  df$Predicted_Winner <- if_else(df$P1_ELO > df$P2_ELO, 1, 2)
  df$Actual_Winner <- 1
  df$ELO_Correct <- if_else(df$Actual_Winner == df$Predicted_Winner, 1, 0)
  
  kscore <- mean(df$ELO_Correct)
  
  if(kscore > best_ks){
    best_ks <- kscore
    best_kv <- i
  }

  kv <- c(kv, i)
  ks <- c(ks, kscore)
  
  brier <- brierscore(Actual_Winner ~ Predicted_Winner, data = df)
  brier_score <- round(sum(brier)/length(brier),4)
  
  if(brier_score < best_brs){
    best_brs <- brier_score
    best_brk <- i
  }
  
  brs <- c(brs, brier_score)
  brk <- c(brk, i)

  #write_csv(df, paste0("wta_tune_elo_k", i, ".csv"))
  
  pl <- list(union(wta$winner_name, wta$loser_name))
  init_rtng <- c()
  
  for(n in 1:lengths(pl)){
    rtng <- 1500
    names(rtng) <- pl[[1]][[n]]
    init_rtng <- c(init_rtng, rtng)
  }
  
  plyr1Prob <- c()
  plyr2Prob <- c()
  plyr1New <- c()
  plyr2New <- c()
  plyr1Old <- c()
  plyr2Old <- c()
  
}
 
sum(df$winner_rank < df$loser_rank) / nrow(df)

kdf <- tibble(k = kv, score = ks)

ggplot(kdf) +
  aes(k, score) +
  geom_point(col = "red") +
  geom_smooth()

bdf <- tibble(k = brk, score = brs)

ggplot(bdf) +
  aes(k, score) +
  geom_point(col = "red") +
  geom_smooth()
