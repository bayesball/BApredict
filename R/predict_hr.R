predict_hr <- function(d, lastname, 
                       ITER=10000){
  standings <- NULL
  for(j in 1:6){
      s <- htmltab("https://www.si.com/mlb/standings", 
                       which=j)
      names(s)[1] <- "Team"
      standings <- rbind(standings, s)
  }
  
  pdata <- d[str_detect(d$playerID, lastname), ]
  tm <- standings[str_detect(standings[, 1], 
                             pdata$Team), ]
  future_games <- 162 - as.numeric(tm$WinsW) - 
                        as.numeric(tm$LossesL)
  hrfit <- fit_bb_model(list(y=d$HR, n=d$AB))
  future_AB <- round(pdata$AB / (162 - future_games) * 
                 future_games)
  p_HR <- rbeta(ITER, hrfit$eta * hrfit$K + pdata$HR,
             (1 - hrfit$eta) * hrfit$K + 
               pdata$AB - pdata$HR)
  future_HR <- rbinom(ITER, prob=p_HR, size=future_AB)
  list(current_HR = pdata$HR, current_AB = pdata$AB,
       future_G = future_games,
       future_AB = future_AB, future_HR = future_HR)
}