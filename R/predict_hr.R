predict_hr <- function(d, lastname, 
                       future_games,
                       ITER=10000){
  pdata <- d[str_detect(d$playerID, lastname), ]
  
  hrfit <- fit_bb_model(list(y=d$HR, n=d$AB))

  future_AB <- round(pdata$AB / (162 - future_games) * 
                 future_games)
  
  p_HR <- rbeta(ITER, hrfit$eta * hrfit$K + pdata$HR,
             (1 - hrfit$eta) * hrfit$K + 
               pdata$AB - pdata$HR)
  
  future_HR <- rbinom(ITER, prob=p_HR, size=future_AB)
  
  list(current_HR = pdata$HR, current_AB = pdata$AB,
       future_HR = future_HR)
}