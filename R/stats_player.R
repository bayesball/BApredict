stats_player <- function(output, player){
  out$S[str_detect(output$S$playerID, player), ]
}
