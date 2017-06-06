stats_player <- function(output, player){
  output$S[str_detect(output$S$playerID, player), ]
}
