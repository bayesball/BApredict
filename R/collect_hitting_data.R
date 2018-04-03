collect_hitting_data <- function(){
  read_html("https://www.si.com/mlb/stats") %>% 
    html_table(fill = TRUE) -> d
  d1 <- d[[1]]
  names(d1)[13] <- "AVG"
  names(d1)[11] <- "SO"
#  d1$playerID <- paste(1:(dim(d1)[1]),
#                      d1$Player, sep=" ")
  dplyr::select(d1, Player, Team, AB, H, HR, SO, BB)
}
