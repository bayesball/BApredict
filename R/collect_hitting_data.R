collect_hitting_data <- function(){
  d1 <- htmltab("https://www.si.com/mlb/stats", which=1)
  d1 <- mutate(d1,
               AB = as.numeric(AB),
               H = as.numeric(H),
               HR = as.numeric(HR),
               K = as.numeric(K),
               BB = as.numeric(BB))
  d1$playerID <- paste(1:(dim(d1)[1]),
                       d1$Player, sep=" ")
  names(d1)[11] <- "SO"
  dplyr::select(d1, playerID, Team, AB, H, HR, SO, BB)
}
