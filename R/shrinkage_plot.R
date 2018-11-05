shrinkage_plot <- function(S, N = 15, seed = 12){
  # S has variables Player, Individual, Multilevel
  set.seed(seed)
  S2 <- gather(sample_n(S, size = N), 
               Type, AVG, -Player)
  S2a <- filter(S2, Type == "Individual")
  ggplot(S2, aes(Type, AVG, group=Player)) +
    geom_line() + geom_point() +
    ggplot2::annotate("text", x = 0.75, y = S2a$AVG, 
                      label = S2a$Player) +
    ggtitle("Shrinkage Plot") +
    theme(plot.title = element_text(
      colour = "blue", size = 18, hjust = 0.5))
}