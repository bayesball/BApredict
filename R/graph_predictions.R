graph_predictions <- function(d2){
  ggplot(d2$S, aes(H / AB1, Comp.Est)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color="red") +
  xlab("Current AVG") +
  ylab("Prediction") +
  ggtitle(paste("Final Season Predictions on", Sys.Date())) +
  theme(plot.title =
          element_text(colour = "blue", size = 18,
                       hjust = 0.5))
}
