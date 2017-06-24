graph_components <- function(out){
  d1 <- data.frame(SO.Rate = out$S$SO / out$S$AB,
                 BABIP.Rate = out$S$H.HR / out$S$AB.SO.HR,
                 AVG = out$S$H / out$S$AB1,
                 Type="Observed")
  d2 <- data.frame(SO.Rate = out$S$SO.Rate,
                 BABIP.Rate = out$S$H.Rate,
                 AVG = out$S$Comp.Est,
                 Type="Predicted")
  d <- rbind(d1, d2)
  d$BA <- ifelse(d$AVG > .3, "AVG > .300", "AVG <= .300")
  ggplot(d, aes(SO.Rate, BABIP.Rate, color=BA)) + 
    geom_point() +
    facet_wrap(~ Type, ncol=1) +
    coord_equal()
}
