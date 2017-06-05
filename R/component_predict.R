component_predict <- function(d){
  # input d - a list with components playerID, AB, H, HR, SO
  # output - a list with components
  #     S - a data frame with all of the component and shrinkage estimates
  #     component -- values of K and eta for all the component fits
  #     shrinkage -- values of K and eta for the shrinkage fit

  ## ------------------------------------------------------------------------
  fit.model <- function(data){
    mode <- laplace(betabinexch, c(1, 1),
                    cbind(data$y, data$n))$mode
    eta <- exp(mode[1]) / (1 + exp(mode[1]))
    K <- exp(mode[2])
    list(eta=eta, K=K,
         d=data.frame(data, est=(data$y + K * eta) / (data$n + K)))
  }
  ## ------------------------------------------------------------------------
  S.SO <- fit.model(data.frame(playerID=d$playerID,
                               y=d$SO, n=d$AB))
  S.HR <- fit.model(data.frame(playerID=d$playerID,
                               y=d$HR, n=d$AB - d$SO))
  S.H <- fit.model(data.frame(playerID=d$playerID,
                              y=d$H - d$HR,
                              n=d$AB - d$HR - d$SO))
  S <- merge(S.SO$d, S.HR$d, by="playerID")
  S <- merge(S, S.H$d, by="playerID")
  names(S) <- c("playerID", "SO", "AB", "SO.Rate",
                "HR", "AB.SO", "HR.Rate",
                "H.HR", "AB.SO.HR", "H.Rate")

  component.fit <- data.frame(eta=c(S.SO$eta, S.HR$eta, S.H$eta),
                              K=c(S.SO$K, S.HR$K, S.H$K))
  row.names(component.fit) <- c("SO", "HR", "H")

  ## ------------------------------------------------------------------------
  S$Est <- with(S,
                (1 - SO.Rate) * (HR.Rate + (1 - HR.Rate) * H.Rate))

  ## ------------------------------------------------------------------------
  S2 <- fit.model(data.frame(playerID=d$playerID,
                             y=d$H, n=d$AB))
  shrinkage.fit <- c(eta=S2$eta, K=S2$K)

  S <- merge(S, S2$d, by="playerID")
  names(S)[c(11:14)] <- c("Comp.Est", "H", "AB1", "Shrinkage.Est")
  list(S=S, component=component.fit, shrinkage=shrinkage.fit)
}
