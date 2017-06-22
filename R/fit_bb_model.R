fit_bb_model <- function(data){
  mode <- laplace(betabinexch, c(1, 1),
                  cbind(data$y, data$n))$mode
  eta <- exp(mode[1]) / (1 + exp(mode[1]))
  K <- exp(mode[2])
  list(eta=eta, K=K,
       d=data.frame(data, est=(data$y + K * eta) / (data$n + K)))
}