\name{shrinkage_plot}
\alias{shrinkage_plot}

\title{
  Shrinkage plot
}
\description{
  Shrinkage plot for multilevel estimates
}
\usage{
  function(S, N = 15, seed = 12)
}
\arguments{
  \item{S}{
    Data frame with variables Player, Individual, Multilevel
  }
  \item{N}{
    Number of random players to graph
  }
  \item{seed}{
  Random number seed
  }
}

\value{
  ggplot2 object of the shrinkage plot
}
\author{
  Jim Albert
}

\examples{
 ## Not run: 
 d <- collect_hitting_data()
 fit <- fit_bb_model(list(y = d$AVG, n = d$n))
 S <- data.frame(Player = d$playerID, 
 Individual = d$H / d$AB, Multilevel = fit$d$est)
 shrinkage_plot(S)
}
