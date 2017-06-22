\name{predict_hr}
\alias{predict_hr}

\title{
  Simulates predictive distribution of season home runs
}
\description{
  Simulates predictive distribution of season home runs using a beta-binomial model
}
\usage{
  predict_hr(d, lastname, 
                       future_games,
                       ITER=10000)
}
\arguments{
  \item{d}{
    data frame with variables AB and HR
  }
  \item{lastname}{
      last name of player of interest
  }
  \item{future_games}{
      number of games in remainder of season
  }
}

\value{
  \item{current_HR}{
     number of current home runs
  }
  \item{current_AB}{
     number of current at-bats
  }
  \item{future_HR}{
  vector of simulated values of future home run counts from predictive distribution
  }
}
\author{
  Jim Albert
}

\examples{
 ## Not run: 
 d <- collect_hitting_data()
 S <- predict_hr(d, "Trout", 90)
}
