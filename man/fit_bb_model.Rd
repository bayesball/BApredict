\name{fit_bb_model}
\alias{fit_bb_model}

\title{
  Fits a beta-binomial exchangeable model
}
\description{
  Fits a beta-binomial exchangeable model
}
\usage{
  fit_bb_model(d)
}
\arguments{
  \item{d}{
    list with components y and n
  }
}

\value{
  \item{eta}{estimate of mean of proportion}
  \item{K}{estimate of precision parameter}
  \item{est}{posterior mean estimates at the rates}
}
\author{
  Jim Albert
}

\examples{
 d <- list(y = c(2,5,4,10,14),
           n = rep(20, 5))
 fit_bb_model(d)
}
