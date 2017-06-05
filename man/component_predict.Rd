\name{component_predict}
\alias{component_predict}

\title{
  Computes component predictions of batting avereages
}
\description{
  Computes component predictions of batting avereages
}
\usage{
  component_predict(d)
}
\arguments{
  \item{d}{
    data frame with variables playerID, AB, H, HR, SO
  }
}

\value{
  \item{S}{
  data frame with variables playerID, SO, AB, SO.Rate, HR, AB.SO, HR.Rate, H.HR, AB.SO.HR, H.Rate, Comp.Est, H, AB1, Shrinkage.Est
  }
  \item{component}{data frame with values of eta and K for each component fit}
  \item{shrinkage}{values of eta and K for general multilevel fit}
}
\author{
  Jim Albert
}

\examples{
 ## Not run: 
 d <- collect_hitting_data()
 S <- component_predict(d)
}
