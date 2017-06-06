\name{newgraph}
\alias{newgraph}

\title{
  Scatterplots of observed and predicted component rates
}
\description{
  Scatterplots of observed and predicted component rates
}
\usage{
  newgraph(d2)
}
\arguments{
  \item{d2}{
    output from component_predict() function
  }
}

\value{
  ggplot2 object of the scatterplots
}
\author{
  Jim Albert
}

\examples{
 ## Not run: 
 d <- collect_hitting_data()
 S <- component_predict(d)
 newgraph(S)
}
