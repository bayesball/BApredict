\name{graph_predictions}
\alias{graph_predictions}

\title{
  Scatterplot of observed and predicted batting averages
}
\description{
  Scatterplot of observed and predicted batting averages
}
\usage{
  graph_predictions(d2)
}
\arguments{
  \item{d2}{
    output from component_predict() function
  }
}

\value{
  ggplot2 object of the scatterplot
}
\author{
  Jim Albert
}

\examples{
 ## Not run: 
 d <- collect_hitting_data()
 S <- component_predict(d)
 graph_predictions(S)
}
