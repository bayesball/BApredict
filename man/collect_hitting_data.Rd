\name{collect_hitting_data}
\alias{collect_hitting_data}

\title{
  Collect Hitting Data fromo SI Site
}
\description{
  Downloads current hitting data from Sports Illustrated website
}
\usage{
  collect_hitting_data()
}
\value{
  data frame with variables playerID, AB, H, HR, SO
}
\author{
  Jim Albert
}

\examples{
 ## Not run: 
 d <- collect_hitting_data()
}
