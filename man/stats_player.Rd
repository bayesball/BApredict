\name{stats_player}
\alias{stats_player}

\title{
  Finds hitting measures for a specific player
}
\description{
  Finds hitting measures for a specific player
}
\usage{
  stats_player(output, player)
}
\arguments{
\item{output}{
    output of component_predict function
  }
  \item{player}{
    name of player
  }
}

\value{
  hitting statistics for the player
}
\author{
  Jim Albert
}

\examples{
 ## Not run: 
 d <- collect_hitting_data()
 S <- component_predict(d)
 stats_player(S, "Trout")
}
