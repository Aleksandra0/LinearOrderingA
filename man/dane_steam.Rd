\name{dane_steam}
\alias{dane_steam}
\docType{data}
\title{
Data set containing Games from platform Steam
}
\description{
Data set contains 29 games from the Steam Platofrm, Released between 2007 and 2012. 

}
\usage{data("dane_steam")}
\format{
  A data frame with 29 observations on the following 7 variables.
  \describe{
    \item{\code{name}}{a character vector}
    \item{\code{release_date}}{a numeric vector}
    \item{\code{achievements}}{a numeric vector}
    \item{\code{positive_ratings}}{a numeric vector}
    \item{\code{negative_ratings}}{a numeric vector}
    \item{\code{owners}}{a numeric vector}
    \item{\code{price}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Original data set can be found on: https://www.kaggle.com/datasets/nikdavis/steam-store-games
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(dane_steam)
## maybe str(dane_steam) ; plot(dane_steam) ...
}
\keyword{datasets}
