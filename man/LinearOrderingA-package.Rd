\name{LinearOrderingA-package}
\alias{LinearOrderingA-package}
\alias{LinearOrderingA}
\docType{package}
\title{
\packageTitle{LinearOrderingA}
}
\description{
\packageDescription{LinearOrderingA}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{LinearOrderingA}
\packageIndices{LinearOrderingA}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
\packageAuthor{LinearOrderingA}

Maintainer: \packageMaintainer{LinearOrderingA}
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
if (!require(readxl)) install.packages("readxl")
library(readxl)
dane_steam <- read_excel("dane_steam.xlsx")
dane_steam <- dane_steam[-c(4),]
rank_object = linearOrdering.default(dane_steam[,1], dane_steam[,c(2,4,6)], destimulants = dane_steam[,c(5,7)], nominants = dane_steam[,3], optimals = c(50), method = "hellwig")
print.linearOrdering(rank_object)
plot.linearOrdering(rank_object)
plot.linearOrdering(rank_object, type="top three")

}
