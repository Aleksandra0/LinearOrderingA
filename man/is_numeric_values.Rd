\name{is_numeric_values}
\alias{is_numeric_values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checking whether provided list contains numeric values
}
\description{
Function is checking whether provided list contains numeric values
}
\usage{
is_numeric_values(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
vec: list (of any size)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
boolean (TRUE if all values are bumeric, FALSE otherwise)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (vec)
{
    all(sapply(vec, is.numeric))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
