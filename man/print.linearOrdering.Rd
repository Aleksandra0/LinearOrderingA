\name{print.linearOrdering}
\alias{print.linearOrdering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prints to console: call method of function, method used for Linear Ordering, Ranking with ordered Items.
}
\description{
Function prints to console: call method of function, method used for Linear Ordering, Ranking with ordered Items
  }
\usage{
print.linearOrdering(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
rank_object: list (result of linearOrdering function - custom list containing call method to function, data frame with ordered items (Name, Result), method used for linear ordering)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  nothing
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
function (x)
{
    cat("call: ")
    print(x$call)
    cat("Method used for Linear Ordeing: ")
    if (x$method == "standsum") {
        print("Standardized Sums Method")
    }
    else if (x$method == "hellwig") {
        print("Hellwig's Method")
    }
    else {
        print("Topsis Method")
    }
    cat("Rank (Name, Score):\n")
    for (i in 1:nrow(x$rank)) {
        if (i == 1 || i == 2 || i == 3) {
            cat("\033[1;33m")
        }
        cat(i)
        cat(": ")
        cat(paste(x$rank[i, ], collapse = "\t"))
        cat("\n")
        cat("\033[0m")
    }
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
