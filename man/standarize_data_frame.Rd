\name{standarize_data_frame}
\alias{standarize_data_frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardize each column of the data frame.
}
\description{
Function standardize each column of the data frame
}
\usage{
standarize_data_frame(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
df: data frame
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
standardized_df: data frame (Data Frame with standradized values in each column)

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
function (df)
{
    stopifnot(is_numeric_values(df))
    standardized_df <- as.data.frame(lapply(df, function(x) (x -
        mean(x))/sd(x)))
    return(standardized_df)
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
