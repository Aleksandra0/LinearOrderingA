\name{plot.linearOrdering}
\alias{plot.linearOrdering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots table with all of the ordered items or top three results.
}
\description{
Function plots table with all of the ordered items or top three results. Function uses plotly and ggplot2 packages.
}
\usage{
plot.linearOrdering(x, type = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
rank_object: list (result of linearOrdering function - custom list containing call method to function, data frame with ordered items (Name, Result), method used for linear ordering)
}
  \item{type}{
type: character, (function is ploting table with all of the ordered items for value "all" or top three items for value "top three"), default value for type is "all"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%% nothing
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
function (x, type = "all")
{
    library(plotly)
    if (type == "all") {
        table_plot <- plot_ly(type = "table", header = list(values = colnames(x$rank),
            align = "center", fill = list(color = "#daf0ff")),
            cells = list(values = list(x$rank$Name, x$rank$Result),
                align = "center"), columnwidth = c(80, 80))
        table_plot
    }
    else if (type == "top three") {
        if (nrow(x$rank) >= 3) {
            x$rank$Rank <- rank(x$rank$Result, ties.method = "min")
            ggplot(x$rank[1:3, ], aes(x = reorder(Name, -Result),
                y = Result, fill = factor(Result))) + geom_bar(stat = "identity") +
                scale_fill_manual(values = c("#a77044", "#d7d7d7",
                  "#d6af36")) + labs(title = "Ranking - Top Three Results",
                x = "Name", y = "Score") + theme_minimal()
        }
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
