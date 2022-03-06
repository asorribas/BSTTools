\name{TableRR}
\alias{TableRR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TableRR
}
\description{
Compute a risk ratio table.
}
\usage{
TableRR(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A variable of exposed groups.
}
  \item{y}{
A variable of events.
}
}
\details{
The risk ration is computed assuming a cohort study defined by groups in x.
    The risk is computed for the second level of y.
}
\value{
A formated risk ratio table
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
function (x, y) 
{
    df <- table(y, x)
    res <- riskratio(df)$measure \%>\% round(2)
    res \%>\% kbl(caption = "Risk ratio") \%>\% kable_styling(full_width = F, 
        html_font = "Cambria")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
