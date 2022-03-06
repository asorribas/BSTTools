\name{TableMeansCI}
\alias{TableMeansCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TableMeansCI
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TableMeansCI(x, y, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
function (x, y, data)
{
    group <- enquo(y)
    xvar <- enquo(x)
    res <- data \%>\% group_by(!!group) \%>\% summarise(mean = mean(!!xvar) \%>\%
        round(2), low = t.test(!!xvar)$conf.int[1] \%>\% round(2),
        upper = t.test(!!xvar)$conf.int[2] \%>\% round(2))
    res \%>\% kbl(caption = "95\% CI for means") \%>\% kable_styling(full_width = F,
        html_font = "Cambria")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
