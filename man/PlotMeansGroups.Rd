\name{PlotMeansGroups}
\alias{PlotMeansGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PlotMeansGroups
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMeansGroups(x, y, g, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{g}{
%%     ~~Describe \code{g} here~~
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
function (x, y, g, data)
{
    group <- enquo(y)
    xvar <- enquo(x)
    fg <- enquo(g)
    res <- data \%>\% group_by(!!group, !!fg) \%>\% summarise(m = mean(!!xvar),
        lower = t.test(!!xvar)$conf.int[1], upper = t.test(!!xvar)$conf.int[2])
    ggplot(res, aes(x = !!group, y = m, group = !!fg)) + geom_errorbar(aes(ymax = upper,
        ymin = lower, color = !!fg), size = 1, width = 0.3, position = position_dodge(0.9)) +
        geom_point(size = 4, shape = 21, color = "black", fill = "white",
            position = position_dodge(0.9))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
