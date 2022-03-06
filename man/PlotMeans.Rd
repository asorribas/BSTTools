\name{PlotMeans}
\alias{PlotMeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PlotMeans
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMeans(x, y, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The variable for which means are to be computed.
}
  \item{y}{
Grouping variable. The means and CI are computed for each level of y.
}
  \item{data}{
data.frame that contains the variables
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
#
PlotMeans(age,sex,data=predimed)
predimed \%>\% with(PlotMeans(age,sex,data=.))

## The function is currently defined as
function (x, y, data)
{
    group <- enquo(y)
    xvar <- enquo(x)
    res <- data \%>\% group_by(!!group) \%>\% summarise(m = mean(!!xvar),
        low = t.test(!!xvar)$conf.int[1], upper = t.test(!!xvar)$conf.int[2])
    ggplot(res, aes(x = !!group, y = m)) + geom_errorbar(aes(ymax = upper,
        ymin = low), size = 1, width = 0.3, color = "blue") +
        geom_point(size = 4, shape = 21, color = "black", fill = "white")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
