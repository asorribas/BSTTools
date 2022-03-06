\name{PlotPropTableCI}
\alias{PlotPropTableCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PlotPropTableCI
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotPropTableCI(df, xlab = "", ylab = "", legend = "", method = "sisonglaz")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
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
function (df, xlab = "", ylab = "", legend = "", method = "sisonglaz")
{
    res <- TablePropCI(df, method)
    ggplot(res, aes(x = Cols, fill = Rows, est)) + geom_bar(stat = "identity",
        position = position_dodge()) + geom_errorbar(aes(ymin = lwr.ci,
        ymax = upr.ci), position = position_dodge(0.9), size = 1,
        color = "black", width = 0.1) + geom_point(aes(x = Cols,
        fill = Rows), colour = "red", size = 3, position = position_dodge(0.9)) +
        scale_fill_manual(values = c("grey", "orange", "brown",
            "red")) + xlab(xlab) + ylab(ylab) + labs(fill = legend) +
        ylim(0, 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
