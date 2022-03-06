\name{PlotPropCI}
\alias{PlotPropCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PlotPropCI
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotPropCI(x, y, xlab = "", ylab = "", legend = "", method = "sisonglaz")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A factor in a data.frame. Proportions are computed for the different levels of variabel indicated in y.
}
  \item{y}{
A factor in a data.frame. Proportion of the levels in x are computed for each level of y
}
  \item{xlab}{
An optional label for x
}
  \item{ylab}{
An optional label for y
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

with(regicor,
     PlotPropCI(death,smoker))
     
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
