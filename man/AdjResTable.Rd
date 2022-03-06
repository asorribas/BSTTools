\name{AdjResTable}
\alias{AdjResTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AdjResTable}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AdjResTable(x, y, caption = "", full_width = T, count = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{caption}{
%%     ~~Describe \code{caption} here~~
}
  \item{full_width}{
%%     ~~Describe \code{full_width} here~~
}
  \item{count}{
%%     ~~Describe \code{count} here~~
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
function (x, y, caption = "", full_width = T, count = T)
{
    if (class(x) != "factor")
        return(paste("Sorry, the first variable is not a factor. This function requires two factor variables"))
    if (class(y) != "factor")
        return(paste("Sorry, the second variable is not a factor. This function requires two factor variables"))
    tab <- table(x, y)
    print(tab)
    name <- levels(x)
    p1 <- tab \%>\% chisq.residuals(std = T)
    p2 <- tab \%>\% prop.table(2) * 100
    p2 <- round(p2, 2)
    if (count) {
        res <- c()
        for (i in 1:dim(p1)[1]) {
            r0 <- c(name[i], "n", tab[i, ])
            r1 <- c("", "\%", p2[i, ])
            r2 <- c("", "Adj.res", p1[i, ])
            res <- rbind(res, r0, r1, r2)
            rownames(res) <- NULL
        }
    }
    else {
        res <- c()
        for (i in 1:dim(p1)[1]) {
            r1 <- c(name[i], "\%", p2[i, ])
            r2 <- c("", "Adj.res", p1[i, ])
            res <- rbind(res, r1, r2)
            rownames(res) <- NULL
        }
    }
    res \%>\% kbl(caption = caption) \%>\% kable_styling(full_width = full_width,
        html_font = "Cambria")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
