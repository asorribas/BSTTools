\name{PlotProp}
\alias{PlotProp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PlotProp title
}
\description{
Print plots
}
\usage{
PlotProp(x, y, xlab = "", ylab = "", legend = "", main = "",
    method = "sisonglaz", stacked = F, percent = T,
    colors = c("grey", "orange", "lightblue", "pink"))
}

\arguments{
  \item{x}{
A factor variable in a data.frame.
}
  \item{y}{
A factor variable in a data.frame. This will be used as groups and percentages of variable x will be computed for each level of y.
}
  \item{xlab}{
A character label for the x axis.
}
  \item{ylab}{
A character label for the y axis.
}
  \item{legend}{
A character label for the variable y.
}
  \item{main}{
A character label for the title othe plot.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{stacked}{
If TRUE, a stacked bar plot is obtaind. Default is FALSE.
}
  \item{percent}{
If TRUE (default), percent values are printed.
}
  \item{colors}{
A vector with color names to be used in bars. Default is c("grey", "orange", "lightblue", "pink")
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

birthwt \%>\% with(PlotProp(factor(race),factor(ht)))
birthwt \%>\% with(PlotProp(factor(race),factor(ht)), stacked=T)
birthwt \%>\% with(PlotProp(factor(race),factor(ht)), stacked=T) + coord_flip()

## The function is currently defined as
function (x, y, xlab = "", ylab = "", legend = "", main = "",
    method = "sisonglaz", stacked = F, percent = T, colors = c("grey",
        "orange", "lightblue", "pink"))
{
    if (class(x) != "factor")
        return(paste("Sorry, the first variable is not a factor. This function requires two factor variables"))
    if (class(y) != "factor")
        return(paste("Sorry, the second variable is not a factor. This function requires two factor variables"))
    df <- table(x, y)
    res <- TablePropCI(df, method)
    if (percent & stacked) {
        p <- geom_text(aes(label = paste(round(est * 100, 2),
            "\%")), stat = "identity", position = position_fill(vjust = 0.5))
    }
    else if (percent & stacked == F) {
        p <- geom_text(aes(label = paste(est * 100, "\%")), position = position_dodge(width = 0.9),
            vjust = -0.25)
    }
    else {
        p <- NULL
    }
    if (stacked) {
        ggplot(res, aes(x = Cols, fill = Rows, est)) + geom_bar(stat = "identity",
            color = "black") + scale_fill_manual(values = colors) +
            xlab(xlab) + ylab(ylab) + labs(fill = legend) + ylim(0,
            1) + ggtitle(main) + p
    }
    else {
        ggplot(res, aes(x = Cols, fill = Rows, est)) + geom_bar(stat = "identity",
            position = position_dodge(), color = "black") + scale_fill_manual(values = colors) +
            xlab(xlab) + ylab(ylab) + labs(fill = legend) + ylim(0,
            1) + ggtitle(main) + p
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
