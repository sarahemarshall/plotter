\name{createImagePlot}
\alias{createImagePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createImagePlot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createImagePlot(data, x, y, z, main = "", expbreakseq = NULL, xlabel = x, ylabel = y, zlabel = z, xlims = NULL, ylims = NULL, onefig = FALSE, figprefix = NA, log = TRUE, plotcols = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{expbreakseq}{
%%     ~~Describe \code{expbreakseq} here~~
}
  \item{xlabel}{
%%     ~~Describe \code{xlabel} here~~
}
  \item{ylabel}{
%%     ~~Describe \code{ylabel} here~~
}
  \item{zlabel}{
%%     ~~Describe \code{zlabel} here~~
}
  \item{xlims}{
%%     ~~Describe \code{xlims} here~~
}
  \item{ylims}{
%%     ~~Describe \code{ylims} here~~
}
  \item{onefig}{
%%     ~~Describe \code{onefig} here~~
}
  \item{figprefix}{
%%     ~~Describe \code{figprefix} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{plotcols}{
%%     ~~Describe \code{plotcols} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (data, x, y, z, main = "", expbreakseq = NULL, xlabel = x, 
    ylabel = y, zlabel = z, xlims = NULL, ylims = NULL, onefig = FALSE, 
    figprefix = NA, log = TRUE, plotcols = NULL, ...) 
{
    if (!is.na(figprefix)) {
        outname <- paste(figprefix, ".pdf", sep = "")
        endOfpath <- max(gregexpr("/", outname)[[1]])
        outname <- paste0(substr(outname, 1, endOfpath), substr(outname, 
            endOfpath + 1, nchar(outname)))
        pdf(file = outname)
    }
    xmat <- as.numeric(unique(data[, x]))
    ymat <- as.numeric(unique(data[, y]))
    smat <- reshape(data[, c(x, y, z)], v.names = z, idvar = x, 
        timevar = y, direction = "wide")
    smat <- as.matrix(smat[, 2:ncol(smat)])
    if (is.null(expbreakseq)) {
        expbreakseq = sort(c(min(data[, c(z)]), 0.1, 2, 5, 10, 
            50, 100, 500, 1000, 10000, 50000, 1e+06, 1e+07, 1e+08, 
            1e+09, 1e+10, max(data[, c(z)])))
    }
    if (log) {
        breakseq = c(log(expbreakseq))
        smat.adj <- log(smat)
    }
    else {
        breakseq = expbreakseq
        smat.adj <- smat
    }
    numcols = length(breakseq) - 1
    imageplotlims = c(min(breakseq), max(breakseq))
    if (onefig) {
        par(pty = "s")
        par(cex = 0.8, cex.axis = 1.5, cex.lab = 2, mar = c(5.1, 
            6.1, 4.1, 2.1))
    }
    if (is.null(plotcols)) {
        plotcols = jet.colors(length(breakseq) - 1)
    }
    s = simage(xmat, ymat, smat.adj, col = plotcols, breaks = breakseq, 
        lab.breaks = expbreakseq, legend.args = list(text = TeX(zlabel), 
            cex = 2, line = 1, at = 2.1), legend.mar = 8, legend.width = 2, 
        slim = imageplotlims, main = main, las = 1, xlab = TeX(xlabel), 
        ylab = TeX(ylabel), las = 1)
    if (!is.na(figprefix)) 
        dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
