\name{addSubplot}
\alias{addSubplot}
addSubplot
\title{
addSubplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addSubplot(subPlotParams, x, y, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subPlotParams}{
%%     ~~Describe \code{subPlotParams} here~~
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
function (subPlotParams, x, y, z) 
{
    df.subplot <- subPlotParams$df
    xlim.subplot <- subPlotParams$xlim
    ylim.subplot <- subPlotParams$ylim
    cex.axis.subplot <- subPlotParams$cex.axis
    text.subplot <- subPlotParams$text.subplot
    cex.points <- subPlotParams$cex.points
    x.plotSize <- subPlotParams$x.plotSize
    y.plotSize <- subPlotParams$y.plotSize
    pchs <- subPlotParams$pchs
    ltys <- subPlotParams$ltys
    cols <- subPlotParams$cols
    lwds <- subPlotParams$lwds
    zval = unique(df.subplot[, z])
    subplot(by(df.subplot, df.subplot[, z], function(xx) {
        if (unique(match(xx[, z], zval)) == 1) {
            plot(NA, NA, ylab = "", xlab = "", xlim = xlim.subplot, 
                ylim = ylim.subplot, cex.axis = cex.axis.subplot)
            text(text.subplot$x, text.subplot$y, text.subplot$text, 
                pos = text.subplot$pos, cex = text.subplot$cex)
        }
        points(xx[, x], xx[, y], type = "o", pch = pchs[match(xx[, 
            z], zval)], cex = cex.axis.subplot, col = cols[match(xx[, 
            z], zval)], lty = ltys[match(xx[, z], zval)], lwd = lwds[match(xx[, 
            z], zval)])
    }), x = grconvertX(x.plotSize, from = "npc"), y = grconvertY(y.plotSize, 
        from = "npc"), type = "fig", pars = list(mar = c(1, 1, 
        0, 0) + 0.1, mgp = c(3, 0.5, 0)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
