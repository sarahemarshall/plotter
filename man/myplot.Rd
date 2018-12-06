\name{myplot}
\alias{myplot}
myplot
\title{
myplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
myplot(data, x, y, z, w = NA, plotType = "l", lineStyle = rep(1:6, 10), pchs = 0:18, cols = 1:100, legpos = "topright", main = "", xlabel = x, ylabel = y, zlabel = z, xlims = NULL, ylims = NULL, onefig = FALSE, addPoint = list(type = FALSE), figprefix = NA, cexleg = 1, bwPlot = FALSE, figprefix_bw = "", mar.add = NULL, subplot = FALSE, subPlotParameters = NULL, text.plot = NULL, sig = 10, logscale = NULL, ...)
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
  \item{w}{
%%     ~~Describe \code{w} here~~
}
  \item{plotType}{
%%     ~~Describe \code{plotType} here~~
}
  \item{lineStyle}{
%%     ~~Describe \code{lineStyle} here~~
}
  \item{pchs}{
%%     ~~Describe \code{pchs} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{legpos}{
%%     ~~Describe \code{legpos} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
  \item{addPoint}{
%%     ~~Describe \code{addPoint} here~~
}
  \item{figprefix}{
%%     ~~Describe \code{figprefix} here~~
}
  \item{cexleg}{
%%     ~~Describe \code{cexleg} here~~
}
  \item{bwPlot}{
%%     ~~Describe \code{bwPlot} here~~
}
  \item{figprefix_bw}{
%%     ~~Describe \code{figprefix_bw} here~~
}
  \item{mar.add}{
%%     ~~Describe \code{mar.add} here~~
}
  \item{subplot}{
%%     ~~Describe \code{subplot} here~~
}
  \item{subPlotParameters}{
%%     ~~Describe \code{subPlotParameters} here~~
}
  \item{text.plot}{
%%     ~~Describe \code{text.plot} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
}
  \item{logscale}{
%%     ~~Describe \code{logscale} here~~
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
function (data, x, y, z, w = NA, plotType = "l", lineStyle = rep(1:6, 
    10), pchs = 0:18, cols = 1:100, legpos = "topright", main = "", 
    xlabel = x, ylabel = y, zlabel = z, xlims = NULL, ylims = NULL, 
    onefig = FALSE, addPoint = list(type = FALSE), figprefix = NA, 
    cexleg = 1, bwPlot = FALSE, figprefix_bw = "", mar.add = NULL, 
    subplot = FALSE, subPlotParameters = NULL, text.plot = NULL, 
    sig = 10, logscale = NULL, ...) 
{
    if (is.na(w)) {
        par(mfrow = c(1, 1), ...)
        numw = 1
        namew = ""
    }
    else {
        numw = length(unique(data[, w]))
        if (onefig == TRUE) {
            par(mfrow = c(1, 1), ...)
        }
        else {
            if (numw == 1) 
                par(mfrow = c(1, 1), ...)
            else if (numw <= 2) 
                par(mfrow = c(1, 2), ...)
            else if (numw <= 3) 
                par(mfrow = c(1, 3), ...)
            else if (numw <= 4) 
                par(mfrow = c(2, 2), ...)
            else if (numw <= 6) 
                par(mfrow = c(2, 3), ...)
            else if (numw <= 8) 
                par(mfrow = c(2, 4), ...)
            else par(mfrow = c(3, 3), ...)
        }
    }
    for (i in 1:numw) {
        if (is.na(w)) 
            data.sub = data
        else {
            data.sub = subset(data, subset = (data[, w] == unique(data[, 
                w])[i]))
            namew = paste(w, unique(data[, w])[i], sep = "")
        }
        title = paste(main, "\n", namew)
        if (!is.na(figprefix)) {
            outname <- paste(figprefix, gsub(".", "", namew, 
                fixed = TRUE), ".pdf", sep = "")
            endOfpath <- max(gregexpr("/", outname)[[1]])
            outname <- paste0(substr(outname, 1, endOfpath), 
                figprefix_bw, substr(outname, endOfpath + 1, 
                  nchar(outname)))
            pdf(file = outname)
            title = ""
            mar.default <- c(5, 4, 4, 2) + 0.1
            if (is.null(mar.add)) {
                mar.add <- c(0, 1, 0, 0)
            }
            par(mar = mar.default + mar.add)
        }
        if (is.null(xlims)) 
            new_xlims = c(min(data.sub[, x]), max(data.sub[, 
                x]))
        else new_xlims = xlims
        if (is.null(ylims)) {
            new_ylims = c(min(data.sub[, y]), max(data.sub[, 
                y]))
        }
        else if (is.list(ylims)) {
            if (any(is.na(ylims[[i]]))) {
                if (which(is.na(ylims[[i]])) == 1) 
                  new_ylims = c(min(data.sub[, y]), ylims[[i]][2])
                else if (which(is.na(ylims[[i]])) == 2) 
                  new_ylims = c(ylims[[i]][1], max(data.sub[, 
                    y]) * 1.2)
            }
            else {
                new_ylims = ylims[[i]]
            }
        }
        else if (any(is.na(ylims))) {
            if (which(is.na(ylims)) == 1) 
                new_ylims = c(min(data.sub[, y]), ylims[2])
            else if (which(is.na(ylims)) == 2) 
                new_ylims = c(ylims[1], max(data.sub[, y]) * 
                  1.2)
        }
        else {
            new_ylims = ylims
        }
        if (is.null(logscale)) {
            logs <- ""
        }
        else {
            logs <- logscale
        }
        plot(data.sub[, x], data.sub[, y], type = "n", xlim = new_xlims, 
            ylim = new_ylims, main = title, xlab = TeX(xlabel), 
            ylab = TeX(ylabel), log = logs, ...)
        zval = unique(data.sub[, z])
        numz = length(zval)
        by(data.sub, data.sub[, z], function(xx) {
            points(xx[, x], xx[, y], type = plotType, pch = pchs[match(xx[, 
                z], zval)], col = cols[match(xx[, z], zval)], 
                lty = lineStyle[match(xx[, z], zval)], ...)
        })
        if (addPoint$type == FALSE) {
        }
        else {
            if (addPoint["zline"] > 0 && addPoint["type"] == 
                "min") {
                by(data.sub, data.sub[, z], function(xx) {
                  if ((unique(xx[, z]) == addPoint$zline && i \%in\% 
                    addPoint$wgraph)) {
                    minPointY = min(xx[, y])
                    minPointY_indy = which(xx[, y] == minPointY)
                    minPointX = xx[minPointY_indy, x]
                    points(minPointX, minPointY, type = "p", 
                      pch = 16, col = cols[match(xx[, z], zval)], 
                      ...)
                    text(minPointX, minPointY, labels = paste("(", 
                      round(minPointX, sig), ",", round(minPointY, 
                        sig), ")"), pos = 3, ...)
                  }
                })
            }
            else if (addPoint$zline > 0 && i \%in\% addPoint$wgraph && 
                addPoint$type == "intersection") {
                if (is.null(addPoint$subset)) 
                  data.sub.point <- data.sub
                else data.sub.point <- data.sub[addPoint$subset, 
                  ]
                a = aggregate(data.sub.point[, y], by = list(data.sub.point[, 
                  x]), FUN = summary)
                aa = cbind(var1 = a$Group.1, a$x)
                dif = aa[, "Max."] - aa[, "Min."]
                mindif = min(dif)
                indy = which(mindif == dif)
                xval = aa[indy, "var1"]
                yvals = data.sub.point[data.sub.point[, x] == 
                  xval, y]
                points(xval, mean(yvals), pch = 16)
                text(xval, mean(yvals), paste("(", round(xval, 
                  sig), ", ", round(mean(yvals), sig), ")", sep = ""), 
                  pos = 4, cex = 1)
            }
        }
        if (legpos[1] != "off") {
            if (length(legpos) > 1 && length(legpos) != numw) 
                stop("functions_plot: not enough legpos values provided")
            if (is.list(legpos)) {
                legpos_w <- legpos[[i]]
            }
            else if (length(legpos) > 1) {
                legpos_w <- legpos[i]
            }
            else {
                legpos_w <- legpos[1]
            }
            if (class(zval) == "numeric" || class(zval) == "integer") {
                if (length(zlabel) == 1) 
                  legtext <- sprintf(zlabel, zval)
                else {
                  legtext <- sprintf(zlabel[1], zval[1:zlabel[2]])
                  legtext <- c(legtext, sprintf(zlabel[3], zval[(as.numeric(zlabel[2]) + 
                    1):length(zval)]))
                }
                legend(legpos_w, legend = TeX(legtext), pch = pchs, 
                  col = cols, lty = lineStyle, pt.bg = cols, 
                  cex = cexleg, lwd = 2)
            }
            else {
                legend(legpos_w, legend = paste(zlabel, zval), 
                  pch = pchs, col = cols, cex = cexleg)
            }
        }
        if (!is.null(text.plot)) {
            text(text.plot$x, text.plot$y, text.plot$text, pos = text.plot$pos, 
                cex = text.plot$cex)
        }
        if (subplot) {
            addSubplot(subPlotParams = subPlotParameters, x = x, 
                y = y, z = z)
        }
    }
    if (!is.na(figprefix)) {
        graphics.off()
    }
    if (bwPlot) {
        figprefix_bw <- figprefix
        if (!is.na(figprefix)) 
            figprefix_bw <- "bw_"
        lineStyle.bw <- lineStyle
        if (length(unique(lineStyle)) < numz) 
            lineStyle.bw <- 1:numz
        subPlotParameters$cols <- rep(1, numz)
        subPlotParameters$ltys <- lineStyle.bw
        numz.total = length(unique(data[, z]))
        myplot(data, x, y, z, w, plotType, lineStyle.bw, pchs, 
            cols = rep("black", numz.total), legpos, main, xlabel, 
            ylabel, zlabel, xlims, ylims, onefig, addPoint, figprefix, 
            cexleg = 1, bwPlot = FALSE, figprefix_bw = figprefix_bw, 
            subplot = subplot, subPlotParameters = subPlotParameters, 
            ...)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
