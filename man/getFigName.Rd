\name{getFigName}
\alias{getFigName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getFigName
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getFigName(str, u, x, y, z, w, numSimsCW = "unknown", theory = FALSE, outpath = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{
%%     ~~Describe \code{str} here~~
}
  \item{u}{
%%     ~~Describe \code{u} here~~
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
  \item{numSimsCW}{
%%     ~~Describe \code{numSimsCW} here~~
}
  \item{theory}{
%%     ~~Describe \code{theory} here~~
}
  \item{outpath}{
%%     ~~Describe \code{outpath} here~~
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
function (str, u, x, y, z, w, numSimsCW = "unknown", theory = FALSE, 
    outpath = "") 
{
    ulen <- length(u)
    u.char <- which(names(u) \%in\% c("distOn", "distOff", "processType"))
    u.numeric = setdiff(1:ulen, u.char)
    if (is.null(u)) {
        pars = ""
    }
    else {
        if (any(unlist(lapply(u, length)) > 1)) 
            stop("Error in getFigName(). At least one variable has more than one value.", 
                lapply(u, length))
        pars = paste(paste(names(u)[u.numeric], as.numeric(u[u.numeric]), 
            sep = "", collapse = ""), paste(names(u)[u.char], 
            (u[u.char]), sep = "", collapse = ""), sep = "")
        pars = gsub(".", "", pars, fixed = TRUE)
    }
    nsims.name <- get.nsims.name(numSimsCW)
    if (theory) 
        figname = paste(outpath, "fig", str, "_", x, y, z, "_", 
            "theory", "_", pars, sep = "")
    else figname = paste(outpath, "fig", str, "_", x, y, z, "_", 
        nsims.name, "_", pars, sep = "")
    figname = gsub(".", "dot", figname, fixed = TRUE)
    return(figname)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
