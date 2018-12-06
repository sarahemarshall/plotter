\name{getUniqueValues}
\alias{getUniqueValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getUniqueValues
}
\description{
Determines unique values in a dataframe, excluding named columns.
}
\usage{
getUniqueValues(df, x, y, z, w, ncols = dim(df)[2])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A dataframe
}
  \item{x}{
Name of a column in df
}
  \item{y}{
Name of a column in df
}
  \item{z}{
Name of a column in df
}
  \item{w}{
Name of a column in df
}
  \item{ncols}{
Number of columns of df to use
}
}
\details{
Returns unique values.
}
\author{
Sarah Marshall
}
\examples{

df <- data.frame(x = 1:12, y = 2:13, z=NA, w = NA, a = rep(1:3, 4), b=rep(5:8, each=3), c = rep(letters[1:3], 4), d = 12:1)

getUniqueValues(df, x="x", y="y", z="z", w="w", ncols=3)
getUniqueValues(df, x="x", y="y", z="z", w="w", ncols=6)
getUniqueValues(df, x="x", y=NULL, z="z", w=NULL, ncols=8)
}

