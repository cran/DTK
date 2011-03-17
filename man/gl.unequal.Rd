\name{gl.unequal}
\alias{gl.unequal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate Levels with Unequal Sample SIzes}
\description{
Produces a vector of samples of unequal sizes. Useful when observations are already sorted into groups by levels.
}
\usage{
gl.unequal(n = "number of levels", k = "numeric vector of sample sizes")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Scalar determining the number of levels.}
  \item{k}{Numeric vector specifying the sample size at each level.}
}
\value{
Produces a factored vector.}

\author{Matthew K. Lau, Department of Biological Sciences, Northern Arizona University, AZ}
\note{Be sure that you precisely specify the above arguments to correspond to your observation vector.
}
\seealso{  \code{\link{gl}}, \code{\link{rep}} }
\examples{
gl.unequal(n=3,k=c(25,30,35))
}
