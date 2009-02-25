\name{DTK.plot}
\alias{DTK.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DTK Test Confidence Interval Plot}
\description{
Produces a plot of the confidence intervals produced by the function \code{\link{DTK.test}}.
}
\usage{
DTK.plot(x = "DTK.test output")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{DTK.test output list object.}
}
\details{
SEE EXAMPLE.
}
\value{
Produces a formatted plot of all confidence intervals of pairwise comparisons of means.
}

\author{Matthew K. Lau, Department of Biological Sciences, Northern Arizona University, AZ}
\seealso{\code{\link{DTK.test}}, \code{\link{TK.test}}, \code{\link{TukeyHSD}}, \code{\link{qtukey}}}
\examples{
x=runif(54,0,100)
f<-gl.unequal(n=5,k=c(10,9,10,15,10))
DTK.result<-DTK.test(x=x,f=f,a=0.05)
DTK.result
DTK.plot(DTK.result)
}

