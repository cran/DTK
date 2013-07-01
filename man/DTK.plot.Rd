\name{DTK.plot}
\alias{DTK.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DTK Test Confidence Interval Plot}
\description{
Produces a plot of the confidence intervals produced by the function DTK.test.
}
\usage{
DTK.plot(x = "DTK.test output")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{DTK.test output list object.}
}
\details{
Produces a formatted plot of all confidence intervals of pairwise
comparisons of means. The itervals are plotted in red (=significant) and
black (=non-significant) with grey dashed lines to help distinguish the comparisons. 
}

\value{
SEE EXAMPLE.
}

\author{Matthew K. Lau, Department of Biological Sciences, Northern Arizona University, AZ}
\seealso{\code{\link{DTK.test}}, \code{\link{TK.test}}, \code{\link{TukeyHSD}}, \code{\link{qtukey}}}
\examples{
x=c(rnorm(25,5,2),rnorm(30,5,5),rnorm(35,15,5))
f<-gl.unequal(n=3,k=c(25,30,35))
DTK.result<-DTK.test(x=x,f=f,a=0.05)
DTK.result
DTK.plot(DTK.result)
}

