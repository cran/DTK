\name{TK.test}
\alias{TK.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tukey's Honestly Significant Difference Test}
\description{
This is a reformatted function for DTK.test function-like inputs to use the TukeyHSD function.
}
\usage{
TK.test(x = "data vector", f = "factor vector", a = "alpha level")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data vector}
  \item{f}{Factor vector}
  \item{a}{Alpha, significance level. DEFAULT=0.05}
}

\value{TukeyHSD list output.}

\author{Matthew K. Lau, Department of Biological Sciences, Northern Arizona University, AZ}
\seealso{\code{\link{DTK.test}}, \code{\link{DTK.plot}}, \code{\link{TukeyHSD}}, \code{\link{qtukey}}}
\examples{
x=c(rnorm(25,5,2),rnorm(30,5,5),rnorm(35,15,5))
f<-gl.unequal(n=3,k=c(25,30,35))
DTK.result<-DTK.test(x=x,f=f,a=0.05)
TK.result<-TK.test(x=x,f=f,a=0.05)
DTK.result
TK.result
}

