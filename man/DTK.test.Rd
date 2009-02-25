\name{DTK.test}
\alias{DTK.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Dunnett's Modified Tukey-Kramer Pairwise Multiple Comparison Test}
\description{
Conducts a pairwise multiple comparison test for mean differences with unequal sample sizes and no assumption of equal population variances. 
}
\usage{
DTK.test(x = "data vector", f = "factor vector", a = "alpha level")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Numeric data vector.}
  \item{f}{Factored level vector.}
  \item{a}{Alpha, significance level. DEFAULT=0.05}
}
\details{
Input data as vectors. 
}
\value{
  \item{[[1]]}{"a" or the alpha significance level}
  \item{[[2]]}{Matrix containing the pair-waise comparisons as row names and the pairwise mean differences and lower and upper confidence interval values in columns, respectively}
}
\references{Dunnett,C.W. (1980) Pairwise Multiple Comparisons in the Unequal Variance Case. Journal of the American Statistical Association. 75 (372): 796-800.}
\author{Matthew K. Lau, Department of Biological Sciences, Northern Arizona University, AZ}
\note{In the case of equal sample sizes and equal population variances, Dunnett's test produces slightly wider (i.e. more conservative) confidence intervals than the Tukey-Kramer procedure. This is because of differences in the degrees of freedom used for determining the Studentized Range values. In cases where variances are unequal, however, the Tukey-Kramer test, which uses the pooled variance, will spread variance across levels and produce misleading results.
}
\seealso{\code{\link{DTK.plot}}, \code{\link{gl.unequal}}, \code{\link{TK.test}}, \code{\link{TukeyHSD}}, \code{\link{qtukey}}}
\examples{
x=runif(54,0,100)
f<-gl.unequal(n=5,k=c(10,9,10,15,10))
DTK.result<-DTK.test(x=x,f=f,a=0.05)
DTK.result
DTK.plot(DTK.result)
}
