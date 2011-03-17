\name{DTK-package}
\alias{DTK-package}
\alias{DTK}
\docType{package}
\title{
Dunnett-Tukey-Kramer Pairwise Multiple Comparison Test Adjusted for Unequal Variances and Unequal Sample Sizes
}
\description{
Functions for conducting and plotting Dunnett's (1980) modified Tukey-Kramer pairwise multiple comparison test accounting for unequal variance and unequal sample sizes.}
\details{
\tabular{ll}{
Package: \tab DTK\cr
Type: \tab Package\cr
Version: \tab 3.1\cr
Date: \tab 2011-03-17\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
This package was created to analyze multi-level one-way experimental designs. It is designed to handle vectorized observation and factor data where there are unequal sample sizes and population variance homogeneity can not be assumed. To conduct the Dunnett modified Tukey-Kramer test (a.k.a. the T3 Procedure), create two vectors: one for your observations and one for the factor level of each observation. The function, \code{\link{gl.unequal}}, provides a means to more conveniently produce a factor vector with unequal sample sizes. Next, use the \code{\link{DTK.test}} function to conduct the test and save the output as an object to input into the \code{\link{DTK.plot}} function, which produces a confidence interval plot for each of the pairwise comparisons. Lastly, the function (\code{\link{TK.test}}) conducts the original Tukey-Kramer test.
}
\author{
Matthew K. Lau, Department of Biological Sciences, Northern Arizona University, AZ

Maintainer: Matthew K. Lau <mkl48@nau.edu>
}
\note{I would like to acknowledge the invaluable help of Professor Brent
  Burch in the Department of Mathematics and Statistics at Northern
  Arizona University and suggestions by multiple R- users: including
  M. Nunez, B. Roustan and S. Marshall.}

\references{

Dunnett, C.W. (1980) Pairwise Multiple Comparisons in the Unequal Variance Case. Journal of the American Statistical Association. 75 (372): 796-800. 

}
\seealso{\code{ \link{gl.unequal}}, \code{\link{DTK.test}}, \code{\link{DTK.plot}}, \code{\link{TK.test}}
}
