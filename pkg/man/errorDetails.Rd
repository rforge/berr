\name{errorDetails}
\alias{errorDetails}
\title{errorDetails}
\description{Calculate the components of the exact breakpoint error. We assume
there is a latent piecewise constant signal defined on
1,...,last.base.}
\usage{
errorDetails(guess,
breaks,
last.base)
}
\arguments{
  \item{guess}{Integer vector of bases after which you estimate a break occured.}
  \item{breaks}{Integer vector of bases after which there are breaks in the latent
signal.}
  \item{last.base}{Integer scalar, the last base of the latent signal to model.}
}

\value{A list with components used to calculate the breakpoint error.}

\author{Toby Dylan Hocking}




\examples{
## First define a latent signal and calculate its exact breakpoints.
seg.size <- 10000
means <- c(-3,0,3,0,2,-1,3)/3
mu <- do.call(c,lapply(means,rep,seg.size))
mu.break.after <- which(diff(mu)!=0)

## Choose some bases to sample, and pick them from a normal
## distribution.
base <- as.integer(seq(1,length(mu),l=500))
set.seed(1)
signal <- rnorm(length(base),mu[base],1)

## Segment that signal with cghseg, and quantify model fit using the
## breakpointError.
result <- run.cghseg(signal,base,maxSegments=15)
str(errorDetails(result$breaks[[4]], mu.break.after, length(mu)))
str(errorDetails(result$breaks[[10]], mu.break.after, length(mu)))
}
