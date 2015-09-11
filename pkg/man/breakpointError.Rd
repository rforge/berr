\name{breakpointError}
\alias{breakpointError}
\title{breakpointError}
\description{Calculate the exact breakpoint error. We assume there is a latent
piecewise constant signal defined on 1,...,last.base.}
\usage{breakpointError(guess, breaks, last.base)}
\arguments{
  \item{guess}{Integer vector of bases after which you estimate a break occured.}
  \item{breaks}{Integer vector of bases after which there are breaks in the latent
signal.}
  \item{last.base}{Integer scalar, the last base of the latent signal to model.}
}

\value{Numeric scalar giving the breakpoint error of the guess.}

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
par(mfrow=c(3,1),las=1)
plot(signal~base,
     main="Latent signal (blue) and noisy observations (black)")
lines(seq_along(mu),mu,lwd=3,col=signal.colors[["latent"]])
abline(v=mu.break.after+1/2,lwd=2,col=signal.colors[["latent"]],lty="dashed")

## Segment that signal with cghseg, and quantify model fit using the
## breakpointError.
result <- run.cghseg(signal,base)
result$error <-
  sapply(result$breaks, breakpointError, mu.break.after, length(mu))
plot(signal~base,
     main="Estimated signal (green) and noisy observations (black)")
k <- 4
with(subset(result$seg,segments==k),{
  segments(first.base,mean,last.base,mean,
           col=signal.colors[["estimate"]],lwd=3)
  abline(v=first.base[-1],
         col=signal.colors[["estimate"]],lty="dashed",lwd=2)
})

## Plot the breakpointError versus estimated model size.
plot(result$error,type="n",
     main="breakpointError of estimated signals",
     xlab="k = number of segments in estimated signal",
     ylab="breakpointError(k)")
abline(v=k,lwd=10,col=signal.colors[["estimate"]])
lines(result$error)
points(result$error)
}
