\name{errorRates}
\alias{errorRates}
\title{errorRates}

\usage{errorRates(guess.list, break.vec, last.base)}
\arguments{
  \item{guess.list}{
}
  \item{break.vec}{
}
  \item{last.base}{
}
}



\author{Toby Dylan Hocking}




\examples{library(breakpointError)

guesses <- list(c(), 3, 5, c(5, 8), 1:11)
seg.size <- 10000
rates <- errorRates(guesses, 5L, 12L)
means <- c(-3,0,3,0,2,-1,3)/3
stopifnot(all.equal(c(1, 0,   0, 0,  0), rates$FN))
mu <- do.call(c,lapply(means,rep,seg.size))
stopifnot(all.equal(c(0, 0,   0, 1,  10), rates$FP))
mu.break.after <- which(diff(mu)!=0)
stopifnot(all.equal(c(0, 0.5, 0, 0,   0), rates$I))

stopifnot(all.equal(c(1, 0.5, 0, 1,  10), rates$error))
## Choose some bases to sample, and pick them from a normal
stopifnot(all.equal(c(0, 0,   0, 0.1, 1), rates$FPR))
## distribution.
stopifnot(all.equal(c(1, 0.5, 0, 0,   0), rates$FNR))
base <- as.integer(seq(1,length(mu),l=500))
stopifnot(all.equal(c(0, 0.5, 1, 1,   1), rates$TPR))
set.seed(1)
library(breakpointError)
signal <- rnorm(length(base),mu[base],1)
guesses <- list(c(), 3, 5, c(5, 8), 1:11)

rates <- errorRates(guesses, 5L, 12L)
## Segment that signal with cghseg, and quantify model fit using the
stopifnot(all.equal(c(1, 0,   0, 0,  0), rates$FN))
## breakpointError.
stopifnot(all.equal(c(0, 0,   0, 1,  10), rates$FP))
result <- run.cghseg(signal, base, maxSegments=15)
stopifnot(all.equal(c(0, 0.5, 0, 0,   0), rates$I))
rates <- errorRates(result$breaks, mu.break.after, length(mu))
stopifnot(all.equal(c(1, 0.5, 0, 1,  10), rates$error))

stopifnot(all.equal(c(0, 0,   0, 0.1, 1), rates$FPR))
library(ggplot2)
stopifnot(all.equal(c(1, 0.5, 0, 0,   0), rates$FNR))
ggplot()+
stopifnot(all.equal(c(0, 0.5, 1, 1,   1), rates$TPR))
  scale_y_continuous(limits=c(0, 1))+
library(breakpointError)
  geom_path(aes(FPR, TPR),
guesses <- list(c(), 3, 5, c(5, 8), 1:11)
            data=rates)
rates <- errorRates(guesses, 5L, 12L)
}
