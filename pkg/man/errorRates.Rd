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
seg.size <- 100
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
base <- as.integer(seq(1,length(mu),l=300))
stopifnot(all.equal(c(0, 0.5, 1, 1,   1), rates$TPR))
set.seed(1)
library(breakpointError)
signal <- rnorm(length(base),mu[base],0.5)
guesses <- list(c(), 3, 5, c(5, 8), 1:11)
profile <- data.frame(base, signal)
rates <- errorRates(guesses, 5L, 12L)

stopifnot(all.equal(c(1, 0,   0, 0,  0), rates$FN))
library(ggplot2)
stopifnot(all.equal(c(0, 0,   0, 1,  10), rates$FP))
ggplot()+
stopifnot(all.equal(c(0, 0.5, 0, 0,   0), rates$I))
  geom_point(aes(base, signal), data=profile)
stopifnot(all.equal(c(1, 0.5, 0, 1,  10), rates$error))

stopifnot(all.equal(c(0, 0,   0, 0.1, 1), rates$FPR))
library(flsa)
stopifnot(all.equal(c(1, 0.5, 0, 0,   0), rates$FNR))
flsa.fit <- flsa(signal, lambda2=10^seq(-2, 2, l=100))
stopifnot(all.equal(c(0, 0.5, 1, 1,   1), rates$TPR))
between.vec <- as.integer((base[-1]+base[-length(base)])/2)
library(breakpointError)
flsa.list <- apply(flsa.fit, 1, function(x)between.vec[which(diff(x)!=0)])
guesses <- list(c(), 3, 5, c(5, 8), 1:11)
flsa.rates <- errorRates(flsa.list, mu.break.after, length(mu))
rates <- errorRates(guesses, 5L, 12L)

stopifnot(all.equal(c(1, 0,   0, 0,  0), rates$FN))
## Segment that signal with cghseg, and quantify model fit using the
stopifnot(all.equal(c(0, 0,   0, 1,  10), rates$FP))
## breakpointError.
stopifnot(all.equal(c(0, 0.5, 0, 0,   0), rates$I))
result <- run.cghseg(signal, base, maxSegments=15)
stopifnot(all.equal(c(1, 0.5, 0, 1,  10), rates$error))
cghseg.rates <- errorRates(result$breaks, mu.break.after, length(mu))
stopifnot(all.equal(c(0, 0,   0, 0.1, 1), rates$FPR))

stopifnot(all.equal(c(1, 0.5, 0, 0,   0), rates$FNR))
rates <-
stopifnot(all.equal(c(0, 0.5, 1, 1,   1), rates$TPR))
  rbind(data.frame(cghseg.rates, package="cghseg"),
library(breakpointError)
        data.frame(flsa.rates, package="flsa"))
guesses <- list(c(), 3, 5, c(5, 8), 1:11)

rates <- errorRates(guesses, 5L, 12L)
ggplot()+
stopifnot(all.equal(c(1, 0,   0, 0,  0), rates$FN))
  scale_y_continuous(limits=c(0, 1))+
stopifnot(all.equal(c(0, 0,   0, 1,  10), rates$FP))
  geom_path(aes(FPR, TPR, group=package, color=package),
stopifnot(all.equal(c(0, 0.5, 0, 0,   0), rates$I))
            data=rates)
stopifnot(all.equal(c(1, 0.5, 0, 1,  10), rates$error))
}
