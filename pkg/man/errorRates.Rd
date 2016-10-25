\name{errorRates}
\alias{errorRates}
\title{errorRates}
\description{Compute false positive and false negative rates in order to draw
ROC curves.}
\usage{errorRates(guess.list, break.vec, last.base)}
\arguments{
  \item{guess.list}{List of integer vectors, model guesses.}
  \item{break.vec}{Integer vector, breakpoints in true signal.}
  \item{last.base}{Integer scalar, last base in true signal.}
}

\value{data.frame with columns FP, FN, FPR, FNR, TPR.}

\author{Toby Dylan Hocking}




\examples{
seg.size <- 100
means <- c(-3,0,3,0,2,-1,3)/3
mu <- do.call(c,lapply(means,rep,seg.size))
mu.break.after <- which(diff(mu)!=0)

## Choose some bases to sample, and pick them from a normal
## distribution.
base <- as.integer(seq(1,length(mu),l=300))
set.seed(1)
signal <- rnorm(length(base),mu[base],0.5)
profile <- data.frame(base, signal)

library(ggplot2)
ggplot()+
  geom_point(aes(base, signal), data=profile)

library(flsa)
flsa.fit <- flsa(signal, lambda2=10^seq(-2, 2, l=100))
between.vec <- as.integer((base[-1]+base[-length(base)])/2)
flsa.list <- apply(flsa.fit, 1, function(x)between.vec[which(diff(x)!=0)])
flsa.rates <- errorRates(flsa.list, mu.break.after, length(mu))

## Segment that signal with cghseg, and quantify model fit using the
## breakpointError.
result <- run.cghseg(signal, base, maxSegments=15)
cghseg.rates <- errorRates(result$breaks, mu.break.after, length(mu))

rates <-
  rbind(data.frame(cghseg.rates, package="cghseg"),
        data.frame(flsa.rates, package="flsa"))

getMin <- function(df, package, xdiff){
  m <- subset(df, seq_along(error)==which.min(error))
  data.frame(m, package, xdiff)
}
min.err <-
  rbind(getMin(cghseg.rates, "cghseg", -1),
        getMin(flsa.rates, "flsa", 1))

ggplot()+
  ggtitle("ROC curves and minimum breakpointError of two models")+
  scale_y_continuous("True positive rate", limits=c(0, 1))+
  scale_x_continuous("False positive rate")+
  guides(color="none")+
  geom_point(aes(FPR, TPR, color=package),
             data=min.err,
             pch=1)+
  geom_text(aes(FPR+xdiff*0.01, TPR, color=package,
                label=sprintf("FP=\%d\nFN=\%d\nI=\%.1f\n\%s",
                  FP, FN, I, package)),
            data=min.err)+
  geom_path(aes(FPR, TPR, group=package, color=package),
            data=rates)

}
