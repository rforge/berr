\name{estimateBreaks}
\alias{estimateBreaks}
\title{estimateBreaks}
\description{Estimate the base positions after which a break occured in the
latent signal, given an imperfect estimate.}
\usage{
estimateBreaks(estimate,
base = seq_along(estimate))
}
\arguments{
  \item{estimate}{Numeric vector: the estimated signal.}
  \item{base}{Integer vector: the base positions where the estimated signal was
sampled.}
}

\value{Integer vector: the estimated positions of breaks in the latent
signal.}

\author{Toby Dylan Hocking}




\examples{
latent <- c()
latent.segs <- data.frame()
first <- 1
for(mu in c(-2,0,2)){
  size <- 30
  last <- first + size -1
  latent.segs <- rbind(latent.segs,data.frame(first,last,mean=mu))
  latent <- c(latent,rep(mu,size))
  first <- last+1
}
latent.breaks <- estimateBreaks(latent)
set.seed(1)
base <- sort(sample(seq_along(latent),length(latent)/5))
signal <- rnorm(length(base),latent[base])
atcg <- sample(c("A","T","C","G"),length(latent),replace=TRUE)

par(mfrow=c(2,1),las=1)
plot(signal~base,main="Noisy (black) and latent (blue) signals")
## To make a precise drawing of the latent signal, first consider
## that it is defined on discrete bases, which we plot here.
text(seq_along(latent),-1,atcg,cex=0.5)
## The segment from base i to base j should be drawn from i-1/2 to
## j+1/2.
with(latent.segs,{
  segments(first-1/2,mean,last+1/2,mean,col=signal.colors[["latent"]])
})
## and if there is a break after base i, it should be drawn at
## i+1/2.
abline(v=latent.breaks+1/2,col=signal.colors[["latent"]],lty="dashed")

plot(signal~base,main="Noisy (black) and estimated (green) signals")
text(seq_along(latent),-1,atcg,cex=0.5)
## Making a precise drawing of the estimated signal is easy since
## the run.cghseg function returns a data.frame of estimated
## segments, and the first.base and last.base columns can be
## directly plotted.
kmax <- 3
model <- run.cghseg(signal, base, kmax)
yhat <- subset(model$segments,segments==kmax)
with(yhat,segments(first.base,mean,last.base,mean,
                   col=signal.colors[["estimate"]]))
## Likewise, the breakpoints can be drawn using break.df$base.
break.df <- subset(model$break.df,segments==kmax)
abline(v=break.df$base,col=signal.colors[["estimate"]],lty="dashed")
}
