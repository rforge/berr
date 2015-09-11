\name{errorComponents}
\alias{errorComponents}
\title{errorComponents}
\description{Create a data.frame of components of the breakpointError that is
easy to plot.}
\usage{errorComponents(guess.list, breaks, last.base)}
\arguments{
  \item{guess.list}{List of integer vectors of guesses.}
  \item{breaks}{Integer vector of bases after which there is a break in the latent
signal.}
  \item{last.base}{Integer scalar, the last base in the latent signal.}
}

\value{Components of the breakpointError, described in a data.frame with
columns type, error, segments.}

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
result <- run.cghseg(signal, base, maxSegments=15)
components <- errorComponents(result$breaks, mu.break.after, length(mu))
library(ggplot2)
p <- ggplot(components,aes(segments,error))+
  geom_line(aes(size=type,colour=type,linetype=type))+
  scale_linetype_manual(values=fp.fn.linetypes)+
  scale_colour_manual(values=fp.fn.colors)+
  scale_size_manual(values=fp.fn.sizes)
library(directlabels)
direct.label(p+guides(linetype="none",colour="none",size="none"),
             dl.combine("first.qp","last.qp"))
}
