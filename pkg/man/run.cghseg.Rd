\name{run.cghseg}
\alias{run.cghseg}
\title{run cghseg}
\description{Estimate the least squares model for a noisy signal. We use the
cghseg package, which implements the pruned dynamic programming
method of Rigaill (2010) to find, for all k=1,...,maxSegments:
argmin_{x has k segments} ||Y-x||^2_2.}
\usage{
run.cghseg(Y,
base = seq_along(Y),
maxSegments = 20)
}
\arguments{
  \item{Y}{Numeric vector of the noisy signal to segment.}
  \item{base}{Integer vector of bases where Y is sampled.}
  \item{maxSegments}{Maximum number of segments to consider.}
}

\value{List containing the solutions. The "segments" element is a
data.frame that describes the segmentation model, with 1 line for
each segment.}

\author{Toby Dylan Hocking}




\examples{
set.seed(1)
y <- c(rnorm(50),rnorm(100,2))
kmax <- 3
result <- run.cghseg(y,maxSegments=kmax)
signal.df <- data.frame(signal=y,base=seq_along(y))
library(ggplot2)
ggplot()+
  geom_point(aes(base,signal),data=signal.df)+
  geom_segment(aes(first.base,mean,xend=last.base,yend=mean),
               data=result$segments,colour=signal.colors[["estimate"]],lwd=3)+
  geom_vline(aes(xintercept=base),data=result$break.df,
             colour=signal.colors[["estimate"]],linetype="dashed")+
  facet_grid(segments~.,labeller=function(var,val)sprintf("\%s segments",val))
}
