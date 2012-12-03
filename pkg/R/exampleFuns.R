### Colors to use when drawing signals.
signal.colors <- c(estimate="#0adb0a",
                   latent="#0098ef")

### Colors to use when drawing curves for FP, FN, ...
fp.fn.colors <- c(FP="skyblue",
                  FN="#E41A1C",
                  E="black",
                  I="black")

### Linetypes to use when drawing curves for FP, FN, ...
fp.fn.linetypes <- c(E="solid",
                     FP="solid",
                     FN="solid",
                     I="dashed")

### Sizes to use when drawing curves for FP, FN, ...
fp.fn.sizes <- c(E=1,
                 FP=3,
                 FN=3,
                 I=1)/1.2


run.cghseg <- structure(function
### Estimate the least squares model for a noisy signal. We use the
### cghseg package, which implements the pruned dynamic programming
### method of Rigaill (2010) to find, for all k=1,...,maxSegments:
### argmin_{x has k segments} ||Y-x||^2_2.
(Y,
### Numeric vector of the noisy signal to segment.
 base=seq_along(Y),
### Integer vector of bases where Y is sampled.
 maxSegments=20
### Maximum number of segments to consider.
 ){
  stopifnot(length(Y)==length(base))
  if(any(diff(base) < 0)){
    stop("need to sort signal so bases are increasing")
  }
  require(cghseg)
  n <- length(Y)
  kmax <- min(maxSegments,n)#k is the number of SEGMENTS not BREAKPOINTS
  result <- cghseg:::segmeanCO(Y,kmax)
  result$segments <- data.frame()
  result$breaks <- list()
  result$break.df <- data.frame()
  for(k in 1:kmax){
    ends <- result$t.est[k, 1:k]
    starts <- c(1,ends[-length(ends)]+1)
    signal <- rep(NA,k)
    for(seg.i in seq_along(starts)){
      start <- starts[seg.i]
      end <- ends[seg.i]
      signal[seg.i] <- mean(Y[start:end])
    }
    first.base <- base[starts]
    last.base <- base[ends]
    breaks <- floor(c(first.base[-1]+last.base[-k])/2)+1/2
    modelSegs <- data.frame(first.index=starts,last.index=ends,
                            first.base=c(first.base[1]-1/2,breaks),
                            last.base=c(breaks,last.base[k]+1/2),
                            mean=signal,segments=k)

    result$breaks[[k]] <- as.integer(breaks-1/2)

    if(k>1){
      result$break.df <- rbind(result$break.df,{
        data.frame(base=breaks,segments=k)
      })
    }

    result$segments <- rbind(result$segments,modelSegs)
  }
  result
### List containing the solutions. The "segments" element is a
### data.frame that describes the segmentation model, with 1 line for
### each segment.
},ex=function(){
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
    facet_grid(segments~.,labeller=function(var,val)sprintf("%s segments",val))
})

estimateBreaks <- structure(function
### Estimate the base positions after which a break occured in the
### latent signal, given an imperfect estimate.
(estimate,
### Numeric vector: the estimated signal.
 base=seq_along(estimate)
### Integer vector: the base positions where the estimated signal was
### sampled.
 ){
  break.after <- which(diff(estimate)!=0)
  as.integer(floor((base[break.after]+base[break.after+1L])/2))
### Integer vector: the estimated positions of breaks in the latent
### signal.
},ex=function(){
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
})


