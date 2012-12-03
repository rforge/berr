errorDetails <- structure(function
### Calculate the components of the exact breakpoint error. We assume
### there is a latent piecewise constant signal defined on
### 1,...,last.base.
(guess,
### Integer vector of bases after which you estimate a break occured.
 breaks,
### Integer vector of bases after which there are breaks in the latent
### signal.
 last.base
### Integer scalar, the last base of the latent signal to model.
 ){
  .Call("errorDetails_interface",as.integer(breaks),
        as.integer(guess),as.integer(last.base),
        PACKAGE="breakpointError")
### A list with components used to calculate the breakpoint error.
},ex=function(){
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
})

errorComponents <- structure(function
### Create a data.frame of components of the breakpointError that is
### easy to plot.
(guess.list,
### List of integer vectors of guesses.
 breaks,
### Integer vector of bases after which there is a break in the latent
### signal.
 last.base
### Integer scalar, the last base in the latent signal.
 ){
  stopifnot(is.list(guess.list))
  stopifnot(is.integer(last.base))
  stopifnot(length(last.base)==1)
  stopifnot(is.integer(breaks))
  components <- data.frame()
  for(k in 1:length(guess.list)){
    guess <- guess.list[[k]]
    L <- errorDetails(guess, breaks, last.base)
    components <- rbind(components,with(L,{
      e <- c(FN=sum(false.negative),
             FP=sum(false.positive,guess.unidentified),
             I=sum(imprecision))
      e[["E"]] <- sum(e)
      data.frame(type=names(e),error=e,segments=k,row.names=NULL)
    }))
  }
  components$type <- factor(components$type,c("FP","FN","E","I"))
  components
### Components of the breakpointError, described in a data.frame with
### columns type, error, segments.
},ex=function(){
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
})

breakpointError <- structure(function
### Calculate the exact breakpoint error. We assume there is a latent
### piecewise constant signal defined on 1,...,last.base.
(guess,
### Integer vector of bases after which you estimate a break occured.
 breaks,
### Integer vector of bases after which there are breaks in the latent
### signal.
 last.base
### Integer scalar, the last base of the latent signal to model.
 ){
  e <- .C("breakpointError_interface",
     as.integer(breaks),as.integer(length(breaks)),
     as.integer(guess),as.integer(length(guess)),
     as.integer(last.base),error=double(1),
     PACKAGE="breakpointError")$error
  if(e<0){
    if(e == -1){
      stop("guess out of range")
    }
    if(e == -2){
      stop("duplicate guess")
    }
    if(e == -3){
      stop("break out of range")
    }
    if(e == -4){
      stop("duplicate break")
    }
    stop("unrecognized error code")
  }
  e
### Numeric scalar giving the breakpoint error of the guess.
},ex=function(){
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
})
