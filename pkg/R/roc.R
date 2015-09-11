errorRates <- structure(function(guess.list, break.vec, last.base){
  stopifnot(is.list(guess.list))
  stopifnot(is.integer(last.base))
  stopifnot(length(last.base)==1)
  stopifnot(is.integer(break.vec))
  error.by.model <- list()
  n.breaks <- length(break.vec)
  n.possible.breaks <- last.base - 1L
  n.not.breaks <- n.possible.breaks - n.breaks
  for(model.i in seq_along(guess.list)){
    guess.vec <- guess.list[[model.i]]
    details.list <- errorDetails(guess.vec, break.vec, last.base)
    FN <- sum(details.list$false.negative)
    FP <- with(details.list, sum(false.positive, guess.unidentified))
    I <- sum(details.list$imprecision)
    FNR <- (FN+I)/n.breaks
    error.by.model[[model.i]] <- 
      data.frame(model.i,
                 FN, FP, I, n.breaks, n.not.breaks,
                 error=FP + FN + I,
                 FPR=FP/n.not.breaks,
                 FNR,
                 TPR=1-FNR,
                 row.names=NULL)
  }
  do.call(rbind, error.by.model)
}, ex=function(){
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
  rates <- errorRates(result$breaks, mu.break.after, length(mu))

  library(ggplot2)
  ggplot()+
    scale_y_continuous(limits=c(0, 1))+
    geom_path(aes(FPR, TPR),
              data=rates)
})
  
