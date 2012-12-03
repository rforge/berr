library(breakpointError)
bk <- c(1,3,6)
g <- c(4,7,10,15)
breakpointError(bk,g,20L)
breakpointError(bk,bk,20L)
breakpointError(g,g,20L)
res <- errorDetails(bk,g,20L)
str(res)
with(res,cbind(left,breaks,right))

calcError <- function(res){
  with(res,sum(false.positive,false.negative,imprecision,guess.unidentified))
}
bkpt.err <- function(guess,breaks,d){
  details <- errorDetails(as.integer(guess),as.integer(breaks),as.integer(d))
  Rerr <- calcError(details)
  Cerr <- breakpointError(guess,breaks,d)
  stopifnot(Rerr==Cerr)
  Cerr
}

## check that guessing exact right results in zero error.
should.be.perfect <- list(errorDetails(bk,bk,20L),
                          errorDetails(integer(),integer(),20L))
for(L in should.be.perfect){
  err <- calcError(L)
  stopifnot(err == 0)
}

## check that guessing farther away is worse.
stopifnot(bkpt.err(5L,6L,11L) < bkpt.err(5L,7L,11L))

## check that false positives are more costly.
stopifnot(bkpt.err(integer(),integer(),11L) <
          bkpt.err(integer(),5L,11L))

## check that false negatives are more costly.
stopifnot(bkpt.err(5L,5L,11L) <
          bkpt.err(5L,integer(),11L))

## check empty breaks and guess.
checkEmpty <- function(L,should.be.empty){
  for(N in should.be.empty){
    stopifnot(length(L[[N]])==0)
  }
}
checkEmpty(errorDetails(integer(),bk,20L),c("guess","guess.unidentified"))
checkEmpty(errorDetails(bk,integer(),20L),
           c("breaks","false.positive","false.negative","imprecision",
             "left","right"))

checkError <- function(expr,should.be){
  msg <- tryCatch(expr,error=function(L)L[["message"]])
  stopifnot(is.character(msg))
  stopifnot(length(msg)==1)
  stopifnot(msg == should.be)
}
## these should be errors.
funs <- list(errorDetails, bkpt.err, breakpointError)
for(fun in funs){
  checkError(fun(2L,0L,10L),"break out of range")
  checkError(fun(20L,10L,10L),"guess out of range")
  checkError(fun(20L,9L,10L),"guess out of range")
  checkError(fun(integer(),c(9L,9L),10L),"duplicate break")
  checkError(fun(c(9L,9L),integer(),10L),"duplicate guess")
}
