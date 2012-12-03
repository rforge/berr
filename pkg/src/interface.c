/* -*- compile-command: "R CMD INSTALL .." -*- */
#include <R.h>
#include <Rinternals.h>
#include "breakpointError.h"

SEXP 
errorDetails_interface
(SEXP breaks, SEXP guess, SEXP last_base){
  SEXP details, names, false_positive, false_negative, imprecision,
    left, right, guess_unidentified;
  PROTECT(names = allocVector(STRSXP, 8));
  SET_STRING_ELT(names,0,mkChar("breaks"));
  SET_STRING_ELT(names,1,mkChar("guess"));
  SET_STRING_ELT(names,2,mkChar("false.positive"));
  SET_STRING_ELT(names,3,mkChar("false.negative"));
  SET_STRING_ELT(names,4,mkChar("imprecision"));
  SET_STRING_ELT(names,5,mkChar("guess.unidentified"));
  SET_STRING_ELT(names,6,mkChar("left"));
  SET_STRING_ELT(names,7,mkChar("right"));

  PROTECT(details = allocVector(VECSXP, 8));
  namesgets(details, names);
  SET_VECTOR_ELT(details,0,breaks);
  SET_VECTOR_ELT(details,1,guess);
  // First allocVector, then SET_VECTOR_ELT.
  PROTECT(false_positive = allocVector(REALSXP, length(breaks)));
  PROTECT(false_negative = allocVector(REALSXP, length(breaks)));
  PROTECT(imprecision = allocVector(REALSXP, length(breaks)));
  PROTECT(guess_unidentified = allocVector(REALSXP, length(guess)));
  PROTECT(left = allocVector(INTSXP, length(breaks)));
  PROTECT(right = allocVector(INTSXP, length(breaks)));

  SET_VECTOR_ELT(details,2,false_positive);
  SET_VECTOR_ELT(details,3,false_negative);
  SET_VECTOR_ELT(details,4,imprecision);
  SET_VECTOR_ELT(details,5,guess_unidentified);
  SET_VECTOR_ELT(details,6,left);
  SET_VECTOR_ELT(details,7,right);
  
  int status = errorDetails(INTEGER(breaks),length(breaks),
			    INTEGER(left),INTEGER(right),
			    REAL(false_positive),REAL(false_negative),
			    REAL(imprecision),
			    INTEGER(guess),length(guess),
			    REAL(guess_unidentified),
			    INTEGER(last_base)[0]);

  if(status == GUESS_OUT_OF_RANGE){
    error("guess out of range");
  }
  if(status == DUPLICATE_GUESS){
    error("duplicate guess");
  }
  if(status == BREAK_OUT_OF_RANGE){
    error("break out of range");
  }
  if(status == DUPLICATE_BREAK){
    error("duplicate break");
  }

  UNPROTECT(8);
  return details;
}

void
breakpointError_interface
(int *break_ptr,int *break_count,
 int *guess_ptr,int *guess_count,
 int *last_base,double *error){
  *error = breakpointError(break_ptr,break_count[0],
			   guess_ptr,guess_count[0],
			   last_base[0]);
}
