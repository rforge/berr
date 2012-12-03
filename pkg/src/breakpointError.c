#include <stdlib.h>
#include "breakpointError.h"

int compare_bases (const void *a, const void *b){
  return ( *(int*)a - *(int*)b );
}

/*
  Calculate all the components of the breakpoint error,
  and return a status code 0=successful calculation of the error,
  others=problem with the input.
 */
int
errorDetails
(int *break_ptr, int break_count,
 int *left, int *right, 
 double *false_positive, double *false_negative, double *imprecision,
 int *guess_ptr, int guess_count, 
 double *guess_unidentified,
 int last_base){
  int i,j, sum;
  qsort(break_ptr, break_count, sizeof(int), compare_bases);
  qsort(guess_ptr, guess_count, sizeof(int), compare_bases);
  for(j=0; j<guess_count; j++){
    if(guess_ptr[j] < 1 || guess_ptr[j] >= last_base){
      return GUESS_OUT_OF_RANGE;
    }
    if(j>0 && guess_ptr[j-1] == guess_ptr[j]){
      return DUPLICATE_GUESS;
    }
    guess_unidentified[j] = 1.0;
  }

  for(i=0; i<break_count; i++){
    // reality checks.
    if(break_ptr[i] < 1 || break_ptr[i] >= last_base){
      return BREAK_OUT_OF_RANGE;
    }
    if(i>0 && break_ptr[i-1] == break_ptr[i]){
      return DUPLICATE_BREAK;
    }
    // set default values i.e. J(i) = 0.
    false_negative[i]=1.0;
    false_positive[i]=0.0;
    imprecision[i]=0.0;
    // calculate the R_i values.
    if(i == break_count-1){
      right[i] = last_base - 1;
    }else{
      sum = break_ptr[i+1]+break_ptr[i];
      if(sum % 2 != 0){ //then it is odd
	sum -= 1; //subtract 1 so we take the floor.
      }
      right[i] = sum/2;
    }
    // calculate the L_i values.
    if(i == 0){
      left[i] = 1;
    }else{
      left[i] = right[i-1] + 1;
    }
  }

  i=0;
  j=0;
  int guesses_for_this_break = 0;
  double g, b, l, r;
  double score = -1.0, min_imprecision = 1.0;
  while(i < break_count && j < guess_count){
    g = (double)guess_ptr[j];
    b = (double)break_ptr[i];
    l = (double)left[i];
    r = (double)right[i];
    if(g < b){
      score = (b-g)/(b-l);
    }else if(g == b){
      score = 0.0;
    }else if(g <= r){
      score = (g-b)/(r-b);
    }
    if(score >= 0.0){//current guess is in this break region.
      if(score < min_imprecision){
	min_imprecision = score;
      }
      //Rprintf("score=%f min=%f i=%d j=%d\\n",score,min_imprecision,i,j);
      guess_unidentified[j] = 0.0;
      //move on to the next guess.
      guesses_for_this_break += 1;
      j += 1;
      score = -1.0;
    }else{ //no guesses in this break region.
      if(guesses_for_this_break > 0){
	false_positive[i] = guesses_for_this_break - 1;
	false_negative[i] = 0.0;
	imprecision[i] = min_imprecision;
      }
      //move on to the next break.
      i += 1;
      guesses_for_this_break = 0;
      min_imprecision = 1.0;
    }
  }

  // process last break region.
  if(break_count && guesses_for_this_break > 0){
    false_positive[i] = guesses_for_this_break - 1;
    false_negative[i] = 0.0;
    imprecision[i] = min_imprecision;
  }
  return 0;
}

/*
  Shortcut function if you just want the breakpoint error as a scalar
  and you don't want the breakdown via FP, FN, etc.

  returns a negative number if there were bad input values.
 */
double 
breakpointError
(int *break_ptr, int break_count,
 int *guess_ptr, int guess_count,
 int last_base){
  double *guess_unidentified = (double*)malloc(guess_count*sizeof(double)),
    *false_positive = (double*)malloc(break_count*sizeof(double)),
    *false_negative = (double*)malloc(break_count*sizeof(double)),
    *imprecision = (double*)malloc(break_count*sizeof(double));
  int
    *left = (int*)malloc(break_count*sizeof(int)),
    *right = (int*)malloc(break_count*sizeof(int));
  int status = errorDetails(break_ptr,break_count,
			    left,right, 
			    false_positive,false_negative,imprecision,
			    guess_ptr,guess_count,
			    guess_unidentified,
			    last_base);
  if(status != 0){
    return -1.0 * (double) status;
  }
  int i;
  double error = 0.0;
  for(i=0; i<break_count; i++){
    error += false_positive[i] + false_negative[i] + imprecision[i];
  }
  for(i=0; i<guess_count; i++){
    error += guess_unidentified[i];
  }
  free(guess_unidentified);
  free(false_negative);
  free(false_positive);
  free(imprecision);
  free(left);
  free(right);
  return error;
}
