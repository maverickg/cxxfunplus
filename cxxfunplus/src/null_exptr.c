#include <R.h>
#include <Rinternals.h>

/*
 * Tell if it is a NULL native symbol 
 */ 
SEXP is_Null_NS(SEXP ns) {
  SEXP ans; 
  PROTECT(ans = allocVector(LGLSXP, 1));
  PROTECT(ns);
  if (TYPEOF(ns) == EXTPTRSXP) { 
    Rprintf("ptr=%p.\n", EXTPTR_PTR(ns)); 
    if (EXTPTR_PTR(ns) == NULL) LOGICAL(ans)[0] = 1; 
    else LOGICAL(ans)[0] = 0; 
  } 
  UNPROTECT(2); 
  return ans;
} 

