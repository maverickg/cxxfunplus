file_ext <- function(x) {
  # obtain the file extension 
  # copied from tools package 
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

obj_size_str <- function(x) {
  if (x >= 1024^3)       return(paste(round(x/1024^3, 1L), "Gb"))
  else if (x >= 1024^2)  return(paste(round(x/1024^2, 1L), "Mb"))
  else if (x >= 1024)    return(paste(round(x/1024, 1L), "Kb"))
  return(paste(x, "bytes")) 
} 

# not needed anymore since we have is.loaded in package:base,
# which is unknown to me before.
# 
#   is.null.ptr <- function(ns) {
#     .Call("is_Null_NS", ns) 
#   } 

## This is the implementation of 
## is_Null_NS in C, just in case we would need it in the future. 

##############################################################
#   #include <R.h>
#   #include <Rinternals.h>
#   
#   #ifdef __cplusplus
#   extern "C" {
#   #endif
#   
#   extern SEXP is_Null_NS(SEXP ns);
#   
#   #ifdef __cplusplus
#   }
#   #endif
#   
#   /*
#    * Tell if it is a NULL native symbol 
#    */
#   SEXP is_Null_NS(SEXP ns) {
#     SEXP ans;
#     PROTECT(ans = allocVector(LGLSXP, 1));
#     LOGICAL(ans)[0] = 1;
#     PROTECT(ns);
#     if (TYPEOF(ns) == EXTPTRSXP) { 
#       // Rprintf("ptr=%p.\n", EXTPTR_PTR(ns));
#       if (EXTPTR_PTR(ns) != NULL) LOGICAL(ans)[0] = 0;
#     } 
#     UNPROTECT(2);
#     return ans;
#   } 
#   
###############################################################

is_null_cxxfun <- function(cx) {
  # Tell if the returned object from cxxfunction in package inline
  # contains null pointer 
  env <- environment(cx@.Data)
  !is.loaded(env$f) 

  #  add <- body(cx@.Data)[[2]]
  #  # add is of class NativeSymbol
  #  .Call("is_Null_NS", add) 
} 
