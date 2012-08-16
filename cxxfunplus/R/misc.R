is.null.ptr <- function(ns) {
  .Call("is_Null_NS", ns) 
} 

is.null.cx <- function(cx) {
  # Tell if the returned object from cxxfunction in package inline
  # contains null pointer 
  add <- body(cx@.Data)[[2]]
  # add is of class NativeSymbol
  .Call("is_Null_NS", add) 
} 
