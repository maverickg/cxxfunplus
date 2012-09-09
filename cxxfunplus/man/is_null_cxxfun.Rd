\name{is_null_cxxfun}
\alias{is_null_cxxfun}
\title{Tell if the address of functions created by \code{cxxfunction} 
       points to NULL}

\description{
  The function object returned by \code{cxxfunction} cannot be 
  saved across R sessions. This function can be used to see
  if we still have a valid function object. Also this function 
  can be used for functions returned by \code{grab_cxxfun}
  of S4 class \code{cxxdso} since these functions are essentially
  created by \code{cxxfunction} or similarly. 
}

\usage{
is_null_cxxfun(cx)
}
\arguments{
  \item{cx}{A function of class \code{CFunc}
}
}
\details{
  R could not save the function objects that point to dynamically loaded
  functions, especially for those function created on the fly using package
  \pkg{inline} at least for one reason that those DSO's are deleted after
  quitting R. So it is always safe to tell if it is valid before call functions
  created by \code{cxxfunction}. 
}
\value{
  Logical: \code{TRUE} null pointer; \code{FALSE}, not 
  null, this function can still be called. 
}
\seealso{
\code{\link[inline:cxxfunction]{cxxfunction}} 
}
