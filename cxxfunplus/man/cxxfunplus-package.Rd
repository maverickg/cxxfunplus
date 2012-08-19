\name{cxxfunplus-package}
\alias{cxxfunplus-package}
\alias{cxxfunplus}
\docType{package}
\title{
cxxfunplus: save the dynamic shared objects (DSO) for cxxfunction
}
\description{
  The cxxfunction function in \pkg{inline} could not save 
  the dynamic shared objects (DSO) created in a session. 
  We provide a mechanism to save the DSO's if for 
  example, \code{save.image} is called. 
}
\details{
\tabular{ll}{
Package: \tab cxxfunplus\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-08-16\cr
License: \tab GPL-3 \cr
}
Instead of calling cxxfunction in \pkg{inline}, call \code{cxxfunctionplus} 
in this package, from which an S4 class of \code{cxxdso} is returned.  
We could use generic function \code{grab.cxxfun} of class \code{cxxdso}
to retrieve the functions typically returned by \code{cxxfunction}. 
}
\author{
Jiqiang Guo <guojq28@gmail.com>

Maintainer: Jiqiang Guo <guojq28@gmail.com>
}
\keyword{package}
\seealso{
\code{\link{cxxfunctionplus}}, 
\code{\link[inline:inline-package]{inline}}
}
