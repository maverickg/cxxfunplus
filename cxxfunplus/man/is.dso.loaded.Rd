\name{is.dso.loaded-methods}
\docType{methods}
\alias{is.dso.loaded}
\alias{is.dso.loaded-methods}
\alias{is.dso.loaded,cxxdso-method}
\title{Tell if a \code{cxxdso} object is loaded} 
\description{
  The \code{is.dso.loaded} function tell if the dynamic shared object (DSO, or DLL) in 
  an object of \code{cxxdso}, created by function \code{\link{cxxfunctionplus}}, 
  is loaded. 
}
\section{Methods}{
\describe{
  \item{\code{signature(x = "cxxdso")}}{Tell if a \code{cxxdso} object is 
        loaded in the sense that the contained DSO is loaded or not.}}
}

\seealso{
\code{\linkS4class{cxxdso}}
}
\examples{
\dontrun{
dso <- cxxfunctionplus(signature(), "return R_NilValue ;")
print(is.dso.loaded(dso))
}} 

