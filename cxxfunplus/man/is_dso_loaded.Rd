\name{is_dso_loaded-methods}
\docType{methods}
\alias{is_dso_loaded}
\alias{is_dso_loaded-methods}
\alias{is_dso_loaded,cxxdso-method}
\title{Tell if a \code{cxxdso} object is loaded} 
\description{
  The \code{is_dso_loaded} function tell if the dynamic shared object (DSO, or DLL) in 
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
print(is_dso_loaded(dso))
}} 

