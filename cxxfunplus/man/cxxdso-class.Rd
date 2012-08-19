\name{cxxdso-class}
\Rdversion{1.1}
\docType{class}
\alias{cxxdso-class}
%\alias{grab.cxxfun,cxxdso-method}
%\alias{is.dso.loaded,cxxdso-method}

\title{Class \code{"cxxdso"}}
\description{
  An S4 class for saving the dynamic shared objects created on the fly
}
\section{Objects from the Class}{
  Objects can be created by calls of \code{cxxfunctionplus}. 
}
\section{Slots}{
  \describe{
    \item{\code{sig}:}{Object of class \code{"list"} The signatures of functions defined.}
    \item{\code{dso.saved}:}{Object of class \code{"logical"} Whether to save the DSO or not.}
    \item{\code{dso.filename}:}{Object of class \code{"character"} The original file name for the DSO when it is created (no extension).}
    \item{\code{dso.bin}:}{Object of class \code{"raw"} The raw vector containing the DSO if \code{dso.saved} is \code{TRUE}}
    \item{\code{system}:}{The operating system where the object is created.} 
    \item{\code{.MISC}:}{Object of class \code{"environment"} An environment to
                         save the functions returned by \code{cxxfunction} with
                         name \code{cxxfun} and the last path for the DSO with
                         name \code{dso.last.path}.}
  }
}
\section{Methods}{
  \describe{
    \item{grab.cxxfun}{\code{signature(object = "cxxdso")}: Return the function objects contained.}
    \item{is.dso.loaded}{\code{signature(object = "cxxdso")}: Tell if the DSO (DLL) is loaded.}
    \item{getDynLib}{\code{signature(x = "cxxdso")}: Obtain the DLL associated.} 
  }
}

\seealso{
\code{\link{getDynLib}}, \code{\link{grab.cxxfun}}, and 
\code{\link{cxxfunctionplus}} 
}
\examples{
showClass("cxxdso")
}
\keyword{classes}
