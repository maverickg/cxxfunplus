\name{cxxdso-class}
\Rdversion{1.1}
\docType{class}
\alias{cxxdso-class}
\alias{show,cxxdso-method}
%\alias{grab_cxxfun,cxxdso-method}
%\alias{is_dso_loaded,cxxdso-method}

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
    \item{\code{dso_saved}:}{Object of class \code{"logical"} Whether to save the DSO or not.}
    \item{\code{dso_filename}:}{Object of class \code{"character"} The original file name for the DSO when it is created (no extension).}
    % \item{\code{dso_bin}:}{Object of class \code{"raw"} The raw vector containing the DSO if \code{dso_saved} is \code{TRUE}}
    \item{\code{system}:}{The operating system where the object is created.} 
    \item{\code{.MISC}:}{Object of class \code{"environment"} An environment to
                         save the functions returned by \code{cxxfunction} with
                         name \code{cxxfun}, the last full path for the DSO with
                         name \code{dso_last_path}, and the vector of \code{raw}
                         for saving the binary dynamic shared object (DSO) with 
                         name \code{dso_bin}.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(x = "cxxdso")}: Print a summary of the object.} 
    \item{grab_cxxfun}{\code{signature(object = "cxxdso")}: Return the function objects contained.}
    \item{is_dso_loaded}{\code{signature(object = "cxxdso")}: Tell if the DSO (DLL) is loaded.}
    \item{getDynLib}{\code{signature(x = "cxxdso")}: Obtain the DLL associated.} 
  }
}

\seealso{
\code{\link{getDynLib}}, \code{\link{grab_cxxfun}}, and 
\code{\link{cxxfunctionplus}} 
}
\examples{
showClass("cxxdso")
}
\keyword{classes}
