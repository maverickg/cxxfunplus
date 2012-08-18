\name{cxxfunctionplus}
\alias{cxxfunctionplus}
\title{To created an S4 class \code{cxxdso} from C++ code}
\description{
  This is a wrap-up of function \code{cxxfunction} in package \pkg{inline}. 
  Additionally, this function returns an object of class \code{cxxdso},
  which could be saved and reused across R sessions. All arguments except
  \code{save.dso} are passed to function \code{cxxfunction}. 
}
\usage{
cxxfunctionplus(sig = character(), body = character(), 
                plugin = "default", includes = "", 
                settings = getPlugin(plugin), 
                save.dso = FALSE, ..., verbose = FALSE)
}
\arguments{
  \item{sig}{Signature of the function. A named character vector}
  \item{body}{A character vector with C++ code to include in the body of the compiled C++ function}
  \item{plugin}{Name of the plugin to use. See \code{\link[inline:getPlugin]{getPlugin}} for details about plugins. }
  \item{includes}{User includes, inserted after the includes provided by the plugin. }
  \item{settings}{Result of the call to the plugin}
  \item{save.dso}{Determine whether to save the compiled code (DSO).} 
  \item{\dots}{Further arguments to the plugin}
  \item{verbose}{verbose output}
}
\value{An object of S4 class \code{cxxdso}.} 

\examples{\dontrun{
src <-  ' return ScalarReal(INTEGER(x)[0] * REAL(y)[0]);'
dso <- cxxfunctionplus(signature(x = "integer", y = "numeric"), src)
show(dso)
}}

\seealso{
  \code{\link[inline:cxxfunction]{cxxfunction}} and \code{\linkS4class{cxxdso}} 
}

