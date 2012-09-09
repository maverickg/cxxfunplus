
setClass(Class = "cxxdso",
         representation = representation(
           sig = "list", # A list of function signature that would be returned by cxxfuncion 
           dso_saved = "logical", # flag for if the dso is saved or not
           # dso_last_path = 'character', # where the dso is saved last time 
           dso_filename = "character", # the dso file name when it is created the first time
           system = "character", # what is the OS (R.version$system)?  
           .MISC = "environment" # an envir to save 
                                 #  1. the returned object by cxxfuncion using name cxxfun 
                                 #  2. the file path used last time using name dso_last_path 
                                 #  3. The binary dso with name dso_bin, which is a raw vector.  
                                 #     We put it here since the environment is not copied 
                                 #     when assigned to another 
                                 #     http://cran.r-project.org/doc/manuals/R-lang.html#Environment-objects
         ),
         validity = function(object) {
           length(object@sig) > 0 && identical(object@system, R.version$system)
         })

setGeneric(name = "grab_cxxfun",
           def = function(object, ...) { standardGeneric("grab_cxxfun")})

setGeneric(name = "is_dso_loaded",
           def = function(object, ...) { standardGeneric("is_dso_loaded")})

setMethod("show", "cxxdso", 
          function(object) {
            cat("S4 class cxxdso: dso_saved = ", object@dso_saved, 
                ", dso_filename = ", object@dso_filename, 
                ", size = ", obj_size_str(object.size(object@.MISC$dso_bin)), ".\n", sep = '')  
            cat("And dso_last_path = '", object@.MISC$dso_last_path, "'.\n", sep = '')
            cat("Created on: ", object@system, ".\n", sep = '')
            loaded <- if (is_dso_loaded(object)) 'YES' else 'NO' 
            cat("Loaded now: ", loaded, ".\n", sep = '')
            cat("The signatures is/are as follows: \n")
            print(object@sig); 
          })

setMethod('is_dso_loaded', signature(object = 'cxxdso'), 
          function(object) {
            f2 <- sub("\\.[^.]*$", "", basename(object@.MISC$dso_last_path)) 
            dlls <- getLoadedDLLs()
            f2 %in% names(dlls)
          }) 

setMethod('grab_cxxfun', signature(object = "cxxdso"), 
          function(object) { 
            if (!is_null_cxxfun(object@.MISC$cxxfun)) 
              return(object@.MISC$cxxfun)
            if (!object@dso_saved) 
              stop("the cxx fun is NULL now and this cxxdso is not saved")

            # If the file is still loaded  
            # from the help of function dyn.load 
            #   The function ‘dyn.unload’ unlinks the DLL.  Note that unloading a
            #   DLL and then re-loading a DLL of the same name may or may not
            #   work: on Solaris it uses the first version loaded.
            f <- sub("\\.[^.]*$", "", basename(object@dso_filename)) 
            f2 <- sub("\\.[^.]*$", "", basename(object@.MISC$dso_last_path)) 
            dlls <- getLoadedDLLs()
            if (f2 %in% names(dlls)) { # still loaded 
              DLL <- dlls[[f2]] 
              assign('cxxfun', cx, object@.MISC) 
              return(cxxfun_from_dll(object@sig, object@.MISC$cxxfun@code, DLL, check.dll = FALSE)) 
            }
            
            # not loaded  
            if (!identical(object@system, R.version$system)) 
              stop(paste("this cxxdso object was created on system '", object@system, "'", sep = ''))
            cx <- cxxfun_from_dso_bin(object) 
            assign('cxxfun', cx, object@.MISC) 
            return(cx) 
          }) 

setMethod("getDynLib", signature(x = "cxxdso"),
          function(x) { 
            fx <- grab_cxxfun(x) 
            env <- environment(fx@.Data)
            f <- get("f", env)
            dlls <- getLoadedDLLs()
            if (!f  %in% names(dlls)) 
              stop(paste('dso ', f, ' is not loaded', sep = ''))
            dlls[[f]]
          })
