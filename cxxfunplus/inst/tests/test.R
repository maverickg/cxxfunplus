
require(cxxfunplus)
dso <- cxxfunctionplus(signature(x = "integer", y = "numeric"), 
                       'return ScalarReal(INTEGER(x)[0] * REAL(y)[0]);', 
                       save_dso = TRUE)
show(dso) 
fx <- grab_cxxfun(dso)
fx(3L, 4)
