#include <R.h>
#include <Rinternals.h>
#include <R_ext/Print.h>
#include <Rdefines.h>
#include <stdlib.h>
#include <stdio.h>

SEXP optim_array_loop(SEXP d_path) {
     FILE *ifp;
     const char* c_path;
     SEXP out;
     c_path = CHARACTER_VALUE(d_path);
     ifp = fopen(c_path,"r");
     if(ifp == NULL){
          Rf_error("Could not open file %s.",c_path);
     }
     // Loop through the file twice
     // First loop.  Determine size of output array and check for ``errors'' in the data
     
     // First loop over
     PROTECT(out = NEW_INTEGER(d1*d2*d3*d4));
     // Second loop.  Fill in values in the output
     // Second loop over
     
     return(out);
}

void c2Rdataframe(int* cframe, int d1, int d2, int d3, int d4, SEXP *Rframe){
     
     // Declare variables
     SEXP basePackage;
     SEXP Rvec;
     SEXP Rd;
     
     PROTECT(
          basePackage = eval( lang2( 
               install("getNamespace"),
               ScalarString(mkChar("base"))),
               R_GlobalEnv
          )
     );
     
     PROTECT(Rd = NEW_INTEGER(4));
     INTEGER(Rd)[0] = d1;
     INTEGER(Rd)[1] = d2;
     INTEGER(Rd)[2] = d3;
     INTEGER(Rd)[3] = d4;
     PROTECT(Rvec = NEW_NUMERIC(d1*d2*d3*d4));
     for(int i = 0 ; i < d1*d2*d3*d4; ++i){
          INTEGER(Rvec)[i] = cframe[i];
     }
     
     UNPROTECT(4);
     return;
}