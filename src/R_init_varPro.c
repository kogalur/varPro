
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> 
#include <R_ext/Rdynload.h>
extern SEXP varProStrength(SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP, SEXP, SEXP, SEXP, SEXP);
static const R_CallMethodDef CallEntries[] = {
    {"varProStrength", (DL_FUNC) &varProStrength, 50},
    {NULL, NULL, 0}
};
void R_init_varPro(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
