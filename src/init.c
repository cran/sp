#include <R.h>
#include <Rinternals.h>
#include "sp.h"

#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[]  = {
/*    {"pipbb", (DL_FUNC) &pipbb, 3},
    {"between", (DL_FUNC) &between, 3}, */
    {"setup_poly_minmax", (DL_FUNC) &setup_poly_minmax, 1},
    {"InPoly", (DL_FUNC) &InPoly, 2},
    {"spRFindCG", (DL_FUNC) &spRFindCG, 6},
    {NULL, NULL, 0}
};

static R_CallMethodDef CallEntries[] = {
/*    {"insiders", (DL_FUNC) &insiders, 2}, */
    {"R_point_in_polygon_sp", (DL_FUNC) &R_point_in_polygon_sp, 4},
    {NULL, NULL, 0}
};



void R_init_sp(DllInfo *dll)
{
    R_useDynamicSymbols(dll, FALSE);
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
}

