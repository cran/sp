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
    {"sp_gcdist", (DL_FUNC) &sp_gcdist, 5},
    {"sp_dists", (DL_FUNC) &sp_dists, 7},
    {"sp_lengths", (DL_FUNC) &sp_lengths, 5},
    {NULL, NULL, 0}
};

static R_CallMethodDef CallEntries[] = {
/*    {"insiders", (DL_FUNC) &insiders, 2}, */
    {"R_point_in_polygon_sp", (DL_FUNC) &R_point_in_polygon_sp, 4},
	{"sp_zerodist", (DL_FUNC) &sp_zerodist, 3},
    {"pointsInBox", (DL_FUNC) &pointsInBox, 3},
    {"tList", (DL_FUNC) &tList, 2},
    {NULL, NULL, 0}
};


void R_init_sp(DllInfo *dll)
{
    R_useDynamicSymbols(dll, FALSE);
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
}

