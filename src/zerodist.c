#define USING_R 1
#include "S.h" 

#ifdef USING_R
# include <R.h>
# include <Rdefines.h>
/* # include <Rinternals.h> */
# define S_EVALUATOR
#endif

#include "sp.h"

SEXP sp_zerodist(SEXP pp, SEXP pncol, SEXP zero) {
	int i, j, k, ncol, nrow, nzero = 0, *which = NULL;
	double **x, *xi, *xj, d, dist, zerodist2;
	SEXP ret = NULL;

	S_EVALUATOR
	ncol = INTEGER_POINTER(pncol)[0];
	nrow = LENGTH(pp)/ncol;
	zerodist2 = NUMERIC_POINTER(zero)[0] * NUMERIC_POINTER(zero)[0];
	x = (double **) malloc(nrow * sizeof(double *));
	if (x == NULL)
		error("could not allocate memory in zerodist");
	for (i = 0; i < nrow; i++)
		x[i] = &(NUMERIC_POINTER(pp)[i*ncol]);

	for (i = 0; i < nrow; i++) {
		xi = x[i];
		for (j = 0; j < i; j++) {
			xj = x[j];
			for (k = 0, dist = 0.0; k < ncol; k++) {
				d = (xi[k] - xj[k]);
				dist += d * d;
			}
			if (dist <= zerodist2) {
				which = (int *) realloc(which, (nzero+2) * sizeof(int));
				if (which == NULL)
					error("could not allocate memory in zerodist");
				which[nzero] = j; /* lowest */
				which[nzero + 1] = i;
				nzero += 2;
			}
		}
		R_CheckUserInterrupt();
	}
	free(x);
	PROTECT(ret = NEW_INTEGER(nzero));
	for (i = 0; i < nzero; i++)
		INTEGER_POINTER(ret)[i] = which[i];
	if (which != NULL)
		free(which);
	UNPROTECT(1);
	return(ret);
}

/*
static SEXP sp_push_zerodist(SEXP ret, int i, int j) {
	
}
*/
