#define USING_R 1
#include "S.h" 

#ifdef USING_R
# include <R.h>
# include <Rinternals.h>
# define S_EVALUATOR
#endif

#include "sp.h"

SEXP sp_zerodist(SEXP pp, SEXP pncol, SEXP zero, SEXP lonlat) {
	unsigned int i, j, k, ncol, nrow, nzero = 0, *which = NULL, ll;
	double **x, *xi, *xj, d, dist, zerodist2;
	SEXP ret = NULL;

	S_EVALUATOR
	ncol = INTEGER_POINTER(pncol)[0];
	ll = INTEGER_POINTER(lonlat)[0];
	if (ll && ncol != 2)
		error("for longlat data, coordinates should be two-dimensional");
	nrow = LENGTH(pp)/ncol;
	zerodist2 = NUMERIC_POINTER(zero)[0] * NUMERIC_POINTER(zero)[0];
	x = (double **) malloc((size_t) nrow * sizeof(double *));
	if (x == NULL)
		error("could not allocate vector of %u bytes in zerodist",
			nrow * sizeof(double *));
	for (i = 0; i < nrow; i++)
		x[i] = &(NUMERIC_POINTER(pp)[i*ncol]);

	for (i = 0; i < nrow; i++) {
		xi = x[i];
		for (j = 0; j < i; j++) {
			xj = x[j];
			if (ll) {
				sp_gcdist(xi, xj, xi+1, xj+1, &d);
				dist = d * d;
			} else {
				for (k = 0, dist = 0.0; k < ncol; k++) {
					d = (xi[k] - xj[k]);
					dist += d * d;
				}
			}
			if (dist <= zerodist2) {
				which = (unsigned int *) realloc(which, (size_t) (nzero+2) * sizeof(unsigned int));
				if (which == NULL)
					error("could not allocate vector of %u bytes in zerodist",
						nzero + 2);
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

SEXP sp_duplicates(SEXP pp, SEXP pncol, SEXP zero, SEXP lonlat) {
	unsigned int i, j, k, ncol, nrow, ll, next;
	double **x, *xi, *xj, d, dist, zerodist2;
	SEXP ret = NULL;

	S_EVALUATOR
	ncol = INTEGER_POINTER(pncol)[0];
	ll = INTEGER_POINTER(lonlat)[0];
	if (ll && ncol != 2)
		error("for longlat data, coordinates should be two-dimensional");
	nrow = LENGTH(pp)/ncol;
	zerodist2 = NUMERIC_POINTER(zero)[0] * NUMERIC_POINTER(zero)[0];
	x = (double **) malloc((size_t) nrow * sizeof(double *));
	if (x == NULL)
		error("could not allocate vector of %u bytes in zerodist",
			nrow * sizeof(double *));
	for (i = 0; i < nrow; i++)
		x[i] = &(NUMERIC_POINTER(pp)[i*ncol]);

	PROTECT(ret = NEW_INTEGER(nrow));
	if (nrow > 0)
		INTEGER_POINTER(ret)[0] = 0;
	for (i = 1; i < nrow; i++) {
		xi = x[i];
		INTEGER_POINTER(ret)[i] = i;
		next = 0;
		for (j = 0; next == 0 && j < i; j++) { /* find match */
			if (INTEGER_POINTER(ret)[j] == j) { /* this is a new point */
				xj = x[j];
				if (ll) {
					sp_gcdist(xi, xj, xi+1, xj+1, &d);
					dist = d * d;
				} else {
					for (k = 0, dist = 0.0; k < ncol; k++) {
						d = (xi[k] - xj[k]);
						dist += d * d;
					}
				}
				if (dist <= zerodist2) { /* match! */
					INTEGER_POINTER(ret)[i] = j;
					next = 1; /* break for loop */
				}
			}
		} /* for j */
		R_CheckUserInterrupt();
	}
	free(x);
	UNPROTECT(1);
	return(ret);
}
