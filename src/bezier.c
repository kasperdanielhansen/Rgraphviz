#include "common.h"
#include "util.h"

SEXP Rgraphviz_bezier(SEXP Rpnts, SEXP Rn, SEXP Rt) {
    SEXP curPnts, out;
    int n, k;
    double x, y, t, tmp;
    
    x = y = 0;
    n = INTEGER(Rn)[0]-1;
    t = REAL(Rt)[0];

    for (k = 0; k <= n; k++) {
	curPnts = VECTOR_ELT(Rpnts, k);
	tmp = Rf_choose(n,k) * pow(t, k) * (pow((1-t), (n-k)));
	x += INTEGER(curPnts)[0] * tmp;
	y += INTEGER(curPnts)[1] * tmp; 
    }

    PROTECT(out = allocVector(REALSXP, 2));
    REAL(out)[0] = x;
    REAL(out)[1] = y;
    UNPROTECT(1);
    return(out);
}

