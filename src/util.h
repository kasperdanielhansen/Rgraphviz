#ifndef RGRAPHVIZ_UTIL_H
#define RGRAPHVIZ_UTIL_H 1

#define STR(SE) CHAR(STRING_ELT(SE,0))

#define CHECK_Rgraphviz_graph(s) do { \
     if (TYPEOF(s) != EXTPTRSXP || \
         R_ExternalPtrTag(s) != Rgraphviz_graph_type_tag) \
         error("bad graph reference"); \
} while (0)

// these are for other C-functions
SEXP assignAttrs(SEXP, SEXP, SEXP);
SEXP buildRagraph(Agraph_t *);
SEXP getListElement(SEXP list, char *str);
SEXP R_scalarInteger(int);
SEXP R_scalarLogical(Rboolean);
SEXP R_scalarReal(double);  
SEXP R_scalarString(const char *v);

#endif // RGRAPHVIZ_UTIL_H

