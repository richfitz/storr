#include <R.h>
#include <Rinternals.h>

/* Assumes the `~` in the path is already expanded out with normalizePath()
 * or path.expand() in R. Otherwise, we will get a segfault.
 */
extern "C" SEXP read_text_file(SEXP path, SEXP nchar) {
  size_t size = asInteger(nchar) * sizeof(char);
  char *buf = (char*) malloc(size);
  FILE *fp;
  fp = fopen(CHAR(asChar(path)), "rb");
  fread(buf, size, 1, fp);
  fclose(fp);
  SEXP out = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, mkChar(buf));
  UNPROTECT(1);
  free(buf);
  return out;
}

static const R_CallMethodDef callMethods[] = {
  {"read_text_file", (DL_FUNC) &read_text_file, 2},
  {NULL, NULL, 0}
};

void R_init_storr(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE); 
}
