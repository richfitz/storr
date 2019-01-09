#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <Rversion.h>

SEXP Cread_text_file(SEXP path, SEXP nchar) {
  FILE *fp;
  fp = fopen(CHAR(asChar(path)), "rb");
  if (fp == NULL) {
    Rf_error("File %s does not exist", path);
  }
  int n = asInteger(nchar) + 1; // Need an extra character for '\0'.
  char *buf = (char*) malloc(n * sizeof(char));
  fgets(buf, n, fp);
  fclose(fp);
  SEXP out = PROTECT(mkString(buf));
  UNPROTECT(1);
  free(buf);
  return out;
}

static const R_CallMethodDef call_methods[] = {
  {"Cread_text_file", (DL_FUNC) &Cread_text_file, 2},
  {NULL, NULL, 0}
};

void R_init_storr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  #if defined(R_VERSION) && R_VERSION >= R_Version(3, 4, 0)
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
  #endif
}
