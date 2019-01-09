#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <Rversion.h>

#define MAX_HASH_LENGTH 128

SEXP Cread_text_file(SEXP path, SEXP nchar) {
  FILE *fp;
  fp = fopen(CHAR(asChar(path)), "rb");
  if (fp == NULL) {
    Rf_error("File %s does not exist.", path);
  }
  int n = asInteger(nchar) + 1; // Need an extra character for '\0'.
  char buf[MAX_HASH_LENGTH + 1];
  char *res = fgets(buf, n, fp);
  if (res == NULL) {
    Rf_error("File %s is empty.", path);
  }
  fclose(fp);
  SEXP out = PROTECT(mkString(buf));
  UNPROTECT(1);
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
