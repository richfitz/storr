#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <Rversion.h>

// This is more then sufficient for all the hashes covered by the
// digest package.
#define MAX_HASH_LENGTH 128

SEXP read_text_file(SEXP r_path, SEXP r_nchar) {
  const char * path = CHAR(STRING_ELT(r_path, 0));
  const int nchar = asInteger(r_nchar);

  FILE *fp = fopen(path, "rb");
  if (fp == NULL) {
    Rf_error("File '%s' does not exist", path);
  }
  char buf[MAX_HASH_LENGTH + 1];
  char *res = fgets(buf, nchar + 1, fp); // +1 for \0
  if (res == NULL) {
    Rf_error("File '%s' is empty", path);
  }
  fclose(fp);
  SEXP out = PROTECT(mkString(buf));
  UNPROTECT(1);
  return out;
}

static const R_CallMethodDef call_methods[] = {
  {"Cread_text_file", (DL_FUNC) &read_text_file, 2},
  {NULL, NULL, 0}
};

void R_init_storr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 4, 0)
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
#endif
}
