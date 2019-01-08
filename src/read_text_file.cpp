#include <R.h>
#include <Rinternals.h>

extern "C" SEXP read_text_file(SEXP path, SEXP nchar) {
  size_t size = asInteger(nchar) * sizeof(char);
  char *buf = (char*) calloc(1, size);
  FILE *fp;
  fp = fopen(CHAR(asChar(path)), "rb");
  fread(buf, size, 1, fp);
  buf[size] = '\0'; // Very important: https://wiki.sei.cmu.edu/confluence/pages/viewpage.action?pageId=87152233
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
