#include <fstream>
#include <sstream>
#include <string>
#include <R.h>
#include <Rinternals.h>

extern "C" SEXP read_text_file(SEXP path) {
  std::ifstream t(CHAR(asChar(path)));
//  std::stringstream ss;
  std::string s;
  std::getline(t, s);
//  ss << t.rdbuf();
//  std::string s = ss.str();
  SEXP out = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, mkChar(s.c_str()));
  UNPROTECT(1);
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
