#include <fstream>
#include <sstream>
#include <string>
#include <R.h>
#include <Rinternals.h>

SEXP read_text_file(SEXP path) {
  std::ifstream t(CHAR(asChar(path)));
  std::stringstream ss;
  ss << t.rdbuf();
  std::string s = ss.str();
  return mkChar(s.c_str());
}

static const R_CallMethodDef callMethods[] = {
  {"read_text_file", (DL_FUNC) &read_text_file, 1},
  {NULL, NULL, 0}
};

void R_init_storr(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE); 
}
