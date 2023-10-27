// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// get_depths
NumericVector get_depths(double px, double py, NumericVector& x, NumericVector& y, int num);
RcppExport SEXP _viewscape_get_depths(SEXP pxSEXP, SEXP pySEXP, SEXP xSEXP, SEXP ySEXP, SEXP numSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type px(pxSEXP);
    Rcpp::traits::input_parameter< double >::type py(pySEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type num(numSEXP);
    rcpp_result_gen = Rcpp::wrap(get_depths(px, py, x, y, num));
    return rcpp_result_gen;
END_RCPP
}
// multiLabel
Rcpp::List multiLabel(Rcpp::NumericMatrix& vpts, Rcpp::NumericMatrix& dsm, const int max_dis, const double vpth, const double h);
RcppExport SEXP _viewscape_multiLabel(SEXP vptsSEXP, SEXP dsmSEXP, SEXP max_disSEXP, SEXP vpthSEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type vpts(vptsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type dsm(dsmSEXP);
    Rcpp::traits::input_parameter< const int >::type max_dis(max_disSEXP);
    Rcpp::traits::input_parameter< const double >::type vpth(vpthSEXP);
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(multiLabel(vpts, dsm, max_dis, vpth, h));
    return rcpp_result_gen;
END_RCPP
}
// visibleLabel
Rcpp::IntegerMatrix visibleLabel(const Rcpp::NumericVector& viewpoint, const Rcpp::NumericMatrix& dsm, const double h, const int max_dis);
RcppExport SEXP _viewscape_visibleLabel(SEXP viewpointSEXP, SEXP dsmSEXP, SEXP hSEXP, SEXP max_disSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type viewpoint(viewpointSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type dsm(dsmSEXP);
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    Rcpp::traits::input_parameter< const int >::type max_dis(max_disSEXP);
    rcpp_result_gen = Rcpp::wrap(visibleLabel(viewpoint, dsm, h, max_dis));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_viewscape_get_depths", (DL_FUNC) &_viewscape_get_depths, 5},
    {"_viewscape_multiLabel", (DL_FUNC) &_viewscape_multiLabel, 5},
    {"_viewscape_visibleLabel", (DL_FUNC) &_viewscape_visibleLabel, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_viewscape(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
