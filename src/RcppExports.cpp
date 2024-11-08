// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// VM
Rcpp::NumericMatrix VM(const Rcpp::IntegerMatrix& viewshed, const Rcpp::IntegerMatrix& dsm, const Rcpp::NumericMatrix& slp, const Rcpp::NumericMatrix& asp, const Rcpp::NumericVector viewpt, const double h, const int resolution);
RcppExport SEXP _viewscape_VM(SEXP viewshedSEXP, SEXP dsmSEXP, SEXP slpSEXP, SEXP aspSEXP, SEXP viewptSEXP, SEXP hSEXP, SEXP resolutionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::IntegerMatrix& >::type viewshed(viewshedSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerMatrix& >::type dsm(dsmSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type slp(slpSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type asp(aspSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type viewpt(viewptSEXP);
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    Rcpp::traits::input_parameter< const int >::type resolution(resolutionSEXP);
    rcpp_result_gen = Rcpp::wrap(VM(viewshed, dsm, slp, asp, viewpt, h, resolution));
    return rcpp_result_gen;
END_RCPP
}
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
// sectorMask
Rcpp::IntegerMatrix sectorMask(const Rcpp::IntegerMatrix& viewshed, const Rcpp::NumericVector viewpt, const Rcpp::NumericVector fov);
RcppExport SEXP _viewscape_sectorMask(SEXP viewshedSEXP, SEXP viewptSEXP, SEXP fovSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::IntegerMatrix& >::type viewshed(viewshedSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type viewpt(viewptSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type fov(fovSEXP);
    rcpp_result_gen = Rcpp::wrap(sectorMask(viewshed, viewpt, fov));
    return rcpp_result_gen;
END_RCPP
}
// reference
Rcpp::IntegerMatrix reference(const Rcpp::NumericVector& viewpoint, const Rcpp::NumericMatrix& dsm, const double h, const int max_dis, const double refraction_factor);
RcppExport SEXP _viewscape_reference(SEXP viewpointSEXP, SEXP dsmSEXP, SEXP hSEXP, SEXP max_disSEXP, SEXP refraction_factorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type viewpoint(viewpointSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type dsm(dsmSEXP);
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    Rcpp::traits::input_parameter< const int >::type max_dis(max_disSEXP);
    Rcpp::traits::input_parameter< const double >::type refraction_factor(refraction_factorSEXP);
    rcpp_result_gen = Rcpp::wrap(reference(viewpoint, dsm, h, max_dis, refraction_factor));
    return rcpp_result_gen;
END_RCPP
}
// LOS
Rcpp::IntegerMatrix LOS(const Rcpp::NumericVector& viewpoint, const Rcpp::NumericMatrix& dsm, const double h, const int max_dis, const double refraction_factor);
RcppExport SEXP _viewscape_LOS(SEXP viewpointSEXP, SEXP dsmSEXP, SEXP hSEXP, SEXP max_disSEXP, SEXP refraction_factorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type viewpoint(viewpointSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type dsm(dsmSEXP);
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    Rcpp::traits::input_parameter< const int >::type max_dis(max_disSEXP);
    Rcpp::traits::input_parameter< const double >::type refraction_factor(refraction_factorSEXP);
    rcpp_result_gen = Rcpp::wrap(LOS(viewpoint, dsm, h, max_dis, refraction_factor));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_viewscape_VM", (DL_FUNC) &_viewscape_VM, 7},
    {"_viewscape_get_depths", (DL_FUNC) &_viewscape_get_depths, 5},
    {"_viewscape_sectorMask", (DL_FUNC) &_viewscape_sectorMask, 3},
    {"_viewscape_reference", (DL_FUNC) &_viewscape_reference, 5},
    {"_viewscape_LOS", (DL_FUNC) &_viewscape_LOS, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_viewscape(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
