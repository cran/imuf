// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/math/constants/constants.hpp>
#include <boost/qvm/all.hpp>
using namespace Rcpp;
using namespace boost::math::constants;
using namespace boost::qvm;

vec<double, 3> numericVecToVec(NumericVector v) {
  return vec<double, 3> { v[0], v[1], v[2] };
}

quat<double> vecToQuat(vec<double, 3> v) {
  return quat<double> { 0, X(v), Y(v), Z(v) };
}

NumericVector quatToNumericVec(quat<double> q) {
  return NumericVector { S(q), X(q), Y(q), Z(q) };
}

quat<double> numericVecToQuat(NumericVector v) {
  return quat<double> { v[0], v[1], v[2], v[3] };
}

//'
//' 'compUpdate' update orientation with 3-axis acc and gyr data
//'
//' @param acc A numeric 3-vector of 3-axis accelerometer readings in g
//' @param gyr A numeric 3-vector of 3-axis gyroscope readings in rad/sec
//' @param dt A numeric of time duration in sec
//' @param initQuat A numeric 4-vector of the starting orientation in quaternion
//' @param gain A numeric gain factor between 0 and 1
//' @returns A numeric 4-vector of the ending orientation in quaternion
//' @export
//'
//' @examples
//' compUpdate(c(0, 0, -1), c(1, 0, 0), 0.1, c(1, 0, 0, 0), 0.1)
//'
// [[Rcpp::export]]
NumericVector compUpdate(NumericVector acc, NumericVector gyr, double dt, NumericVector initQuat, double gain) {
  //
  // coordinate system is NED
  const vec<double, 3> accVec = numericVecToVec(acc);
  const vec<double, 3> gyrVec = numericVecToVec(gyr);
  const quat<double> qt = numericVecToQuat(initQuat);
  //
  // make gryVec into a quat for Euler Equation
  const quat<double> gyrQuat = vecToQuat(gyrVec);
  const double gyrQuatNorm = mag(gyrQuat);
  const double delta = gyrQuatNorm * dt / 2.0;
  quat<double> qt1;
  if (delta == 0) {
    qt1 = qt;
  } else {
    qt1 = qt * cos(delta) + qt * 1 / gyrQuatNorm * sin(delta) * gyrQuat;
  }
  normalize(qt1);
  //
  // rotate acc by qt1
  const vec<double, 3> accWorld = qt1 * accVec;
  //
  // complement with accelerometer info
  const vec<double, 3> g = { 0, 0, -1 };
  const vec<double, 3> n = cross(accWorld, g);
  const double magN = mag(n);
  const double qaDotG = dot(accWorld, g);
  const double phi = std::atan2(magN, qaDotG);
  //
  // adjust rotation quaternion by nudging it towards g by gain * phi
  quat<double> qAdj = rot_quat(n, gain * phi);
  normalize(qAdj);
  quat<double> qout = qAdj * qt1;
  normalize(qout);

  return quatToNumericVec(qout);
}
