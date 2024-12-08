// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

using namespace Rcpp;
using Eigen::Quaternion;
using Eigen::QuaternionBase;
using Eigen::VectorXd;

VectorXd numericVecToVec(NumericVector v) {
  VectorXd vout(3); vout << v[0], v[1], v[2];
  return vout;
}

NumericVector vecToNumericVec(VectorXd v) {
  NumericVector vout = { v[0], v[1], v[2] };
  return vout;
}

Quaternion<double> vecToQuat(VectorXd v) {
  return Quaternion<double> { 0, v[0], v[1], v[2] };
}

Quaternion<double> numericVecToQuat(NumericVector v) {
  Quaternion<double> q(v[0], v[1], v[2], v[3]);
  return q;
}

NumericVector quatToNumericVec(Quaternion<double> q) {
  return NumericVector { q.w(), q.x(), q.y(), q.z() };
}

Quaternion<double> qTimesD(Quaternion<double> q, double d) {
  // a function to multiply a quaternion by a double
  Quaternion<double> qout(q.coeffs() * d);
  return qout;
}

Quaternion<double> operator* (const Quaternion<double>& q, double d) { return qTimesD(q, d); }
Quaternion<double> operator* (double d, const Quaternion<double>& q) { return qTimesD(q, d); }
Quaternion<double> operator/ (const Quaternion<double>& q, double d) { return qTimesD(q, 1/d); }

Quaternion<double> operator+ (const Quaternion<double>& q1, const Quaternion<double>& q2) {
  Quaternion<double> qout;
  qout.w() = q1.w() + q2.w();
  qout.x() = q1.x() + q2.x();
  qout.y() = q1.y() + q2.y();
  qout.z() = q1.z() + q2.z();
  return qout;
}

//'
//' Update orientation with 3-axis acc and gyr data
//'
//' @description
//' `compUpdate()` uses complementary filtering to update the orientation, given
//' an initial orientation, readings of a 3-axis accelerometer and a 3-axis
//' gyroscope. time duration, and a gain factor
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

  const VectorXd accVec = numericVecToVec(acc);
  const VectorXd gyrVec = numericVecToVec(gyr);
  const Quaternion<double> qt = numericVecToQuat(initQuat);
  //
  // make gryVec into a quat for Euler Equation
  const Quaternion<double> gyrQuat = vecToQuat(gyrVec);
  const double gyrQuatNorm = gyrQuat.norm();
  const double delta = gyrQuatNorm * dt / 2.0;
  Quaternion<double> qt1;
  if (delta == 0) {
    qt1 = qt;
  } else {
    qt1 = qt * cos(delta) + qt * 1 / gyrQuatNorm * sin(delta) * gyrQuat;
  }
  qt1.normalize();
  //
  // transform acc from body-fixed frame to world frame
  const VectorXd accWorld = qt1 * accVec;
  //
  // complement with accelerometer info
  VectorXd g(3); g << 0, 0, -1;
  Quaternion<double> qAdj = Quaternion<double>::FromTwoVectors(accWorld, g);
  //
  // adjust rotation quaternion by nudging it towards g by gain * phi
  Quaternion<double> qout = qt1.slerp(gain, qAdj * qt1);
  //
  return quatToNumericVec(qout);
}

//'
//' Rotate a 3-vector by a quaternion
//'
//' @description
//' `rotV()` rotates a 3-vector by a quaternion expressed as a unit 4-vector in
//' (w,x,y,z) convention
//'
//' @param quat A numeric unit 4-vector (w,x,y,z) for a rotation quaternion
//' @param vin A numeric 3-vector to be rotated by quat
//' @returns A numeric 3-vector after the rotation
//' @export
//'
//' @examples
//' q <- c(cos(pi/4), sin(pi/4), 0, 0)
//' vin <- c(0, 1, 0)
//' rotV(q, vin)
//'
// [[Rcpp::export]]
NumericVector rotV(NumericVector quat, NumericVector vin) {
  const Quaternion<double> q = numericVecToQuat(quat);
  const VectorXd v = numericVecToVec(vin);
  const VectorXd vout = q._transformVector(v);
  NumericVector nvout = vecToNumericVec(vout);
  return nvout;
}

