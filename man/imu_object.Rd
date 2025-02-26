% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imu_object.R
\name{imu_object}
\alias{imu_object}
\title{Animate in real time movement of an inertial measurement unit in shiny}
\usage{
imu_object(data = c(1, 0, 0, 0), width = NULL, height = NULL, elementId = NULL)
}
\arguments{
\item{data}{A \code{vector} of numeric 4-vectors each of which a unit quaternion (w,x,y,z)}

\item{width, height}{width and height of imu_object htmlwidget
specified in any valid \code{CSS} size unit}

\item{elementId}{\code{string} id as a valid \code{CSS} element id.}
}
\description{
Create in real time animated movements of an inertial measurement unit in shiny
}
