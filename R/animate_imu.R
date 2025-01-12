#' Animate movement of an inertial measurement unit
#'
#' Create interactive animated movements of an inertial measurement unit
#'
#' @param data A \code{list} of numeric 4-vectors each of which a unit quaternion (w,x,y,z)
#' @param dt A numeric of time duration in milli-seconds
#' @param width,height  width and height of animate_imu htmlwidget
#'          specified in any valid \code{CSS} size unit
#' @param elementId \code{string} id as a valid \code{CSS} element id.
#'
#' @import htmlwidgets
#'
#' @export
animate_imu <- function(data, dt, width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    data = data,
    dt = dt
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'animate_imu',
    x,
    width = width,
    height = height,
    package = 'imuf',
    elementId = elementId
  )
}

animate_imu_html <- function(id, style, class, ...){
  htmltools::tags$canvas(id = id, class = class)
}

#' Shiny bindings for animate_imu
#'
#' Output and render functions for using animate_imu within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a animate_imu
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name animate_imu-shiny
#'
#' @export
animate_imuOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'animate_imu', width, height, package = 'imuf')
}

#' @rdname animate_imu-shiny
#' @export
renderAnimate_imu <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, animate_imuOutput, env, quoted = TRUE)
}
