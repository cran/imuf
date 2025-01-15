## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(imuf)
library(purrr)
#
lst_ned_in <- as.list(as.data.frame(t(walking_shin_1))) %>% unname
dt <- 1/50
#
myCompUpdate <- function(initQ, accgyr) {
  acc <- accgyr[1:3]
  gyr <- accgyr[4:6]
  gain <- 0.1
  orientation <- compUpdate(acc, gyr, dt, initQ, gain)
  orientation
}
#
orientations <- purrr::accumulate(lst_ned_in, myCompUpdate, .init = c(1, 0, 0, 0))
#
head(orientations)

## -----------------------------------------------------------------------------
#
# time increment for animation needs to be in milli-seconds
animate_imu(orientations, dt*1000)

## -----------------------------------------------------------------------------
if (interactive()) {
  library(shiny)

  ui = pageWithSidebar(
    headerPanel("render an IMU animation example"),
    sidebarPanel(),
    mainPanel(animate_imuOutput('orientations'))
  )
  
  server = function(input, output, session) {
    output$orientations <- renderAnimate_imu(
      animate_imu(orientations, dt*1000)
    )
  }
  
  shinyApp(ui = ui, server = server)
}

