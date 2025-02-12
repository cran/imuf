## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
if (interactive()) {
  library(shiny)
  library(serial)
  library(imuf)
  library(stringr)

  getCon <- function(port) {
    #
    # set up connection for serial port
    con <- serial::serialConnection(name = "testcon", port = port,
                                    mode = "115200,n,8,1", newline = 1, translation = "crlf"
    )
    if (serial::isOpen(con)) close(con)
    con
  }
  
  readFromSerial <- function(con) {
    #
    # helper - function to convert sensor coord to NED
    bmi2ned <- function(bmi) {
      # convert bmi coord to ned coord
      c(bmi[1], -bmi[2], -bmi[3])
    }
    #
    # helper - function to convert deg to radian
    toRad <- function(deg) {
      deg * pi/180
    }
    #
    # functions to process and validate data read from serial port
    isValidLength <- function(x) {
      minLength <- 32
      if (x <= minLength) return(FALSE)
      return(TRUE)
    }
    str2Vec <- function(x) {
      #
      # data from the IMU is a row of 6 comma-separated floats:
      # accx, accy, accz, gyrx, gyry, gyrz
      y <- stringr::str_split_1(x, ",") %>%
        trimws() %>%
        as.numeric() %>%
        suppressWarnings()
      y
    }
    isValidNumElements <- function(x) {
      if (length(x) != 6) return(FALSE)
      return(TRUE)
    }
  
    while (TRUE) {
      nInQ <- serial::nBytesInQueue(con)["n_in"]
      if(!isValidLength(nInQ)) next
      #
      a <- serial::read.serialConnection(con) %>% str2Vec()
      if(!isValidNumElements(a)) next
      #
      # a is the IMU output we want, exit infinite loop
      break
    }
    # gyr from bmi270 IMU is in deg/sec, need to convert to rad/sec
    list(acc = bmi2ned(a[1:3]),
         gyr = bmi2ned(a[4:6]) %>% toRad())
  }
  
  runshiny <- function(port) {
    #
    ui = fluidPage(
      actionButton("do", "Start animation"),
      imu_objectOutput("imu1")
    )
  
    server = function(input, output, session) {
  
      # initial orientation
      quat0 <- c(cos(pi/4), sin(pi/4), 0, 0)
  
      observeEvent(input$do, {
        con <- getCon(port)
        open(con)
        quat <- quat0
        while (TRUE) {
          accgyr <- readFromSerial(con)
          quat <- compUpdate(accgyr$acc, accgyr$gyr, dt = 1/50, initQ = quat, gain = 0.1)
          imu_proxy("imu1") %>%
            imu_send_data(data = quat)
        }
      })
  
      output$imu1 <- renderImu_object(
        imu_object(quat0)
      )
    }
    shinyApp(ui = ui, server = server)
  }
}

