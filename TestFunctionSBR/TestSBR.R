library(shiny.exe)
hostWin(
    appDir = 'C:/Users/LENOVO/Desktop/TestFunctionSBR',
    port = getOption('shiny.port'),
    launch.browser = TRUE,
    host = '127.0.0.1',
                  workerId = '',
                  quiet = FALSE,
                  display.mode = c('auto', 'normal', 'showcase'),
                  test.mode = getOption('shiny.testmode', FALSE))
