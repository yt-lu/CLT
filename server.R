library(shiny)
library(shinyalert)
library(DT)

Sys.setenv(TZ = 'US/Eastern')

shinyServer(function(input, output, session) {
  
  #########################
  # Global
  #########################
  v <- reactiveValues(xbar = NA, samplesize = NA) # Save local xbars
  
  # 1 for new draw. 0 for existing draw
  # Change to 0 after a submission to avoid duplicated submission
  newdraw <- reactiveVal(1) 
  
  
  #########################
  # Population
  #########################
  userdata <- reactive({
    if (is.null('population.csv')) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else {
      a <- read.csv('population.csv', header = TRUE)
      return(a)
    }
  })
    
  observeEvent(input$file, {
    
    msg_new_game <- HTML("Type the admin password to proceed.")
    shinyalert(
      msg_new_game, type = "input", inputType = 'password',html = TRUE,
      callbackR = function(x) { 
        if(x == 'GoBears') {
        close(file('population.csv', open="w"))
        y <- read.csv(input$file$datapath,
                             header = TRUE)
        write.table(y, 'population.csv', row.names = F, quote = F, sep = ',')
        } else {
          shinyalert('Incorrect password.', type = 'info')
        }
      })
  })
    

  
  # Output: text population
  output$text1 <- renderText({
    out <- paste0("<strong>Population</strong>")
    out
  })
  
  # Output: Population data table ----
  output$population <- renderTable({
          return(userdata())
          },striped = TRUE,colnames = FALSE)
  
  
  #########################
  # Sample
  #########################
  x <- eventReactive(input$draw, {
        s <- userdata()[sample(1:nrow(userdata()), input$n),]
        newdraw(1)
      return(s)
  })
  
  
  # Output: text sample
  output$text2 <- renderText({
    out <- paste0("<strong>Sample</strong>")
    out
  })
  
  
  # Output: Sample data table
  output$sample <- renderTable({
        return(x())
        },striped = TRUE,colnames = FALSE)
  
  
  #########################
  # Distribution graph
  #########################
  observeEvent(input$submit, {
    xbar <- round(mean(x()$value), 1)
    if (input$xbar == xbar & newdraw() == 1) {
      a <- data.frame(xbar, input$n)
      write.table(a, 'report.csv', append = T, row.names = FALSE, quote = FALSE, sep = ',',
                  col.names = FALSE)
      v$xbar <- c(v$xbar, xbar)
      newdraw(newdraw() - 1)
    }
   })
  

  output$graph <- renderPlot({
    if (input$class) {
      report <- read.csv('report.csv' ,strip.white = TRUE, header = TRUE)
      xs <- report[which(report$samplesize == input$n), 2]
      
    } else {
      xs <- round(as.numeric(v$xbar[-1]), 1)
    }
      m <- length(xs)
      if (m > 0) {
        td <- do.call(rbind, lapply(1:m, (function(i){ 
          c(xs[i], 0.1 + (sum(xs[1:i] == xs[i]) - 1 ) / 20 ) # Count how many identical values has occured before the i-th
        })))
        plot(x= td[,1], y = td[,2], 
             pch=19, xlim = c(0, 25), ylim = c(0, m/3), yaxt = 'n',
             col = ifelse(td[,1] == xs[m] & td[, 2] == max(td[which(td[,1] == xs[m]), 2]), "black", "gray"),
             main = '', 
             xlab = '', ylab = '')
      } else {
        plot.new()
      }
    
    title(main = "Sampling Distribution of\n Sample Mean")
  })

  # observeEvent(input$reset, {
  #   msg_new_game <- HTML("This will erase the current class report. Type the admin code to proceed.")
  #   shinyalert(
  #     msg_new_game, type = "input", html = TRUE,
  #     callbackR = function(x) { if(x == 'GoBears') {
  #         close( file( 'report.csv', open="w" ) )
  #         write.table(data.frame(BodyTemp = NA, HeartRate = NA, CreditHours = NA),
  #                     'report.csv', row.names = F, quote = F, sep = ',')}
  #       })
  #   })

 })
