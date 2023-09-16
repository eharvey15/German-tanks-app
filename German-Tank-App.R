
library(shiny)
library(ggplot2)
library(ggthemes)

options(shiny.sanitize.errors = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("German Tank Problem"),
  
  
  "For background information about this famous problem, ",
  tags$a(href="https://www.albert.io/blog/german-tank-problem-explained-ap-statistics-review/", 
         "click here.", target="blank"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      checkboxGroupInput("estimator",
                         "choice of estimator (up to 3)",
                         choices = c("mean + 2sd", 
                                     "2 * mean",
                                     "2 * median",
                                     "Q1 + Q3",
                                     "Q3 + 1.5 * IQR",
                                     "mean + median",
                                     "max", 
                                     "min + max", 
                                     "min + max -1",
                                     "max + max/n -1"),
                         selected = c("2 * median","mean + 2sd","max")),
      
      numericInput("sample_size", 
                   "Number of tanks captured:", 
                   min=1, 
                   max="population_size", 
                   value = 10),
      
      numericInput("population_size",
                   "Actual Number of Tanks (hypothetical):",
                   min="sample_size",
                   max=1000,
                   value=100),
      
      numericInput("iterations", 
                   "Number of iterations:",
                   min=100,
                   max=9999, 
                   value = 1000),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 10,
                  max = 120,
                  value = 70),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot1", height = "220px"),
      plotOutput("distPlot2", height = "220px"),
      plotOutput("distPlot3", height = "220px"),
      
      
      
    )
  ),
  
  "For a more advanced discussion of this problem, ",
  tags$a(href="http://www.gtmath.com/2018/06/parameter-estimation-part-2-german-tank.html", 
         "click here.", target="blank"),
  
  "For a proof of why Max + Max/n -1 is UMVUE (Minimum Variance Unbiased Estimator), ",
  tags$a(href="http://www.gtmath.com/2019/10/parameter-estimation-part-3-MVUE.html", 
         "click here.", target="blank"),
  
  "To learn more about the discrete uniform distribution, which is the German Tank Problem setting, ",
  tags$a(href="https://en.wikipedia.org/wiki/Discrete_uniform_distribution", 
         "click here.", target="blank"),
  
  
  "App developed by ",
  tags$a(href="mailto:eharvey0f@nobles.edu", 
         "Edward Harvey.", target="blank"),
  "Mr. Harvey teaches math and statistics at the Noble and Greenough School in Dedham, MA."
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot1 <- renderPlot({
    
    validate(
      need(input$estimator[1], "Choose an estimator")
    )
    
    samp_dist <- rep(NA,input$iterations)
    
    for (i in seq(input$iterations)) {
      sample_data <- sample(x=seq(input$population_size), 
                            size=input$sample_size, 
                            replace = FALSE)
      
      samp_dist[i] <- if(input$estimator[1] == "min + max"){
        min(sample_data)+max(sample_data)
        
      } else if(input$estimator[1] == "mean + 2sd"){
        mean(sample_data)+2*sd(sample_data)
        
      } else if (input$estimator[1] == "max + max/n -1"){
        max(sample_data)+max(sample_data)/input$sample_size-1
      }
      else if (input$estimator[1] == "max"){
        max(sample_data)
      }
      
      else if (input$estimator[1] == "2 * mean"){
        2* mean(sample_data)
      }
      
      else if (input$estimator[1] == "min + max -1"){
        min(sample_data)+max(sample_data)-1
      }
      ####
      else if (input$estimator[1] == "2 * median"){
        2* median(sample_data)
      }
      
      else if (input$estimator[1] == "Q1 + Q3"){
        quantile(sample_data,.25)+quantile(sample_data,.75)
      }
      
      else if (input$estimator[1] == "mean + median"){
        mean(sample_data) + median(sample_data)
      }
      
      else if (input$estimator[1] == "Q3 + 1.5 * IQR"){
        quantile(sample_data,.75) + 1.5 * (quantile(sample_data,.75)-quantile(sample_data,.25))
      }
      
    }
    
    estimate <- mean(samp_dist)
    bias <- (estimate - input$population_size)
    variance <- mean((samp_dist-input$population_size)^2)
    se <- sqrt(variance)
    mse <- bias^2 + variance
    
    table1 <- data.frame(estimate, input$population_size, bias, variance, se, mse)
    colnames(table1) <- c("Estimate", "Actual", "Bias", "Variance","Standard Error", "MSE")
    
    output$table1 <- renderTable(table1, bordered = TRUE, align = "c")
    
    
    samp_dist <- as.data.frame(samp_dist)
    
    
    
    
    # draw the histogram with the specified number of bins
    
    
    ggplot(samp_dist)+
      geom_histogram(aes(x=samp_dist, y = ..density..), bins = input$bins)+
      geom_vline(aes(color="Actual", xintercept = input$population_size), lwd=1, lty="solid")+
      geom_vline(aes(color="Estimate", xintercept = estimate), lwd=2, lty="dotted")+
      theme_economist()+
      xlab("Number of Tanks")+
      ylab("Relative Frequency")+
      scale_color_manual(name= "", values=c(Actual="blue", Estimate="red"))+
      scale_x_continuous(limits=c(input$population_size*0.25,
                                  input$population_size*(2))
      )+
      scale_y_continuous(limits=c(0,0.07))+
      ggtitle(input$estimator[1])
  })
  
  output$distPlot2 <- renderPlot({
    
    validate(
      need(input$estimator[2], "Choose a second estimator")
    )
    
    samp_dist <- rep(NA,input$iterations)
    
    for (i in seq(input$iterations)) {
      sample_data <- sample(x=seq(input$population_size), 
                            size=input$sample_size, 
                            replace = FALSE)
      
      samp_dist[i] <- if(input$estimator[2] == "min + max"){
        min(sample_data)+max(sample_data)
        
      } else if(input$estimator[2] == "mean + 2sd"){
        mean(sample_data)+2*sd(sample_data)
        
      } else if (input$estimator[2] == "max + max/n -1"){
        max(sample_data)+max(sample_data)/input$sample_size-1
      }
      else if (input$estimator[2] == "max"){
        max(sample_data)
      }
      
      else if (input$estimator[2] == "2 * mean"){
        2* mean(sample_data)
      }
      
      else if (input$estimator[2] == "min + max -1"){
        min(sample_data)+max(sample_data)-1
      }
      ####
      else if (input$estimator[2] == "2 * median"){
        2* median(sample_data)
      }
      
      else if (input$estimator[2] == "Q1 + Q3"){
        quantile(sample_data,.25)+quantile(sample_data,.75)
      }
      
      else if (input$estimator[2] == "mean + median"){
        mean(sample_data) + median(sample_data)
      }
      
      else if (input$estimator[2] == "Q3 + 1.5 * IQR"){
        quantile(sample_data,.75) + 1.5 * (quantile(sample_data,.75)-quantile(sample_data,.25))
      }
      
    }
    
    estimate <- mean(samp_dist)
    bias <- (estimate - input$population_size)
    variance <- mean((samp_dist-input$population_size)^2)
    se <- sqrt(variance)
    mse <- bias^2 + variance
    
    table1 <- data.frame(estimate, input$population_size, bias, variance, se, mse)
    colnames(table1) <- c("Estimate", "Actual", "Bias", "Variance","Standard Error", "MSE")
    
    output$table2 <- renderTable(table1, bordered = TRUE, align = "c")
    
    
    samp_dist <- as.data.frame(samp_dist)
    
    
    # draw the histogram with the specified number of bins
    
    
    ggplot(samp_dist)+
      geom_histogram(aes(x=samp_dist, y = ..density..), bins = input$bins)+
      geom_vline(aes(color="Actual", xintercept = input$population_size), lwd=1, lty="solid")+
      geom_vline(aes(color="Estimate", xintercept = estimate), lwd=2, lty="dotted")+
      theme_economist()+
      xlab("Number of Tanks")+
      ylab("Relative Frequency")+
      scale_color_manual(name= "", values=c(Actual="blue", Estimate="red"))+
      scale_x_continuous(limits=c(input$population_size*0.25,
                                  input$population_size*(2))
      )+
      scale_y_continuous(limits=c(0,0.07))+
      ggtitle(input$estimator[2])
  })
  
  output$distPlot3 <- renderPlot({
    
    validate(
      need(input$estimator[3], "Choose a third estimator")
    )
    
    samp_dist <- rep(NA,input$iterations)
    
    for (i in seq(input$iterations)) {
      sample_data <- sample(x=seq(input$population_size), 
                            size=input$sample_size, 
                            replace = FALSE)
      
      samp_dist[i] <- if(input$estimator[3] == "min + max"){
        min(sample_data)+max(sample_data)
        
      } else if(input$estimator[3] == "mean + 2sd"){
        mean(sample_data)+2*sd(sample_data)
        
      } else if (input$estimator[3] == "max + max/n -1"){
        max(sample_data)+max(sample_data)/input$sample_size-1
      }
      else if (input$estimator[3] == "max"){
        max(sample_data)
      }
      
      else if (input$estimator[3] == "2 * mean"){
        2* mean(sample_data)
      }
      
      else if (input$estimator[3] == "min + max -1"){
        min(sample_data)+max(sample_data)-1
      }
      ####
      else if (input$estimator[3] == "2 * median"){
        2* median(sample_data)
      }
      
      else if (input$estimator[3] == "Q1 + Q3"){
        quantile(sample_data,.25)+quantile(sample_data,.75)
      }
      
      else if (input$estimator[3] == "mean + median"){
        mean(sample_data) + median(sample_data)
      }
      
      else if (input$estimator[3] == "Q3 + 1.5 * IQR"){
        quantile(sample_data,.75) + 1.5 * (quantile(sample_data,.75)-quantile(sample_data,.25))
      }
      
    }
    
    estimate <- mean(samp_dist)
    bias <- (estimate - input$population_size)
    variance <- mean((samp_dist-input$population_size)^2)
    se <- sqrt(variance)
    mse <- bias^2 + variance
    
    table1 <- data.frame(estimate, input$population_size, bias, variance, se, mse)
    colnames(table1) <- c("Estimate", "Actual", "Bias", "Variance","Standard Error", "MSE")
    
    output$table1 <- renderTable(table1, bordered = TRUE, align = "c")
    
    
    samp_dist <- as.data.frame(samp_dist)
    
    
    
    
    # draw the histogram with the specified number of bins
    
    
    ggplot(samp_dist)+
      geom_histogram(aes(x=samp_dist, y = ..density..), bins = input$bins)+
      geom_vline(aes(color="Actual", xintercept = input$population_size), lwd=1, lty="solid")+
      geom_vline(aes(color="Estimate", xintercept = estimate), lwd=2, lty="dotted")+
      theme_economist()+
      xlab("Number of Tanks")+
      ylab("Relative Frequency")+
      scale_color_manual(name= "", values=c(Actual="blue", Estimate="red"))+
      scale_x_continuous(limits=c(input$population_size*0.25,
                                  input$population_size*(2))
      )+
      scale_y_continuous(limits=c(0,0.07))+
      ggtitle(input$estimator[3])
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
