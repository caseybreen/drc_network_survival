#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
source("who_sampling_library.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Sample size calculations"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("N", 
                    "Size of DSS site population",
                    min=1.5e3, max=5e5, value=15e5, step=1000),
        sliderInput("d.bar", 
                    "Average number of confidants",
                    min=0.1, max=10, value=1.5, step=.05),
        sliderInput("tau", 
                    "Fraction of confidants aware of abortion",
                    min=0.05, max=1, value=2/3),
        sliderInput("p.true", 
                    "Abortion rate (per 1,000)",
                    min=1, max=100, value=25),
        sliderInput("M", 
                    "Number of simulations",
                    min=100, max=10000, value=100, step=100),
        sliderInput("num.n",
                    "Number of sample sizes to check",
                    min=5, max=25, value=10, step=1),
        numericInput("seed",
                     "RNG seed",
                     value=101),
        br(),
        actionButton("run_simulation", "Run simulation"),
        br(),
        br(),
        p("Note: higher values of M will lead to more precise estimates, but may take a lot of computation time"),
  
        downloadButton("downloadData", "Download results")
      ),
      mainPanel(
        plotOutput("cvplot"),
        plotOutput("moeplot")
      )
      # Show a plot of the generated distribution
      #mainPanel(
      #   plotOutput("distPlot")
      #)
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  reactive_values <- reactiveValues(res=NULL)
  
  run_simulation <- observeEvent(input$run_simulation, {
    cat('starting simulation\n')
    
    
    sim.seed <- input$seed
    M <- input$M
    p <- input$p.true / 1000
    tau <- input$tau
    d.bar <- input$d.bar
    N <- input$N
    num.n <- input$num.n
    
    set.seed(sim.seed)
    
    population <- data.frame(d.i = rpois(N, lambda = d.bar))
    
    population$y.i <- map_int(population$d.i, 
                              ~ rbinom(n=1, size=.x, p)) 
    
    #n.vals <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 
    #            2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000,
    #            11000, 12000, 13000, 14000, 15000)
    #n.vals <- c(100, 500, 800, 1000, 
    #            2000, 5000, 10000)
    
    n.vals <- round(seq(from=100, to=10000, length.out=num.n))
    
    withProgress(message = 'Running simulation', value=0, {
    map_df(n.vals,
           function(n) {
             #withProgress(message = 'Running simulation', value=0, {
             
             
    withProgress(message = paste0("n=",n), value=0, {
             ests <- map_df(1:M,
                            function(rep) {
                              force(rep)
                              
                              this.sample.idx <- sample(1:nrow(population), n, replace=FALSE)
                              this.y <- population[this.sample.idx, 'y.i']
                              this.d <- population[this.sample.idx, 'd.i']
                              this.est <- (mean(this.y)/mean(this.d))
                              
                              #incProgress(1/(M*length(n.vals)))
                              incProgress(1/M)
             
                              #return(this.est)
                              return(data.frame(estimate=this.est,
                                                num=mean(this.y),
                                                denom=mean(this.d)))
                            })  
             
    })
             approx.res <- approx.var(p, n, d.bar, N, population)
             approx.res2 <- approx.var2(p, n, d.bar)
             
             num.analytic.uv <- num.unit.var(population)
             num.analytic.uv2 <- num.unit.var2(d.bar, p)
             
             denom.analytic.uv <- denom.unit.var(d.bar)
             
             res.moe <- 1.96 * sqrt(var(ests$estimate))
             res.cv <- sqrt(var(ests$estimate)) / mean(ests$estimate)
             
             incProgress(1/length(n.vals))
             
             return(data.frame(n=n,
                               M=M,
                               estimate.mean=mean(ests$estimate),
                               estimate.var=var(ests$estimate),
                               estimate.moe=res.moe,
                               estimate.cv=res.cv,
                               approx.var=approx.res,
                               approx.var2=approx.res2,
                               num.uv=var(ests$num),
                               num.analytic.uv=num.analytic.uv/n,
                               num.analytic.uv2=num.analytic.uv2/n,
                               denom.uv=var(ests$denom),
                               denom.analytic.uv=denom.analytic.uv/n,
                               denom.var=var(ests$denom)))
           }) -> res
    })
    
    reactive_values$res <- res
    
  })
  
  output$cvplot <- renderPlot({
    cat('rendering CV plot\n')
    
    if (is.null(reactive_values$res)) return()
  
    up2 <- ggplot(reactive_values$res) +
      geom_line(aes(x=n, y=estimate.cv)) +
      theme_minimal() +
      xlab('Sample size') +
      ylab('Approximate\ncoefficient of variation\n(CV)')
    
    cvplot <- up2 + 
              xlim(0, 2600) + 
              annotation_custom(grob=ggplotGrob(up2), 
                                xmin=750, xmax=2500,
                                ymin=.25, ymax=.6)  
    
    return(cvplot)
  
  })
  
  output$moeplot <- renderPlot({
    cat('rendering MOE plot\n')
    
    if (is.null(reactive_values$res)) return()
    up <- ggplot(reactive_values$res) +
      geom_line(aes(x=n, y=estimate.moe)) +
      theme_minimal() +
      xlab('Sample size') +
      ylab('Approximate\nmargin of error (MOE)')
    
    moeplot <- up + xlim(0, 2600) + 
      annotation_custom(grob=ggplotGrob(up), 
                        xmin=600, xmax=2500,
                        ymin=.01, ymax=.02)
    
    return(moeplot)
  
  })
  
  # see
  # https://stackoverflow.com/questions/40666542/shiny-download-table-data-and-plot
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste0('results.csv')  
    },
    content <- function(file) {
     write_csv(reactive_values$res, file) 
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

