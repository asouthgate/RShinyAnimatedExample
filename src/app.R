library(shiny)
source('./diffusion.R')

#options(error = function() {traceback(2, max.lines=1000); if(!interactive()) quit(save="no", status=1, runLast=T)})

MAX_N_TERMS=50
precompute_bvec(MAX_N_TERMS)

ui <- fluidPage(

  titlePanel("Diffusion Example"),

  hr(style="border-color: grey;"),

  sidebarLayout(

    sidebarPanel(
         selectInput("n_terms", "Number of terms",choices=c(1, 3, 10, 20, 30, MAX_N_TERMS), selected=MAX_N_TERMS),
         numericInput('delta_t', 'Time delta', 0.0001, min=0.0001, step=0.001),
         helpText("Want help?"),

         hr(style="border-color: grey;"),

         # start / reset buttons on same row
         fluidRow(
           column(7, uiOutput("resetbutton")),
           column(5, uiOutput("startbutton"))
         ),

         hr(style="border-color: grey;"),

         fluidRow(
           column(7, actionButton("stop","Stop")),
           column(5, actionButton("play","Play"))
         ),

        withMathJax(),
        helpText('foobar')
    ),
#   sidebarPanel(
#        helpText('bar to the baz!')
#      helpText('An irrational number \\(\\sqrt{2}\\)'),
#               and a fraction $$1-\\frac{1}{2}$$'),
#      helpText('and a fact about \\(\\pi\\):
#               $$\\frac2\\pi = \\frac{\\sqrt2}2 \\cdot
#               \\frac{\\sqrt{2+\\sqrt2}}2 \\cdot
#               \\frac{\\sqrt{2+\\sqrt{2+\\sqrt2}}}2 \\cdots$$'),
#      uiOutput('ex1'),
#      uiOutput('ex2'),
#      uiOutput('ex3'),
#      uiOutput('ex4'),
#   ),

    mainPanel(
      helpText('some baz'),
      plotOutput('mygraph')        
    )

  )
  
)

server <- function(input, output) {
  
  xvals = seq(0, 1, by=0.01)

  waits <- reactiveValues()
  waits$time <- 0
  waits$data <- sapply(xvals, cal_u_t0)

  
  # Why do we create the actionButton and renderUI in the server part? reactivity?
  # use waits$var if you want to change the elements as you go
  output$resetbutton <- renderUI({
    { lbl <- 'Reset' }
    actionButton('reset', label=lbl)
  })
  
  forward <- function() {
     if (session$timer_stopped) {
         waits$time = waits$time + input$delta_t
         waits$data <- sapply(xvals, cal_u, t=waits$time, n_terms=input$n_terms)
     }
  }
  
  session <- reactiveValues()
  session$timer <- reactiveTimer(Inf)
  session$timer_stopped <- 0

  
  observeEvent(input$play, {
    session$timer_stopped <- 1
    session$timer <- reactiveTimer(30)
  })  

  observeEvent(session$timer(), {
    forward()
  })

  
  observeEvent(input$stop,{
    session$timer_stopped <- 0
    session$timer <- reactiveTimer(Inf)
  })
   
  # reset button is pressed
  observeEvent(input$reset, {
    session$timer_stopped <- 0
    waits$time <- 0
    session$timer <- reactiveTimer(Inf)
    waits$data <- sapply(xvals, cal_u_t0)
  })
  
  ## main plot output
  output$mygraph <- renderPlot({
        bp <- plot(xvals, waits$data, ylim=c(-0.5, 0.5), type='l', ylab='u(x,t)', xlab='x')
    },
    height=700
  )  
  
}

runApp(shinyApp(ui, server), launch.browser = TRUE)
