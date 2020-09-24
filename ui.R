
shiny::fluidPage(
  shiny::tags$style("* { font-family: Arial; }"),
  shiny::titlePanel("Exploring Optimal SARS-CoV-2 Viral Test Allocation for New York"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fluidRow(
        shiny::column(2,
                      shiny::img(src="2color-bluebg.png",height=100,width=100)
        ),
        shiny::column(1
        ),
        shiny::column(9,
                      print("Add description here\n") ,
                      shiny::br(),
                      a("\n Blahblah add more information link here. ",target="_blank",href="RShinyDoc.pdf")
        )
      ),
      shiny::br(),
      
      shiny::tabsetPanel(
        ### PATIENT CHARACTERISTICS
        shiny::tabPanel('Historical Data',
                        shiny::fluidRow(
                          shiny::column(12,
                                        shiny::sliderInput("w", "Shrinkage weight (w):", value= 1, min = 0, max = 1, step = 0.1),
                                        shiny::sliderInput("falseneg", "False negative probability (beta):", value= 0.3, min = 0.01, max = 0.4, step = 0.01),
                                        shiny::sliderInput("falsepos", "False positive probability (alpha):", value= 0.01, min = 0.01, max = 0.05, step = 0.01),
                                        shiny::sliderInput("targetpos", "Target positivity rate (w<1 only):", value= 0.02, min =0.01, max = 0.15, step = 0.01),
                                        shiny::sliderInput("ta", "Proportion of asymptomatic cases (ta):", value= 0.75, min =0.55, max = 0.95, step = 0.01),
                                        shiny::sliderInput("mult", "Case underreporting factor:", value= 5, min = 1, max = 20, step = 1)
                          )#end column
                        ), #end fluidrow,
           value = 'Historical'
        ),#end tabPanel
        shiny::tabPanel('Forecasting',
                        shiny::fluidRow(
                          shiny::column(12,
                                        shiny::sliderInput("w2", "Shrinkage weight (w):", value= 1, min = 0, max = 1, step = 0.1),
                                        shiny::sliderInput("falseneg2", "False negative probability (beta):", value= 0.3, min = 0.01, max = 0.4, step = 0.01),
                                        shiny::sliderInput("falsepos2", "False positive probability (alpha):", value= 0.01, min = 0.01, max = 0.05, step = 0.01),
                                        shiny::sliderInput("targetpos2", "Target positivity rate (w<1 only):", value= 0.02, min =0.01, max = 0.15, step = 0.01),
                                        shiny::sliderInput("ta2", "Proportion of asymptomatic cases (ta):", value= 0.75, min =0.55, max = 0.95, step = 0.01),
                                        shiny::sliderInput("num2", "Peak number of cases in population:", value= 200000, min = 10000, max = 500000, step = 10000),
                                        shiny::sliderInput("Total2", "Number of Available Tests (Weekly):", value= 50000, min =25000, max = 200000, step = 5000)
                          )#end column
                        ), #end fluidrow,
            value = 'Forecasting'
        ),#end tabPanel
        id = 'TABCHOSEN'
      ), #end tabsetPanel
      print("Contact: Dr. Lauren J Beesley, lbeesley@umich.edu \n") ,
      shiny::br()
      , width = 4),#end sidebarpanel
    
    ###################
    ### RIGHT PANEL ###
    ################### 
    shiny::mainPanel(
      shiny::fluidRow(
        shiny::column(6,shiny::plotOutput("plots", inline = TRUE)), 
      ),
      shiny::br()
      , width = 6)	    
    
  )#end sidebarlayout 
  
  
)#end fluidpage

