require(ggplot2)
require(dplyr)
require(shiny)
require(readr)
require(reshape2)
require(gridExtra)
require(optiSolve)



library(ggplot2)
library(dplyr)
library(shiny)
library(readr)
library(reshape2)
library(gridExtra)
library(optiSolve)


STARTING_WEIGHTS = matrix(c(0,1,0.3,0),nrow=1,ncol = 4)
rownames(STARTING_WEIGHTS) = 'w'
colnames(STARTING_WEIGHTS) = c('Weeks 1-2', 'Weeks 3-8', 'Weeks 9-13', 'Weeks 14+')

STARTING_AGES = matrix(c(0.24, 0.43, 0.21, 0.12),nrow=1,ncol = 4)
rownames(STARTING_AGES) = 'Proportion'
colnames(STARTING_AGES) = c("age 0-17","age 18-49","age 50-64","age 65+")

ui<- shiny::fluidPage(
  shiny::tags$style("* { font-family: Arial; }"),
  shiny::titlePanel("Exploring Optimal SARS-CoV-2 Viral Test Allocation"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
        shiny::fluidRow(shiny::column(12,shiny::img(src="2color-bluebg.png",height=100,width=100))),
        shiny::fluidRow(
        print("This application calculates optimal SARS-CoV-2 viral test allocation by age and symptom severity given various input parameters. 
                            Users can (1) explore how results under optimal test allocation compare to historical New York City case numbers and test positive rates, 
                            (2) explore test allocation for different hypothetical pandemic scenarios in New York City, and 
                            (3) explore allocation of two available tests at the local level (e.g. rapid vs. RT-PCR test allocation for a university)   \n")
        ),
        shiny::hr(),
      shiny::fluidRow(
        shiny::tabsetPanel(
          ### PATIENT CHARACTERISTICS
          shiny::tabPanel('Case Numbers under Optimal Test Allocation vs. Reported for New York City',
                          shiny::br(),
                          shiny::column(6,
                                        shiny::selectInput("w", "Goal of testing:", choices=c('Detecting Cases (w=1)','Outbreak Surveillance (w=0)')),
                                        shiny::sliderInput("falseneg", "False negative rate of test:", value= 0.3, min = 0.01, max = 0.4, step = 0.01),
                                        shiny::sliderInput("falsepos", "False positive rate of test:", value= 0.01, min = 0.005, max = 0.05, step = 0.005)
                          ),
                          shiny::column(6,
                                        shiny::sliderInput("ta", "Proportion of true cases w/o symptoms:", value= 0.75, min =0.30, max = 0.95, step = 0.01),
                                        #shiny::br(),
                                        shiny::sliderInput("mult", "Case under-reporting factor:", value= 5, min = 1, max = 20, step = 1),
                                        conditionalPanel(condition = "input.w=='Outbreak Surveillance (w=0)'",
                                        shiny::sliderInput("targetpos", "Target test positivity rate:", value= 0.03, min =0.01, max = 0.15, step = 0.01))
                                        
                          ),
                          value = 'Historical'
          ),#break tabPanel
          shiny::tabPanel('Test Allocation for Hypothetical Pandemic Scenarios in New York City',
                          shiny::br(),
                          shiny::column(6,
                                        shiny::selectInput("w2", "Goal of testing:", choices=c('Detecting Cases (w=1)','Outbreak Surveillance (w=0)')),
                                        shiny::sliderInput("falseneg2", "False negative rate of test:", value= 0.3, min = 0.01, max = 0.4, step = 0.01),
                                        shiny::sliderInput("falsepos2", "False positive rate of test:", value= 0.01, min = 0.005, max = 0.05, step = 0.005)),
                          shiny::column(6,
                                        shiny::sliderInput("Total2", "Number of Available Tests (Weekly):", value= 50000, min =25000, max = 200000, step = 5000),
                                        conditionalPanel(
                                          condition = "input.w2=='Detecting Cases (w=1)'",
                                          shiny::sliderInput("num2_w1", "Number of cases in population:", value= 200000, min = 10000, max = 500000, step = 10000)
                                        ),
                                        conditionalPanel(
                                          condition = "input.w2=='Outbreak Surveillance (w=0)'",
                                          shiny::sliderInput("num2_w0", "Number of cases in population:", value= 10000, min = 500, max = 50000, step = 500)
                                        )   ,
                                        conditionalPanel(condition = "input.w2=='Outbreak Surveillance (w=0)'",
                                                         shiny::sliderInput("targetpos2", "Target positivity rate:", value= 0.03, min =0.01, max = 0.15, step = 0.01))
                          ),
                          value = 'Simulations'
          ),#break tabPanel
          shiny::tabPanel('Allocation of Two Tests at Local Level',
                          shiny::br(),
                          shiny::column(6,
                                        shiny::sliderInput("cost", "Total budget:", value= 100000, min =5000, max = 300000, step = 5000),
                                        shiny::sliderInput("y", "Cost per test (test 1, test 2):", value= c(5,135), min =0, max = 200, step = 1),
                                        shiny::sliderInput("size3", "Total population size:", value= 50000, min = 5000, max = 100000, step = 5000),
                                        shiny::sliderInput("num3", "Number of cases in population:", value= 500, min = 5, max = 5000, step = 10)
                          ),  
                          shiny::column(6,
                                        shiny::sliderInput("falsenegt", "False negative rate (test 2, test 1):", value= c(0.20,0.70), min = 0.01, max = 0.9, step = 0.01),
                                        shiny::sliderInput("falsepost1", "False positive rate (test 1):", value= 0.01, min = 0.005, max = 0.05, step = 0.005),
                                        shiny::sliderInput("falsepost2", "False positive rate (test 2):", value= 0.01, min = 0.005, max = 0.05, step = 0.005)
                          ),
                          shiny::fluidRow(
                            shiny::column(10,
                                          shinyMatrix::matrixInput("ages",label = "Enter proportion of people in each age category [0-1]:",
                                                                   rows=list(names = FALSE,editableNames = FALSE), cols = list(names = TRUE,editableNames = FALSE),
                                                                   value = STARTING_AGES, class = "numeric"))),
                          value = 'Two_Tests'
          ),#end tabPanel
          id = 'TABCHOSEN'
        ), #end tabsetPanel
        shiny::column(12,
                      print("Contact: Dr. Lauren J Beesley, lbeesley@umich.edu \n")),
        shiny::br()
      )
      , width = 4),#end sidebarpanel
    
    ###################
    ### RIGHT PANEL ###
    ################### 
    shiny::mainPanel(
      shiny::fluidRow(
                      conditionalPanel( condition = "input.TABCHOSEN == 'Historical'", shiny::img(src="ObjectiveFunction1.png",height=200,width=850)),
                      conditionalPanel( condition = "input.TABCHOSEN == 'Simulations'", shiny::img(src="ObjectiveFunction2.png",height=200,width=900)),
                      conditionalPanel( condition = "input.TABCHOSEN == 'Two_Tests'", shiny::img(src="ObjectiveFunction3.png",height=200,width=900))
      ),
      shiny::fluidRow(
        shiny::plotOutput("plotmerge", inline = TRUE)
      # shiny::plotOutput("plot1", inline = TRUE),
      # shiny::plotOutput("plot2", inline = TRUE),
      # shiny::plotOutput("plot3", inline = TRUE),
      # shiny::plotOutput("plot4", inline = TRUE)
      ),
      shiny::br(),
     conditionalPanel( condition = "input.TABCHOSEN == 'Two_Tests'",shiny::br(), shiny::img(src="UniversityTesting.png",height=150,width=500)),
      
      shiny::br()
      , width = 6)	    
    
  )#end sidebarlayout 
  
  
)#end fluidpage






source(file = 'www/find_t_f_V2_NY.R')
source(file = 'www/each_generation_withZ_comb_V2.R')
source(file = 'www/each_generation_withZ_comb_twoTests.R')








### GLOBAL VARIABLES ###
testdata.url = 'https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/tests.csv'
testdata = try(readr::read_csv(url(testdata.url)))
if(class(testdata)[1]=='try-error'){
  testdata = readr::read_csv(file = 'www/tests.csv')
}
ndays = nrow(testdata)
G = ceiling(ndays/7)
data = as.data.frame(matrix(NA, nrow = G, ncol = 3))
colnames(data) = c("Generation","Tests","ReportedCases")
NY_date = c()
for (g in 1:G){
  start = ifelse(g==1,1,(g-1)*7+1)
  end = min(g*7, nrow(testdata))
  data[g,'Generation'] = g
  data[g,'Tests'] = sum(testdata$TOTAL_TESTS[start:end])
  data[g,'ReportedCases'] = sum(testdata$POSITIVE_TESTS[start:end])
  NY_date = c(NY_date,testdata$DATE[start])
}
options(scipen=999)



server = function(input, output, session ) {
  
  
  
  Optimizer_panel1 <- function(input){
    ensure_reactive = paste0(input)
    ### INPUTS ###
    N = 8175133 
    weights = rep(ifelse(input$w == 'Detecting Cases (w=1)',1,0),G) #1
    ta = as.numeric(input$ta)
    ts = (1-ta)/4
    targetpos = as.numeric(input$targetpos)
    mult= as.numeric(input$mult) 
    falseneg = as.numeric(input$falseneg)
    falsepos = as.numeric(input$falsepos)
    ### OPTIMIZE ###
    result = data.frame(matrix(NA,nrow = G, ncol = 10))
    colnames(result) = c("date","positive.rate","severe","mild","asymptomatics","age1","age2","age3","age4","positive.tests")
    Test.result = data.frame(matrix(NA,nrow = G,ncol = 8))
    colnames(Test.result) = c("date","severe","mild","asymptomatics","age1","age2","age3","age4")
    for(i in 1:nrow(result)){
      model = each_generation_withZ(N = N, D = data$ReportedCases[i]*mult,
                                    ta = ta, ts = ts, Total=data$Tests[i],
                                    beta=falseneg, alpha=falsepos, c_pos=targetpos, w=weights[i])
      result[i,'date'] = i
      result[i,'positive.rate'] = floor(model$P.hat)/data$Tests[i]
      result[i,'severe'] = model$selectProb.s
      result[i,'mild'] = model$selectProb.m
      result[i,'asymptomatics'] = model$selectProb.a
      result[i,'age1'] = model$selectProb.age1
      result[i,'age2'] = model$selectProb.age2
      result[i,'age3'] = model$selectProb.age3
      result[i,'age4'] = model$selectProb.age4
      result[i,'positive.tests'] = model$P.hat
      Test.result[i,'date'] = i
      Test.result[i,'severe'] = floor(sum(model$assignedTests[1:4]))
      Test.result[i,'mild'] = floor(sum(model$assignedTests[5:8]))
      Test.result[i,'asymptomatics'] = floor(sum(model$assignedTests[9:12]))      
      Test.result[i,'age1'] = floor(model$assignedTests[1]+model$assignedTests[5]+model$assignedTests[9])
      Test.result[i,'age2'] = floor(model$assignedTests[2]+model$assignedTests[6]+model$assignedTests[10])
      Test.result[i,'age3'] = floor(model$assignedTests[3]+model$assignedTests[7]+model$assignedTests[11])
      Test.result[i,'age4'] = floor(model$assignedTests[4]+model$assignedTests[8]+model$assignedTests[12])
    }
    break.ref = as.character(NY_date)
    break.ref = gsub('/','-',break.ref)
    break.ref = gsub('-2020','',break.ref)
    result[result<0] = 0
    Test.result[Test.result<0] = 0
    return(list(break.ref=break.ref,result=result,Test.result=Test.result))
  }
  
  Optimizer_panel2 <- function(input){
    ensure_reactive = paste0(input)
    ### INPUTS ###
    N = 8175133 
    targetpos = as.numeric(input$targetpos2)
    true_cases= ifelse(input$w2 == 'Detecting Cases (w=1)', as.numeric(input$num2_w1) ,as.numeric(input$num2_w0))
    falseneg = as.numeric(input$falseneg2)
    falsepos = as.numeric(input$falsepos2) 
    weights = as.numeric(input$w2 == 'Detecting Cases (w=1)')
    ### OPTIMIZE ###
    ta.list = seq(0.5,0.9,by=0.05)
    result = data.frame(matrix(NA,nrow = length(ta.list), ncol = 10))
    colnames(result) = c("date","positive.rate","severe","mild","asymptomatics","age1","age2","age3","age4","positive.tests")
    Test.result = data.frame(matrix(NA,nrow = length(ta.list),ncol = 8))
    colnames(Test.result) = c("date","severe","mild","asymptomatics","age1","age2","age3","age4")
    Assigned_Tests = data.frame(matrix(NA, nrow = length(ta.list), ncol = 12))
    for(i in 1:nrow(result)){
      ### Fixed D. Note: D = (4/(1-ta))*number of severe cases
      model = each_generation_withZ(N = N, D = true_cases,
                                    ta = ta.list[i], ts = (1-ta.list[i])/4, Total=input$Total2,
                                    beta=falseneg, alpha=falsepos, c_pos=targetpos, w=weights)
      result[i,'date'] = i
      result[i,'positive.rate'] = floor(model$P.hat)/input$Total2
      result[i,'severe'] = model$selectProb.s
      result[i,'mild'] = model$selectProb.m
      result[i,'asymptomatics'] = model$selectProb.a
      result[i,'age1'] = model$selectProb.age1
      result[i,'age2'] = model$selectProb.age2
      result[i,'age3'] = model$selectProb.age3
      result[i,'age4'] = model$selectProb.age4
      result[i,'positive.tests'] = model$P.hat
      Test.result[i,'date'] = i
      Test.result[i,'severe'] = floor(sum(model$assignedTests[1:4]))
      Test.result[i,'mild'] = floor(sum(model$assignedTests[5:8]))
      Test.result[i,'asymptomatics'] = floor(sum(model$assignedTests[9:12]))    
      Test.result[i,'age1'] = floor(model$assignedTests[1]+model$assignedTests[5]+model$assignedTests[9])
      Test.result[i,'age2'] = floor(model$assignedTests[2]+model$assignedTests[6]+model$assignedTests[10])
      Test.result[i,'age3'] = floor(model$assignedTests[3]+model$assignedTests[7]+model$assignedTests[11])
      Test.result[i,'age4'] = floor(model$assignedTests[4]+model$assignedTests[8]+model$assignedTests[12])
      Assigned_Tests[i,] = model$assignedTests
    }
    result[result<0] = 0
    Test.result[Test.result<0] = 0
    Assigned_Tests[Assigned_Tests<0] = 0
    return(list(ta.list=ta.list,result=result,Test.result=Test.result, Assigned_Tests=Assigned_Tests))
  }
  
  Optimizer_panel3 <- function(input){
    shiny::validate(need(sum(input$ages>=0 & input$ages <=1)==4 & sum(input$ages)==1, 'age proportion must be in [0,1] and proportions must sum to 1 across age categories'))    
    shiny::validate(need(input$y[1]<input$y[2], 'cost for test 1 should be less than test 2'))  
    shiny::validate(need(input$falsenegt[1]<input$falsenegt[2], 'false negative rate for test 2 should be less than for test 1')) 
    ensure_reactive = paste0(input)
    ### INPUTS ###
    N = as.numeric(input$size3) 
    money = as.numeric(input$cost)
    y1 = min(as.numeric(input$y))
    y2 = max(as.numeric(input$y))
    falsenegt1 = max(as.numeric(input$falsenegt))
    falsenegt2 = min(as.numeric(input$falsenegt))
    falsepost1 = as.numeric(input$falsepost1)
    falsepost2 = as.numeric(input$falsepost2)
    agedist = input$ages
    ### OPTIMIZE ###
    ta.list = seq(0.5,0.9,by=0.05)
    result = data.frame(matrix(NA,nrow = length(ta.list), ncol = 11))
    names(result) = c("ta","positive","positive.rate","test1","test2","test1_s","test1_m","test1_a","test2_s","test2_m","test2_a")
    for(i in 1:nrow(result)){
      model = each_generation_withZ_twoTests(N = N,
                                             D = input$num3,
                                             ta = ta.list[i],
                                             ts = (1-ta.list[i])/4,
                                             money = money,
                                             y1 = y1,
                                             y2 = y2,
                                             beta1=falsenegt1,
                                             alpha1=falsepost1,
                                             beta2 = falsenegt2,
                                             alpha2 = falsepost2, 
                                             agedist = agedist)
      result[i,'ta'] = ta.list[i]
      result[i,'positive'] = floor(model$P.hat)
      result[i,'positive.rate'] = floor(model$P.hat)/floor(model$Total)
      
      result[i,'test1'] = floor(sum(model$assignedTests[1:12])) # Test 1
      result[i,'test2'] = floor(sum(model$assignedTests[13:24])) # Test 2
      
      result[i,'test1_s'] = floor(sum(model$assignedTests[1:4])) # Test 1, symp=s
      result[i,'test1_m'] = floor(sum(model$assignedTests[5:8])) # Test 1, symp=m
      result[i,'test1_a'] = floor(sum(model$assignedTests[9:12])) # Test 1, symp=a
      
      result[i,'test2_s'] = floor(sum(model$assignedTests[13:16])) # Test 2, symp=s
      result[i,'test2_m'] = floor(sum(model$assignedTests[17:20])) # Test 2, symp=m
      result[i,'test2_a'] = floor(sum(model$assignedTests[21:24])) # Test 2, symp=a
    }
    result[result<0] = 0
    return(list(ta.list=ta.list,result=result))
  }

  
  

  
  
  
  GetPlots <- function(input,RESULTS){
    ensure_reactive = paste0(input)
    ####################
    ### PLOT RESULTS ###
    ####################

    if(input$TABCHOSEN =='Historical'){
      
      break.ref <- RESULTS$break.ref
      result <- RESULTS$result
      Test.result <- RESULTS$Test.result
      ### INPUTS ###
      N = 8175133 
      weights = rep(ifelse(input$w == 'Detecting Cases (w=1)',1,0),G) #1
      ta = as.numeric(input$ta)
      ts = (1-ta)/4
      targetpos = as.numeric(input$targetpos)
      mult= as.numeric(input$mult) 
      falseneg = as.numeric(input$falseneg)
      falsepos = as.numeric(input$falsepos)
      ### plot 1.1 ###
      Test.result.symptom = data.frame('date' = break.ref,
                                       'asymptomatic' = Test.result[,'asymptomatics'],
                                       'mild' = Test.result[,'mild'],
                                       'severe' = Test.result[,'severe'])
      Test.symptom.long = reshape2::melt(Test.result.symptom,id.vars = "date")
      p1 = ggplot(data=Test.symptom.long,aes(x=date,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref)+
        ggtitle("Optimal test allocation by symptom group in New York City")+
        ylab("Number of tests")+
        xlab("Date")+
        guides(fill=guide_legend(title=""))+
        #scale_fill_manual(values=c('#999999','#E69F00',"#0099CC"))+
        scale_fill_manual(values=c('#9E0142','#F46D43',"#FEE08B"),
                          labels=c('severe', 'mild', 'asymptomatic'),
                          breaks=c('severe', 'mild', 'asymptomatic'))+
        # ggthemes::theme_economist()+
        theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1), legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))+
        annotate(geom = 'point', x=c(1:G), y=data$Tests, pch = 21, col = 'black', bg = 'white', size = 2)+
        annotate(geom = 'point', x=1, y=max(data$Tests), pch = 21, col = 'black', bg = 'white', size = 2)+
        annotate(geom = 'text', x=1.5, y=max(data$Tests), label = 'reported number of tests available', hjust=0)

        ### plot 1.2 ###
      Test.result.age = data.frame('date' = break.ref,
                                   'age1' = Test.result[,'age1'],
                                   'age2' = Test.result[,'age2'],
                                   'age3' = Test.result[,'age3'],
                                   'age4' = Test.result[,'age4'])
      Test.age.long = reshape2::melt(Test.result.age,id.vars = "date")
      
      p2 = ggplot(data=Test.age.long,aes(x=date,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref)+
        ggtitle("Optimal test allocation by age group in New York City")+
        ylab("Number of tests")+
        xlab("Date")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
                          labels = c("age 0-17","age 18-49","age 50-64","age 65+"))+
        #ggthemes::theme_economist()+
        theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1), legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))+
        annotate(geom = 'point', x=c(1:G), y=data$Tests, pch = 21, col = 'black', bg = 'white', size = 2)+
        annotate(geom = 'point', x=1, y=max(data$Tests), pch = 21, col = 'black', bg = 'white', size = 2)+
        annotate(geom = 'text', x=1.5, y=max(data$Tests), label = 'reported number of tests available', hjust=0)

      ### plot 1.3 ###
      T_rptCase_simuCase = data.frame('date'=break.ref,
                                      'reported cases' = data$ReportedCases,
                                      'estimated positive tests' = result$positive.tests)
      p3 = ggplot(data=T_rptCase_simuCase,aes(x=date,y=reported.cases,group=1))+
        geom_bar(aes(y=reported.cases,fill='reported cases'),stat="identity",position = 'identity',alpha=1, color= "gray30")+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref)+
        geom_line(aes(y=estimated.positive.tests,color='optimal test strategy (hypothetical)'),size=1.5)+
        geom_point(aes(y=estimated.positive.tests), bg = alpha('darkred',0.7), shape = 21, size = 4)+
        scale_color_manual(labels=c("optimal test strategy (hypothetical)"),
                           values=c("optimal test strategy (hypothetical)"="darkred"))+
        scale_fill_manual(values = "azure3")+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.box="horizontal")+
        xlab("Date") +
        ylab("Positive Cases")+
        guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
        ggtitle("Case numbers under optimal test allocation for New York City")+
        theme(legend.title = element_blank(),legend.position="top",
              axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      ### plot 1.4 ###
      pr= data.frame('date' = break.ref,
                     'reported' = round(data$ReportedCases/data$Tests,3),
                     'findMoreCases' = round(result$positive.rate,3))
      p4=ggplot(data=pr,aes(x=date,y=reported, group=1))+
        geom_bar(aes(y=reported,fill='reported test positive rate'),stat="identity",position = 'identity',alpha=1, color= "gray30")+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref)+
        geom_line(aes(y=findMoreCases,color='optimal test strategy (hypothetical)'),size=1.5)+
        geom_point(aes(y=findMoreCases), bg = alpha('darkred',0.7), shape = 21, size = 4)+
        scale_color_manual(labels=c("optimal test strategy (hypothetical)"),
                           values=c("optimal test strategy (hypothetical)"="darkred"))+
        scale_fill_manual(values = "azure3")+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.box="horizontal")+
        xlab("Date")+
        ylab("Positive rate")+
        guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
        ggtitle("Test positive rate under optimal test allocation for New York City")+
        theme(legend.title = element_blank(),legend.position="top",
              axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      #A1 = gridExtra::grid.arrange(p3,p4,p1,p2,nrow=2)
      A1 = list(plot1 = p3, plot2 = p4, plot3 = p1, plot4 = p2)
      
      
    }else if(input$TABCHOSEN =='Simulations'){
      
      ta.list <- RESULTS$ta.list
      result <- RESULTS$result
      Test.result <- RESULTS$Test.result
      Assigned_Tests <- RESULTS$Assigned_Tests
      ### INPUTS ###
      N = 8175133 
      targetpos = as.numeric(input$targetpos2)
      true_cases= ifelse(input$w2 == 'Detecting Cases (w=1)', as.numeric(input$num2_w1) ,as.numeric(input$num2_w0))
      falseneg = as.numeric(input$falseneg2)
      falsepos = as.numeric(input$falsepos2) 
      weights = as.numeric(input$w2 == 'Detecting Cases (w=1)')
      
      ### plot 1.1 ###
      Test.result.symptom = data.frame('date' = ta.list,
                                       'asymptomatic' = Test.result[,'asymptomatics'],
                                       'mild' = Test.result[,'mild'],
                                       'severe' = Test.result[,'severe'])
      Test.symptom.long = reshape2::melt(Test.result.symptom,id.vars = "date")
      p1 = ggplot(data=Test.symptom.long,aes(x=factor(date),y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=ta.list, labels = ta.list)+
        ggtitle("Tests allocated to each symptom group")+
        ylab("Number of tests")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#9E0142','#F46D43',"#FEE08B"),
                          labels=c('severe', 'mild', 'asymptomatic'),
                          breaks=c('severe', 'mild', 'asymptomatic'))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      

      ### plot 1.3 ###
      T_rptCase_simuCase = data.frame('date'=ta.list,
                                      'estimated positive tests' = result$positive.tests/input$Total2)
      p2 = ggplot(data=T_rptCase_simuCase,aes(x=factor(date),group=1))+
        scale_x_discrete(breaks=ta.list, labels = ta.list)+
        geom_line(aes(y=estimated.positive.tests,color='optimal test strategy'),size=1.5)+
        geom_point(aes(y=estimated.positive.tests), bg = alpha('darkred',0.7), shape = 21, size = 4)+
        #scale_y_continuous()+
        scale_color_manual(labels=c("optimal test strategy"),
                           values=c("optimal test strategy"="darkred"))+
        scale_fill_manual(values = "azure3")+
        ggtitle("Test positive rate with optimal test allocation")+
        ylab("Positive rate")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      if(input$w2 == 'Detecting Cases (w=1)'){
        p2 = p2 + ylim(0,1)
      }else{
        p2 = p2 + geom_hline(yintercept = input$targetpos)#+
         #annotate(geom = 'label', x=1, y=input$targetpos, label = 'Target positive rate', hjust=0)
      }
      p2 = p2+
        theme(plot.margin=unit(c(40,5.5,5.5,5.5),"pt"))
 

      ### plot 1.1 ###
      Test.result.symptom.long = data.frame('date' = rep(ta.list,12),
                                       'value' = as.vector(as.matrix(Assigned_Tests)),
                                       'symptoms' = c(rep('severe symptoms', length(ta.list)*4),
                                                      rep('mild symptoms', length(ta.list)*4),
                                                      rep('asymptomatic', length(ta.list)*4)),
                                       'age' = c(rep(rep(c("age 0-17","age 18-49","age 50-64","age 65+"), each = length(ta.list)),3)))
     # Test.result.symptom.long = Test.result.symptom.long[Test.result.symptom.long$date %in% c(0.5, 0.6, 0.7, 0.8,0.9),]
      
      Test.result.symptom.long$symptoms = factor(Test.result.symptom.long$symptoms, levels = c('severe symptoms', 'mild symptoms', 'asymptomatic'))
      p3 = ggplot(data=Test.result.symptom.long,aes(x=factor(date),y=value,fill=age))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=ta.list, labels = ta.list)+
        ggtitle("Tests allocated to each symptom and age group ")+
        ylab("Number of tests")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
                          labels = c("age 0-17","age 18-49","age 50-64","age 65+"))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=14,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))+
        facet_grid(. ~ symptoms,shrink = TRUE, scales = "free", space = "free_x")+
        theme(strip.text.x = element_text(size = 14))

      LOWER = age_distribution(ta=0.55, ts = ((1-0.55)/4), D=true_cases, N=N)
      Test.age.long = rbind(data.frame(Symptoms = 'severe', 
                                       Age = c("age 0-17","age 18-49","age 50-64","age 65+"),
                                       Proportions = LOWER$age_given_severe),
                            data.frame(Symptoms = 'mild', 
                                       Age = c("age 0-17","age 18-49","age 50-64","age 65+"),
                                       Proportions = LOWER$age_given_mild),
                            data.frame(Symptoms = 'asymptomatic', 
                                       Age = c("age 0-17","age 18-49","age 50-64","age 65+"),
                                       Proportions = LOWER$age_given_asymp))
      p4 = ggplot(data=Test.age.long,aes(x=factor(Symptoms),y=Proportions,fill=Age))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30', width = 0.4)+
        scale_y_continuous()+
        scale_x_discrete(breaks=c('asymptomatic', 'mild', 'severe'), labels = c('asymptomatic', 'mild symptoms', 'severe symptoms'))+
        ggtitle("Population age distribution by symptoms")+
        ylab("Proportion of people")+
        xlab("Symptoms")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
                          labels = c("age 0-17","age 18-49","age 50-64","age 65+"))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))+
        theme(plot.margin=unit(c(40,5.5,5.5,5.5),"pt"))
      
      
      

     # LAYOUT = rbind(c(1,1,1,2,2,2),c(3,3,3,3,4,4))
     # A1 = gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2, layout_matrix = LAYOUT)
      
      
      #LAYOUT = rbind(c(1,1),c(2,3))
      #A1 = gridExtra::grid.arrange(p3,p2,p4,nrow=2, layout_matrix = LAYOUT)
      A1 = list(plot1 = p3, plot2 = p2, plot3 = p4)
      
    }else{
      ta.list <- RESULTS$ta.list
      result <- RESULTS$result
      ### INPUTS ###
      N = as.numeric(input$size3) 
      money = as.numeric(input$cost)
      y1 = min(as.numeric(input$y))
      y2 = max(as.numeric(input$y))
      falsenegt1 = max(as.numeric(input$falsenegt))
      falsenegt2 = min(as.numeric(input$falsenegt))
      falsepost1 = as.numeric(input$falsepost1)
      falsepost2 = as.numeric(input$falsepost2)
      agedist = input$ages
      ######################## plot 1.1 #######################
      Test.type = data.frame('ta' = result$ta,
                             'test1' = y1*result$test1,
                             'test2' = y2*result$test2)
      Total_Tests = result$test1+result$test2
      Test.type.long = reshape2::melt(Test.type,id.vars = "ta")
      p1 = ggplot(data=Test.type.long,aes(x=ta,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack')+
        geom_hline(yintercept = money)+
        scale_x_continuous(breaks = ta.list)+
        # annotate(geom='point',x=result$ta,y=Total_Tests,shape = 21, fill= 'white', colour = 'black', size = 5)+
        # annotate(geom = 'point', x=0.49, y=money*0.9, pch = 21, col = 'black', bg = 'white', size = 5)+
        # annotate(geom = 'text', x=0.50, y=money*0.9, label = 'total number of tests', hjust=0)+
        # annotate(geom = 'text', x=0.90, y=Total_Tests[length(result$ta)]+(money/10), label =Total_Tests[length(result$ta)], hjust=0.5)+
        ggtitle('Budget Allocation Between Tests')+
        ylab("Total Budget")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(breaks = c('test1', 'test2'),
                          labels=c("Test 1 (e.g. rapid antigen)", "Test 2 (e.g. RT-PCR)"),
                          values=c('#ABDDA4',"#5E4FA2"))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+#, axis.text.y.right = element_text(color = "darkred"))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      
      
      ######################## plot 1.2 #######################
      Test.type = data.frame('ta' = result$ta,
                             'test1' = result$test1,
                             'test2' = result$test2)
      Test.type.long = reshape2::melt(Test.type,id.vars = "ta")
      p2 = ggplot(data=Test.type.long,aes(x=ta,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack')+
        scale_x_continuous(breaks = ta.list)+
        scale_y_continuous()+
        ggtitle('Number of Allocated Tests')+
        ylab("Total Tests")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(breaks = c('test1', 'test2'),
                          labels=c("Test 1 (e.g. rapid antigen)", "Test 2 (e.g. RT-PCR)"),
                          values=c('#ABDDA4',"#5E4FA2"))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
 
      Test.type = data.frame(ta = result$ta,
                                  positive_tests = result$positive,
                                  underreporting = round(as.numeric(input$num3/result$positive),2))
      Test.type$underreporting_trans = (Test.type$underreporting/max(Test.type$underreporting))*input$num3
      p5 = ggplot(data=Test.type,aes(x=ta, group = 1))+
        geom_bar(aes(y=positive_tests, fill='positive_tests'),stat="identity",position = 'stack')+
        geom_hline(yintercept = input$num3)+
        geom_line(aes(y=underreporting_trans,color='under-reporting factor'),size=1.5)+
        geom_point(aes(y=underreporting_trans, shape = 'under-reporting factor'), bg = alpha('darkred',0.7), size = 4)+
        scale_x_continuous(breaks = ta.list)+
        scale_y_continuous(sec.axis = sec_axis( trans=~.*1, name="Under-reporting Factor", 
                                                breaks =c(0,input$num3/max(Test.type$underreporting),input$num3), 
                                                labels = c(0,1,max(Test.type$underreporting))))+
        ggtitle('Number of Positive Tests')+
        ylab("Total Positive Tests")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""), color=guide_legend(title=""), shape=guide_legend(title=""))+
        scale_fill_manual(breaks = c('positive_tests'),
                          labels=c("positive tests"),
                          values=c('#3288BD'))+
        scale_shape_manual(breaks = c('under-reporting factor'),
                          labels=c("under-reporting factor"),
                          values=21)+
        scale_color_manual(labels=c("under-reporting factor"),
                           values=c("under-reporting factor"="darkred"))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      
      # p4=ggplot(data=pr,aes(x=date,group=1))+
      #   geom_bar(aes(y=reported,fill='reported test positive rate'),stat="identity",position = 'identity',alpha=1, color= "gray30")+
      #   scale_y_continuous()+
      #   scale_x_discrete(breaks=break.ref)+
      #   geom_line(aes(y=findMoreCases,color='optimal test strategy'),size=1.5)+
      #   geom_point(aes(y=findMoreCases), bg = alpha('darkred',0.7), shape = 21, size = 4)+
      #   scale_color_manual(labels=c("optimal test strategy"),
      #                      values=c("optimal test strategy"="darkred"))+
      #   scale_fill_manual(values = "azure3")+
      #   ggtitle("Test positive rate with optimal test allocation in New York City")+
      #   ylab("Positive rate")+
      #   xlab("Date")+
      #   guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
      #   theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),legend.position="top",
      #         legend.text=element_text(size=14), text = element_text(size=14))+
      #   theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      #         panel.background = element_rect(fill = 'gray95', colour = 'white'))
      # 
      
      
      
      #66C2A5','#3288BD
      
      ######################## plot 1.3 #######################
      Test1 = data.frame('ta' = result$ta,
                         'asymptomatic' = result$test1_a,
                         'mild' = result$test1_m,
                         'severe' = result$test1_s)
      Test1.long = reshape2::melt(Test1, id.vars='ta')
      
      p3 = ggplot(data=Test1.long,aes(x=ta,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack')+
        scale_x_continuous(breaks = ta.list)+
        scale_y_continuous()+
        ggtitle("Test 1 (e.g. rapid antigen) allocation by symptoms")+
        ylab("Number of tests")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#9E0142','#F46D43',"#FEE08B"),
                          labels=c('severe', 'mild', 'asymptomatic'),
                          breaks=c('severe', 'mild', 'asymptomatic'))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      
      ######################## plot 1.4 #######################
      Test2 = data.frame('ta' = result$ta,
                         'asymptomatic' = result$test2_a,
                         'mild' = result$test2_m,
                         'severe' = result$test2_s)
      Test2.long = reshape2::melt(Test2, id.vars='ta')
      
      p4 = ggplot(data=Test2.long,aes(x=ta,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack')+
        scale_x_continuous(breaks = ta.list)+
        scale_y_continuous()+
        ggtitle("Test 2 (e.g. RT-PCR) allocation by symptoms")+
        ylab("Number of tests")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#9E0142','#F46D43',"#FEE08B"),
                          labels=c('severe', 'mild', 'asymptomatic'),
                          breaks=c('severe', 'mild', 'asymptomatic'))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      #A1 = gridExtra::grid.arrange(p1,p5,p3,p4,nrow=2,ncol=2)
      A1 = list(plot1 = p1, plot2 = p5, plot3 = p3, plot4 = p4)
    }
    return(A1)
  }
  
  
  
  
  RESULTS <- reactiveValues()
  PLOTS <- reactiveValues()
  shiny::observe({
    if(input$TABCHOSEN == 'Historical'){
      temp_func <- shiny::reactive({Optimizer_panel1(input)})
      temp = temp_func()
      RESULTS$break.ref <- temp$break.ref
      RESULTS$result <- temp$result
      RESULTS$Test.result <- temp$Test.result
      temp_func <- shiny::reactive({GetPlots(input,RESULTS)})
      PLOTS = temp_func()
      output[['plot1']] <- shiny::renderPlot({PLOTS[[1]]}, height = 250, width = 500)
      output[['plot2']] <- shiny::renderPlot({PLOTS[[2]]}, height = 250, width = 500)
      output[['plot3']] <- shiny::renderPlot({PLOTS[[3]]}, height = 250, width = 500)
      output[['plot4']] <- shiny::renderPlot({PLOTS[[4]]}, height = 250, width = 500)
      output[['plotmerge']] <- shiny::renderPlot({gridExtra::grid.arrange(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]], nrow = 2)}, height = 600, width = 1100)
      
    }else if(input$TABCHOSEN == 'Simulations'){
      temp_func <- shiny::reactive({Optimizer_panel2(input)})
      temp = temp_func()
      RESULTS$ta.list <- temp$ta.list
      RESULTS$result <- temp$result
      RESULTS$Test.result <- temp$Test.result
      RESULTS$Assigned_Tests <- temp$Assigned_Tests
      temp_func <- shiny::reactive({GetPlots(input,RESULTS)})
      PLOTS = temp_func()
      output[['plot1']] <- shiny::renderPlot({PLOTS[[1]]}, height = 300, width = 1100)
      output[['plot2']] <- shiny::renderPlot({PLOTS[[2]]}, height = 300, width = 520)
      output[['plot3']] <- shiny::renderPlot({PLOTS[[3]]}, height = 300, width = 520)
      output[['plot4']] <- NULL
      LAYOUT = rbind(c(1,1),c(2,3))
      output[['plotmerge']] <- shiny::renderPlot({gridExtra::grid.arrange(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]], nrow = 2,  layout_matrix = LAYOUT)}, height = 600, width = 1100)
      
    }else{
      temp_func <- shiny::reactive({Optimizer_panel3(input)})
      temp = temp_func()
      RESULTS$ta.list <- temp$ta.list
      RESULTS$result <- temp$result
      temp_func <- shiny::reactive({GetPlots(input,RESULTS)})
      PLOTS = temp_func()
      output[['plot1']] <- shiny::renderPlot({PLOTS[[1]]}, height = 300, width = 520)
      output[['plot2']] <- shiny::renderPlot({PLOTS[[2]]}, height = 300, width = 520)
      output[['plot3']] <- shiny::renderPlot({PLOTS[[3]]}, height = 300, width = 520)
      output[['plot4']] <- shiny::renderPlot({PLOTS[[4]]}, height = 300, width = 520)
      output[['plotmerge']] <- shiny::renderPlot({gridExtra::grid.arrange(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]], nrow = 2)}, height = 600, width = 1100)

    }
  })
  
  

  #output$plots = shiny::renderPlot({  GetPlots(input,RESULTS)}, height = 600, width = 1100)

  
  
}#end of server


shiny::shinyApp(ui, server)

