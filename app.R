

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
      shiny::fluidRow(
        shiny::column(2,
                      shiny::img(src="2color-bluebg.png",height=100,width=100)
        ),
        shiny::column(1
        ),
        shiny::column(9,
                      print("This application calculates optimal SARS-CoV-2 viral test allocation by age and symptom severity given various input parameters. Users can (1) explore how this optimal strategy differs from historical testing in New York City, (2) explore test allocation for a hypothetical population similar to New York, and (3) explore allocation of two available tests at the local level (e.g. rapid vs. RT-PCR test allocation for a university)   \n")
        ),
        shiny::hr(),
        shiny::tabsetPanel(
          ### PATIENT CHARACTERISTICS
          shiny::tabPanel('Historical New York Data',
                          shiny::br(),
                          shiny::column(6,
                                        shiny::selectInput("w", "Goal of testing:", choices=c('Detecting Cases (w=1)','Outbreak Surveillance (w=0)')),
                                        shiny::sliderInput("falseneg", "False negative probability:", value= 0.3, min = 0.01, max = 0.4, step = 0.01),
                                        shiny::sliderInput("ta", "Proportion of cases w/o symptoms:", value= 0.75, min =0.30, max = 0.95, step = 0.01)
                          ),
                          shiny::column(6,
                                        shiny::sliderInput("falsepos", "False positive probability:", value= 0.01, min = 0.01, max = 0.05, step = 0.01),
                                        shiny::br(),
                                        shiny::sliderInput("mult", "Case under-reporting factor:", value= 5, min = 1, max = 20, step = 1),
                                        conditionalPanel(condition = "input.w=='Outbreak Surveillance (w=0)'",
                                        shiny::sliderInput("targetpos", "Target positivity rate:", value= 0.03, min =0.01, max = 0.15, step = 0.01))
                                        
                          ),
                          value = 'Historical'
          ),#break tabPanel
          shiny::tabPanel('Hypothetical Population like New York',
                          shiny::br(),
                          shiny::column(6,
                                        shiny::selectInput("w2", "Goal of testing:", choices=c('Detecting Cases (w=1)','Outbreak Surveillance (w=0)')),
                                        shiny::sliderInput("Total2", "Number of Available Tests (Weekly):", value= 50000, min =25000, max = 200000, step = 5000),
                                        shiny::sliderInput("falseneg2", "False negative probability:", value= 0.3, min = 0.01, max = 0.4, step = 0.01)),
                          shiny::column(6,
                                        shiny::sliderInput("falsepos2", "False positive probability:", value= 0.01, min = 0.01, max = 0.05, step = 0.01),
                                        
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
                                        shiny::sliderInput("y", "Cost per test (test 1, test 2):", value= c(5,200), min =0, max = 300, step = 1),
                                        shiny::sliderInput("size3", "Total population size:", value= 50000, min = 5000, max = 100000, step = 5000),
                                        shiny::sliderInput("num3", "Number of cases in population:", value= 500, min = 5, max = 5000, step = 10)
                          ),  
                          shiny::column(6,
                                        shiny::sliderInput("falsenegt", "False negative rate (test2, test 1):", value= c(0.20,0.70), min = 0.01, max = 0.9, step = 0.01),
                                        shiny::sliderInput("falsepost1", "False positive rate (test 1):", value= 0.01, min = 0.01, max = 0.05, step = 0.01),
                                        shiny::sliderInput("falsepost2", "False positive rate (test 2):", value= 0.01, min = 0.01, max = 0.05, step = 0.01)
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
        shiny::column(6,
                      shiny::plotOutput("plots", inline = TRUE),
                      conditionalPanel( condition = "input.TABCHOSEN == 'Historical'", shiny::img(src="Objective1.png",height=200,width=900)),
                      conditionalPanel( condition = "input.TABCHOSEN == 'Simulations'", shiny::img(src="Objective2.png",height=200,width=900)), 
                      conditionalPanel( condition = "input.TABCHOSEN == 'Two_Tests'", shiny::img(src="Objective3.png",height=200,width=900)))
      ),
      shiny::br()
      , width = 6)	    
    
  )#end sidebarlayout 
  
  
)#end fluidpage






source(file = 'www/find_t_f_V2_NY.R')
source(file = 'www/each_generation_withZ_comb_V2.R')
source(file = 'www/each_generation_withZ_comb_twoTests.R')
library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)



#library('extraDistr')
#library('dplyr')
#library('nleqslv')
#library('optimx')
#library('reshape2')
#ibrary('scales')
#library('ggthemes')
#library('ggpubr')
#library('gridExtra')
#library('lpSolve')
#library('scales')
#library('NlcOptim')




### GLOBAL VARIABLES ###
#library (readr)
testdata.url = 'https://raw.githubusercontent.com/nychealth/coronavirus-data/master/tests.csv'
testdata = readr::read_csv(url(testdata.url))
ndays = nrow(testdata)
G = ceiling(ndays/7)



server = function(input, output, session ) {
  
  
  GetPlots <- function(input){
    shiny::validate(
      need(sum(input$ages>=0 & input$ages <=1)==4 & sum(input$ages)==1, 'age proportion must be in [0,1] and proportions must sum to 1 across age categories')
    )    
    shiny::validate(
      need(input$y[1]<input$y[2], 'cost for test 1 should be less than test 2')
    )  
    
    shiny::validate(
      need(input$falsenegt[1]<input$falsenegt[2], 'false negative rate for test 2 should be less than for test 1')
    ) 
    options(scipen=999)
    if(input$TABCHOSEN == 'Historical'){
      N = 8175133 
      weights = rep(ifelse(input$w == 'Detecting Cases (w=1)',1,0),G) #1
      ta = as.numeric(input$ta)
      ts = (1-ta)/4
      targetpos = as.numeric(input$targetpos)
      mult= as.numeric(input$mult) #5 
      falseneg = as.numeric(input$falseneg)
      falsepos = as.numeric(input$falsepos)
    }else if(input$TABCHOSEN == 'Simulations'){
      N = 8175133 
      ta = seq(0.5, 0.9, 0.05)
      targetpos = as.numeric(input$targetpos2)
      true_cases= ifelse(input$w2 == 'Detecting Cases (w=1)', as.numeric(input$num2_w1) ,as.numeric(input$num2_w0))
      severe_cases= 5000
      falseneg = as.numeric(input$falseneg2)
      falsepos = as.numeric(input$falsepos2) 
      weights = as.numeric(input$w2 == 'Detecting Cases (w=1)')
    }else{
      N = as.numeric(input$size3) 
      weights = 1
      money = as.numeric(input$cost)
      y1 = min(as.numeric(input$y))
      y2 = max(as.numeric(input$y))
      falsenegt1 = max(as.numeric(input$falsenegt))
      falsenegt2 = min(as.numeric(input$falsenegt))
      falsepost1 = as.numeric(input$falsepost1)
      falsepost2 = as.numeric(input$falsepost2)
      agedist = input$ages
    }
    
    
    if(input$TABCHOSEN == 'Historical'){
      data = as.data.frame(matrix(NA, nrow = G, ncol = 3))
      colnames(data) = c("Generation","Tests","ReportedCases")
      # The first generation
      data[1,1] = 1
      data[1,2] = sum(testdata$TOTAL_TESTS[1:7])
      data[1,3] = sum(testdata$POSITIVE_TESTS[1:7])
      NY_date = testdata$DATE[1]
      # generation 2 to G-1
      for (g in 2:(G-1)){
        data[g,1] = g
        start = (g-1)*7+1
        end = g*7
        data[g,2] = sum(testdata$TOTAL_TESTS[start:end])
        data[g,3] = sum(testdata$POSITIVE_TESTS[start:end])
        NY_date = c(NY_date,testdata$DATE[start])
      }
      # the final generation
      data[G,1] = G
      start = end+1
      end = nrow(testdata)
      data[G,2] = sum(testdata$TOTAL_TESTS[start:end])
      data[G,3] = sum(testdata$POSITIVE_TESTS[start:end])
      NY_date = c(NY_date,testdata$DATE[start])
      ########################## Find the optimal test allocation strategy #################
      result = matrix(NA,nrow = G, ncol = 10)
      colnames(result) = c("date","positive.rate","severe","mild","asymptomatics",
                           "age1","age2","age3","age4","positive.tests")
      Test.result = matrix(NA,nrow = G,ncol = 8)
      colnames(Test.result) = c("date","asymptomatics","mild","severe","age1","age2","age3","age4")
      for(i in 1:nrow(result)){
        result[i,1] = i
        model = each_generation_withZ(N = N,
                                      D = data$ReportedCases[i]*mult,
                                      ta = ta,
                                      ts = ts,
                                      Total=ifelse(input$TABCHOSEN !='Historical', input$Total2, data$Tests[i]),
                                      beta=falseneg,
                                      alpha=falsepos,
                                      c_pos=targetpos,
                                      w=weights[i])
        result[i,2] = floor(model$P.hat)/ifelse(input$TABCHOSEN !='Historical', input$Total2, data$Tests[i])
        result[i,3] = model$selectProb.s
        result[i,4] = model$selectProb.m
        result[i,5] = model$selectProb.a
        result[i,6] = model$selectProb.age1
        result[i,7] = model$selectProb.age2
        result[i,8] = model$selectProb.age3
        result[i,9] = model$selectProb.age4
        result[i,10] = model$P.hat
        Test.result[i,1] = i
        Test.result[i,2] = floor(sum(model$asignedTests[9:12]))
        Test.result[i,3] = floor(sum(model$asignedTests[5:8]))
        Test.result[i,4] = floor(sum(model$asignedTests[1:4]))
        Test.result[i,5] = floor(model$asignedTests[1]+model$asignedTests[5]+model$asignedTests[9])
        Test.result[i,6] = floor(model$asignedTests[2]+model$asignedTests[6]+model$asignedTests[10])
        Test.result[i,7] = floor(model$asignedTests[3]+model$asignedTests[7]+model$asignedTests[11])
        Test.result[i,8] = floor(model$asignedTests[4]+model$asignedTests[8]+model$asignedTests[12])
      }
      result = as.data.frame(result)
      break.ref = as.character(NY_date)
      break.ref = gsub('/','-',break.ref)
      break.ref = gsub('-2020','',break.ref)
      result[result<0] = 0
    }else if(input$TABCHOSEN == 'Simulations' ){
      ta.list = seq(0.5,0.9,by=0.05)
      result = matrix(NA,nrow = length(ta.list), ncol = 10)
      colnames(result) = c("date","positive.rate","severe","mild","asymptomatics",
                           "age1","age2","age3","age4","positive.tests")
      Test.result = matrix(NA,nrow = length(ta.list),ncol = 8)
      colnames(Test.result) = c("date","asymptomatics","mild","severe","age1","age2","age3","age4")
      for(i in 1:nrow(result)){
        result[i,1] = i
        ### Fixed D. Note: D = (4/(1-ta))*number of severe cases
        model = each_generation_withZ(N = N,
                                      D = true_cases,
                                      ta = ta.list[i],
                                      ts = (1-ta.list[i])/4,
                                      Total=input$Total2,
                                      beta=falseneg,
                                      alpha=falsepos,
                                      c_pos=targetpos,
                                      w=weights)
        
        ### Fixed number of severe cases
        # tf = find_t_f(ta=ta.list[i],
        #               ts=(1-ta.list[i])/4)
        # model = each_generation_withZ(N = N,
        #                               D = as.numeric(4*severe_cases/(1 - tf$ta)),
        #                               ta = ta.list[i],
        #                               ts = (1-ta.list[i])/4,
        #                               Total=input$Total2,
        #                               beta=falseneg,
        #                               alpha=falsepos,
        #                               c_pos=targetpos,
        #                               w=weights)

        result[i,2] = floor(model$P.hat)/input$Total2
        result[i,3] = model$selectProb.s
        result[i,4] = model$selectProb.m
        result[i,5] = model$selectProb.a
        result[i,6] = model$selectProb.age1
        result[i,7] = model$selectProb.age2
        result[i,8] = model$selectProb.age3
        result[i,9] = model$selectProb.age4
        result[i,10] = model$P.hat
        Test.result[i,1] = i
        Test.result[i,2] = floor(sum(model$asignedTests[9:12]))
        Test.result[i,3] = floor(sum(model$asignedTests[5:8]))
        Test.result[i,4] = floor(sum(model$asignedTests[1:4]))
        Test.result[i,5] = floor(model$asignedTests[1]+model$asignedTests[5]+model$asignedTests[9])
        Test.result[i,6] = floor(model$asignedTests[2]+model$asignedTests[6]+model$asignedTests[10])
        Test.result[i,7] = floor(model$asignedTests[3]+model$asignedTests[7]+model$asignedTests[11])
        Test.result[i,8] = floor(model$asignedTests[4]+model$asignedTests[8]+model$asignedTests[12])
      }
      result = as.data.frame(result)
      result[result<0] = 0
    }else{
      ta.list = seq(0.5,0.9,by=0.05)
      result = matrix(NA,nrow = length(ta.list), ncol = 11)
      result = data.frame(result)
      names(result) = c("ta","positive cases","positive.rate","test1","test2","test1_s","test1_m","test1_a","test2_s","test2_m","test2_a")
      for(i in 1:nrow(result)){
        result[i,'ta'] = ta.list[i]
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
        result[i,'positive'] = floor(model$P.hat)
        result[i,'positive.rate'] = floor(model$P.hat)/floor(model$Total)
        
        result[i,'test1'] = floor(sum(model$asignedTests[1:12])) # Test 1
        result[i,'test2'] = floor(sum(model$asignedTests[13:24])) # Test 2

        result[i,'test1_s'] = floor(sum(model$asignedTests[1:4])) # Test 1, symp=s
        result[i,'test1_m'] = floor(sum(model$asignedTests[5:8])) # Test 1, symp=m
        result[i,'test1_a'] = floor(sum(model$asignedTests[9:12])) # Test 1, symp=a

        result[i,'test2_s'] = floor(sum(model$asignedTests[13:16])) # Test 2, symp=s
        result[i,'test2_m'] = floor(sum(model$asignedTests[17:20])) # Test 2, symp=m
        result[i,'test2_a'] = floor(sum(model$asignedTests[21:24])) # Test 2, symp=a
        result[result<0] = 0
      }
      result = as.data.frame(result)
    }

    ####################
    ### PLOT RESULTS ###
    ####################
    
 
    if(input$TABCHOSEN =='Historical'){
      T_rptCase_simuCase = data.frame('date'=break.ref,
                                      'reported cases' = data$ReportedCases,
                                      'estimated positive tests' = result$positive.tests)
      
      
      ### plot 1.1 ###
      Test.result.symptom = data.frame('date' = break.ref,
                                       'asymptomatic' = Test.result[,2],
                                       'mild' = Test.result[,3],
                                       'severe' = Test.result[,4])
      Test.symptom.long = reshape2::melt(Test.result.symptom,id.vars = "date")
      p1 = ggplot(data=Test.symptom.long,aes(x=date,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref)+
        ggtitle("Tests allocated to each symptom group in New York City")+
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
        annotate(geom = 'text', x=1.5, y=max(data$Tests), label = 'number of tests available', hjust=0)
      
      ### plot 1.2 ###
      Test.result.age = data.frame('date' = break.ref,
                                   'age1' = Test.result[,5],
                                   'age2' = Test.result[,6],
                                   'age3' = Test.result[,7],
                                   'age4' = Test.result[,8])
      Test.age.long = reshape2::melt(Test.result.age,id.vars = "date")
      
      p2 = ggplot(data=Test.age.long,aes(x=date,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref)+
        ggtitle("Tests allocated to each age group in New York City")+
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
        annotate(geom = 'text', x=1.5, y=max(data$Tests), label = 'number of tests available', hjust=0)
      
      
      
      ### plot 1.3 ###
      
      p3 = ggplot(data=T_rptCase_simuCase,aes(x=date,y=reported.cases,group=1))+
        geom_bar(aes(y=reported.cases,fill='reported cases'),stat="identity",position = 'identity',alpha=1, color= "gray30")+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref)+
        geom_line(aes(y=estimated.positive.tests,color='optimal test strategy'),size=1.5)+
        geom_point(aes(y=estimated.positive.tests), bg = alpha('darkred',0.7), shape = 21, size = 4)+
        scale_color_manual(labels=c("optimal test strategy"),
                           values=c("optimal test strategy"="darkred"))+
        scale_fill_manual(values = "azure3")+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.box="horizontal")+
        xlab("Date") +
        ylab("Positive Cases")+
        guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
        ggtitle("Number of positive cases with optimal test allocation in New York City")+
        theme(legend.title = element_blank(),legend.position="top",
              axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      ### plot 1.4 ###
      pr= data.frame('date' = break.ref,
                     'reported' = round(data$ReportedCases/data$Tests,3),
                     'findMoreCases' = round(result$positive.rate,3))
      p4=ggplot(data=pr,aes(x=date,group=1))+
        geom_bar(aes(y=reported,fill='reported test positive rate'),stat="identity",position = 'identity',alpha=1, color= "gray30")+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref)+
        geom_line(aes(y=findMoreCases,color='optimal test strategy'),size=1.5)+
        geom_point(aes(y=findMoreCases), bg = alpha('darkred',0.7), shape = 21, size = 4)+
        scale_color_manual(labels=c("optimal test strategy"),
                           values=c("optimal test strategy"="darkred"))+
        scale_fill_manual(values = "azure3")+
        ggtitle("Test positive rate with optimal test allocation in New York City")+
        ylab("Positive rate")+
        xlab("Date")+
        guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
        theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      A1 = gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)
      
      
    }else if(input$TABCHOSEN =='Simulations'){
      


      ### plot 1.1 ###
      Test.result.symptom = data.frame('date' = ta.list,
                                       'asymptomatic' = Test.result[,2],
                                       'mild' = Test.result[,3],
                                       'severe' = Test.result[,4])
      Test.symptom.long = reshape2::melt(Test.result.symptom,id.vars = "date")
      p1 = ggplot(data=Test.symptom.long,aes(x=factor(date),y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=ta.list, labels = ta.list)+
        ggtitle("Tests allocated to each symptom group")+
        ylab("Number of tests")+
        xlab("Probability of having no symptoms for an infected individual)")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#9E0142','#F46D43',"#FEE08B"),
                          labels=c('severe', 'mild', 'asymptomatic'),
                          breaks=c('severe', 'mild', 'asymptomatic'))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      ### plot 1.2 ###
      Test.result.age = data.frame('date' = ta.list,
                                   'age1' = Test.result[,5],
                                   'age2' = Test.result[,6],
                                   'age3' = Test.result[,7],
                                   'age4' = Test.result[,8])
      Test.age.long = reshape2::melt(Test.result.age,id.vars = "date")
      
      p2 = ggplot(data=Test.age.long,aes(x=factor(date),y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=ta.list, labels = ta.list)+
        ggtitle("Tests allocated to each age group")+
        ylab("Number of tests")+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
                          labels = c("age 0-17","age 18-49","age 50-64","age 65+"))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      T_rptCase_simuCase = data.frame('date'=ta.list,
                                      'estimated positive tests' = result$positive.tests/input$Total2)
      
      ### plot 1.3 ###
      p3 = ggplot(data=T_rptCase_simuCase,aes(x=factor(date),group=1))+
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
        p3 = p3 + ylim(0,1)
      }else{
        p3 = p3 + geom_hline(yintercept = input$targetpos)
      }

      
      LOWER = age_distribution(ta=0.55, ts = ((1-0.55)/4), D=true_cases)
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
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=c('asymptomatic', 'mild', 'severe'), labels = c('asymptomatic', 'mild', 'severe'))+
        ggtitle("Population age distribution by symptoms")+
        ylab("Proportion of people")+
        xlab("Symptoms")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
                          labels = c("age 0-17","age 18-49","age 50-64","age 65+"))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))
      
      
      
      
      A1 = gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)
      
    }else{
      
      ######################## plot 1.1 #######################
      Test.type = data.frame('ta' = result$ta,
                             'test1' = y1*result$test1,
                             'test2' = y2*result$test2)
      Test.type.long = reshape2::melt(Test.type,id.vars = "ta")
      p1 = ggplot(data=Test.type.long,aes(x=ta,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack')+
        scale_x_continuous(breaks = ta.list)+
        scale_y_continuous()+
        ggtitle('Budget Allocation Between Tests')+
        ylab("Total Budget")+
        geom_hline(yintercept = money)+
        xlab("Probability of having no symptoms for an infected individual")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(breaks = c('test1', 'test2'),
                          labels=c("Test 1 (e.g. rapid antigen)", "Test 2 (e.g. RT-PCR)"),
                          values=c('#ABDDA4',"#5E4FA2"))+
        theme(axis.text.x = element_text(angle = 0,hjust=0.5, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
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
      
      A1 = gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
      
    }
    return(A1)
  }
  
  
  output$plots = shiny::renderPlot({  GetPlots(input)}, height = 600, width = 1100)
}#end of server


shiny::shinyApp(ui, server)

