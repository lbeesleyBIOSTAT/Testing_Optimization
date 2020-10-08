

STARTING_WEIGHTS = matrix(c(0,1,0.3,0),nrow=1,ncol = 4)
rownames(STARTING_WEIGHTS) = 'w'
colnames(STARTING_WEIGHTS) = c('Weeks 1-2', 'Weeks 3-8', 'Weeks 9-13', 'Weeks 14-31')


ui<- shiny::fluidPage(
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
                      print("This application calculates optimal SARS-CoV-2 viral test allocation by age and symptoms severity given various input parameters. Users can explore how this optimal strategy differs from historical testing in New York City and or explore test allocation for a hypothetical infection wave in a population similar to New York.  \n")
        ),
      shiny::hr(),
      shiny::tabsetPanel(
        ### PATIENT CHARACTERISTICS
        shiny::tabPanel('Historical Data',
                        shiny::br(),
                        shiny::column(6,
                            shiny::sliderInput("w", "Shrinkage weight (w):", value= 1, min = 0, max = 1, step = 0.1),
                            shiny::sliderInput("falseneg", "False negative probability:", value= 0.3, min = 0.01, max = 0.4, step = 0.01),
                            shiny::sliderInput("ta", "Proportion of cases w/o symptoms:", value= 0.75, min =0.30, max = 0.95, step = 0.01)
                        ),
                        shiny::column(6,
                                      shiny::sliderInput("targetpos", "Target positivity rate (w<1 only):", value= 0.02, min =0.01, max = 0.15, step = 0.01),
                                      shiny::sliderInput("falsepos", "False positive probability:", value= 0.01, min = 0.01, max = 0.05, step = 0.01),
                                      shiny::br(),
                                      shiny::sliderInput("mult", "Case under-reporting factor:", value= 5, min = 1, max = 20, step = 1)
                        ),
                        value = 'Historical'
        ),#end tabPanel
        shiny::tabPanel('Hypothetical Scenario',
                        shiny::br(),
                        shiny::column(6,
                                      #shiny::sliderInput("w2", "Shrinkage weight (w):", value= 1, min = 0, max = 1, step = 0.1),
                                      shiny::sliderInput("Total2", "Number of Available Tests (Weekly):", value= 50000, min =25000, max = 200000, step = 5000),
                                      shiny::sliderInput("falseneg2", "False negative probability:", value= 0.3, min = 0.01, max = 0.4, step = 0.01),
                                      shiny::sliderInput("ta2", "Proportion of cases w/o symptoms:", value= 0.75, min =0.55, max = 0.95, step = 0.01)),  
                        shiny::column(6,
                                      shiny::sliderInput("targetpos2", "Target positivity rate (w<1 only):", value= 0.02, min =0.01, max = 0.15, step = 0.01),
                                      shiny::sliderInput("falsepos2", "False positive probability:", value= 0.01, min = 0.01, max = 0.05, step = 0.01),
                                      shiny::sliderInput("num2", "Peak number of cases in population:", value= 200000, min = 10000, max = 500000, step = 10000)
                        ),
                        shiny::fluidRow(
                          shiny::column(12,
                                      shinyMatrix::matrixInput("weights",label = "Enter objective function w for each time interval (0-1):",
                                                               rows=list(names = TRUE,editableNames = FALSE), cols = list(names = TRUE,editableNames = FALSE),
                                                               value = STARTING_WEIGHTS, class = "numeric"))),
                        value = 'Forecasting'
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
                      shiny::img(src="ObjectiveFunction.png",height=200,width=900)), 
      ),
      shiny::br()
      , width = 6)	    
    
  )#end sidebarlayout 
  
  
)#end fluidpage






source(file = 'www/find_t_f_V2_NY.R')
source(file = 'www/each_generation_withZ_comb_V2.R')
library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)
library(purrr)



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
      need(sum(input$weights>=0 & input$weights <=1)==4, 'w must be in [0,1]')
    )
    
    
    options(scipen=999)
    N = 8175133 
    if(input$TABCHOSEN == 'Historical'){
    weights = rep(as.numeric(input$w),31) #1
    ta = as.numeric(input$ta)
    ts = (1-ta)/5
    targetpos = as.numeric(input$targetpos)
    mult= as.numeric(input$mult) #5 
    falseneg = as.numeric(input$falseneg)
    falsepos = as.numeric(input$falsepos)
    }else{
      #w = as.numeric(input$w2) #1
      ta = as.numeric(input$ta2)
      ts = (1-ta)/5
      targetpos = as.numeric(input$targetpos2)
      mult= as.numeric(input$num2)/38537 #5 
      falseneg = as.numeric(input$falseneg2)
      falsepos = as.numeric(input$falsepos2) 
      weights_short = as.numeric(as.vector(input$weights))
      weights = c(rep(weights_short[1],2), rep(weights_short[2],6), rep(weights_short[3],5), rep(weights_short[4],18))
    }


    #data.frame(ta = ta, tm = 1-ta-ts, ts = ts)
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
      xlab(" ")+
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
      annotate(geom = 'point', x=c(1:31), y=data$Tests, pch = 21, col = 'black', bg = 'white', size = 2)+
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
      xlab("")+
      guides(fill=guide_legend(title=""))+
      scale_fill_manual(values=c('#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
                        labels = c("age 0-17","age 18-49","age 50-64","age 65+"))+
      #ggthemes::theme_economist()+
      theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1), legend.position="top",
            legend.text=element_text(size=14), text = element_text(size=14))+
      theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = 'gray95', colour = 'white'))+
      annotate(geom = 'point', x=c(1:31), y=data$Tests, pch = 21, col = 'black', bg = 'white', size = 2)+
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
      xlab("") +
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
      xlab("")+
      guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
      theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),legend.position="top",
            legend.text=element_text(size=14), text = element_text(size=14))+
      theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = 'gray95', colour = 'white'))
    
    A1 = gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)
    
    
    }else{

      
      T_rptCase_simuCase = data.frame('date'=break.ref,
                                      'reported cases' = (as.numeric(as.character(data$ReportedCases))*mult),
                                      'estimated positive tests' = as.numeric(as.character(result$positive.tests)))

      ### plot 1.1 ###
      Test.result.symptom = data.frame('date' = break.ref,
                                       'asymptomatic' = Test.result[,2],
                                       'mild' = Test.result[,3],
                                       'severe' = Test.result[,4])
      Test.symptom.long = reshape2::melt(Test.result.symptom,id.vars = "date")
      p1 = ggplot(data=Test.symptom.long,aes(x=date,y=value,fill=variable))+
        geom_bar(stat="identity",position = 'stack', colour = 'gray30')+
        scale_y_continuous()+
        scale_x_discrete(breaks=break.ref, labels = paste0('wk ', c(1:length(break.ref))))+
        ggtitle("Tests allocated to each symptom group")+
        ylab("Number of tests")+
        xlab(" ")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#9E0142','#F46D43',"#FEE08B"),
                          labels=c('severe', 'mild', 'asymptomatic'),
                          breaks=c('severe', 'mild', 'asymptomatic'))+
        theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))

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
        scale_x_discrete(breaks=break.ref, labels = paste0('wk ', c(1:length(break.ref))))+
        ggtitle("Tests allocated to each age group")+
        ylab("Number of tests")+
        xlab("")+
        guides(fill=guide_legend(title=""))+
        scale_fill_manual(values=c('#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
                          labels = c("age 0-17","age 18-49","age 50-64","age 65+"))+
        theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))

      
      ### plot 1.3 ###
      MAXIMUM = max(c(as.numeric(as.character(T_rptCase_simuCase$reported.cases)),as.numeric(as.character(T_rptCase_simuCase$estimated.positive.tests))), na.rm=T)
      p3 = ggplot(data=T_rptCase_simuCase,aes(x=date,group=1))+
        geom_bar(aes(y=reported.cases,fill='population cases'),stat="identity",position = 'identity',alpha=1, color= "gray30")+
        scale_x_discrete(breaks=break.ref, labels = paste0('wk ', c(1:length(break.ref))))+
        geom_line(aes(y=estimated.positive.tests,color='optimal test strategy'),size=1.5)+
        geom_point(aes(y=estimated.positive.tests), bg = alpha('darkred',0.7), shape = 21, size = 4)+
        #scale_y_continuous()+
        scale_color_manual(labels=c("optimal test strategy"),
                           values=c("optimal test strategy"="darkred"))+
        scale_fill_manual(values = "azure3")+
        ggtitle("Test positive rate with optimal test allocation")+
        ylab("Positive rate")+
        xlab("")+
        guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
        scale_y_continuous(sec.axis = sec_axis(trans = ~., name="w for objective function", breaks = as.numeric(c(0,0.25,0.5,0.75,1)*MAXIMUM), labels = as.character(c(0,0.25,0.5,0.75,1))))+
        theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14),
              axis.line.y.right = element_line(color = "darkblue"), 
              axis.ticks.y.right = element_line(color = "darkblue"),
              axis.text.y.right = element_text(color = "darkblue"),
              axis.title.y.right = element_text(color = "darkblue"))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))+
        annotate(geom = 'segment',x=1-0.4, xend= 2+0.4, y = as.numeric(input$weights[1,1])*MAXIMUM, yend =  as.numeric(input$weights[1,1])*MAXIMUM, col = 'darkblue', lwd = 2)+
        annotate(geom = 'segment',x=3-0.4, xend = 8+0.4, y = as.numeric(input$weights[1,2])*MAXIMUM, yend =  as.numeric(input$weights[1,2])*MAXIMUM, col = 'darkblue', lwd = 2)+
        annotate(geom = 'segment',x=9-0.4, xend = 13+0.4, y = as.numeric(input$weights[1,3])*MAXIMUM, yend =  as.numeric(input$weights[1,3])*MAXIMUM, col = 'darkblue', lwd = 2)+
        annotate(geom = 'segment',x=14-0.4, xend = 31+0.4, y = as.numeric(input$weights[1,4])*MAXIMUM, yend = as.numeric(input$weights[1,4])*MAXIMUM, col = 'darkblue', lwd = 2)
      
      
      ### plot 1.4 ###
      pr= data.frame('date' = break.ref,
                     'reported' = round(data$ReportedCases*mult/N,3),
                     'findMoreCases' = round(result$positive.rate,3))
      MAXIMUM = max(c(as.numeric(pr$reported),as.numeric(pr$findMoreCases)))
      p4=ggplot(data=pr,aes(x=date,group=1))+
        geom_bar(aes(y=reported,fill='population infection rate'),stat="identity",position = 'identity',alpha=1, color= "gray30")+
        scale_x_discrete(breaks=break.ref, labels = paste0('wk ', c(1:length(break.ref))))+
        geom_line(aes(y=findMoreCases,color='optimal test strategy'),size=1.5)+
        geom_point(aes(y=findMoreCases), bg = alpha('darkred',0.7), shape = 21, size = 4)+
        #scale_y_continuous()+
        scale_color_manual(labels=c("optimal test strategy"),
                           values=c("optimal test strategy"="darkred"))+
        scale_fill_manual(values = "azure3")+
        ggtitle("Test positive rate with optimal test allocation")+
        ylab("Positive rate")+
        xlab("")+
        guides(color=guide_legend(title=""), fill = guide_legend(title=""))+
        scale_y_continuous(sec.axis = sec_axis(trans = ~., name="w for objective function", breaks = as.numeric(c(0,0.25,0.5,0.75,1)*MAXIMUM), labels = as.character(c(0,0.25,0.5,0.75,1))))+
        theme(axis.text.x = element_text(angle = 60,hjust=1, vjust = 1),legend.position="top",
              legend.text=element_text(size=14), text = element_text(size=14),
              axis.line.y.right = element_line(color = "darkblue"), 
              axis.ticks.y.right = element_line(color = "darkblue"),
              axis.text.y.right = element_text(color = "darkblue"),
              axis.title.y.right = element_text(color = "darkblue"))+
        theme(title=element_text(size=12,face="bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = 'gray95', colour = 'white'))+
        annotate(geom = 'segment',x=1-0.4, xend= 2+0.4, y = as.numeric(input$weights[1,1])*MAXIMUM, yend =  as.numeric(input$weights[1,1])*MAXIMUM, col = 'darkblue', lwd = 2)+
        annotate(geom = 'segment',x=3-0.4, xend = 8+0.4, y = as.numeric(input$weights[1,2])*MAXIMUM, yend =  as.numeric(input$weights[1,2])*MAXIMUM, col = 'darkblue', lwd = 2)+
        annotate(geom = 'segment',x=9-0.4, xend = 13+0.4, y = as.numeric(input$weights[1,3])*MAXIMUM, yend =  as.numeric(input$weights[1,3])*MAXIMUM, col = 'darkblue', lwd = 2)+
        annotate(geom = 'segment',x=14-0.4, xend = 31+0.4, y = as.numeric(input$weights[1,4])*MAXIMUM, yend = as.numeric(input$weights[1,4])*MAXIMUM, col = 'darkblue', lwd = 2)
      
      A1 = gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)
      
    }
    return(A1)
  }


  output$plots = shiny::renderPlot({  GetPlots(input)}, height = 600, width = 1100)
}#end of server


shiny::shinyApp(ui, server)

