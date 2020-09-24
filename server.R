
source(file = 'www/find_t_f_V2_NY.R')
source(file = 'www/each_generation_withZ_comb_V2.R')
library(ggplot2)




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



server = function(input, output ) {
  
  ###########################
  ### Define Color Scheme ###
  ###########################
  
  GetPlots <- function(input){
    N = 8175133 
    w = as.numeric(input$w) #1
    ta = as.numeric(input$ta)
    ts = (1-ta)/5
    targetpos = as.numeric(input$targetpos)
    mult= as.numeric(input$mult) #5 
    falseneg = as.numeric(input$falseneg)
    falsepos = as.numeric(input$falsepos)

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
                                    Total=ifelse(input$TABCHOSEN !='Historical', input$Total, data$Tests[i]),
                                    beta=falseneg,
                                    alpha=falsepos,
                                    c_pos=targetpos,
                                    w=w)
      result[i,2] = model$P.hat/data$Tests[i]
      result[i,3] = model$selectProb.s
      result[i,4] = model$selectProb.m
      result[i,5] = model$selectProb.a
      result[i,6] = model$selectProb.age1
      result[i,7] = model$selectProb.age2
      result[i,8] = model$selectProb.age3
      result[i,9] = model$selectProb.age4
      result[i,10] = model$P.hat
      
      Test.result[i,1] = i
      Test.result[i,2] = sum(model$asignedTests[9:12])
      Test.result[i,3] = sum(model$asignedTests[5:8])
      Test.result[i,4] = sum(model$asignedTests[1:4])
      Test.result[i,5] = model$asignedTests[1]+model$asignedTests[5]+model$asignedTests[9]
      Test.result[i,6] = model$asignedTests[2]+model$asignedTests[6]+model$asignedTests[10]
      Test.result[i,7] = model$asignedTests[3]+model$asignedTests[7]+model$asignedTests[11]
      Test.result[i,8] = model$asignedTests[4]+model$asignedTests[8]+model$asignedTests[12]
    }
    result = as.data.frame(result)
    
    break.ref = as.character(NY_date)
    break.ref = gsub('/','-',break.ref)
    break.ref = gsub('-2020','',break.ref)
    
    T_rptCase_simuCase = data.frame('date'=break.ref,
                                    'reported cases' = data$ReportedCases,
                                    'estimated positive tests' = result$positive.tests)
    
    
    

    ####################
    ### PLOT RESULTS ###
    ####################


    ### plot 1.1 ###
    Test.result.symptom = data.frame('date' = break.ref,
                                     'asymptomatics' = Test.result[,2],
                                     'mild' = Test.result[,3],
                                     'severe' = Test.result[,4])
    Test.symptom.long = reshape2::melt(Test.result.symptom,id.vars = "date")
    p1 = ggplot(data=Test.symptom.long,aes(x=date,y=value,fill=variable))+
      geom_bar(stat="identity",position = 'stack')+
      scale_y_continuous()+
      scale_x_discrete(breaks=break.ref)+
      ggtitle("Tests allocation to each symptomatic group in New York City")+
      ylab("Number of tests")+
      xlab(" ")+
      guides(fill=guide_legend(title=""))+
      scale_fill_manual(values=c('#999999','#E69F00',"#0099CC"))+
      ggthemes::theme_economist()+
      theme(axis.text.x = element_text(angle = 60,vjust=0.5))+
      theme(title=element_text(size=10,face="bold"))
    
    ### plot 1.2 ###
    Test.result.age = data.frame('date' = break.ref,
                                 'age1' = Test.result[,5],
                                 'age2' = Test.result[,6],
                                 'age3' = Test.result[,7],
                                 'age4' = Test.result[,8])
    Test.age.long = reshape2::melt(Test.result.age,id.vars = "date")
    
    p2 = ggplot(data=Test.age.long,aes(x=date,y=value,fill=variable))+
      geom_bar(stat="identity",position = 'stack')+
      scale_y_continuous()+
      scale_x_discrete(breaks=break.ref)+
      ggtitle("Tests allocation to each age group in New York City")+
      ylab("Number of tests")+
      xlab("")+
      guides(fill=guide_legend(title=""))+
      scale_fill_manual(values=c('#000066','#00CC66','#FFFF00','#FF3300'),
                        labels = c("age 0-17","age 18-49","age 50-64","age 65+"))+
      ggthemes::theme_economist()+
      theme(axis.text.x = element_text(angle = 60,vjust=0.5))+
      theme(title=element_text(size=10,face="bold"))
    
    
    ### plot 1.3 ###
    
    p3 = ggplot(data=T_rptCase_simuCase,aes(x=date,y=reported.cases,group=1))+
      geom_bar(aes(y=reported.cases,fill='reported cases'),stat="identity",position = 'identity',alpha=1)+
      scale_y_continuous()+
      scale_x_discrete(breaks=break.ref)+
      geom_line(aes(y=estimated.positive.tests,color='optimal strategy'),size=1.5)+
      geom_point(aes(y=estimated.positive.tests))+
      scale_color_manual(labels=c("optimal strategy"),
                         values=c("optimal strategy"="#0066CC"))+
      scale_fill_manual(values = "red")+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal")+
      xlab("") +
      ylab("")+
      ggtitle("Reported cases and estimated optimal positive cases")+
      ggthemes::theme_economist()+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 60,vjust=0.5))+
      theme(title=element_text(size=10,face="bold"))
    
    ### plot 1.4 ###
    
    result_w1 = result
    result_w0 = result
    pr= data.frame('date' = break.ref,
                   'reported' = round(data$ReportedCases/data$Tests,3),
                   'findMoreCases' = round(result_w1$positive.rate,3),
                   'controlPR' = round(result_w0$positive.rate,3))
    p4=ggplot(data=pr,aes(x=date,group=1))+
      geom_line(aes(y=reported,color='reported positive rate'),size=1.5)+
      geom_point(aes(y=reported))+
      geom_line(aes(y=findMoreCases,color='optimalStrategy'),size=1.5)+
      geom_point(aes(y=findMoreCases))+
      scale_y_continuous()+
      scale_x_discrete(breaks=break.ref)+
      ggtitle("Positive rate comparison in New York City")+
      ylab("Positive rate")+
      xlab("")+
      scale_color_manual(values=c('reported positive rate'= '#FF3333', 'optimalStrategy'='#00CC66'))+
      guides(color=guide_legend(title=""))+
      ggthemes::theme_economist()+
      theme(axis.text.x = element_text(angle = 60,vjust=0.5))+
      theme(title=element_text(size=10,face="bold"))
    
    A1 = gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)
    
    return(A1)
  }


  output$plots = shiny::renderPlot({  GetPlots(input)}, height = 500, width = 1100)

}#end of server




