library(shiny)
library(reshape2)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(plotly)
library(tidyquant)
library(tseries)
library(timetk)
library(magrittr)
library(PerformanceAnalytics)
library(tidyr)
library(purrr)
library(tibbletime)
library(gt)
library(shinythemes)
library(dplyr)

# begin with clean slate
rm(list=ls())
# define a function for sample standard deviation
sd_function <- function(x){
  val = sqrt(
    sum(
      (x-mean(x))^2
    )/(length(x)-1)
  )
  return(val)}

sd_roll <- rollify(window=12, .f=sd_function)

library(tibbletime)
library(gt)
library(shinythemes)
sd_function <- function(x){
  val = sqrt(
    sum(
      (x-mean(x))^2
    )/(length(x)-1)
  )
  return(val)}

sd_roll <- rollify(window=12, .f=sd_function)

shinyApp(
  ui=fluidPage(theme=shinytheme("superhero"),
               sidebarLayout(
                 sidebarPanel(
                   fluidRow(textInput("stock1","Select Stock",value="PG"),
                            sliderInput("weight1", label="Select weight",max=1,min=0,value=0.25),
                            textInput("stock2","Select Stock",value="KO"),
                            sliderInput("weight2", label="Select weight",max=1,min=0,value=0.25),
                            textInput("stock3","Select Stock",value="TSLA"),
                            sliderInput("weight3", label="Select weight",max=1,min=0,value=0.25),
                            textInput("stock4","Select Stock",value="AMZN"),
                            sliderInput("weight4", label="Select weight",max=1,min=0,value=0.25)),
                   
                   dateRangeInput("range", label="Select a date range",
                                  start="2015-01-01",
                                  end=Sys.Date()), 
                   numericInput("investment", label="Initial Investment",value=100,
                                min=0, max=100, step=2),
                   actionBttn("click","Launch", icon=icon("play"))), 
                 mainPanel(
                   tabsetPanel(
                     tabPanel(title="Chart",fluidRow(plotlyOutput("plot"),
                                                     plotlyOutput("plot1"),
                                                     plotlyOutput("hist"))
                     ),
                     tabPanel(title="Investment", 
                              plotlyOutput("cum_chart")),
                     tabPanel(title="Data", gt_output("table"))
                   )))),
  server=function(input,output){
    
    
    dataInput <- eventReactive(input$click,{
      #tickers of the stocks
      tickers<-c(input$stock1,input$stock2,input$stock3,input$stock4)
      #import data
      df <-getSymbols(tickers,
                      from=input$range[1],
                      to=input$range[2])%>%
        map(~Ad(get(.)))%>%reduce(merge)%>%
        set_colnames(tickers)%>%
        data.frame(date=index(.))%>%
        gather(asset,value,-date)%>%
        group_by(asset)%>%
        tq_transmute(select="value",mutate_fun = periodReturn,
                     period="monthly",
                     type="log",
                     col_rename = "returns")%>%
        na.omit()
      
      
      return(df)
    })
    #-------------------------------------
    output$mu <- renderValueBox({
      dataInput()%>%
        tq_portfolio(., assets_col="asset", returns_col="returns",
                     weights=c(input$weight1,
                               input$weight2,
                               input$weight3,
                               input$weight4),col_rename="returns")%>%
        data.frame()%>%
        valueBox(value=mean(.$returns)%>%round(digits=3)%>%as.character(),
                 subtitle="Average Mean",
                 icon=icon("coins"),
                 color="green")
      
    })
    
    
    #.--------------------------------------------------------------
    output$plot<- renderPlotly({
      dataInput()%>%
        group_by(asset)%>%
        mutate(sd=sd_roll(returns))%>%
        plot_ly(x=~date,y=~sd, group=~asset,color = ~asset, mode="line")%>%layout(title="Rolling standard deviation",
                                                                                  xaxis=list(title="Date"),
                                                                                  yaxis=list(title="Standard Deviation"),
                                                                                  shapes = list(list(type = "rect",
                                                                                                     fillcolor = "gray", line = list(color = "gray"), opacity = 0.6,
                                                                                                     x0 = "2020-03-12", x1 = "2021-03-01", xref = "x",
                                                                                                     y0 = 0, y1 = 0.25, yref = "y")))
    })
    
    #---------------------------------------------------------------------   
    output$plot1 <- renderPlotly({
      dataInput()%>%
        plot_ly(x=~date, y=~returns, group=~asset,
                mode="lines", color=~asset)%>%layout(title="Chart of returns",
                                                     xaxis=list(title="Date"),
                                                     yaxis=list(title="Rates of returns"),shapes = list(
                                                       list(type = "rect",
                                                            fillcolor = "red", line = list(color = "gray"), opacity = 0.6,
                                                            x0 = "2020-03-12", x1 = "2021-03-01", xref = "x",
                                                            y0 = -0.3, y1 = 0.6, yref = "y")))
    })
    
    
    output$hist <- renderPlotly({
      dataInput()%>%
        plot_ly(x=~asset,y=~returns,type = "violin",
                color = ~asset,
                side = "positive",
                meanline = list(visible = T))
    })
    
    output$table <- render_gt({
      dataInput()%>%gt
    })
    
    
    dataInput1<- eventReactive(input$click,{
      p <-  dataInput()%>%
        tq_portfolio(., assets_col = asset,
                     returns_col = returns,
                     weights=c(input$weight1,input$weight2,input$weight3,input$weight4),
                     col_rename = "cum_returns")%>%mutate(cum_inv=as.numeric(NA))
      
      for (i in 2:nrow(p)){
        p[1,3 ]<-input$investment
        p[i,3]<- p[i-1,3]*(1+p$cum_returns[i])
      }
      return(p)
    })
    
    
    
    dataInput3 <- eventReactive(input$click,{
      #set tickers to names of the stocks
      tickers<-c(input$stock1, input$stock2,input$stock3,input$stock4)
      
      df<-get.hist.quote("^GSPC", start=input$range[1],end=input$range[2])%>%
        data.frame()%>%
        to.monthly(indexAt = "lastof",OHLC=F)%>%
        mutate(date=rownames(.))%>%
        transform(date=as.Date(date),
                  Close=as.numeric(Close))%>%
        tq_transmute(select="Close",mutate_fun = periodReturn,
                     period="monthly",
                     type="log",
                     col_rename = "ret_mkt")
      
      data <-getSymbols(tickers,
                        from=input$range[1],end=input$range[2])%>%
        map(~Ad(get(.)))%>%reduce(merge)%>%
        set_colnames(tickers)%>%
        data.frame(date=index(.))%>%
        gather(asset,value,-date)%>%
        group_by(asset)%>%
        tq_transmute(select="value",mutate_fun = periodReturn,
                     period="monthly",
                     type="log",
                     col_rename = "returns")%>%
        na.omit()%>%
        tq_portfolio(.,assets_col=asset,
                     returns_col=returns,
                     weights=c(input$weight1,
                               input$weight2,
                               input$weight3,
                               input$weight4),
                     col_rename="port_ret")
      
      dataset<-left_join(data,df, by="date")%>%na.omit()%>%
        mutate(
          cum_ret_mkt=as.numeric(NA),
          cum_ret_port=as.numeric(NA))
      
      for(i in 2:nrow(dataset)){
        dataset$cum_ret_mkt[1]<-input$investment
        dataset$cum_ret_mkt[i]<- dataset$cum_ret_mkt[i-1]*(1+dataset$ret_mkt[i])
      }
      
      for(i in 2:nrow(dataset)){
        dataset$cum_ret_port[1]<-input$investment
        dataset$cum_ret_port[i]<-dataset$cum_ret_port[i-1]*(1+dataset$port_ret[i])
      }
      return(dataset)
      
    })
    
    output$cum_chart<- renderPlotly({
      
      dataInput3()%>%
        select(date,cum_ret_port,cum_ret_mkt)%>%
        set_colnames(c("Date","Cumulative_Portfolio_Return","Cumulative_Market_Return"))%>%
        gather(asset,value,-Date)%>%
        plot_ly(x=~Date,y=~value, group=~asset, type="scatter",mode="lines",
                color=~asset)%>%
        layout(title="Cumulative Investment Return",xaxis=list(title="Date"),
               yaxis=list(title="Cumulative Investment ($)",
                          hoverformat = '$.2f'
               ),
               legend = list(x = 0.1, y = 0.9))%>%rangeslider()
    })
    
  }
)