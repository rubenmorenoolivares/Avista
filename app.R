library(dashHtmlComponents) # Vanilla HTML Components for Dash
library(dashTable) # Core Interactive Table Component for Dash
library(DT) # A wrapper of the JavaScript library DataTables
library(formattable) # Create formattable data structures
library(ggplot2) # Create elegant data visualizations using the grammar of graphics
library(highcharter) # A wrapper for the highcharts library
library(knitr) # A general purpose package for dynamic report generation in R
library(leaflet) # Create interactive web maps with the JavaScript leaflet library
library(MOTE) # Effect Size and Confidence Interval Calculator
library(plotly) # Create interactive Web Graphics via plotly.js
library(quantmod) # Quantitative financial modeling framework
library(readxl) # Read Excel Files
library(reshape2) # Flexibly Reshape Data
library(rio) # A Swiss-army knife for data i/o
library(roll) # Rolling Statistics
library(rugarch) # Univariate GARCH models
library(shiny) # Web application framewofk for R
library(shinydashboard) # Create dashboards with Shiny
library(splitstackshape) # Stack and reshape datasets after splitting concatenated values
library(stringi) # Character string processing facilities
library(stringr) # Simple, consistent wrappers for common string operations
library(tidyr) # Tidy Messy Data
library(tidyquant) # Tidy quantitative financial analysis
library(tidyverse) # Easily Install and Load the Tidyverse
library(VaRES) # Computes value at risk and expected shortfall for over 100 parametric distributions
library(xts) # eXtensible time series
library(zoo)
library(formattable)

RRR<-read_excel("C:/Users/rmoreno/OneDrive - Concentric Energy Advisors/Clients/Avista/Consultant to Client/Deliverables/Testing Model/Historical_Rates.xlsx", 
              col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),sheet='Original',guess_max=1048576)

Rates <- read_excel("C:/Users/rmoreno/OneDrive - Concentric Energy Advisors/Clients/Avista/Consultant to Client/Deliverables/Testing Model/Historical_Rates.xlsx", 
                    col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),sheet='Clean',guess_max=1048576)
BRX01<-xts(Rates$FY1,Rates$xDate) # Convert to an xts object
c1<-dim(BRX01) # Let's see the dimensions of BRX01
c2<-as.numeric(c1[1]) # Let's just save the number of observations (rows)
c3<-40 # Number of days to extrapolate volatility for sqrt method
c4<-60 # Number of days of history to include in the volatility calculation
LPCL<-0.025 # Lower probability confidence level used in VaR calculations
UPCL<-0.975 # Upper probability confidence level used in VaR calculations

names(BRX01)[1]<-"FY1" # Relabel the variable
BRX01$rFY1<-dailyReturn(BRX01$FY1) # Calculate the daily log return of FY1
BRX01$rSD60D<-roll_sd(BRX01$rFY1,width=c4) # Calculate the rolling standard deviation with 60 days of history.  This is the One-day Standard Deviation
BRX01$rSDEWMA<-NA # Creates space to save the 1D Standard Deviation per the EWMA specification with a lambda 0.94
BRX01$rSDGARCH<-NA # Creates space to save the 1D Standard Deviation per the  GARCH(1,1) specification
BRX01$fSDGARCH<-NA # Creates space to save the 1D Standard Deviation Forecasted to the c2 days
BRX01$VaRDownHist<-NA # Creates space to save the VaR Upside using the historical method to the c2 period
BRX01$VaRUpHist<-NA # Creates space to save the VaR Upside using the historical method to the c2 period
BRX01$VaRDownEWMA<-NA # Creates space to save the VaR Upside using the historical method to the c2 period
BRX01$VaRUpEWMA<-NA # Creates space to save the VaR Upside using the historical method to the c2 period
BRX01$VaRDownGARCH<-NA # Creates space to save the VaR Upside using the historical method to the c2 period
BRX01$VaRUpGARCH<-NA # Creates space to save the VaR Upside using the historical method to the c2 period

ewma.spec.fixed = ugarchspec(mean.model=list(armaOrder=c(0,0),include.mean=FALSE),variance.model=list(model="iGARCH"),fixed.pars=list(alpha1=1-0.94,omega=0)) # Model specification for EWMA with Lambda = 0.94
igarch.spec=ugarchspec() # Model specification for GARCH (1,1)
mod1<-ugarchfit(ewma.spec.fixed,as.xts(BRX01$rFY1)) # EWMA Lambda 0.94
EWMA_Sigma<-sigma(mod1) # 1D Sigma from EWMA model with a lambda 0.94
mod3=ugarchfit(igarch.spec,as.xts(BRX01$rFY1)) # GARCH(1,1) 
GARCH_Sigma<-sigma(mod3) # 1D Sigma from GARCH(1,1) model

# Let's save the results of the estimates of volatility per the EWMA and the GARCH models back to the original dataset
for (i in 1:c2) {
    BRX01[i,4]<-as.numeric(EWMA_Sigma[i]) # Save the one day standard deviation based on the EWMA model with a lambda of 0.94
    BRX01[i,5]<-as.numeric(GARCH_Sigma[i]) # Save the one day standard deviation based on the GARCH(1,1) model
}


ui <- fluidPage(
        dashboardHeader(title="Avista Interest Rate Review Conceptual Review"),
        dashboardSidebar(),
        dashboardBody(
            box(h4("Issues with Data?"),width=12,align="center"),
            box(plotOutput("rate_plot0"),width=8),
            box(sliderInput("drngs", "Date Range", min = as.Date(min(RRR$xDate)), max = as.Date(max(RRR$xDate)), value = c(as.Date(min(RRR$xDate)), as.Date(max(RRR$xDate)))),width=4),
            box(sliderInput("mxrates", "Interest Rate Range", min = 0, max = 10, value = c(0, 10),step=0.05),width=4),
            hr(),
            box(h4("Interest Rates are Mean Reverting"),width=12,align="center"),
            box(plotOutput("rate_plot"),width=8),
            box(selectInput("plot_variable","Select Variable to Plot:",c("FY1","rFY1"),selected="FY1"),width=4),
            hr(),
            box(h4("How Many Days of History are Relevant?"),width=12,align="center"),
            box(plotOutput("rate_plot3"),width=8),
            box(sliderInput("lambda","Decay Factor (Lambda)",min=85,max=99,value=94,step=1),width=4),
            box(sliderInput("nd20","Days History to Include",min=0,max=780,value=100,step=1),width=4),
            hr(),
            box(h4("Is the Methodoogy to Calculate Volatility Important?"),width=12,align="center"),
            box(plotOutput("rate_plot4"),width=8),
            box(sliderInput("drngt", "Date Range", min = as.Date(min(BRX02$xDate)), max = as.Date(max(BRX02$xDate)), value = c(as.Date(min(BRX02$xDate)), as.Date(max(BRX02$xDate)))),width=4),
            box(sliderInput("mxratet", "Volatility Range", min = 0.005, max = .2, value = c(.005, .07)),width=4),
            hr(),
            box(h4("What is the Effect of Extrapolating Volatility?"),width=12,align="center"),
            box(plotOutput("rate_plot2"),width=8),
            box(sliderInput("d2xv","Days to Extrapolation",min=22,max=780,value=22,step=10),width=4),
            box(sliderInput("drng", "Date Range", min = as.Date(min(Rates$xDate)), max = as.Date(max(Rates$xDate)), value = c(as.Date(min(Rates$xDate)), as.Date(max(Rates$xDate)))),width=4),
            box(sliderInput("mxrate", "Interest Rate Range", min = 0, max = 20, value = c(0, 8)),width=4),
            hr(),
            box(h4("Effect of Different Methodologies on Limit Setting"),width=12,align="center"),
            box(tableOutput('Boundaries'),width=8),
            column(4,dateInput("date","Select Date",value=as.Date(max(BRX02$xDate)))),
            column(4,sliderInput("d2xv2","Days to Extrapolate",min=22,max=780,value=22,step=10)),
            column(4,sliderInput("conf", "Confidence Level", min = 0.01, max = 0.99, value = c(.01, .99))),
            hr(),
            box(h4("Managing to Hedged and Unhedged Positions"),width=12,align="center"),
            box(plotOutput('Risk_plot'),width=8),
            column(4,dateInput("date2","Select Date",value=as.Date(max(BRX02$xDate)))),
            column(4,sliderInput("d2xv3","Days to Extrapolate",min=22,max=780,value=22,step=10)),
            column(4,sliderInput("conf2", "Confidence Level", min = 0.01, max = 0.99, value = c(.01, .99))),
            hr(style="border-color: purple"),
        )
      )


    BRX02<-as.data.frame(BRX01)
    BRX01_Date<-index(BRX01)
    BRX02<-cbind(BRX02,BRX01_Date)
    names(BRX02)[13]<-"xDate"
    BRX02$xDate<-as.Date(BRX02$xDate)
    
    BRX02$VaRDownHist<-round(varlognorm(LPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSD60D)*sqrt(c3),log=FALSE,lower.tail=TRUE),4) # Lower VaR to c3 days with Historical Method
    BRX02$VaRUpHist<-round(varlognorm(UPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSD60D)*sqrt(c3),log=FALSE,lower.tail=TRUE),4) # Upper VaR to c3 days with Historical Method
    BRX02$VaRDownEWMA<-round(varlognorm(LPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSDEWMA)*sqrt(c3),log=FALSE,lower.tail=TRUE),4) # Lower VaR to c3 days with EWMA Method
    BRX02$VaRUpEWMA<-round(varlognorm(UPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSDEWMA)*sqrt(c3),log=FALSE,lower.tail=TRUE),4) # Upper VaR to c3 days with EWMA Method
    BRX02$VaRDownGARCH<-round(varlognorm(LPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSDGARCH)*qlnorm(UPCL),log=FALSE,lower.tail=TRUE),4) # Lower VaR to c3 days with GARCH Method
    BRX02$VaRUpGARCH<-round(varlognorm(UPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSDGARCH)*qlnorm(UPCL),log=FALSE,lower.tail=TRUE),4) # Upper VaR to c3 days with GARCH Method

    EWMA_d<-matrix(NA,780,3)
     for (i in 2:780){
       EWMA_d[(i-1),1]<-i-1
     }
    
    Boundaries<-matrix(NA,7,4)
    Boundaries<-as.data.frame(Boundaries)

    
    
server <- function(input,output){
    
  output$rate_plot0<-renderPlot({
    plot(as.Date(RRR$xDate),RRR$FY0, type="l", xlab="",ylab="Rate %",col="black",las=2,cex.axis=1,font.axis=1,ylim=c(input$mxrates[1],input$mxrates[2]),xlim=c(as.Date(input$drngs[1]),as.Date(input$drngs[2])),main="Examination of Input Data")
    lines(as.Date(RRR$xDate),RRR$FY1,type="l",col="red")
    lines(as.Date(RRR$xDate),RRR$FY2,type="l",col="green")
    lines(as.Date(RRR$xDate),RRR$FY3,type="l",col="blue")
    lines(as.Date(RRR$xDate),RRR$FY4,type="l",col="gray")
    lines(as.Date(RRR$xDate),RRR$FY5,type="l",col="pink")
    lines(as.Date(RRR$xDate),RRR$FY6,type="l",col="purple")
    lines(as.Date(RRR$xDate),RRR$FY7,type="l",col="magenta")
    legend("topright",box.lty=1,cex=0.8,legend=c("FYO","FY1","FY2","FY3","FY4","FY5","FY6","FY7"),text.col=c("black","red","green","blue","gray","pink","purple","magenta"))
  })
    output$rate_plot<-renderPlot({
      data<-switch(input$plot_variable,
                   "Interest Rate FY1"="FY1",
                   "Return FY1"="rFY1")
      if (input$plot_variable=="FY1")
      {
        maint1<-"Interest Rates FY1"
        ylab1<-"Interest Rate %"
      }
      if (input$plot_variable=="rFY1")
      {
        maint1<-"One-Day Log Return FY1"
        ylab1<-"Interest Rate %"
      }
      plot(BRX02$xDate,BRX02[[input$plot_variable]],type="l",xlab="",ylab=ylab1,main=paste0("Statistical Properties: ",maint1))
      })
    output$rate_plot2<-renderPlot({
        mxd<-as.Date(max(Rates$xDate))
        mnd<-as.Date(min(Rates$xDate))
        plot(BRX02$xDate,BRX02$FY1,lty=1,type="l",col="black",ylim=c(input$mxrate[1],input$mxrate[2]),xlim=c(input$drng[1],input$drng[2]),xlab="",ylab="Interest Rate %",main="Effects of Extrapolation of Volatility Different Methods")
        lines(BRX02$xDate,round(varlognorm(LPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSD60D)*sqrt(input$d2xv),log=FALSE,lower.tail=TRUE),4),type="l",lty=3,col="red",lwd=2)
        lines(BRX02$xDate,round(varlognorm(UPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSD60D)*sqrt(input$d2xv),log=FALSE,lower.tail=TRUE),4),type="l",lty=3,col="red",lwd=2)
        lines(BRX02$xDate,round(varlognorm(LPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSDEWMA)*sqrt(input$d2xv),log=FALSE,lower.tail=TRUE),4),type="l",lty=3,col="blue",lwd=2)
        lines(BRX02$xDate,round(varlognorm(UPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSDEWMA)*sqrt(input$d2xv),log=FALSE,lower.tail=TRUE),4),type="l",lty=3,col="blue",lwd=2)
        lines(BRX02$xDate,round(varlognorm(LPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSDGARCH)*qlnorm(0.95),log=FALSE,lower.tail=TRUE),4),type="l",lty=3,col="lawngreen",lwd=2)
        lines(BRX02$xDate,round(varlognorm(UPCL,mu=log(as.numeric(BRX02$FY1)),sigma=as.numeric(BRX02$rSDGARCH)*qlnorm(0.95),log=FALSE,lower.tail=TRUE),4),type="l",lty=3,col="lawngreen",lwd=2)
        legend("topright",legend=c("FY1","Historical and Sqrt(t)","EWMA and Sqrt(t)","Historical GARCH"),col=c("black","red","blue","lawngreen"),text.col=c("black","red","blue","lawngreen"),cex=1)
        })
    output$rate_plot3<-renderPlot({
      lmbd<-input$lambda
      for (i in 2:780){
        EWMA_d[(i-1),1]<-i-1
        EWMA_d[1,2]<-(round(100-lmbd,4))
        EWMA_d[i,2]<-round(EWMA_d[(i-1),2]*(lmbd/100),4)
        EWMA_d[i,3]<-(1/input$nd20)*100
      }
      EWMA_2<-as.data.frame(EWMA_d)
      names(EWMA_2)[1]<-"t"
      names(EWMA_2)[2]<-"Weight"
      plot(EWMA_2$t,EWMA_2$Weight,type="l",xlab="Days of History",ylab="Individual Day Weight %",xlim=c(0,input$nd20),main="Relevance of Historical Data in the EWMA Method")
      lines(EWMA_2$t,EWMA_2$V3,type="l",col="blue",lwd=2)
      abline(h=0,lty=3,col="red",lwd=4)
      legend("topright",legend=c("EWMA Method","Equal Weight"),text.col=c("black","blue"),cex=1)
      })
    output$value<-renderPrint({
      dimvol<-as.numeric(dim(input$checkGroup))      
      input$checkGroup
      })
    output$rate_plot4<-renderPlot({
      plot(BRX02$xDate,BRX02$rSD60D,type="l",col="red",ylab="One Day Volatility",xlab="",ylim=c(input$mxratet[1],input$mxratet[2]),xlim=c(as.Date(input$drngt[1]),as.Date(input$drngt[2])),main="Volatility Under Different Methods",lwd=2)
      lines(BRX02$xDate,BRX02$rSDEWMA,type="l",col="blue",lwd=2)
      lines(BRX02$xDate,BRX02$rSDGARCH,type="l",col="lawngreen",lwd=2)
      legend("topright",legend=c("Historical and Sqrt(t)","EWMA and Sqrt(t)","GARCH"),col=c("red","blue","lawngreen"),text.col=c("red","blue","lawngreen"),cex=0.9)
    })
    

    
    output$Boundaries <- renderTable({
      t100<-subset(BRX02,BRX02$xDate==as.Date(input$date))
      t101<-subset(BRX02,BRX02$xDate<=as.Date(input$date))
      ug_spec=ugarchspec() # Specify what kind of model you want
      ugfit = ugarchfit(spec = ug_spec, data = t101$rFY1) # Model estimation
      ugfore <- ugarchforecast(ugfit, n.ahead = as.numeric(input$d2xv2)) # Forecast conditional Variance c3 days into the future
      ug_f <- last(ugfore@forecast$sigmaFor) # Extract the forecasted sigma
      t103<-as.numeric(t102[1])
      Boundaries[1,1]<-"Selected Interest Rate %"
      Boundaries[2,1]<-"One Day Volatility"
      Boundaries[3,1]<-"Days Extrapolation"
      Boundaries[4,1]<-"Lower Confidence Level"
      Boundaries[5,1]<-"Upper Confidence Level"
      Boundaries[6,1]<-"Upper Boundary"
      Boundaries[7,1]<-"Lower Boundary"
      Boundaries[1,2]<-round(t100[1,1],2)
      Boundaries[1,3]<-round(t100[1,1],2)
      Boundaries[1,4]<-round(t100[1,1],2)
      Boundaries[2,2]<-round(t100[1,3],2)
      Boundaries[2,3]<-round(t100[1,4],2)
      Boundaries[2,4]<-round(ug_f,2)
      Boundaries[3,2]<-trunc(input$d2xv2)
      Boundaries[3,3]<-trunc(input$d2xv2)
      Boundaries[3,4]<-trunc(input$d2xv2)
      Boundaries[4,2]<-input$conf[1]
      Boundaries[4,3]<-input$conf[1]
      Boundaries[4,4]<-input$conf[1]
      Boundaries[5,2]<-input$conf[2]
      Boundaries[5,3]<-input$conf[2]
      Boundaries[5,4]<-input$conf[2]      
      Boundaries[6,2]<-round(varlognorm(as.numeric(input$conf[2]),mu=log(as.numeric(t100[1,1])),sigma=as.numeric(as.numeric(t100[1,3]))*sqrt(input$d2xv2),log=FALSE,lower.tail=TRUE),2)
      Boundaries[7,2]<-round(varlognorm(as.numeric(input$conf[1]),mu=log(as.numeric(t100[1,1])),sigma=as.numeric(as.numeric(t100[1,3]))*sqrt(input$d2xv2),log=FALSE,lower.tail=TRUE),2)
      Boundaries[6,3]<-round(varlognorm(as.numeric(input$conf[2]),mu=log(as.numeric(t100[1,1])),sigma=as.numeric(as.numeric(t100[1,4]))*sqrt(input$d2xv2),log=FALSE,lower.tail=TRUE),2)
      Boundaries[7,3]<-round(varlognorm(as.numeric(input$conf[1]),mu=log(as.numeric(t100[1,1])),sigma=as.numeric(as.numeric(t100[1,4]))*sqrt(input$d2xv2),log=FALSE,lower.tail=TRUE),2)
      Boundaries[6,4]<-round(varlognorm(as.numeric(input$conf[2]),mu=log(as.numeric(t100[1,1])),sigma=as.numeric(as.numeric(ug_f))*qlnorm(as.numeric(.95)),log=FALSE,lower.tail=TRUE),2)
      Boundaries[7,4]<-round(varlognorm(as.numeric(input$conf[1]),mu=log(as.numeric(t100[1,1])),sigma=as.numeric(as.numeric(ug_f))*qlnorm(as.numeric(.95)),log=FALSE,lower.tail=TRUE),2)
      names(Boundaries)[1]<-""
      names(Boundaries)[2]<-"Historical"
      names(Boundaries)[3]<-"EWMA"
      names(Boundaries)[4]<-"GARCH"
      formattable(as.data.frame(Boundaries)) 
      })
    
    output$Risk_plot <- renderPlot({
      t201<-subset(BRX02,BRX02$xDate<=as.Date(input$date2))
      t202<-dim(t201)
      t203<-as.numeric(t202[1]) # This is the number of historical days available
      t204<-as.numeric(t202[2]) # This is the number of variables in the historical data selected
      empty<-matrix(NA,as.numeric(input$d2xv3),as.numeric(t204))
      colnames(empty)=colnames(t201)
      t205<-rbind(t201,empty)
      t206<-dim(t205)
      t207<-as.numeric(t206[1])
      t208<-as.numeric(t203)+1
      t209<-as.Date(last(t201$xDate))
      
      for (i in t208:t207)
      {
        t205[t208,13]<-as.Date(t205$xDate)+days(1)
      }


      plot(as.Date(t208$xDate),as.numeric(t208$FY1))
    })
    
}

shinyApp(ui,server)