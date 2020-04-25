library(shiny)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(data.table)
library(mgcv)

library(directlabels)
library(ggthemes)
library(hexbin)
library(plotly)
library(quadprog)
library(shinyWidgets)
library(shinydashboard)
#install.packages(c('Rttf2pt1','directlables','extrafont','extrafontdb','flexdashboard','gdtools','ggtheme','bexbin','hrbrthemes','metathis'))

on_confirmed <- fread('https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv')
on_case_by_date<-on_confirmed%>%select(Date=Accurate_Episode_Date)%>%group_by(Date)%>%summarise(newcase=n())
on_data<-on_case_by_date%>%mutate(Date=as.Date(Date))%>%filter(Date >= as.Date("2020-02-15"))
#date_seq <- seq(as.Date("2020-01-01"), as.Date(Sys.Date()-1) ,by = "day")
#date_data<-data.frame(Date=date_seq,Day=as.numeric(date_seq-as.Date("2019-12-31")))
#on_data<-left_join(date_data, on_case_by_date, by=c("Date"))
#on_data<-on_data%>%mutate(newcase=ifelse(is.na(newcase),0,newcase),totalcases=cumsum(newcase),previous=lag(newcase))
#on_data<-on_data%>%mutate(previous=ifelse(is.na(previous),0,previous))
on_data<-on_data%>%mutate(Day=as.numeric(as.Date(Date)-as.Date("2020-02-14")))


p1<-ggplot(on_data,aes(Date,newcase))+theme_bw()
p1+geom_point(size=0.5)+ylab("Confirmed cases")+xlab("Date")

b2<-gam(newcase ~ s(Day,bs="tp",k=20),family=nb(),data = on_data[1:60,],method = "REML")



# agrument takes upto next 7 days
Projection<-function(Day_upto)
{
  if(Day_upto %in% 1:5)
  {
    Day<-seq(on_data$Day[nrow(on_data)]+1,on_data$Day[nrow(on_data)]+Day_upto,by=1)
    newdata<-as.data.frame(Day)
    coPred = predict(b2, newdata, se.fit = TRUE)
    newdata <- within(newdata, {
      Predicted_cases <- exp(coPred$fit)
      LL <- exp(coPred$fit - 1.96 * coPred$se.fit)
      UL <- exp(coPred$fit + 1.96 * coPred$se.fit)
    })
    
    p<-ggplot(on_data, aes(x = Day, y = newcase)) + geom_point(size=0.5) +geom_line(colour='blue')+
      theme_bw()+ geom_smooth(aes(x=Day, y=Predicted_cases, ymax=UL, ymin=LL), colour='red', data=newdata, stat='identity')+
      geom_point(aes(x=Day,y=Predicted_cases),data=newdata)+
      scale_x_continuous(breaks=c(0,20,40,60), labels=c("Feb 15", "Mar 05", "Mar 25", "Apr 14"))+
      annotate("text",x=newdata$Day[nrow(newdata)]+2,y=ceiling(newdata$Predicted_cases[nrow(newdata)]+60),label = ceiling(newdata$Predicted_cases[nrow(newdata)]))+
      theme(axis.title=element_text(size=14,face="bold"))+ylab("Confirmed covid cases in Ontario")+xlab("Date")
    print(p)
  }
  else{print("projection is not available beyond 5 days")}
}




ui <- fluidPage(
  
  titlePanel("Projection of confirmed covid cases in Ontario"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("Day_upto", 
                  div(HTML("Projection up to next 5 days")), 
                  min   = 1, 
                  max   = 5, 
                  value = 3, 
                  step  = 1),
      
      hr(),
      br(),
      tags$div("Sudipta Saha, 2020"), 
      tags$a(href="mailto:sudip.saha@mail.utoronto.ca", "sudip.saha@mail.utoronto.ca"),
      br(), br(),
      tags$div("Code is available on Github"),
      # br(),
      tags$a(href="https://github.com/sahasud1/covid19", 
             "https://github.com/sahasud1/covid19"),
      br(), br(),
      tags$div("Template forked from Tinu Schneider's"),
      tags$a(href="https://github.com/tinu-schneider/Flatten_the_Curve", "github") 
      # br(),
      # img(src="license.png"),
      # br()
    ),
    
    mainPanel(
      # p("Note: This tool is not intended to create a prediction."),
      plotOutput("chart", height = "500px"), 
      br(),
      hr(),
      div("The actual cases are presented using blue lines and projections are in red. Last projected cases are printed in the plot. Note that it takes about a week for Ontario officials to confirm test results. Thus, the last week's counts are different compared to other sources.
          The projections are estimated using a simple Generalized Additive model with only one covariate, wihch is Time. The data is available ", 
          shiny::a("here.", href = "https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv")
      ), 
      p(),
      br()
    )
  )
)


server <- function(input, output) {
  
  output$chart <- renderPlot({
    Projection(input$Day_upto)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)



  