
#
# Initial code, mathematical model and idea:
# Michael Höhle, 'Flatten the COVID-19 curve', Mar 16, 2020
# https://staff.math.su.se/hoehle/blog/2020/03/16/flatteningthecurve.html
#       




library(shiny)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(deSolve)
library(data.table)
library(mgcv)
library(tidymv)

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
      annotate("text",x=newdata$Day[nrow(newdata)]+2,y=ceiling(newdata$Predicted_cases[nrow(newdata)]+60),label = ceiling(newdata$Predicted_cases[nrow(newdata)]))
    print(p)
  }
  else{print("projection is not available beyond 5 days")}
}


## CONSTANTS

# Population size 
N <- 1e6 

# Rate at which person stays in the infectious compartment (disease specific and tracing specific)
gamma <- 1/5 

# Initial number of infected people
n_init <- 10



# # Grid where to evaluate
# max_time <- 365 # 150
# times <- seq(0, max_time, by = 0.1)

# R0 for the beta and gamma values
# R0 <- beta*N/gamma

# calculate beta
# Infectious contact rate - beta = R0/N*gamma and when R0  ~2.25 then  2.25/N*gamma
# beta <- 4.5e-07 





# Function to compute the derivative of the ODE system
# -----------------------------------------------------------
#  t - time
#  y - current state vector of the ODE at time t
#  parms - Parameter vector used by the ODE system (beta, gamma)

sir <- function(t, y, parms, 
                social_dist_period, 
                reduction) {
  
  beta0 <- parms[1]
  gamma <- parms[2]
  
  # Reduce contact rate 
  beta_t <- if_else(t <= social_dist_period[1], 
                    beta0,
                    if_else(t <= social_dist_period[2], 
                            beta0 * reduction[1],
                            beta0 * reduction[2]
                    )
  )
  
  S <- y[1]
  I <- y[2]
  
  return(list(c(S = -beta_t * S * I, 
                I =  beta_t * S * I - gamma * I)))
}


## we assume that some globals exist...
solve_ode <- function(sdp, red, typ, beta, max_time) {
  
  # Grid where to evaluate
  times <- reactive({ seq(0, max_time, by = 0.1) })
  
  ode_solution <- lsoda(y = c(N - n_init, n_init), 
                        times = times(), 
                        func  = sir, 
                        parms = c(beta, gamma), 
                        social_dist_period = sdp,
                        reduction = red) %>%
    as.data.frame() %>%
    setNames(c("t", "S", "I")) %>%
    mutate(beta = beta, 
           gama = gamma,
           R0 = N * beta / gamma, 
           s  = S / N, 
           i  = I / N, 
           type = typ)
  
  daily <- ode_solution %>%
    filter(t %in% seq(0, max_time, by = 1)) %>%
    mutate(C = if_else(row_number() == 1, 0, lag(S, k = 1) - S), 
           c = C / N)
  
  daily
}





# add results with intervention and plot
run <- function(sdp, red, r0, max_time) {
  
  beta <- r0 / N * gamma
  
  ode_solution_daily <- solve_ode(
    sdp = c(0, max_time),  # social_dist_period
    red = c(1, 1),         # reduction
    typ = "without", 
    beta = beta, 
    max_time
  )
  
  # solve with interventions
  ode_solution2_daily <- solve_ode(
    sdp = sdp,
    red = red,
    typ = "with", 
    beta = beta, 
    max_time
  )
  
  # Combine the two solutions into one dataset
  ode_df <- rbind(ode_solution_daily, ode_solution2_daily)
  
}


plot_result <- function(ode_df, sdp, max_time, y_axis_fixed) {    
  
  # The final size in the two cases:
  final_sizes <- ode_df %>%
    group_by(type) %>%
    filter(row_number() == n()) %>%
    mutate("final fraction" = scales::percent(1 - s, accuracy = 1)) %>%
    select("final fraction", interventions = type) %>% 
    arrange(desc(interventions))
  
  # Plot
  if (y_axis_fixed) {
    y_max <- 0.09
  } else {
    y_max <- max(ode_df$c, na.rm = TRUE) * 1.05
  }
  y_arrow <- y_max * 0.975
  y_text  <- y_arrow + y_max * 0.01 
  col_sdp <- "lightblue3"
  
  x_labs <- sort(c(0, 100, 200, 300, 365, sdp))
  
  
  pp <- ggplot(ode_df, 
               aes(x = t, 
                   y = 0, 
                   xend = t, 
                   yend = c, 
                   color = type)) + 
    geom_segment(alpha = 0.7) + 
    geom_line(aes(x = t, y = c)) + 
    labs(
      x = "Days", 
      y = NULL, 
      subtitle = "Daily new cases in % of the population", 
      caption  = "sdp: social distance period") +
    scale_x_continuous(labels = x_labs, 
                       breaks = x_labs) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, y_max)) +
    theme(axis.text.y = element_blank(),
          axis.ticks  = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_color_brewer(name = "Interventions", 
                       type = "qual", 
                       palette = 1, 
                       guide = guide_legend(reverse = TRUE)) +
    # sdp 
    geom_vline(xintercept = sdp, lty = 2, color = "darkgray") +
    geom_text(aes(x = sdp[1] + (sdp[2] - sdp[1])/2, 
                  y = y_text, 
                  label = "sdp 1"),
              vjust = 0,
              color = col_sdp) +   
    geom_text(aes(x = sdp[2] + 40, 
                  y = y_text, 
                  label = "sdp 2"),
              vjust = 0,
              color = col_sdp) +   
    geom_segment(aes(
      x = sdp[1],
      y = y_arrow, 
      xend = sdp[2] * 0.99, # shorten
      yend = y_arrow
    ),
    size = 0.3, 
    color = col_sdp,
    arrow = arrow(length = unit(2, "mm"))) +
    geom_segment(aes(
      x = sdp[2]*1.01, # shorten
      y = y_arrow, 
      xend = max_time,
      yend = y_arrow
    ),
    size = 0.3, 
    color = col_sdp,
    arrow = arrow(length = unit(2, "mm")))  +
    # Add final size as table
    annotation_custom(tableGrob(final_sizes, 
                                rows = NULL,
                                theme = ttheme_minimal(
                                  core    = list(fg_params = list(hjust = 0, x = 0.1)),
                                  rowhead = list(fg_params = list(hjust = 0, x = 0))
                                )),
                      xmin = max_time * 0.6,
                      xmax = max_time,
                      ymin = y_max * 0.7,
                      ymax = y_max * 0.9
    )
  
  print(pp)
}


ui <- fluidPage(
  
  titlePanel("Flatten the Curve"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("r0", 
                  div(HTML("Value for R<sub>0</sub>")), 
                  min   = 1.5, 
                  max   = 3.0, 
                  value = 2.25, 
                  step  = 0.25),
      checkboxInput("y_axis_fixed",
                    div(HTML("Fix y axis (helpful to compare different R<sub>0</sub>)")),
                    value = FALSE),
      br(),
      sliderInput("x_max", 
                  "Max days for the model", 
                  min   = 100, 
                  max   = 365, 
                  value = 150,
                  step  =  25),
      hr(),
      br(),
      sliderInput("sdp",
                  "First 'social distance period' (sdp 1)",
                  min   =   0,
                  max   = 150,
                  value = c(30, 60), 
                  step  =   5), 
      br(), br(),
      sliderInput("red_one",
                  "Reduction during first period (sdp 1)",
                  min   = 0.2,
                  max   = 1.0,
                  value = 0.6, 
                  step  = 0.05), 
      sliderInput("red_two",
                  "Reduction after first period (sdp 2)",
                  min   = 0.2,
                  max   = 1.0,
                  value = 0.8, 
                  step  = 0.05), 
      br(),
      hr(),         
      tags$div("Tinu Schneider, 2020"), 
      tags$a(href="mailto:tinu@tinuschneider.ch", "tinu@tinuschneider.ch"),
      br(), br(),
      tags$div("Code is on Github"),
      # br(),
      tags$a(href="https://github.com/tinu-schneider/Flatten_the_Curve", 
             "github.com/tinu-schneider/Flatten_the_Curve"),
      br(), br(),
      tags$div("This work is licensed under a "),
      tags$a(href="http://creativecommons.org/licenses/by-sa/4.0/)", 
             "Creative Commons Attribution-ShareAlike 4.0 International License"), 
      br(),
      img(src="license.png"),
      br()
    ),
    
    mainPanel(
      # p("Note: This tool is not intended to create a prediction."),
      plotOutput("chart", height = "500px"), 
      br(),
      hr(),
      h4("Initial code, mathematical model and idea:"),
      tags$a(href="https://staff.math.su.se/hoehle/blog/2020/03/16/flatteningthecurve.html", 
             "Michael Höhle, 'Flatten the COVID-19 curve'"), 
      p(),
      br()
    )
  )
)


server <- function(input, output) {
  
  
  res <- reactive({
    run(sdp = c(input$sdp), 
        red = c(input$red_one, input$red_two), 
        r0  = input$r0, 
        max_time = input$x_max)
  })
  
  
  output$chart <- renderPlot({
    plot_result(res(), input$sdp, input$x_max, input$y_axis_fixed )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
