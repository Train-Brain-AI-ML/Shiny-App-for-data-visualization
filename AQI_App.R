##############################
##### install libraries######
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("shinyWidgets")
# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("kableExtra")
# install.packages("rsconnect")
# install.packages("cowplot")  

##################### import libraries #############
####################################################
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(rsconnect)
library(cowplot)
library(plotly)


##################################### Server #############################
##########################################################################
server <- function(input, output, session) {
  
  city_day_data = read.csv("clean_city_day_data.csv", header = TRUE,  sep = ",")
  city_day_data$Year <- year(city_day_data$Date)
############################### TAB 2##########################################
###############################################################################
  output$plot1 <- renderPlotly({
    vis1 <- city_day_data %>%  ggplot()
    if(input$feature == "PM2.5"){
      vis1 <- vis1 + geom_col(aes(y=PM2.5, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE )
    }else if(input$feature == "PM10"){
      vis1 <- vis1 + geom_col(aes(y=PM10, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "NO"){
      vis1 <- vis1 + geom_col(aes(y=NO, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "NO2"){
      vis1 <- vis1 + geom_col(aes(y=NO2, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "NOx"){
      vis1 <- vis1 + geom_col(aes(y=NOx, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "NH3"){
      vis1 <- vis1 + geom_col(aes(y=NH3, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "CO"){
      vis1 <- vis1 + geom_col(aes(y=CO, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "SO2"){
      vis1 <- vis1 + geom_col(aes(y=SO2,x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "O3"){
      vis1 <- vis1 + geom_col(aes(y=O3, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "Benzene"){
      vis1 <- vis1 + geom_col(aes(y=Benzene, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "Toluene"){
      vis1 <- vis1 + geom_col(aes(y=Toluene, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "Xylene"){
      vis1 <- vis1 + geom_col(aes(y=Xylene, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }else if(input$feature == "AQI"){
      vis1 <- vis1 + geom_col(aes(y=AQI, x=City, colour=AQI_Bucket, fill=Year),show.legend = FALSE)
    }
    vis1 <- vis1 +theme_bw()+
      ggtitle("Air quality Index of different cities in India")+
            theme(axis.title.x = element_text(colour="Black",size = 12,face = "bold"),
                  axis.title.y = element_text(color = "Black", size = 12,face = "bold"),
                  axis.text.x = element_text(size = 8,face = "bold",colour = "Blue"),
                  axis.text.y = element_text(size = 10,face = "bold", colour = "Blue"),
                  plot.title = element_text(size=15, colour = "Blue"))
                              #input$feature,sep = " ")
    ggplotly(vis1) 
  })
##########################################################################
######################### Tab2 ###########################################
  output$plot2 <- renderPlotly({
    vis2 <- city_day_data %>%  ggplot()
    if(input$feature1 == "PM2.5"){
      vis2 <- vis2 + geom_point(aes(y=PM2.5, x=PM10, colour=Year))
    }else if(input$feature1 == "NO"){
      vis2 <- vis2 + geom_point(aes(y=NO, x=PM10, colour=Year))
    }else if(input$feature1 == "NO2"){
      vis2 <- vis2 + geom_point(aes(y=NO2, x=PM10, colour=Year))
    }else if(input$feature1 == "NOx"){
      vis2 <- vis2 + geom_point(aes(y=NOx, x=PM10, colour=Year))
    }else if(input$feature1 == "NH3"){
      vis2 <- vis2 + geom_point(aes(y=NH3, x=PM10, colour=Year))
    }else if(input$feature1 == "CO"){
      vis2 <- vis2 + geom_point(aes(y=CO, x=PM10, colour=Year))
    }else if(input$feature1 == "SO2"){
      vis2 <- vis2 + geom_point(aes(y=SO2,x=PM10, colour=Year))
    }else if(input$feature1 == "O3"){
      vis2 <- vis2 + geom_point(aes(y=O3, x=PM10, colour=Year))
    }else if(input$feature1 == "Benzene"){
      vis2 <- vis2 + geom_point(aes(y=Benzene, x=PM10, colour=Year))
    }else if(input$feature1 == "Toluene"){
      vis2 <- vis2 + geom_point(aes(y=Toluene, x=PM10, colour=Year))
    }else if(input$feature1 == "Xylene"){
      vis2 <- vis2 + geom_point(aes(y=Xylene, x=PM10, colour=Year))
    }else if(input$feature1 == "AQI"){
      vis2 <- vis2 + geom_point(aes(y=AQI, x=PM10, colour=Year))
    }
    vis2 <- vis2 +theme_bw()+
      ggtitle("Impact of Air substances")+
        theme_bw(base_size = 10)+
        theme(axis.title.x = element_text(colour="Black",size = 15),
              axis.title.y = element_text(color = "Black", size = 15),
              axis.text.x = element_text(size = 8),
              axis.text.y =element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(size=15, colour = "Blue"))
    ggplotly(vis2) 
  })
}
#########################################################################
############################ UI #########################################
ui <- navbarPage(
  "Air quality Index Visualization",
 # titlePanel("Air quality Index Visualization"),
  theme = shinytheme("united"),
 tabsetPanel(

  tabPanel("Main", sidebarLayout(
    sidebarPanel(selectInput(inputId = "feature", label = "Select", 
                               choices = c("PM2.5"="PM2.5",
                                            "PM10"="PM10",
                                            "NO" = "NO",
                                            "NO2"= "NO2",
                                            "NOx"= "NH3",
                                            "CO" = "CO",
                                            "SO2"= "SO2",
                                            "O3" = "O3",
                                            "Benzene"="Benzene",
                                           "Toluene" = "Toluene",
                                            "Xylene" = "Xylene",
                                            "AQI" = "AQI"))),
    mainPanel(
           plotlyOutput("plot1",height="500px",width = "100%")
  ))
  ),
  tabPanel("Data Visualization", sidebarLayout(
    sidebarPanel(selectInput(inputId = "feature1", label = "Select feature", 
                             choices = c("PM2.5"="PM2.5",
                                         "NO" = "NO",
                                         "NO2"= "NO2",
                                         "NOx"= "NH3",
                                         "CO" = "CO",
                                         "SO2"= "SO2",
                                         "O3" = "O3",
                                         "Benzene"="Benzene",
                                         "Toluene" = "Toluene",
                                         "Xylene" = "Xylene",
                                         "AQI" = "AQI"))),
    mainPanel(
      plotlyOutput("plot2",height="500px",width = "100%")
    ))
  ),

  #tabPanel("Data Visualization",fluidRow(plotlyOutput("plot2"))),

################################### TAB 3#########################################
#################################################################################
 tabPanel("Reference", h2("Air quality Index of India",style="color:blue"), 
            h4("This dashboard shows the air quality index of different cities in India. 
            In this dashboard, the dataset has been used which is taken from Kaggle, 
            and this is the reference link:"),
            h4("Air Quality Data in India (2015 - 2020). Kaggle.com. (2021). Retrieved 18 October 2021,
               from https://www.kaggle.com/rohanrao/air-quality-data-in-india."),
            h4("GGplot code refeerence:Page, g. (2021). ggplot2 - Easy Way to Mix Multiple Graphs on The Same Page - Articles - STHDA. Sthda.com. 
            Retrieved 18 October 2021, from http://sthda.com/english/articles/24-ggpubr-publication-ready-plots/
               81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page.")
         
) 
)
)
###################### shiny App #######################################
shinyApp(ui = ui, server = server)

