##############################
##### install libraries######
install.packages("shiny")
install.packages("shinythemes")
install.packages("shinyWidgets")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("kableExtra")
install.packages("rsconnect")
install.packages("cowplot")

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

##################################### Server #############################
##########################################################################
server <- function(input, output, session) {
  
  city_day_data = read.csv("clean_city_day_data.csv", header = TRUE,  sep = ",")
############################### TAB 2##########################################
###############################################################################
  output$plot1 <- renderPlot({
    p<-ggplot(data = city_day_data,aes(City, PM10, fill = AQI_Bucket)) + 
      geom_col(position = "dodge") + 
      ggtitle("Air quality Index of different cities in India according to different particle")+
      theme(axis.title.x = element_text(colour="Black",size = 15),
            axis.title.y = element_text(color = "Black", size = 15),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size=22, colour = "Blue"))
###########################################################################
###########################################################################
    q<-ggplot(data = city_day_data,aes(City, PM2.5, fill = AQI_Bucket)) + 
      geom_col(position = "dodge") + 
      #ggtitle("Air quality Index of different cities")+
      theme(axis.title.x = element_text(colour="Black",size = 15),
            axis.title.y = element_text(color = "Black", size = 15),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10))
############################################################################
############################################################################
    r<-ggplot(data = city_day_data,aes(City,NO2, fill = AQI_Bucket)) +
      geom_line(aes(colour=AQI_Bucket))+
      theme(axis.title.x = element_text(colour="Black",size = 15),
            axis.title.y = element_text(color = "Black", size = 15),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10))
###########################################################################
###########################################################################
    s<-ggplot(data = city_day_data,aes(City,AQI, fill = AQI_Bucket)) + 
      geom_line(aes(colour=AQI_Bucket))+ 
      theme(axis.title.x = element_text(colour="Black",size = 15),
            axis.title.y = element_text(color = "Black", size = 15),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            legend.title = element_text(size=10),
            legend.text = element_text(size=10),
            legend.position = c(1,1),
            legend.justification = c(1,1))
###########################################################################
###########################################################################
    t<-ggplot(data = city_day_data,aes(City,O3, fill = AQI_Bucket)) +
      geom_line(aes(colour=AQI_Bucket))+
      theme(axis.title.x = element_text(colour="Black",size = 15),
            axis.title.y = element_text(color = "Black", size = 15),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),)
###########################################################################
#########################plot all features ################################
    plot_grid(p,r,s,t,q, labels = "AUTO",ncol = 1)
  })
##########################################################################
######################### Tab2 ###########################################
  output$plot2 <- renderPlot({
  city_day_data1 <- city_day_data ##### make copy of dataset
  city_day_data1$Year <- year(city_day_data1$Date) ######## extract year from dataset
  city_day_data1 %>%
    ggplot(aes(y= Year, x = City)) +
    geom_bar(stat = "identity", aes(fill = AQI_Bucket)) +
    facet_wrap(~Year, ncol = 1) +
    ggtitle("Air Quality Index according to years in different cities of india")+
    theme_bw(base_size = 10)+
    theme(axis.title.x = element_text(colour="Black",size = 15),
          axis.title.y = element_text(color = "Black", size = 15),
          axis.text.x = element_text(size = 10),
          axis.text.y =element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(size=22, colour = "Blue"))
  })
}
#########################################################################
############################ UI #########################################
ui <- navbarPage(
  "Air Quality Index",
  theme = shinytheme("flatly"),
 #tabsetPanel(

  tabPanel("Main", fluidRow(plotOutput("plot1",height="500px",width = "100%"))),
  tabPanel("Data Visualization",fluidRow(plotOutput("plot2", height="500px",width = "100%")) ),

 ################################### TAB 3#########################################
#################################################################################
 tabPanel("Reference", h2("Air quality Index of India"), 
            h4("This dashboard shows the air quality index of different cities in India. 
            In this dashboard, the dataset has been used which is taken from Kaggle, 
            and this is the reference link:
            https://www.kaggle.com/rohanrao/air-quality-data-in-india."),
            h4("GGplot code refeerence: http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/")
         
) 
#)
)
###################### shiny App #######################################
shinyApp(ui = ui, server = server)

