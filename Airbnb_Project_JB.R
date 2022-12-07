library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(leaflet)
library(htmltools)


airbnb <- read.csv('https://raw.githubusercontent.com/bustosjose/nyc_airbnb_dashboard/main/airbnb_nyc_clean.csv?token=GHSAT0AAAAAABX5VYBG4EIXG435Q6AL52FYY2UIXQA',stringsAsFactors = F,header=T)




ui <- dashboardPage(
skin = "blue",
  dashboardHeader(title = strong("airbnb NYC")),
  dashboardSidebar( sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("NYC Boroughs", tabName = "borough", icon = icon("bar-chart-o")),
    menuItem( 'Map',tabName = 'map',icon=icon('map')),
    menuItem("Visit airbnb NY", icon = icon("plane"), 
             href = "https://www.airbnb.com/new-york-ny/stays")
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName= 'dashboard',
              frow1 <- fluidRow(
                valueBoxOutput("value1"),
                valueBoxOutput('value2'),
                valueBoxOutput('value3')),
              fluidRow( 
                box(
                  title = "Average Rating by Borough"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotOutput("dashgraph1", height = "300px")),
                box(
                  title = "Average Price by Borough"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotOutput("dashgraph2", height = "300px")),
                box(
                  title = "Average Service Fee by Borough"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotOutput("dashgraph3", height = "300px")),
                box(
                  title = "Average Minimum Night Stay by Borough"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,leafletOutput("nycmap2", height = "300px")))),
      
      
      
      tabItem(tabName = 'borough',
              selectizeInput("ny_b",
                             "Borough",
                             choices = unique(sort((airbnb$neighbourhood_group)))),
              fluidRow(
              box(plotOutput("bgraph1"),
                  collapsible = TRUE),
              box(plotOutput("bgraph2"),
                  collapsible = TRUE),
              box(plotOutput('bgraph3'),
                  collapsible = TRUE),
              box(plotOutput("bgraph4"),
                  collapsible = TRUE))),
      

      tabItem(tabName = 'map',
              leafletOutput("nycmap"),
              fluidRow(
                column(4,
                       selectizeInput("borough",
                                      "Borough",
                                      choices = unique(sort((airbnb$neighbourhood_group)))),

                      
                       sliderInput("price_range",
                                   "Price Range:",
                                   min = 60,
                                   max = 1200,
                                   value = c(300,700)),
                       sliderInput("avg_night",
                                   "Night Range:",
                                   min = 0,
                                   max = 13,
                                   value = c(1,5))),
                column(6,htmlOutput("text"))

        )
      )
    )
  )
)
    
    
  


server <- function(input, output) {

 #DATA FOR FIRST VALUE BOX
 avg_stay <- airbnb %>% group_by(neighbourhood_group) %>% 
   summarize(price = mean(price)) %>% filter (price == max(price))
 avg_stay$price <- round(avg_stay$price, digits = 2)

 #VALUE BOX 1
 output$value1 <- renderValueBox({
   valueBox(
     formatC(avg_stay$neighbourhood_group)
     ,paste('Max Average Stay: $', avg_stay$price)
     ,icon = icon("usd",lib='glyphicon')
     ,color = "purple")  
 }) 
 
 #DATA FOR SECOND VALUE BOX 
 listings <- airbnb %>% group_by(neighbourhood_group) %>% 
   summarize(list_num = sum(calculated_host_listings_count)) %>% filter (list_num == max(list_num))

  #VALUE BOX 2
 output$value2 <- renderValueBox({
   valueBox(
     formatC(listings$list_num, format="d", big.mark=',')
     ,paste('Max Borough Listings:', listings$neighbourhood_group)
     ,icon = icon("stats",lib='glyphicon')
     ,color = "purple")  
 })   
 
 #DATA FOR THIRD VALUE BOX
 rooms <- airbnb %>% group_by(room_type) %>% 
   summarise(room = n()) %>% filter (room == max(room))
 
 #VALUE BOX THREE
 output$value3 <- renderValueBox({
   valueBox(
     formatC(rooms$room, format="d", big.mark=',')
     ,paste('Max Room Listing Type:', rooms$room_type)
     ,icon = icon("stats",lib='glyphicon')
     ,color = "purple")  
 })  
 
 
 
 #########################
 
 
 #DASHBOARD GRAPHS
 
 dash_data <- 
   airbnb %>%
     group_by(neighbourhood_group) %>%
     summarize(avg_rating = mean(review_rate_number),
               avg_num_reviews = mean(number_of_reviews),
               avg_night = mean(minimum_nights),
               avg_price = mean(price),
               avg_fee = mean(service_fee)) %>%
   mutate(total=round(avg_fee,2),
          rate_total = round(avg_rating,1),
          price_total = round(avg_price,2))

   
 output$dashgraph1 <- renderPlot({
   ggplot(
     dash_data, aes(x=neighbourhood_group,y=avg_rating)) + 
     geom_col(fill = "#674ea7") +
     theme_bw()+
     geom_text(aes(label=paste0(rate_total)))+
     labs(title = "Average Rating",
          subtitle = "Average Rating by Borough",
          x="\nBorough\n",
          y="\nAverage Rating\n",
          caption="\nSource: http://insideairbnb.com/explore/")+
     theme(
       plot.title=element_text(hjust=.5,size = 20),
       plot.subtitle=element_text(hjust=.5,size = 10),
       axis.text.y=element_text(size=10),
       axis.title.x=element_text(size=13),
       axis.title.y=element_text(size=13))
 })
 
 output$dashgraph2 <- renderPlot({
   ggplot(
     dash_data, aes(x=neighbourhood_group,y=avg_price)) + 
     geom_col(fill = "#674ea7") +
     theme_bw()+
     geom_text(aes(label=paste0('$',price_total)))+
     labs(title = "Average Price",
          subtitle = "Average Price by Borough",
          x="\nBorough\n",
          y="\nAverage Price\n",
          caption="\nSource: http://insideairbnb.com/explore/")+
     theme(
       plot.title=element_text(hjust=.5,size = 20),
       plot.subtitle=element_text(hjust=.5,size = 10),
       axis.text.y=element_text(size=10),
       axis.title.x=element_text(size=13),
       axis.title.y=element_text(size=13))+
     scale_y_continuous(labels=scales::dollar_format())
 })
 
 output$dashgraph3 <- renderPlot({
   ggplot(
     dash_data, aes(x=neighbourhood_group,y=avg_fee)) + 
     geom_col(fill = "#674ea7") +
     theme_bw()+
     geom_text(aes(label=paste0('$',total)))+
     labs(title = "Average Service Fee",
          subtitle = "Average Service Fee by Borough",
          x="\nBorough\n",
          y="\nAverage Service Fee\n",
          caption="\nSource: http://insideairbnb.com/explore/")+
     theme(
       plot.title=element_text(hjust=.5,size = 20),
       plot.subtitle=element_text(hjust=.5,size = 10),
       axis.text.y=element_text(size=10),
       axis.title.x=element_text(size=13),
       axis.title.y=element_text(size=13))+
     scale_y_continuous(labels=scales::dollar_format())
 })
 

 
 ################################
 
 
 b_data <- reactive({
   airbnb %>%
     filter(neighbourhood_group == input$ny_b)
 })
 
 output$bgraph1 <- renderPlot({
   ggplot(
     b_data() %>%
       group_by(room_type) %>%
       summarize(room_count = n()) %>%
       mutate(total=room_count), aes(x=room_type,y=room_count)) + 
     geom_bar(stat="identity", fill = "#674ea7") +
     theme_bw()+
     geom_text(aes(label=paste0(total,'\n')))+
     labs(title = "Listing Type",
          subtitle = "Room Listing Type Total by Borough",
          x="\nListing Type\n",
          y="\nTotal Count\n",
          caption="\nSource: http://insideairbnb.com/explore/")+
     theme(
       plot.title=element_text(hjust=.5,size = 20),
       plot.subtitle=element_text(hjust=.5,size = 10),
       axis.text.y=element_text(size=10),
       axis.title.x=element_text(size=13),
       axis.title.y=element_text(size=13))
 })
 

 output$bgraph2 <- renderPlot({
   ggplot(
     b_data() %>%
       group_by(construction_year) %>%
       summarize(cons_count = n(),
                 avg_rating = mean(review_rate_number)) %>%
       mutate(total=cons_count), aes(x=construction_year,y=cons_count, fill = avg_rating)) + 
     geom_bar(stat="identity") +
     theme_bw()+
     scale_fill_distiller(palette = "Purples", direction=1)+
     geom_text(aes(label=paste0(total,'\n')))+
     labs(title = "Year Listing Built",
          subtitle = "Year Listing Building Built & Average Rating",
          x="\nYear Built\n",
          y="\nTotal Count\n",
          caption="\nSource: http://insideairbnb.com/explore/")+
     theme(
       plot.title=element_text(hjust=.5,size = 20),
       plot.subtitle=element_text(hjust=.5,size = 10),
       axis.text.y=element_text(size=10),
       axis.title.x=element_text(size=13),
       axis.title.y=element_text(size=13),
       legend.position="top")+
     labs(fill = "Average Rating")
 })

 

 
 
 output$bgraph3 <- renderPlot({
   ggplot(
     b_data() %>%
       group_by(neighbourhood) %>%
       summarize(avg_rating = round(mean(review_rate_number),2)) %>%
       mutate(total=avg_rating) %>%
       arrange(desc(avg_rating)) %>% 
       slice(1:10), aes(x=neighbourhood,y=avg_rating)) + 
     geom_bar(stat="identity", fill = "#674ea7") +
     theme_bw()+
     geom_text(aes(label=paste0(total,'\n')))+
     labs(title = "Top Rated Neighborhoods",
          subtitle = "Top 10 Best Rated Neighborhoods in a Borough",
          x="\nNeighborhood\n",
          y="\nAverage Rating\n",
          caption="\nSource: http://insideairbnb.com/explore/")+
     theme(
       plot.title=element_text(hjust=.5,size = 20),
       plot.subtitle=element_text(hjust=.5,size = 10),
       axis.text.y=element_text(size=10),
       axis.title.x=element_text(size=13),
       axis.title.y=element_text(size=13))+
     coord_flip()
 })
 
 
 output$bgraph4 <- renderPlot({
   ggplot(
     b_data() %>%
       group_by(cancellation_policy) %>%
       summarize(can_poly = n()) %>%
       mutate(total=can_poly), aes(x=cancellation_policy,y=can_poly)) + 
     geom_bar(stat="identity",fill = "#674ea7") +
     theme_bw()+
     geom_text(aes(label=paste0(total,'\n')))+
     labs(title = "Cancellation Policy",
          subtitle = "Cancellation Policy by Borough",
          x="\nCancellation Type\n",
          y="\nNumber of Listings\n",
          caption="\nSource: http://insideairbnb.com/explore/")+
     theme(
       plot.title=element_text(hjust=.5,size = 20),
       plot.subtitle=element_text(hjust=.5,size = 10),
       axis.text.y=element_text(size=10),
       axis.title.x=element_text(size=13),
       axis.title.y=element_text(size=13))
 })
 
 
 
 
 
  
  
  
  ##################################
  
  
  
  output$text <- renderText({
    paste('
    <B>Interactive Map:</B> <br>
    New York interactive map will allow you to view all neighborhoods within New York boroughs.
    Hoovering over each marker will provide additional information.
    The marker color represents the average price of Airbnb rental by neighborhood, use legend as a reference.</br><br>
    <B>Make a Selection:</B> </br>
    Use the slidebar and drop down menus to make selection based on your stay preference.
    Use the slidebar to select the Price Range and Night Range. 
    Use the drop down menu to select a Borough.<br></br><br>
    <B>Errors</B></br>
    Certain range selections may give an error, try another range.')
    
  })
  
  
  #create new data set with all averages to be used in the map
  #use filter to select user input for borough
  #create labels for the circle markers 
  map_data <- reactive({     
    airbnb %>% filter(neighbourhood_group==input$borough) %>%
      group_by(neighbourhood_group,neighbourhood) %>% 
      summarise(
        long=mean(long),
        lat=mean(lat),
        avg_price=round(mean(price),0),
        avg_nights=mean(minimum_nights)) %>%
      mutate(info_labels = paste0("$",as.character(round(avg_price,0)),"/night in, ",
                                 neighbourhood," ",neighbourhood_group))
  })

  #create a color palate for the circle markers
  #representing the avg price
  pal <- colorNumeric(
    palette = "BuPu",
    domain = range(0,1200))
  
  #create the nycmap using Leaflet
  #use filter for user input price range
  output$nycmap <- renderLeaflet({
    map_data() %>%
      filter(avg_nights>=input$avg_night[1] & avg_nights<=input$avg_night[2]) %>%
      filter(avg_price>=input$price_range[1] & avg_price<=input$price_range[2]) %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addCircleMarkers(~long, ~lat,
                       color = ~pal(avg_price),
                       fillOpacity = 3,
                       radius= 4,
                       label= ~info_labels) %>%
      addLegend(pal=pal, 
                values = ~avg_price,
                opacity = 1,
                position = "topright",
                title = "Average Price")
    
  })
  
  
  #######################
  
  #map for dashboard on min night stay per borough
  map_data2 <-     
    airbnb %>% 
    group_by(neighbourhood_group) %>% 
    summarise(
      long=mean(long),
      lat=mean(lat),
      avg_nights=round(mean(minimum_nights))) %>%
    mutate(info_labels = paste0("An average of ", avg_nights, 
                                " minimum night stay per listing in ",neighbourhood_group))
    
  output$nycmap2 <- renderLeaflet({
    map_data2 %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addMarkers(~long, ~lat,
                       label= ~info_labels)
    
  }) 
  
}
shinyApp(ui, server)
