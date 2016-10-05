# Homebound Visits Web App

# Libraries

        library(shiny)
        library(shinyjs)
        library(shinydashboard)
        library(leaflet)
        library(dplyr)
        library(lubridate)
        library(DT)
        library(ggplot2)
        library(plotly)
        library(geosphere) # To calculate miles from lat long

# Data

        download.file("https://docs.google.com/spreadsheet/pub?key=0AgOyIYCzS_DMdEpNZGF4b040WG4zYkxJd0lRRWEtTlE&single=true&gid=2&output=csv",destfile="visits.csv")
        dfVisits <- tbl_df(read.csv("visits.csv"))
        names(dfVisits)[1] <- c("ID")
        dfVisits$ID <- as.numeric(as.character(dfVisits$ID))
        
        download.file("https://docs.google.com/spreadsheet/pub?key=0AqP7_vsrE9jldGhDcWllMk8wZEZNSXcyQS15YTM3QUE&single=true&gid=4&output=csv",destfile="directory.csv")
        dfDirectory <- tbl_df(read.csv("directory.csv"))
        
        df1 <- left_join(dfVisits, dfDirectory) %>% # df1 = All Visits
                mutate(Visit.Date = as.Date(Date, format = "%m/%d/%Y"),
                       Wks.Since.Visit = as.numeric(Sys.Date() - Visit.Date)/7)
        
        df2 <- df1 %>% # df2 = Last Visit
                group_by(ID) %>% 
                filter(rank(Wks.Since.Visit) == 1) %>% 
                mutate(Full.Name = paste0(Last.Name,", ",First.Name)) %>% 
                arrange(desc(Wks.Since.Visit))

shinyServer(function(input, output, session){
        
        # User Flow
        
        # observeEvent(input$linkMap, {
        #         shinyjs::toggle(id = "divMap", anim = TRUE, animType = "Slide")
        # })
        
        observeEvent(input$linkLastVisits, {
                shinyjs::toggle(id = "divLastVisits", anim = TRUE, animType = "Slide")
        })
        
        observeEvent(input$linkAllVisits, {
                shinyjs::toggle(id = "divAllVisits", anim = TRUE, animType = "Slide")
        })
        
        observeEvent(input$linkReport, {
                shinyjs::toggle(id = "divReport", anim = TRUE, animType = "Slide")
        })
        
        # Last Visits
        
        dfLastVisits <- reactive({
                
                dfLastVisits <- ungroup(df2) %>% 
                        mutate(Wks = round(Wks.Since.Visit,1)) %>% 
                        # mutate(longHome = -76.456975, latHome = 40.323069)  %>% 
                        # mutate(Est.Mi = round(1.3 * 0.62137 * distm (matrix(dfLastVisits$longHome, dfLastVisits$latHome), matrix(dfLastVisits$Longitude, dfLastVisits$Latitude), fun = distVincentyEllipsoid) / 1000, 1)) %>%
                        select(Full.Name, Congregation, Date, Address, Room.Number, Phone, Wks) %>% 
                        arrange(desc(Wks))
                
        })
        
        output$dfLastVisits <- DT::renderDataTable({
                
                dfLastVisits()
                
        }, filter="bottom", options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20, 50, 100)))
        
        # Map
        
        output$mymap <- renderLeaflet({
                
                mySelected <- as.numeric(input$dfLastVisits_rows_selected)
                print(mySelected)
                
                dfLeaflet2 <- df2
                dfLeaflet2 <- na.omit(dfLeaflet2)
                dfLeaflet2 <- dfLeaflet2 %>% select(Full.Name, Latitude, Longitude)
                
                dfLeaflet <- dfLastVisits()
                if(length(mySelected)) {dfLeaflet <- dfLeaflet[mySelected,]}
                dfLeaflet <- inner_join(dfLeaflet, dfLeaflet2)
                
                # print.data.frame(dfLeaflet)
                
                factpal <- colorFactor( c(
                        rgb(27,158,119, maxColorValue = 255),
                        rgb(217,95,2, maxColorValue = 255),
                        rgb(117,112,179, maxColorValue = 255),
                        rgb(231,41,138, maxColorValue = 255)
                ), df2$Congregation)
                
                leaflet() %>%
                        setView(lng= -76.418, lat= 40.325 , zoom = 10) %>%
                        addTiles() %>%  
                        addMarkers(lng=dfLeaflet$Longitude, lat=dfLeaflet$Latitude, popup = dfLeaflet$Full.Name)
                
        })
        
        # All Visits
        
        dfAllVisits <- reactive({
                dfAllVisits <- df1
                dfAllVisits
        })
        
        output$dfAllVisits <- DT::renderDataTable({
                
                dfAllVisits <- dfAllVisits() %>%
                        mutate(Wks = round(Wks.Since.Visit,1)) %>% 
                        select(Last.Name, First.Name, Congregation, Date, Wks, Comment)
                dfAllVisits
                
        }, filter="bottom")
        
        
        # Summary Stats by Congregation, by Month
        
        dfReport <- reactive({
                
                dfReport <- df1 %>% 
                        
                        select(Last.Name, First.Name, Visit.Date, Congregation) %>% 
                        mutate(Year = year(Visit.Date),
                               Month = month(Visit.Date),
                               Full.Name = paste0(Last.Name,", ",First.Name)) %>% 
                        group_by(Congregation, Year, Month) %>% 
                        mutate(Total.Visits = n()) %>% 
                        select(Full.Name, Congregation, Year, Month, Total.Visits) %>% 
                        arrange(Full.Name, Year, Month) %>% 
                        unique() %>% 
                        mutate(Unique.Visits = n()) %>% 
                        select(-Full.Name) %>% 
                        unique()
                
                
                dfReport
                
                
        })
        
        output$dfReport <- DT::renderDataTable({
                
                dfReport <- dfReport() 
                dfReport <- dfReport %>% 
                        ungroup() %>% 
                        arrange(desc(Year), desc(Month))
                dfReport
                
                datatable(dfReport, extensions = c('Buttons'), 
                          options = list(dom="Bflrtip", 
                                         pageLength = 5,
                                         lengthMenu = c(5,10,20,50,100),
                                         buttons = c('copy', 'excel')),
                                        # scrollX = TRUE,
                                        # deferRender = TRUE,
                                        # scrollY = 300,
                                        # scroller = TRUE,
                                        # filter="bottom"),
                          filter="bottom",
                          caption = "Open in Browser to Use Download Buttons")
                
        })
        
        

        
        output$myPlot <- renderPlotly({

                dfPlot <- dfReport()
                dfPlot$Date = as.Date(ymd(paste(as.character(dfPlot$Year), as.character(dfPlot$Month), "15", sep="-")))
                dfPlot <- dfPlot %>% filter(Congregation %in% c("Hill","Salem","Zion","St. James"))
               
                p <- ggplot(data=dfPlot, aes(x=Date, y=Total.Visits)) + facet_wrap(~Congregation, scales = "free")
                p <- p + geom_line(col="blue")
                p <- p + expand_limits(y=0) + xlab("") + ylab("")
                p <- p + geom_line(data=dfPlot, aes(x=Date, y=Unique.Visits), color="orange", lty=3)
                p <- p + theme_minimal()
                
                p <- ggplotly(p)
                
                # print(p)
                
                
        })
        
        
})