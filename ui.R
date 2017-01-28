
# Homebound Visits Web App


# Libraries
        
        library(shiny)
        library(shinyjs)
        library(shinydashboard)
        library(leaflet)
        library(DT)
        library(plotly)

# UI
        
myHeader <- dashboardHeader(disable = TRUE)
mySide <- dashboardSidebar(disable = TRUE)
        
myBody <- dashboardBody(
        
        useShinyjs(),
        
        fluidPage(
                
                box(title="LLCM Visitation Web App", width=12, height=50, background = "blue"),
                
                column(
                        width=9,
                        
                        div(id="divBday",
                          
                            box(
                                    width = 12, title = "Birthdays",
                                    DT::dataTableOutput("dfBday")
                            )  
                            
                        ),
                        
                        
                        div(
                                id="divLastVisits",
                                box(
                                        width=12, title="Last Visits",
                                        DT::dataTableOutput("dfLastVisits"),
                                        leafletOutput("mymap") 
                                )
                        ),
                        
                        shinyjs::hidden(
                                div(
                                        id="divAllVisits",
                                        box(
                                                width=12, title = "All Visits",
                                                DT::dataTableOutput("dfAllVisits")
                                        )
                                )
                        ),
                        
                        shinyjs::hidden(
                                div(
                                        id="divReport",
                                        box(
                                                width=12, title = "Monthly Report",
                                                # tags$p("Open in Browser to Use Download Buttons"),
                                                DT::dataTableOutput("dfReport")
                                        ),
                                        box(
                                                width=12,
                                                plotlyOutput("myPlot")         
                                        )
                                )
                        )
                        
                ),
                
                
                column(
                        width = 3, 
                        
                        fixedPanel(
                                
                                tags$b("Show / Hide Options"),
                                tags$br(),
                                actionLink(inputId = "linkLastVisits", "Last Visits"),
                                # tags$br(),
                                # actionLink(inputId = "linkMap", "Map"),
                                tags$br(),
                                actionLink(inputId = "linkAllVisits", "All Visits"),
                                tags$br(),
                                actionLink(inputId = "linkReport", "Monthly Report"),
                                
                                draggable = TRUE, cursor = c("auto")
                                
                        )
                )
        )
)


dashboardPage(header = myHeader, sidebar = mySide, body = myBody)

