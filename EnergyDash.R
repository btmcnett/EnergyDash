#
# Ben McNett
# Energy Consumption Dashboard
# ESCI 599 App Development

# ----------

# Packages

library(shiny)
library(tidyverse)
library(shinythemes)
library(PNWColors)
library(dplyr)
library(networkD3)
library(devtools)
library(ggsankey)
library(lubridate)
library(showtext)
library(tigris)
library(stringr)
library(sf)
library(tmap)
library(lubridate)
library(shinyWidgets)
library(DT)


# ----------

# Data Loading and Pre-processing

# ----------

# state level yearly consumption
stateData <- read.csv("statedata.csv")
stateData <- stateData %>%
  mutate(State = str_trim(State))


## gets state info with polygon
#states <- states(cb = TRUE)
## renames col
#states$State <- states$STUSPS

## extracts only the useful stuff
#states <- states %>% select(State, geometry)
#saveRDS(states, file = "states.rds")
states <- readRDS("states.rds")


## joins it to the state data
joinedStates <- left_join(states, stateData, by = "State")
## gets rid of territories that aren't included in the energy data
joinedStates <- joinedStates %>% 
  na.omit(joinedStates) %>% 
  subset(State != "HI") %>% 
  subset(State != "AK")


# tidy data format
stateData <- joinedStates %>%
  pivot_longer(cols = -c("State", "geometry"),
               names_to = "Year",
               values_to = "Consumption") %>%
# fixes formatting

  ## extracts numbers from string
  mutate(Year = parse_number(Year)) %>%
  ## makes it numeric
  mutate(Years = as.numeric(Year)) %>%
  ## gets rid of excess spaces
  mutate(Consumption = as.numeric(gsub(",", "", Consumption))) %>%
  ## year date format
  mutate(YearDate = ymd(Years, truncated = 2L))






energyOverviewRaw <- read.csv("MER_T01_03.csv")

energyOverview <- energyOverviewRaw %>% 
  # make a new column  called year
  mutate(Year=as.numeric(str_sub(string = YYYYMM,start = 1,end = 4))) %>%
  # make a new column  called month
  mutate(Month=as.numeric(str_sub(string = YYYYMM,start = 5,end = 6))) %>%
  # keep month 13 which is annual
  filter(Month == 13) %>%
  # keep only three columns
  select(Year,Value,Description)

energyOverview <- energyOverview %>%
  filter(Year > 1943)
# check against data
#https://www.eia.gov/totalenergy/data/browser/index.php?tbl=T01.03#/?f=A&start=2021&end=2022&charted=1-2-3-5-12



# clean it up
energyOverview <- energyOverview %>% 
  # remove descriptions we don't want
  filter(Description != "Total Renewable Energy Consumption") %>%
  filter(Description != "Total Primary Energy Consumption") %>% 
  filter(Description != "Total Fossil Fuels Consumption") %>%
  # remove " Consumption" from descriptions 
  mutate(Description = str_remove(Description,pattern = " Consumption")) %>%
  # and remove the text inside parentheses -- this uses a regular expression.
  mutate(Description = str_remove(Description,pattern = " \\(.*\\)"))
# Check levels
unique(energyOverview$Description)

# make a Fossil Fuel and Renewable variable and a deacade variable
energyOverview <- energyOverview %>% 
  mutate(Category = case_when(Description %in%  c("Biomass Energy",
                                                  "Geothermal Energy",
                                                  "Hydroelectric Power",
                                                  "Solar Energy",
                                                  "Wind Energy")  ~ "Renewable",
                              Description %in%  c("Coal",
                                                  "Natural Gas",
                                                  "Petroleum") ~ "Fossil Fuel",
                              Description %in%  "Nuclear Electric Power" ~ "Nuclear")) %>%
  mutate(Decade = case_when( Year >= 1940 & Year < 1950 ~ "1940-1949",
                             Year >= 1950 & Year < 1960 ~ "1950-1959",
                             Year >= 1960  & Year < 1970 ~ "1960-1969",
                             Year >= 1970  & Year < 1980 ~ "1970-1979",
                             Year >= 1980 & Year < 1990 ~ "1980-1989",
                             Year >= 1990 & Year < 2000 ~ "1990-1999",
                             Year >= 2000  & Year < 2010 ~ "2000-2009",
                             Year >= 2010  & Year < 2020 ~ "2010-2019",
                             Year >= 2020 ~ "2020+"))


# rename some stuff and write out
energyOverview <- energyOverview %>% rename(Source = Description, Quads = Value)

energyOverview$YearDate <- ymd(energyOverview$Year, truncated = 2L)

# color scheme

## grab unique sources
sources <- unique(energyOverview$Source)
## sort alphabetically
sources <- sort(sources)
## palette
revcol <- rev(pnw_palette("Bay", length(sources)))

palette <- setNames(revcol, sources)



#tables <- energyOverview %>%
  
#  filter(Year == 2022) %>%
#  mutate(Quads = str_remove(Quads, "Not Available" ),
         # removes the NA values in character format
#         QuadsN = as.double(Quads)) %>%
#  mutate(pct = (QuadsN / sum(QuadsN)))



  # finds the porportion of the mix per year


# ----------

# APP

# ----------

## UI



ui <- fluidPage(
  # sets theme
  theme = shinytheme("journal"),
  setBackgroundImage(src = "back2.jpg"),
  h1("U.S. Energy Consumption Dashboard"),
  h5("Built by Ben McNett"),
  h5("ESCI 599: App Development"),
  hr(),
    navbarPage(title = "Energy Dashboard",

  # consumption over all resources
        tabPanel("Introduction",
                 p("This dashboard allows users to observe the historical consumption and mixes of energy in the United States over a date range of the user's choosing. The Consumption by Source panel allows users to view the consumption (in Quad BTUs) over the selected date range, with the option to select or de-select each source. Users can change the Y-axis Variable to show either consumption or the percentage mix corresponding to how present the energy source was in yearly consumption. The table shows the total consumption of each selected source over time, as well as the percentage for total consumption for each resource, relative to the sum of the consumption of all selected resources. Consumption by Source shows similar information, but allows users to compare three categories of sources, being renewables, fossil fuels, and nuclear. Note that the mix for Nuclear will be 100%, as it only has one source in it's category. The Ranked Consumption panel presents the same information as in the consumption by source panel, but presents the data using a Sankey Bump Diagram, which orders the sources (highest consumption at the top to lowest at the bottom). Lastly, the Consumption by State panel shows a map of the lower 48 states with the yearly consumption per capita (in millions of BTUs). Users can drag the slider to a certain year, and the table will depict show the amount consumed for that year. Breaks were defined using the Jenks method.", br(), br(), "Data is from the U.S. Energy Information Administration (EIA).", br(), "The dataset used for the Consumption by Source, Consumption by Category, and Ranked Consumption panels is from", tags$a("1.3 (Primary Energy Consumption by Source)", href = "https://www.eia.gov/totalenergy/data/monthly/#summary"), "with a time scale at both yearly and monthly increments.",br(),  "The dataset used for the Consumption by State panel is from the", tags$a("State Energy Data System (SEDS): 1960-2022, Ranked by state: Total energy consumption per capita by end-use sector.", href = "https://www.eia.gov/state/seds/seds-data-complete.php#Consumption"), br(),br(), 

"Background photograph is by Charlie Riedel, AP", br(),
"Introduction photograph is by Erika Schultz, Seattle Times", style = "background-color: #FAF9F6; padding: 5px; border: 3px solid #56382E"),
                 fluidRow(
                   column(6, offset = 3,
                          div(
                            img(src = "ErikaSchultzST.jpg", height = "auto", width = "100%"),
                            style = "border: 5px solid white;"
                          )
                   )
                 )
                  ),
        tabPanel("Consumption by Source",
                 hr(),
          sidebarLayout(
            mainPanel(width = 9,
                      plotOutput("consumptionPlot")),

    # First row containing the product, selecting code and title
            sidebarPanel(width = 3,
                         tagList(
                         pickerInput("Source1", "Source",
                                     choices = unique(energyOverview$Source),
                                     width = "100%",
                                     selected = unique(energyOverview$Source),
                                     multiple = TRUE,
                                     options = list(
                                        `actions-box` = TRUE,
                                        `live-search` = TRUE
                                     )
            ),
        
              
            sliderInput("YearDate2", "Select Date Range",
                           min = min(energyOverview$YearDate),
                           max = max(energyOverview$YearDate),
                           value = c(min(energyOverview$YearDate), max(energyOverview$YearDate)),
                           timeFormat = "%Y"
            ),
              
              selectInput("y1", "Y-axis Variable", c("Consumption (Quad BTU)" = "QuadsN", "Mix (%)" = "Percentage")),
            
            br(),
            h5("Total Consumption Over Date Range"),            
            dataTableOutput("consumptionTable")
            
                         )
              
              
            )
            

        
        

          ) # closes sidebar
        ), # closes tab Panel
  
  
  # consumption for just the fossil fuels/renewable
        tabPanel("Consumption by Category",
                 hr(),
          sidebarLayout(
            
            mainPanel(width = 9,
                      plotOutput("ffPlot")),
            
            
            # First row containing the product, selecting code and title
            sidebarPanel(width =3,
                         tagList(
              selectInput("Category", "Category",
                                     choices = c("Fossil Fuel", "Renewable", "Nuclear"),
                                     width = "100%",
                                     selected = "Fossil Fuel"
            ),
            
            
            sliderInput("YearDate3", "Select Date Range",
                           min = min(energyOverview$YearDate),
                           max = max(energyOverview$YearDate),
                           value = c(min(energyOverview$YearDate), max(energyOverview$YearDate)),
                           timeFormat = "%Y"
            ),
            
            selectInput("y2", "Y-axis Variable", c("Consumption (Quad BTU)" = "QuadsN", "Mix (%)" = "Percentage")),
            
            br(),
            h5("Total Consumption Over Date Range"),
            dataTableOutput("CatTable")
            
            
            ))
            


          ) # closes sidebar
        ), # closes tabpenl
  


        tabPanel("Ranked Consumption",
                 hr(),
          sidebarLayout(

            # First row containing the product, selecting code and title
            mainPanel(width = 9,
                      plotOutput("sankey")),
            
            
            
            sidebarPanel(width = 3,
                         tagList(
                         pickerInput("Source2", "Source",
                                     choices = unique(energyOverview$Source),
                                     width = "100%",
                                     selected = unique(energyOverview$Source),
                                     multiple = TRUE,
                                     options = list(
                                       `actions-box` = TRUE,
                                       `live-search` = TRUE
                                     )
            ),
            
            
            sliderInput("YearDate4", "Select Date Range",
                           min = min(energyOverview$YearDate),
                           max = max(energyOverview$YearDate),
                           value = c(min(energyOverview$YearDate), max(energyOverview$YearDate)),
                           timeFormat = "%Y"
            ),
            
            selectInput("y3", "Y-axis Variable", c("Consumption (Quad BTU)" = "QuadsN", "Mix (%)" = "Percentage")),
            
            br(),
            h5("Total Consumption Over Date Range"),
            dataTableOutput("consumptionSankeyTable")
            
            ))
                
                

          ) # closes sidebar
        ), # closes tabPanel




# map
        tabPanel("Consumption by State",
                 hr(),
          sidebarLayout(
            mainPanel(width = 9,
                      tmapOutput("map", width = "100%", height = 600)),
            
            

            sidebarPanel(width = 3,
                         tagList(
              sliderInput("YearDate5", "Select Date Year",
                                     min = min(stateData$Year),
                                     max = max(stateData$Year),
                                     value = min(stateData$Year),
                                     step = 1,
                                     sep = ""
            ),
            
            br(),
            h5("Yearly Consumption"),
            p("Units of millions of BTU per person."),
            dataTableOutput("mapTable")
            ))

            

      

                        ) # closes sidebar
                  ) # closes tabset

      ) # closes navpage

) # closes fluidpage

# ----------

# SERVER

# ----------

server <- function(input, output, session) {

# FILTER FOR THE TABLE
  
  ## Reactive element

  tableDateFilter <- reactive({
    energyOverview %>%
      
      filter(YearDate >= input$YearDate2[1], YearDate <= input$YearDate2[2]) %>%
      filter(Source %in% input$Source1)
  })
  
  ## Consumption Table
  
  output$consumptionTable <- renderDataTable(options = list(lengthChange = FALSE, dom = 't', scrollX = TRUE, order = list(list(2, 'desc'))), {
    tableDateFilter() %>%
      mutate(Quads = str_replace(Quads, "Not Available", "0"),
                     # removes the NA values in character format
                     QuadsN = as.double(Quads),
                     QuadsNSum = sum(QuadsN),
                     Percentage = QuadsN/QuadsNSum) %>%
      group_by(Source) %>%
               summarise(Quads = sum(QuadsN),
               Percentage = sum(Percentage)) %>%
      mutate(Percentage = 100 * Percentage,
             Percentage = round(Percentage, digits = 1),
             Quads = round(Quads, digits = 1))
        
  }, width = "100%")
  
  
# FILTER FOR THE PLOT
  
    ## Reactive element
    
    plotDateFilter <- reactive({
      energyOverview %>%
        
        filter(YearDate >= input$YearDate2[1], YearDate <= input$YearDate2[2]) %>%
        mutate(Quads = str_replace(Quads, "Not Available", "0"),
               # removes the NA values in character format
               QuadsN = as.double(Quads)) %>%
        # groups by year
        group_by(Year) %>%
        # sums total quads in a year for percentage
        mutate(QuadsNSum = sum(QuadsN)) %>%
        # groups by source and the year
        group_by(Source, Year) %>%
        # finds the percentage
        mutate(Percentage = (QuadsN/QuadsNSum) * 100) %>%
        
        ungroup()
    })
    
    ## plot 
    

    
    output$consumptionPlot <- renderPlot(height = 600, {
      
      df <- plotDateFilter() 
      filteredPlotData <- df %>%
        filter(Source %in% input$Source1) 
      
      
      ggplot(filteredPlotData, mapping=aes(x = Year,
                         y= .data[[input$y1]],
                         group = Source, 
                         fill = Source)) +
      # Plots and groups the year and source
      
      
      scale_fill_manual(name = "Energy Source",
                        values = palette) +
      # color scheme for the fill
      
      
      theme(plot.title = element_text(face="bold", margin = margin(0,0,20,0), hjust = 0.5),
            axis.title.x = element_text(face="bold", margin = margin(20,0,0,0)),
            axis.title.y = element_text(face="bold", margin = margin(0,20,0,0)),
            # sets bold axis titles
            
            
            axis.line.x = element_line(linewidth = 1, color = "black"),
            # bolder x axis
            
            axis.line.y = element_line(linewidth = 1, color = "black"),
            # bolder y axis
            
            plot.background = element_rect(fill = "#FAF9F6", color = "#56382E", linewidth = 2),
            
            panel.grid.major =element_line(color= "#d3d3d3"),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      # defining breaks on the y axis
      
      

      # define breaks for each decade
      
      labs(x= "Year", 
           y = if (input$y1 == "QuadsN") "Energy Consumption (Quad BTU)" else "Mix (%)") + 
      
      geom_area(position = "stack")
    # sets the stacked position, color for the outline and width.
  })
  
# FILTER FOR CATEGORY PLOT
    
  ## Reactive element

  plotDateFilterCat <- reactive({
    energyOverview %>%
      
      filter(YearDate >= input$YearDate3[1], YearDate <= input$YearDate3[2]) %>%
      filter(Category == input$Category)
    
  })

  ## Cat Data table
  
  output$CatTable <- renderDataTable(options = list(lengthChange = FALSE, dom = 't', scrollX = TRUE, order = list(list(2, 'desc'))), {
    plotDateFilterCat() %>%
      mutate(Quads = str_replace(Quads, "Not Available", "0"),
             # removes the NA values in character format
             QuadsN = as.double(Quads),
             QuadsNSum = sum(QuadsN),
             Percentage = QuadsN/QuadsNSum) %>%
              # sums by source
      group_by(Source) %>%
      summarise(Quads = sum(QuadsN),
                Percentage = sum(Percentage)) %>%
              # fixes the digits
      mutate(Percentage = 100 * Percentage,
             Percentage = round(Percentage, digits = 1),
             Quads = round(Quads, digits = 1))
    
  }, width = "100%")
  
  ## cat plot
  
  output$ffPlot <- renderPlot(height = 600, {
    
     plotDateFilterCat() %>% mutate(Quads = str_replace(Quads, "Not Available", "0"),
           # removes the NA values in character format
           QuadsN = as.double(Quads)) %>%
      # groups by year
      group_by(Year) %>%
      # sums total quads in a year for percentage
      mutate(QuadsNSum = sum(QuadsN)) %>%
      # groups by source and the year
      group_by(Category, Year) %>%
      # finds the percentage
      mutate(Percentage = (QuadsN/QuadsNSum * 100)) %>%
      
      
      ungroup() %>%
    
    ggplot(mapping=aes(x = Year,
                       y= .data[[input$y2]],
                                         group = Source, 
                                         fill = Source)) +
      # Plots and groups the year and source
      
      
      scale_fill_manual(name = "Energy Source",
                        values = palette) +
      # color scheme for the fill
      
      
      theme(plot.title = element_text(face="bold", margin = margin(0,0,20,0), hjust = 0.5),
            axis.title.x = element_text(face="bold", margin = margin(20,0,0,0)),
            axis.title.y = element_text(face="bold", margin = margin(0,20,0,0)),
            # sets bold axis titles
            
            
            axis.line.x = element_line(linewidth = 1, color = "black"),
            # bolder x axis
            
            axis.line.y = element_line(linewidth = 1, color = "black"),
            # bolder y axis
            
            plot.background = element_rect(fill = "#FAF9F6", color = "#56382E", linewidth = 2),
            
            panel.grid.major =element_line(color= "#d3d3d3"),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      # defining breaks on the y axis
      
      
      
      # define breaks for each decade
      
      labs(x= "Year", 
           y = if (input$y2 == "QuadsN") "Energy Consumption (Quad BTU)" else "Mix (%)") + 
      
      geom_area(position = "stack")
    # sets the stacked position, color for the outline and width.
  })
  
# FILTER FOR SANKEY
  
  ## reactive element
  
  sankeyDateFilter <- reactive({
    energyOverview %>%
      
      filter(YearDate >= input$YearDate4[1], YearDate <= input$YearDate4[2]) %>%
      mutate(Quads = str_replace(Quads, "Not Available", "0"),
             # removes the NA values in character format
             QuadsN = as.double(Quads)) %>%
      # groups by year
      group_by(Year) %>%
      # sums total quads in a year for percentage
      mutate(QuadsNSum = sum(QuadsN)) %>%
      # groups by source and the year
      group_by(Source, Year) %>%
      # finds the percentage
      mutate(Percentage = (QuadsN/QuadsNSum) * 100) %>%
      
      ungroup()
  })
  
  
  ## sankey plot
  
  output$sankey <- renderPlot(height = 600, {
    
    dfSankey <- sankeyDateFilter()
    filteredPlotData <- dfSankey %>%
      filter(Source %in% input$Source2) 
  
    
    ggplot(filteredPlotData, aes(x = Year, 
                                 node = Source, 
                                 fill = Source, 
                                 value = .data[[input$y3]], 
                                 label = Source, 
                                 group = Year)) +
      
      geom_sankey_bump(smooth = 3) +
      
      scale_fill_manual(name = "Energy Source",
                        values = palette) +
      # color scheme for the fill
      
      
      theme(plot.title = element_text(face="bold", margin = margin(0,0,20,0), hjust = 0.5),
            axis.title.x = element_text(face="bold", margin = margin(20,0,0,0)),
            axis.title.y = element_blank(),
            # sets bold axis titles
            
            
            axis.line.x = element_line(linewidth = 1, color = "black"),
            # bolder x axis
            
            axis.line.y = element_blank(),
            # no y axis
            
            plot.background = element_rect(fill = "#FAF9F6", color = "#56382E", linewidth = 2),
            
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            # no x axis labels
            
            panel.grid.major =element_line(color= "#d3d3d3"),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +

      
      
      
      labs(x= "Year", 
           y = if (input$y3 == "QuadsN") "Energy Consumption (Quad BTU)" else "Mix (%)") 
    
    
  })
  
  ## sankey table
  
  tableSankeyFilter <- reactive({
    energyOverview %>%
      
      filter(YearDate >= input$YearDate4[1], YearDate <= input$YearDate4[2]) %>%
      filter(Source %in% input$Source2)
  })
  
  output$consumptionSankeyTable <- renderDataTable(options = list(lengthChange = FALSE, dom = 't', scrollX = TRUE, order = list(list(2, 'desc'))), {
    tableSankeyFilter() %>%
      mutate(Quads = str_replace(Quads, "Not Available", "0"),
             # removes the NA values in character format
             QuadsN = as.double(Quads),
             QuadsNSum = sum(QuadsN),
             Percentage = QuadsN/QuadsNSum) %>%
      group_by(Source) %>%
      summarise(Quads = sum(QuadsN),
                Percentage = sum(Percentage)) %>%
      mutate(Percentage = 100 * Percentage,
             Percentage = round(Percentage, digits = 1),
             Quads = round(Quads, digits = 1))
    
  }, width = "100%")
  
# FILTER FOR MAP
  
  ## reactive for map

  stateDataFiltered <- reactive({
    stateData %>%
      filter(Year == input$YearDate5)
  })
  
  ## reactive for table
  
  stateDataFilteredTable <- reactive({
    stateData %>%
      filter(Year == input$YearDate5) %>%
      st_drop_geometry() 
    })
  
  ## table
  
  output$mapTable <- renderDataTable(options = list(dom ='tp', scrollX = TRUE, scrollY = TRUE, order = list(list(2, 'desc'))), {
    stateDataFilteredTable() %>%

      select(State, Consumption)
    
  }, width = "100%")
  
  
  ## left out code for ggplot version
  
 # output$map <- renderPlotly({
    
#    font_add_google(name = "Cabin Condensed")
    # adds font
    
#    stateDataFiltered() %>%
 #     ggplot(mapping=aes(fill = Consumption)) +
      # fills for the percentage
  #    geom_sf(linewidth = .5, color = "#56382E") +
      # line color and width
 #     theme_minimal() +
#      labs(title = "Yearly Energy Consumption per Capita", subtitle = "in million BTU") +
      # labels
#      theme(panel.grid = element_blank(),
            
      #      axis.text.y = element_blank(),
     #      axis.ticks.y = element_blank(), 
    #       axis.text.x = element_blank(),
   #        axis.ticks.x = element_blank(),
            # no x or y axis labels
            
  #          plot.background = element_rect(fill = "#FAF9F6", color = "#56382E", linewidth = 2),
            # background colors
            
 #           text = element_text(family = "Cabin Condensed", color = "#56382E")) +
      # text sizes and font
      
#     scale_fill_gradientn(colors = rev(pnw_palette(name="Anemone",n=8,type="continuous"))#                           #colors
#    )
# })
  
 ## map

  output$map <- renderTmap(tm_basemap("Esri.WoldTopoMap") +
                             tm_shape(stateDataFiltered()) +
                            tm_polygons(fill = "Consumption",
                                        palette = rev(pnw_palette("Moth")),
                                        style = "fixed",
                                        breaks = c(146.2, 271.9, 361.3, 498.0, 712.0, 994.9),
                                        fill_alpha = 0.9,
                                        id = "Consumption",
                                        border.col = "#56382E"))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# final things to fix tonight - add on to explained, add categorical, add table (in DT format) below the consumption maps
