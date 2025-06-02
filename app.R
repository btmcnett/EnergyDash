#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


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



energyOverviewRaw <- read.csv("MER_T01_03.csv")

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
  mutate(Year = parse_number(Year)) %>%
  mutate(Years = as.numeric(Year)) %>%
  mutate(Consumption = as.numeric(gsub(",", "", Consumption))) %>%
  mutate(YearDate = ymd(Years, truncated = 2L))








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



# Define UI for application that draws a histogram
ui <- fluidPage(
  # sets theme
  theme = shinytheme("journal"),
  h1("U.S. Energy Consumption Dashboard"),
  p("This app allows users to observe the historical mixes and consumption of Energy Consumption in the United States. Multiple plots will be presented, with the option to see the consumption or the mix (as a percentage) in the side tool bar. There is also the option to select a source and isolate it from the others over time, and an option to change the date range over all timeframes. Three different plots are presented, with one showing all the sources together, another showing the sources grouped by category (fossil fuel, renewable), and a sankey diagram to show the rankings of consumption or the mix over time."),
  hr(),
  
  sidebarLayout(
    # First row containing the product, selecting code and title
    sidebarPanel(
      
    
    sliderInput("YearDate1", "Select Date Range",
                min = min(energyOverview$YearDate),
                max = max(energyOverview$YearDate),
                value = c(min(energyOverview$YearDate), max(energyOverview$YearDate)),
                timeFormat = "%Y"
    ),
    
    #selectInput("y", "Y axis", c("rate", "count"))
    
    
    ),
    
    mainPanel(
      fluidRow(
        column(8, tableOutput("consumptionTable"))
        
        
      ) # closes fluid row
    ) # closes main panel
  ), # closes sidebar

  h3("Consumption by Source"),
  p("The plot shows the energy consumption over time with the option to isolate a source over time, select between mix and consumption, and select the date range."),
  hr(),
  
  # consumption over all resources
  
  sidebarLayout(
    # First row containing the product, selecting code and title
    sidebarPanel(selectInput("Source1", "Source",
                             choices = c("All", energyOverview$Source),
                             width = "100%",
                             selected = "All"
    ),

      
    sliderInput("YearDate2", "Select Date Range",
                   min = min(energyOverview$YearDate),
                   max = max(energyOverview$YearDate),
                   value = c(min(energyOverview$YearDate), max(energyOverview$YearDate)),
                   timeFormat = "%Y"
    ),
      
      selectInput("y1", "Y-axis Variable", c("Consumption (Quad BTU)" = "QuadsN", "Mix (%)" = "Percentage"))
      
      
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("consumptionPlot"))
        
        
      ) # closes fluid row
    ) # closes main panel
  ), # closes sidebar
  
  
  h3("Consumption by Category"),
  p("The plot allows you to select between the three categories (renewable, fossil fuels, and nuclear) to see the mix or consumption between each category. Note that the mix for Nuclear will be a solid block."),
  hr(),
  # consumption for just the fossil fuels/renewable

  sidebarLayout(
    # First row containing the product, selecting code and title
    sidebarPanel(selectInput("Category", "Category",
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
    
    selectInput("y2", "Y axis", c("Consumption (Quad BTU)" = "QuadsN", "Mix (%)" = "Percentage"))
    
    
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("ffPlot"))
        
        
      ) # closes fluid row
    ) # closes main panel
  ), # closes sidebar
  
  h3("Sankey Bump Diagram"),
  p("Sankey Diagram showing the flow of consumption over time, with each level (top to bottom) corresponding to the highest values of mix or consumption. The user can select between the source, date range, and consumption or mix. Future options will allow users to select multiple sources at a time."),
  hr(),

  # SANKEY
  sidebarLayout(
    # First row containing the product, selecting code and title
    sidebarPanel(selectInput("Source2", "Source",
                             choices = c("All", energyOverview$Source),
                             width = "100%",
                             selected = 'All'
    ),
    
    
    sliderInput("YearDate4", "Select Date Range",
                   min = min(energyOverview$YearDate),
                   max = max(energyOverview$YearDate),
                   value = c(min(energyOverview$YearDate), max(energyOverview$YearDate)),
                   timeFormat = "%Y"
    ),
    
    selectInput("y3", "Y axis", c("Consumption (Quad BTU)" = "QuadsN", "Mix (%)" = "Percentage"))
    
    
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("sankey"))
        
        
      ) # closes fluid row
    ) # closes main panel
  ), # closes sidebar
        


h3("Consumption by Location"),
p("Below is a map that details the yearly consumption per capita (in millions of Btus) over time. The slider allows the user to select the year. NOTE: the breaks are different every year, which can be misleading. This will be fixed in the final app."),
hr(),

# map
sidebarLayout(
  # First row containing the product, selecting code and title
  sidebarPanel(sliderInput("YearDate5", "Select Date Year",
                           min = min(stateData$Year),
                           max = max(stateData$Year),
                           value = min(stateData$Year),
                           step = 1,
                           sep = ""
  )),
  
  
  mainPanel(
    fluidRow(
      column(12, tmapOutput("map"))
      
      
    ) # closes fluid row
  ) # closes main panel
) # closes sidebar

)


server <- function(input, output, session) {
  
  tableDateFilter <- reactive({
    energyOverview %>%
      
      filter(YearDate >= input$YearDate1[1], YearDate <= input$YearDate1[2])
  })
  
  output$consumptionTable <- renderTable({
    tableDateFilter() %>%
      mutate(Quads = str_replace(Quads, "Not Available", "0"),
             # removes the NA values in character format
             QuadsN = as.double(Quads),
             QuadsNSum = sum(QuadsN),
             Percentage = QuadsN/QuadsNSum) %>%
      group_by(Source) %>%
        summarise(Quads = sum(QuadsN),
                  Percentage = sum(Percentage)) %>%
      mutate(Percentage = 100 * Percentage)
        
  })
  

  
  

  

    
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
    

    output$consumptionPlot <- renderPlot({
      
      df <- plotDateFilter() 
      filteredPlotData <- if (input$Source1 == "All") {
        df
      } else {
        df %>%
          filter(Source == input$Source1)
      }
      
      
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
  


  plotDateFilterCat <- reactive({
    energyOverview %>%
      
      filter(YearDate >= input$YearDate3[1], YearDate <= input$YearDate3[2]) %>%
      filter(Category == input$Category)
    
  

      
#      select(Category, Year, YearDate, Percentage, QuadsN) %>%
      
#      unique()
  })
  
  
  output$ffPlot <- renderPlot({
    
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
  
  
  output$sankey <- renderPlot({
    
    df <- sankeyDateFilter() 
    filteredPlotData <- if (input$Source2 == "All") {
      df
    } else {
      df %>%
        filter(Source == input$Source2)
    }
  
    
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
            
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            # no x axis labels
            
            panel.grid.major =element_line(color= "#d3d3d3"),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +

      
      
      
      labs(x= "Year", 
           y = if (input$y3 == "QuadsN") "Energy Consumption (Quad BTU)" else "Mix (%)") 
    
    
  })
  
  stateDataFiltered <- reactive({
    stateData %>%
      filter(Year == input$YearDate5)
    })
  
  
 # output$map <- renderPlot({
    
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
  


  output$map <- renderTmap(tm_basemap("Esri.WoldTopoMap") +
                             tm_shape(stateDataFiltered()) +
                            tm_polygons(col = "Consumption",
                                        palette = rev(pnw_palette("Moth")),
                                        style = "jenks",
                                        alpha = 0.9,
                                        id = "Consumption",
                                        border.col = "#56382E"))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
