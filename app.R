# LIBRARIES ----

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(readxl)
library(ggplot2)
library(ggthemes)
library(GGally)
library(lubridate)
library(dplyr)
library(tidyr)
library(scales)
library(here)
library(gt)
library(plotly)
library(leaflet)
library(ggmap)
library(rclipboard)
library(stringr)
library(mapboxer)
library(sf)


# GLOBAL VARIABLES ----

options(scipen = 999, warn=0)

ggmap::register_google(key = 'Google Key')

MAPBOX_TOKEN <- "mapbox token"

wintotal_excel_path <-
  "C:/Users/Public/Documents/a la mode/Data/WinTOTAL/Users/User/Worksheets/R-Analysis Subject Specs.xls"

sixMonthsAgo <- Sys.Date() %m-% months(6)


# UI DASHBOARD ----

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "AppraisAlyzer"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home"),
    menuItem("Data Input", tabName = "dataImport"),
    menuItem("Assignment Data Frame (ADF)", tabName = "adf"),
    menuItem("Competitive Market Segment (CMS)", tabName = "cmSegment")#,
    #menuItem("Dataset Comparison", tabName = "datasetComparison"),
    #menuItem("Report Centre", tabName = "reportCentre")
  )),
  dashboardBody(
    includeCSS("www/custom.css"),
    # useShinyjs(),
    # tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.8/clipboard.min.js"),
    
## Panel Home ----
    tabItems(
      tabItem("home",
              div(
                style = "text-align: center;",
                img(
                  src = "logo.png",
                  style = "max-width: 70%; height: auto;",
                  alt = "AppraisAlyzer"
                )
              )),
## Panel Data Input ----
      tabItem("dataImport",
        fluidPage(
          
          tabsetPanel(
            
            tabPanel("Subject Characteristics",
                     div(
                       style = "padding: 20px 5px;",
                       actionButton("import_wintotal", "WinTotal Import", class = "btn-sm btn-success")
                     ),
                     fluidRow(column(3,
                                     selectInput(
                                       "propertyType",
                                       "Property Type",
                                       choices = c("Residential Single Family", "Other")
                                     ),
                     ),
                     column(3,
                            dateInput(
                              "effectiveDate",
                              "Effective Date:",
                              format = "yyyy-mm-dd"
                            )
                     ),
                     column(3,
                            numericInput("SubjectGarageSpaces", "Number of Garage Spaces", value = 0),
                     )
                     ),
                     
                     fluidRow(
                       column(
                         3,
                         textInput("SubjectAddress", "Address", ""),
                         numericInput("SubjectSalePrice", "Sale Price", value = 0),
                         dateInput(
                           "subjectSaleDate",
                           "Sale Date:",
                           format = "yyyy-mm-dd"
                         ),
                         numericInput("SubYB", "Year Built", value = 0),
                         numericInput("SubjectLotSF", "Lot Size (SF)", value = 0),
                         numericInput("SubjectGLA", "Gross Living Area (SF)", value = 0)
                       ),
                       column(3,
                              
                              numericInput("SubjectBeds", "Number of Bedrooms", value = 0),
                              numericInput("SubjectBaths", "Number of Bathrooms", value = 0),
                              
                              
                              selectInput(
                                "SubjectCondition",
                                "Condition",
                                choices = c("Poor", "Fair", "Average", "Good", "Excellent")
                              ),
                              radioButtons(
                                "SubjectBasement",
                                "Basement",
                                choices = c("Yes" = "Yes", "No" = "No")
                              ),
                              radioButtons(
                                "SubjectWaterfront",
                                "Waterfront",
                                choices = c("Yes" = "Yes", "No" = "No"),
                                selected = "No"
                              ),
                       ),
                       column(3,
                              numericInput("SubjectCarportSpaces", "Number of Carport Spaces", value = 0),
                              selectInput("subject_neighbourhood", "Subject Neighbourhood:", choices = NULL, multiple = FALSE)
                       ),
                     )
            ),
            
            tabPanel("MLS Import",
                     div(
                       style = "font-weight: bold;",
                       HTML("<h4>Primary MLS Data CSV File Stats:</h4>"),
                     ),
                     textOutput("numObservations"),
                     textOutput("minDatePending"),
                     textOutput("maxDatePending"),
                     div(
                       style = "border-bottom: 1px solid #ccc; padding-bottom: 10px;"
                     ),
                     fileInput("CSVFilePath", "Import/Append New MLS Data"),
                     actionButton("copyNewMLSButton", "Copy MLS Numbers"),
                     actionButton("saveEditedRowsButton", "Save"),
                     actionButton("reload", "Reload App"),
                     textOutput("messageText"),
                     DTOutput("editUniqueRows")
            ),
            tabPanel("Inventory Statistics",
                     selectInput(
                       "listingsSubAreaInput",
                       "Sub Area:",
                       choices = NULL,
                       multiple = FALSE
                     ),
                     selectInput(
                       "listingsPropertyType",
                       "Property Type:",
                       choices = NULL,
                       multiple = FALSE
                     ),
                     fileInput("listingInventoryFilePath", "Import Listing Inventory Data"),
                     textOutput("messageTextInventory"),
                     dataTableOutput("listings")
                     
            )
          )
        )
      ),
      
## Panel Assignment Data Frame ----
      tabItem("adf",
        fluidPage(
          tabsetPanel(
            tabPanel("ADF Search Criteria",
               fluidPage(
                 useShinyjs(),
                 sidebarLayout(
                   sidebarPanel(
                     id = "adf-custom-sidebar",
                     actionButton("resetADFSearchButton", "Reset"),

                     fluidRow(
                       column(7,
                              selectInput('adfStartMonth', 'Start Month:', month.name, selected = month.name[month(sixMonthsAgo)])
                       ),
                       column(5,
                              selectInput('adfStartYear', 'Start Year:', as.character(2025:2000), selected = as.character(year(sixMonthsAgo)))
                       )
                     ),
                     fluidRow(
                       column(7,
                              selectInput('adfEndMonth', 'End Month:', month.name, selected = month.name[month(Sys.Date())])
                       ),
                       column(5,
                              selectInput('adfEndYear', 'End Year:', as.character(2025:2000), selected = as.character(year(Sys.Date())))
                       )
                     ),
                     selectInput(
                       "adfSubAreaInput",
                       "Sub Area:",
                       choices = NULL,
                       multiple = TRUE
                     ),
                     selectInput(
                       "adfPropertySubType",
                       "Property Type:",
                       choices = NULL,
                       multiple = TRUE
                     ),
                     fluidRow(
                       column(6,
                              numericInput("adfSalePriceLow", "Price Low:", value = 0)
                       ),
                       column(6,
                              numericInput("adfSalePriceHigh", "Price High:", value = 1000000000)
                       )
                     ),
                     fluidRow(
                       column(12,
                              selectInput("adf_neighborhoods", "Select Neighborhood(s):", choices = NULL, multiple = TRUE)
                       )
                     ),
                     fluidRow(
                       column(12,
                              radioButtons('adf_price_per_unit',
                                           'Price per Unit',
                                           c('Bulk','Square foot','Acre','Total Floor Area'))
                       )
                     ),
                     fluidRow(
                       column(12,
                         radioButtons('adf_waterfront',
                                      'Waterfront',
                                      c('None','All','Lake','Ocean','River'))
                       )
                     ),
                     fluidRow(
                       column(12,
                              div(
                                style = "font-weight: bold;",
                                radioButtons('adf_format', 'ADF Document', c('HTML','PDF','Word'),
                                             inline = TRUE),
                                downloadButton("downloadAdfStatsReport", "Download")
                              )
                       )
                     ),
                     fluidRow(
                       column(12,
                              HTML("<b>ADF CSV File</b>"),
                              div(
                                style = "font-weight: bold;",
                                downloadButton("downloadAdfCSV", "Download")
                              ),
                       )
                     ),
                     width = 3
                   ),
                   mainPanel(
                     # outputs
                     fluidRow(
                       div(
                         style = "font-weight: bold;",
                         HTML("<h3>Market Trends Analysis - Assignment Data Frame</h3>"),
                       ),
                       div(
                         HTML("<h4><b>"),
                         style = "font-weight: bold;",
                         textOutput("adfTitle"),
                         HTML("</b></h4>")
                       ),
                       div(
                         HTML("<h4><b><i>"),
                         style = "font-weight: bold;",
                         textOutput("adfTitlePropertyType"),
                         HTML("</i></b></h4>")
                       ),
                       textOutput("adfDateRange"),
                       gt_output(outputId = "adfDataStatsTable"),
                     ),
                     fluidRow(
                       column(
                         6,
                         plotlyOutput("adfSalePrice_SaleDatePlot", height = "350px"),
                       ),
                       column(
                         6,
                         plotlyOutput("adfCountByPriceRangePlot", height = "350px"),
                       )
                       
                     ),
                     fluidRow(
                       style = "margin-top: 10px;",
                       column(
                         6,
                         plotlyOutput("adfCountByMonthPlot", height = "350px"),
                       ),
                       column(
                         6,
                         plotlyOutput("adfDaysOnMarket", height = "350px"),
                       )
                     ),
                     fluidRow(
                       style = "margin-top: 10px;",
                       column(
                         6,
                         plotlyOutput("adfSaleToListprice", height = "350px"),
                       ),
                       column(
                         6,
                         plotlyOutput("adfListingsInventory", height = "350px"),
                       )
                     ),
                     fluidRow(
                       style = "margin-top: 10px;",
                       column(
                         6,
                         plotlyOutput("adfMarketPriceTrends", height = "350px")
                       ),
                       column(
                         6,
                         plotlyOutput("adfSalesVsInventory", height = "350px")
                       )
                     ),
                     fluidRow(
                       style = "margin-top: 10px;",
                       column(6,
                              plotlyOutput("adfInventoryAbsorption", height = "350px")
                       ),
                       column(6,
                              plotlyOutput("adfInventoryVsSalesPlot", height = "350px")
                       )
                     ),
                     fluidRow(
                       style = "margin-top: 10px;",
                       column(6,
                              plotlyOutput("adfAnnualPriceTrendComparison", height = "350px")
                       ),
                       column(6,
                              plotlyOutput("adfAnnualSaleCountComparison", height = "350px")
                       )
                     ),
                   )
                 )
               )
            ),
            tabPanel("ADF Data",
                     gt_output(outputId = "adfAttributeStatsTable"),
                     DTOutput("adf_data_table") # Display the outlier table
            ),
            tabPanel("ADF Outliers",
                     div(
                       style = "padding: 20px 5px;",
                       actionButton("reset_adf_outliers", "Reset Outliers", class = "btn-sm btn-success")
                     ),
                     DTOutput("ADFOutlierTable") # Display the outlier table
            ),
            tabPanel("ADF Location Map",
                     fluidRow(
                       column(12,
                              #plotlyOutput("adf_data_map", height = "120vh") # Adjust height as needed
                              mapboxerOutput("adf_data_map",
                                             height = "80vh")      
                       )
                     )
            ),
            tabPanel("ADF Comparison Coefficients",
                     fluidRow(
                       div(
                         style = "margin-top: 30px; line-height: 2; font-weight: bold;",
                         "Correlation Calculator"
                       )
                       
                     ),
                     fluidRow(
                       column(3,
                              selectInput("variable_1", "Variable 1:", choices = NULL, multiple = FALSE)
                       ),
                       column(3,
                              selectInput("variable_2", "Variable 2:", choices = NULL, multiple = FALSE)
                       ),
                       column(3,
                              div(
                                style = "margin-top: 0px; line-height: 2; font-weight: bold;",
                                "Coefficient",
                                textOutput("coefText")
                              )
                       ),
                       
                     ),
                     fluidRow(
                       div(
                         style = "margin-top: 30px; line-height: 2; font-weight: bold;",
                         "Correlation Matrix"
                       )
                     ),
                     fluidRow(
                       column(12,
                              plotOutput("correlation_matrix_plot")
                       )
                     ),
                     fluidRow(
                       div(
                         style = "margin-top: 30px; line-height: 2; font-weight: bold;",
                         "Correlation Coefficients based on Sale Price"
                       )
                     ),
                     fluidRow(
                       column(12,
                              DTOutput("adf_coefficient_table")
                       )
                     ),
                     
                     
            ),
          )
        ),
      ),
      
## Panel Competitive Market Segment ----
      tabItem("cmSegment",
        fluidPage(
          tabsetPanel(
            tabPanel("CMS Criteria",
               sidebarLayout(
                 sidebarPanel(
                   actionButton("resetCMSSearchButton", "Reset"),
                   selectInput(
                     "cms_view",
                     "View",
                     choices = NULL,
                     multiple = TRUE
                   ),
                   selectInput(
                     "cms_extras",
                     "Extras",
                     choices = NULL,
                     multiple = TRUE
                   ),
                   radioButtons('cms_basement',
                                'Basement',
                                c('All','Yes','No'),
                                inline = FALSE),
                   fluidRow(
                     column(12,
                            div(
                              style = "font-weight: bold;",
                              radioButtons('cms_format', 'CMS Document', c('HTML','PDF','Word'),
                                           inline = TRUE),
                              downloadButton("downloadCMSReport", "Download")
                            )
                     )
                   ),
                   width = 2
                 ),
                 mainPanel(
                   fluidRow(
                     div(
                       style = "font-size: 1.5em; font-weight: bold;",  # Inline CSS styles
                       textOutput("cmsTitle")
                     ),
                     div(
                       style = "font-weight: bold;",
                       HTML("<h4>Market Trends Analysis - Competitive Market Segment</h4>"),
                     ),
                     textOutput("cmsDateRange"),
                     gt_output(outputId = "cmsDataStatsTable")
                   ),
                   fluidRow(
                     style = "margin-top: 10px;",
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalePriceDistribution", height = "300px")
                     ),
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalesByYearBuilt", height = "300px"),
                       numericRangeInput(inputId = "yearBuiltRange", label = "Year Built Range:",
                                         value = c(0, 0))
                     ),
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalesByGLA", height = "300px"),
                       numericRangeInput(inputId = "glaRange", label = "GLA Range:",
                                         value = c(0, 0))
                     )
                   ),
                   fluidRow(
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalesByLotSize", height = "300px"),
                       numericRangeInput(inputId = "lotSizeRange", label = "Lot Size Range:",
                                         value = c(0, 0))
                     ),
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalesByBeds", height = "300px"),
                       numericRangeInput(inputId = "bedsRange", label = "Beds Range:",
                                         value = c(0, 0))
                     ),
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalesByBaths", height = "300px"),
                       numericRangeInput(inputId = "bathsRange", label = "Baths Range:",
                                         value = c(0, 0))
                     )
                   ),
                   fluidRow(
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalesByCondition", height = "300px"),
                       numericRangeInput(inputId = "conditionRange", label = "Condition Range:",
                                         value = c(0, 0))
                     ),
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalesByGarage", height = "300px"),
                       numericRangeInput(inputId = "garageRange", label = "Garage Spaces Range:",
                                         value = c(0, 0))
                     ),
                     column(
                       4,
                       align = "center",
                       plotlyOutput("cmsSalesByCarport", height = "300px"),
                       numericRangeInput(inputId = "carportRange", label = "Carport Spaces Range:",
                                         value = c(0, 0))
                     )
                   ),
                   fluidRow(
                     column(
                       6,
                       align = "center",
                       plotlyOutput("cmsSalesBySaleDatePlot", height = "300px")
                     )
                   )
               
                 )
               )
            ),
            
            tabPanel("CMS Data Table",
                     actionButton("copyMLSButton", "Copy MLS Numbers"),
                     gt_output(outputId = "cmsDataSummaryTable"),
                     DTOutput("cms_data_table")
            ),
            tabPanel("CMS Location Map",
                     fluidRow(
                       column(12,
                              #plotlyOutput("cms_data_map", height = "120vh") # Adjust height as needed
                              mapboxerOutput("cms_data_map",
                                             height = "80vh")
                       )
                     )
            ),
            
          )
        )
      )#,
      #tabItem("datasetComparison",
      #  div(
      #    style = "text-align: center;",
      #    "Dataset Comparison"
      #  )
      #),
      #tabItem("reportCentre",
      #  div(
      #    style = "text-align: center;",
      #    "Report Centre"
      #  )
      #)
    )
  )
)
# SERVER FUNCTIONS ----
server <- function(input, output, session) {
  # GENERAL SERVER SETUP ----
  observe({
    runjs(
      "shinyjs.setTabOrder('subjectData', ['SubjectAddress', 'propertyType', 'SubYB', 'SubjectLotSF', 'SubjectGLA', 'SubjectBeds', 'SubjectBaths', 'SubjectBasement', 'SubjectSalePrice', 'SubjectCondition']);"
    )
  })
  
  # REACTIVE VARIABLE DECLARATIONS----  
  excel_data <- reactiveVal(NULL)
  
  masterData <- reactiveVal(NULL)
  adfData <- reactiveVal(data.frame())
  adfDataHistorical <- reactiveVal(data.frame())
  adfDataOutliers <- reactiveVal(data.frame())
  
  adf_min <- reactiveVal(0)
  adf_max <- reactiveVal(0)
  
  adfTitle <- reactiveVal(NULL)
  adfTitleSubAreas <- reactiveVal(NULL)
  adfTitlePropertyType <- reactiveVal(NULL)
  
  adfSubArea <- reactive(NULL)
  adfPropertyType <- reactive(NULL)
  adfMinDate <- reactiveVal(NULL)
  adfMaxDate <- reactiveVal(NULL)
  
  adf_listing_data <- reactiveVal(NULL)
  
  cmsData <- reactiveVal(data.frame())
  cmsDataOutliers <- reactiveVal(NULL)
  cmsTitle <- reactiveVal(NULL)
  cmsMinDate <- reactiveVal(NULL)
  cmsMaxDate <- reactiveVal(NULL)
  
  cms_year_built_min <- reactiveVal(0)
  cms_year_built_max <- reactiveVal(2050)
  cms_year_built_range_low <- reactiveVal(NULL)
  cms_year_built_range_high <- reactiveVal(NULL)
  cms_year_built_values <- reactiveVal(c(0, 100000))
  
  cms_gla_min <- reactiveVal(0)
  cms_gla_max <- reactiveVal(0)
  cms_gla_range_low <- reactiveVal(NULL)
  cms_gla_range_high <- reactiveVal(NULL)
  cms_gla_values <- reactiveVal(c(0, 100000000))
  
  cms_lot_size_min <- reactiveVal(0)
  cms_lot_size_max <- reactiveVal(0)
  cms_lot_size_range_low <- reactiveVal(NULL)
  cms_lot_size_range_high <- reactiveVal(NULL)
  cms_lot_size_values <- reactiveVal(c(0, 100000000))
  
  cms_beds_min <- reactiveVal(0)
  cms_beds_max <- reactiveVal(0)
  cms_beds_range_low <- reactiveVal(NULL)
  cms_beds_range_high <- reactiveVal(NULL)
  cms_beds_values <- reactiveVal(c(0, 100000000))
  
  cms_baths_min <- reactiveVal(0)
  cms_baths_max <- reactiveVal(0)
  cms_baths_range_low <- reactiveVal(NULL)
  cms_baths_range_high <- reactiveVal(NULL)
  cms_baths_values <- reactiveVal(c(0, 100000000))
  
  cms_condition_min <- reactiveVal(0)
  cms_condition_max <- reactiveVal(0)
  cms_condition_range_low <- reactiveVal(NULL)
  cms_condition_range_high <- reactiveVal(NULL)
  cms_condition_values <- reactiveVal(c(0, 100000000))
  
  cms_garage_min <- reactiveVal(0)
  cms_garage_max <- reactiveVal(0)
  cms_garage_range_low <- reactiveVal(NULL)
  cms_garage_range_high <- reactiveVal(NULL)
  cms_garage_values <- reactiveVal(c(0, 100))
  
  cms_carport_min <- reactiveVal(0)
  cms_carport_max <- reactiveVal(0)
  cms_carport_range_low <- reactiveVal(NULL)
  cms_carport_range_high <- reactiveVal(NULL)
  cms_carport_values <- reactiveVal(c(0, 100))
  
  changePerDayCMS <- reactiveVal(0)
  
  clipMLSNumbers <- reactiveVal(data.frame())
  
  adf_outlier_table <- reactiveVal(data.frame())
  
  subjectLat <-  reactiveVal(NULL)
  subjectLon <-  reactiveVal(NULL)

  # LOAD SAVED MLS DATA ----
  
  if (file.exists("data/ADFMaster.csv")) {
    master_df <- read.csv("data/ADFMaster.csv")
    master_df$Date.Pending <- as.Date(master_df$Date.Pending)
    master_df$Price.Sold <- as.numeric(gsub("[^0-9.]", "", master_df$Price.Sold))
    master_df$Price.List <- as.numeric(gsub("[^0-9.]", "", master_df$Price.List))
    master_df$Price.Original <- as.numeric(gsub("[^0-9.]", "", master_df$Price.Original))
    
    
    updateSelectInput(session, "adfSubAreaInput", choices = c("Select an option" = "", unique(master_df$`Sub.Area`), selected = NULL))
    updateSelectInput(session, "adfPropertySubType", choices = c("Select an option" = "", unique(master_df$`Property.Sub.Type`), selected = NULL))
    updateSelectInput(session, "listingsSubAreaInput", choices = c("Select an option" = "", unique(master_df$`Sub.Area`), selected = NULL))
    updateSelectInput(session, "listingsPropertyType", choices = c("Select an option" = "", unique(master_df$`Property.Sub.Type`), selected = NULL))
    
    unique_views <- unique(trimws(unlist(strsplit(master_df$View, ","))))
    unique_views <- unique_views[!is.na(unique_views)]
    updateSelectInput(session, "cms_view", choices = c("Select an option" = "", unique_views))
    
    condition_choices = c('Poor','Fair','Average','Good','Excellent')
    updateSelectInput(session, "cms_condition", choices = c("Select an option" = "", condition_choices))
    
    unique_extras <- unique(trimws(unlist(strsplit(master_df$`Other.Structures`, ","))))
    unique_extras <- unique_extras[!is.na(unique_extras)]
    updateSelectInput(session, "cms_extras", choices = c("Select an option" = "", unique_extras))
    
    # Calculate the number of observations
    num_observations <- nrow(master_df)
    output$numObservations <- renderText({
      paste("Observations:", num_observations)
    })
    
    # Calculate the minimum and maximum of Date.Pending
    min_date_pending <- min(master_df$Date.Pending, na.rm = TRUE)
    max_date_pending <- max(master_df$Date.Pending, na.rm = TRUE)

    output$minDatePending <- renderText({
      paste("Minimum Date Sold:", min_date_pending)
    })
    output$maxDatePending <- renderText({
      paste("Maximum Date Sold:", max_date_pending)
    })
    
    #browser()

    masterData(master_df)
  }
  
  # IMPORT NEW MLS DATA ----
  
  observeEvent(input$CSVFilePath, {
    # Function to assign condition scores based on keywords
    assign_condition_score <- function(remarks, condition_keywords) {
      max_score <- 3  # Default score is 3 when no keywords match
      
      
      # Loop through the keywords categories and assign scores
      for (category in colnames(condition_keywords)) {
        keywords <- condition_keywords[[category]]
        #print(keywords[1])
        #print(remarks)
        # Check if any keyword matches in the remarks
        if (!is.na(keywords[1]) && !is.na(remarks) && keywords[1] != "") {
          matched_keywords <- sapply(keywords, function(keyword) {
            if (!is.na(keyword) && keyword != "" && any(str_detect(remarks, keyword))) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          })
          if (any(matched_keywords)) {
            case_score <- case_when(
              category == "poor" ~ 1,
              category == "fair" ~ 2,
              category == "good" ~ 4,
              category == "excellent" ~ 5,
              TRUE ~ NA
            )
            if (!is.na(case_score)) {
              max_score <- max(max_score, case_score)
            }
          }
        }
      }
      
      return(max_score)
    }
    
    #GeoCode address function
    geocode_address <- function(address) {
      result <- geocode(address, output = "latlona", source = "google")
      return(result)
    }
    
    if (!is.null(input$CSVFilePath)) {
      #browser()
      if (!file.exists("data/ADFMaster.csv")) {
        import_df <- read.csv(input$CSVFilePath$datapath)
        #Create new columns
        import_df$location <- NA
        import_df$lat <- NA
        import_df$lon <- NA
        import_df$condition <- NA
        
        #Merge address info into Location column
        import_df <- unite(import_df, col='location', c('Address', 'City', 'Province', 'Postal.Code'), sep=',', remove = FALSE)
        # Identify rows where both 'lon' and 'lat' are missing or empty
        #rows_to_geocode <- which(is.na(import_df$lon) | import_df$lon == "") & (is.na(import_df$lat) | import_df$lat == "")
        # Geocode the identified rows
        #import_df[rows_to_geocode, c("lon", "lat")] <- geocode(import_df$location[rows_to_geocode])
        
        for(i in 1:nrow(unique_import_rows)) {
          location <- unique_import_rows$location[i]
          geocoded <- geocode_address(location)
          unique_import_rows$lat[i] <- geocoded$lat
          unique_import_rows$lon[i] <- geocoded$lon
        }
        
        #Run condition algorithm
        condition_keywords <- read.csv("data/condition_keywords.csv")
        import_df$Remarks <- tolower(import_df$Remarks)
        import_df$Remarks <- str_replace_all(import_df$Remarks, "-", " ")
        
        # Apply the function to the "Remarks" column and update the "condition" column
        import_df$condition <- ifelse(!is.na(import_df$condition),
                                      import_df$condition,
                                      sapply(import_df$Remarks, assign_condition_score, condition_keywords = condition_keywords))
        
        #print(str(import_df))
        
        write.csv(import_df, "data/ADFMaster.csv", row.names = FALSE)
        output$messageText <- renderText({ 
          paste("New primary CSV created with", nrow(import_df), "rows.") 
        })
        master_df <- import_df
        updateSelectInput(session, "adfSubAreaInput", choices = c("Select an option" = "", unique(master_df$`Sub.Area`), selected = NULL))
        updateSelectInput(session, "adfPropertySubType", choices = c("Select an option" = "", unique(master_df$`Property.Sub.Type`), selected = NULL))
        updateSelectInput(session, "listingsSubAreaInput", choices = c("Select an option" = "", unique(master_df$`Sub.Area`), selected = NULL))
        updateSelectInput(session, "listingsPropertyType", choices = c("Select an option" = "", unique(master_df$`Property.Sub.Type`), selected = NULL))
        
      } 
      
       else {
         master_df <- masterData()
         import_df <- read.csv(input$CSVFilePath$datapath)
         
         unique_import_rows <- import_df[!import_df$`MLS..No` %in% master_df$`MLS..No`, ]
         
         if (nrow(unique_import_rows) != 0) {

           #Create new columns
           unique_import_rows$condition <- NA_character_
           unique_import_rows$location <- NA_character_
           unique_import_rows$lat <- NA_character_
           unique_import_rows$lon <- NA_character_
           #Merge address info into Location column
           unique_import_rows$location <- paste(unique_import_rows$Address, unique_import_rows$City, unique_import_rows$Province, unique_import_rows$Postal.Code, sep=',')
           
           #Run condition algorithm
           unique_import_rows$Remarks <- tolower(unique_import_rows$Remarks)
           
           if(any(grepl("-", unique_import_rows$Remarks))) {
             unique_import_rows$Remarks <- gsub("-", " ", unique_import_rows$Remarks)
           }
           
           #browser()
           # Clean up NAs in land to 0 or empty strings
           unique_import_rows$Occupant.Type[unique_import_rows$Property.Sub.Type == "Land"] <- ""
           unique_import_rows$Fin.SqFt.Total[unique_import_rows$Property.Sub.Type == "Land"] <- 0
           unique_import_rows$SqFt.Fin.Lower[unique_import_rows$Property.Sub.Type == "Land"] <- 0
           unique_import_rows$SqFt.Fin.Main[unique_import_rows$Property.Sub.Type == "Land"] <- 0
           unique_import_rows$SqFt.Fin.Second[unique_import_rows$Property.Sub.Type == "Land"] <- 0
           unique_import_rows$SqFt.Fin.Third[unique_import_rows$Property.Sub.Type == "Land"] <- 0
           unique_import_rows$Year.Built..Est.[unique_import_rows$Property.Sub.Type == "Land"] <- 0
           unique_import_rows$Layout[unique_import_rows$Property.Sub.Type == "Land"] <- ""
           unique_import_rows$Basement.Description[unique_import_rows$Property.Sub.Type == "Land"] <- ""
           unique_import_rows$Exterior.Features[unique_import_rows$Property.Sub.Type == "Land"] <- ""
           unique_import_rows$Lot.Features[unique_import_rows$Property.Sub.Type == "Land"] <- ""
           unique_import_rows$View[unique_import_rows$Property.Sub.Type == "Land"] <- ""
           unique_import_rows$Waterfront.Features[unique_import_rows$Property.Sub.Type == "Land"] <- ""
           
           
           unique_import_rows$Year.Built..Est.[unique_import_rows$Property.Sub.Type == "Recreational"] <- 0
           
           unique_import_rows$Lot.Size.SqFt[unique_import_rows$Property.Sub.Type == "Row/Townhouse"] <- 0
           unique_import_rows$Lot.Size.Acres[unique_import_rows$Property.Sub.Type == "Row/Townhouse"] <- 0
           
           unique_import_rows$Lot.Size.SqFt[unique_import_rows$Property.Sub.Type == "Condo Apartment"] <- 0
           unique_import_rows$Lot.Size.Acres[unique_import_rows$Property.Sub.Type == "Condo Apartment"] <- 0
           
           # Replace NA values in Occupant.Type with an empty string
           unique_import_rows$Occupant.Type[is.na(unique_import_rows$Occupant.Type)] <- ""
           
           unique_import_rows$Exterior.Features[is.na(unique_import_rows$Exterior.Features)] <- ""
           
           unique_import_rows$View[is.na(unique_import_rows$View)] <- ""
           
           unique_import_rows$Waterfront.[is.na(unique_import_rows$Waterfront.)] <- ""
           
           unique_import_rows$Waterfront.Features[is.na(unique_import_rows$Waterfront.Features)] <- ""
           
           unique_import_rows$Lot.Features[is.na(unique_import_rows$Lot.Features)] <- ""
           
           unique_import_rows$Other.Structures[is.na(unique_import_rows$Other.Structures)] <- ""
           
           unique_import_rows$Other.Structures[is.na(unique_import_rows$Other.Structures)] <- ""
           
           # Change NAs in LFA to 0s and remove commas
           unique_import_rows$SqFt.Fin.Lower[is.na(unique_import_rows$SqFt.Fin.Lower)] <- "0"
           unique_import_rows$SqFt.Fin.Lower <- gsub(",", "", unique_import_rows$SqFt.Fin.Lower)
           
           # Change NAs in Main to 0s and remove commas
           unique_import_rows$SqFt.Fin.Main[is.na(unique_import_rows$SqFt.Fin.Main)] <- "0"
           unique_import_rows$SqFt.Fin.Main <- gsub(",", "", unique_import_rows$SqFt.Fin.Main)
           
           # Change NAs in Second to 0s and remove commas
           unique_import_rows$SqFt.Fin.Second[is.na(unique_import_rows$SqFt.Fin.Second)] <- "0"
           unique_import_rows$SqFt.Fin.Second <- gsub(",", "", unique_import_rows$SqFt.Fin.Second)
           
           # Change NAs in Third to 0s and remove commas
           unique_import_rows$SqFt.Fin.Third[is.na(unique_import_rows$SqFt.Fin.Third)] <- "0"
           unique_import_rows$SqFt.Fin.Third <- gsub(",", "", unique_import_rows$SqFt.Fin.Third)
           
           unique_import_rows$Fin.SqFt.Total <- gsub(",", "", unique_import_rows$Fin.SqFt.Total)
           
           
           unique_import_rows$Garage.Spaces[is.na(unique_import_rows$Garage.Spaces)] <- "0"
           
           unique_import_rows$Carport.Spaces[is.na(unique_import_rows$Carport.Spaces)] <- "0"
           
           unique_import_rows$Baths.2pce[is.na(unique_import_rows$Baths.2pce)] <- "0"
           
           unique_import_rows$Baths.3pce[is.na(unique_import_rows$Baths.3pce)] <- "0"
           
           unique_import_rows$Baths.4pce[is.na(unique_import_rows$Baths.4pce)] <- "0"
           
           unique_import_rows$Baths.5pce[is.na(unique_import_rows$Baths.5pce)] <- "0"
           
           unique_import_rows$Basement.Description[is.na(unique_import_rows$Basement.Description)] <- ""
           
           unique_import_rows$Date.Sold[is.na(unique_import_rows$Date.Sold)] <- ""
           
           unique_import_rows$Price.Sold <- gsub("\\$|,", "", unique_import_rows$Price.Sold)
           
           #unique_import_rows$Price.Current <- gsub("\\$|,", "", unique_import_rows$Price.Current)
           
           unique_import_rows$Price.Original <- gsub("\\$|,", "", unique_import_rows$Price.Original)
           
           unique_import_rows$Price.List <- gsub("\\$|,", "", unique_import_rows$Price.List)
           
           unique_import_rows$View.[is.na(unique_import_rows$View.)] <- ""
           
           
           
           #Cleanup NA GLA
           # Convert specified columns to numeric for sum calculation
           lower_numeric <- as.numeric(unique_import_rows$SqFt.Fin.Lower)
           main_numeric <- as.numeric(unique_import_rows$SqFt.Fin.Main)
           second_numeric <- as.numeric(unique_import_rows$SqFt.Fin.Second)
           third_numeric <- as.numeric(unique_import_rows$SqFt.Fin.Third)
           
           # Find rows where Fin.SqFt.Total is NA
           na_indices <- is.na(unique_import_rows$Fin.SqFt.Total)
           
           # Calculate the sum of the specified columns only for rows where Fin.SqFt.Total is NA
           sum_columns <- rowSums(cbind(lower_numeric, main_numeric, second_numeric, third_numeric)[na_indices, ], na.rm = TRUE)
           
           # Convert back to original type before replacing NA values in Fin.SqFt.Total
           unique_import_rows$SqFt.Fin.Lower <- as.character(unique_import_rows$SqFt.Fin.Lower)
           unique_import_rows$SqFt.Fin.Main <- as.character(unique_import_rows$SqFt.Fin.Main)
           unique_import_rows$SqFt.Fin.Second <- as.character(unique_import_rows$SqFt.Fin.Second)
           unique_import_rows$SqFt.Fin.Third <- as.character(unique_import_rows$SqFt.Fin.Third)
           
           # Replace NA values in Fin.SqFt.Total with the calculated sum only for rows where Fin.SqFt.Total is NA
           unique_import_rows$Fin.SqFt.Total[na_indices] <- sum_columns
           
           # Apply the function to the "Remarks" column and update the "condition" column
           condition_keywords <- read.csv("data/condition_keywords.csv")
           
           unique_import_rows$condition <- ifelse(!is.na(unique_import_rows$condition),
                                                  unique_import_rows$condition,
                                                  sapply(unique_import_rows$Remarks, assign_condition_score, condition_keywords = condition_keywords))
           
           unique_import_rows <- unique_import_rows %>%
             mutate(`Fin.SqFt.Total` = if_else(`Property.Sub.Type` == "Land", "0", `Fin.SqFt.Total`))
           
           # Geocode the identified rows
           
           for(i in 1:nrow(unique_import_rows)) {
             location <- unique_import_rows$location[i]
             geocoded <- geocode_address(location)
             unique_import_rows$lat[i] <- geocoded$lat
             unique_import_rows$lon[i] <- geocoded$lon
           }
           
           # Create a reactiveValues object to store unique_import_rows
           rv <- reactiveValues(unique_rows = unique_import_rows)
           #Put MLS Numbers in the clipboard
           clipMLSNumbers(unique_import_rows[, c("MLS..No")])
           
           displayed_columns  <- c("MLS..No", "Address", "City", "Date.Pending", "Price.Sold", "condition")  # Adjust column names as needed
           column_indices <- match(displayed_columns, colnames(rv$unique_rows))

           # Render the datatable with the edited data
           output$editUniqueRows <- renderDT({
             # Check if edited data exists, if not, use original data
             if (!is.null(rv$data)) {
               datatable(rv$data[, displayed_columns, drop = FALSE], editable = TRUE, options = list(pageLength = 25))
             } else {
               datatable(rv$unique_rows[, displayed_columns, drop = FALSE], editable = TRUE, options = list(pageLength = 25))
             }
           })
           
           observeEvent(input$editUniqueRows_cell_edit, {
             info <- input$editUniqueRows_cell_edit
             edit_row <- info$row
             edit_col <- info$col
             edit_value <- info$value
             
             # Determine the correct column name in rv$unique_rows
             col_index <- column_indices[edit_col]
             
             #col_name <- colnames(rv$unique_rows)[edit_col]
             
             # Update the corresponding column in the reactiveValues object
             #rv$unique_rows[edit_row, col_name] <- edit_value
             rv$unique_rows[edit_row, col_index] <- edit_value
           })
           
           
           observeEvent(input$saveEditedRowsButton, {
             if (!is.null(rv$unique_rows)) {
               master_df <- masterData()

               # Update master_df with edited rows
               updated_master_df <- rbind(master_df, rv$unique_rows)
               
               # Save updated master list
               write.csv(updated_master_df, "data/ADFMaster.csv", row.names = FALSE)
               
               # Display message about successful save
               output$messageText <- renderText({
                 paste("Edited rows saved to master list.")
               })
             } else {
               output$messageText <- renderText({
                 paste("No edits to save.")
               })
             }
           })

         } else {
           output$messageText <- renderText({ "No unique rows found." })
         }
         
         updateSelectInput(session, "adfSubAreaInput", choices = c("Select an option" = "", unique(master_df$`Sub.Area`), selected = NULL))
         updateSelectInput(session, "adfPropertySubType", choices = c("Select an option" = "", unique(master_df$`Property.Sub.Type`), selected = NULL))
         updateSelectInput(session, "listingsSubAreaInput", choices = c("Select an option" = "", unique(master_df$`Sub.Area`), selected = NULL))
         updateSelectInput(session, "listingsPropertyType", choices = c("Select an option" = "", unique(master_df$`Property.Sub.Type`), selected = NULL))
         
         num_observations <- nrow(master_df)
         output$numObservations <- renderText({
           paste("Number of Observations:", num_observations)
         })
         
         min_date_pending <- min(master_df$Date.Pending, na.rm = TRUE)
         max_date_pending <- max(master_df$Date.Pending, na.rm = TRUE)
         
         output$minDatePending <- renderText({
           paste("Minimum Date Pending:", min_date_pending)
         })
         output$maxDatePending <- renderText({
           paste("Maximum Date Pending:", max_date_pending)
         })

         masterData(master_df)
         
       }
    }
  })
  
  

  observeEvent(input$copyNewMLSButton, {
    mls_numbers <- clipMLSNumbers()
    clipr::write_clip(mls_numbers)
  })
  
  # IMPORT LISTING INVENTORY DATA ----
  
  observeEvent(input$listingInventoryFilePath, {
    if (!is.null(input$listingsSubAreaInput) && !is.null(input$listingsPropertyType)) {
      #browser()
      # Read the CSV file without modifying column names
      imported_inventory <- read.csv(input$listingInventoryFilePath$datapath, header = TRUE, check.names = FALSE)
      
      # Convert the data to long format
      imported_inventory <- imported_inventory %>%
        pivot_longer(cols = -Month, names_to = "Year", values_to = "count") %>%
        mutate(month = parse_date_time(paste0(Month, " ", Year), orders = "bY")) %>%
        mutate(month = as.Date(month)) %>%
        select(month, count) %>%
        arrange(month)
      
      # Remove rows where count is NA
      imported_inventory <- na.omit(imported_inventory)

      #Build the file name based on sub area
      inventory_file_name = paste0("data/Active Listings - ",input$listingsSubAreaInput," - ",input$listingsPropertyType,".csv")
      
      #If there's already a file import it and merge the data
      if (file.exists(inventory_file_name)) {
        master_inventory <- read.csv(inventory_file_name)
        master_inventory$month <- as.Date(master_inventory$month)
        
        unique_inventory_rows <- imported_inventory[!imported_inventory$month %in% master_inventory$month, ]
        
        updated_master_inventory <- rbind(master_inventory, unique_inventory_rows)
        write.csv(updated_master_inventory, inventory_file_name, row.names = FALSE)
        added_rows <- nrow(unique_inventory_rows)
        if (added_rows == 0) {
          output$messageTextInventory <- renderText({ 
            paste("Inventory data is up to date. Nothing was added")
          }) 
        }
        else {
          output$messageTextInventory <- renderText({ 
            paste("Add inventory data was successful. Added", added_rows, "rows.") 
          })
        }
        master_inventory <- updated_master_inventory
      }
      else {
        write.csv(imported_inventory, inventory_file_name, row.names = FALSE)
        master_inventory <- imported_inventory
        added_rows <- nrow(master_inventory)
        output$messageTextInventory <- renderText({ 
          paste("Add inventory data successful. Added", added_rows, "rows.") 
        })
        
      }
      
      output$listings <- renderDataTable({
        master_inventory
      })
    }
    else {
      showModal(
        modalDialog(
          title = "Message",
          "Choose a Sub Area",
          footer = NULL,  # Remove the footer (OK button)
          easyClose = TRUE  # Allow closing the popup by clicking outside
        )
      )
    }
  })
  
  # ADF CALCULATION BASED ON INPUTS ----
  adf_data_calc <- reactive({
    #browser()
    req(input$adfStartMonth, input$adfStartYear)
    startDate <- paste(input$adfStartYear, match(input$adfStartMonth, month.name), "01", sep = "-")
    endDate <- paste(input$adfEndYear, match(input$adfEndMonth, month.name), "01", sep = "-")
    # Adjust endDate to the last day of the selected month
    endDate <- as.Date(endDate) %m+% months(1) %m-% days(1)
    
    # Subtract one year from startDate
    startDatePrevious <- as.Date(startDate) %m-% years(1)
    
    # Subtract one year from startDate
    endDatePrevious <- as.Date(endDate) %m-% years(1)

    data <- subset(masterData(),
                   Sub.Area %in% input$adfSubAreaInput &
                   Property.Sub.Type %in% input$adfPropertySubType &
                   Date.Pending >= as.Date(startDatePrevious) &
                   Date.Pending <= as.Date(endDate) &
                   Price.Sold >= input$adfSalePriceLow &
                   Price.Sold <= input$adfSalePriceHigh &
                   (
                     (input$adf_waterfront == "None" & Waterfront. == "No") |
                       (input$adf_waterfront == "All") |
                       (input$adf_waterfront == "Lake" & Waterfront.Features == "Lake") |
                       (input$adf_waterfront == "Ocean" & Waterfront.Features == "Ocean") |
                       (input$adf_waterfront == "River" & Waterfront.Features == "River")
                   )
    )

    data$month <- month(data$`Date.Pending`)
    data$year <- year(data$`Date.Pending`)
    data$`Date.Pending` <- as.Date(data$`Date.Pending`)
    data$`Price.List` <- as.numeric(gsub("[^0-9.]", "", data$`Price.List`))
    data$`Price.Original` <- as.numeric(gsub("[^0-9.]", "", data$`Price.Original`))

        data <- data %>%
      mutate(SPAP = `Price.Sold` / `Price.List`) %>%
      mutate(SPOAP = `Price.Sold` / `Price.Original`)
    
    # data$SPAP <- data$`Price.Sold` / data$`Price.List`
    # data$SPOAP <- data$`Price.Sold` / data$`Price.Original`
    
    data$`Fin.SqFt.Total` <- as.numeric(gsub(",", "", data$`Fin.SqFt.Total`))
    data$`Lot.Size.SqFt` <- as.numeric(gsub(",", "", data$`Lot.Size.SqFt`))
    data$`Year.Built..Est.` <- as.numeric(data$`Year.Built..Est.`)
    
    # Get the IDs (or other unique identifiers) of the outlier rows
    outlier_ids <- adfDataOutliers()[["MLS..No"]]
    # Filter out these IDs from adfData
    data <- data %>%
      filter(!(MLS..No %in% outlier_ids))

    # For Sub Areas
    adfTitleSubAreas(paste(paste(sub("^PA ", "", input$adfSubAreaInput), collapse = ", ")))
    
    # For Property Types
    adfTitlePropertyType(paste(paste(sub("^PA ", "", input$adfPropertySubType), collapse = ", ")))
    

    cmsTitle(paste0(sub("^PA ", "", input$adfSubAreaInput)," - ", input$adfPropertySubType))
    
    previous_data <- data %>% 
      filter(`Date.Pending` > startDatePrevious & `Date.Pending` < endDatePrevious)
    
    adfDataHistorical(previous_data)

    data <- data %>% 
      filter(`Date.Pending` > startDate & `Date.Pending` < endDate)
    
    ## Reduce ADF to neighbourhoods ----
    if (!is.null(input$adf_neighborhoods)) {
      #browser()
      filtered_df <- data.frame()
      folder_path <- "neighbourhoods"
      geojson_files <- list.files(path = folder_path, pattern = ".geojson", full.names = TRUE)
      selected_neighborhoods <- input$adf_neighborhoods  # Now it's a vector of selected neighborhoods
      #print(selected_neighborhoods)
      neighborhoods_str <- paste(selected_neighborhoods, collapse = ", ")

      adfTitle(paste(adfTitleSubAreas(), " - ",neighborhoods_str))
      #print(adfTitle())

      #browser()
      #filtered_df <- data.frame()

      for (file in geojson_files) {
        geojson <- sf::st_read(file, quiet = TRUE)
        matching_neighborhoods <- geojson$name[geojson$name %in% selected_neighborhoods]

        for (selected_neighborhood in matching_neighborhoods) {
          #browser()
          # Filter the selected neighborhood
          selected_neighborhood_geo <- geojson[geojson$name == selected_neighborhood, ]

          # Extract the polygon geometry
          polygon_geometry <- selected_neighborhood_geo$geometry

          # Ensure CRS of 'data' and 'polygon_geometry' are the same
          # Assuming 'data' is a data.frame with lon and lat columns
          data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = st_crs(polygon_geometry))

          # Check and set CRS if necessary
          if (st_crs(data_sf) != st_crs(polygon_geometry)) {
            data_sf <- st_transform(data_sf, st_crs(polygon_geometry))
          }

          # Filter points within 'polygon_geometry'
          neighbourhhood_df <- data_sf[st_within(data_sf, polygon_geometry, sparse = FALSE), ]

          # Convert back to data frame with lat and lon
          coords <- st_coordinates(neighbourhhood_df)
          neighbourhhood_df_back <- data.frame(st_drop_geometry(neighbourhhood_df), lon = coords[, 1], lat = coords[, 2])

          # The filtered data frame
          filtered_df <- rbind(filtered_df, neighbourhhood_df_back)
        }
      }
      #browser()
      data <- filtered_df
    }
    else {
      adfTitle(adfTitleSubAreas())
    }

    return(data)
  })
  
  # ADF LOCATION MAP ----
  observe({
    # req ensures that adf_data_calc is not NULL
    req(masterData())
    # Update adfData with the value from adf_data_calc
    adfData(adf_data_calc())
  })
  
  output$adf_data_map <- renderMapboxer({
    LAYER_ID <- "sales"
    adf_data <- adfData()
    
    if (!is.null(subjectLat()) || !is.null(subjectLon())) {
      subject_lon <- subjectLon()
      subject_lat <- subjectLat()
    } else {
      subject_lon <- mean(adf_data$lon)
      subject_lat <- mean(adf_data$lat)
    }

    colnames(adf_data)[colnames(adf_data) == "lon"] <- "lng"
    colnames(adf_data)[colnames(adf_data) == "MLS..No"] <- "mls"
    colnames(adf_data)[colnames(adf_data) == "Date.Pending"] <- "sale_date"
    colnames(adf_data)[colnames(adf_data) == "Price.Sold"] <- "sale_price"
    colnames(adf_data)[colnames(adf_data) == "Year.Built..Est."] <- "year_built"
    colnames(adf_data)[colnames(adf_data) == "Lot.Size.SqFt"] <- "lot_size"
    colnames(adf_data)[colnames(adf_data) == "Fin.SqFt.Total"] <- "gla"

    adf_data$mls <- as.character(adf_data$mls)
    adf_data$lot_size <- as.character(adf_data$lot_size)
    
    adf_data$poplng <- as.character(adf_data$lng)
    adf_data$poplat <- as.character(adf_data$lat)
    
    #print(str(adf_data))

    my_center <- c(lng = mean(adf_data$lng, na.rm = TRUE), lat = mean(adf_data$lat, na.rm = TRUE))

    map <- as_mapbox_source(adf_data,
                            lng = "lng",
                            lat = "lat"
                            ) %>%
      mapboxer(center = my_center,
               zoom = 11,
               style = "mapbox://styles/mapbox/outdoors-v12", token = MAPBOX_TOKEN) %>%
        add_navigation_control() %>%

        add_circle_layer(
          id = LAYER_ID,
          popup = "<p>MLS No.: {{mls}}<br>{{location}}<br>Date of Sale: {{sale_date}}<br>Sale Price: {{sale_price}} <br>Lot size: {{lot_size}}<br>Year Built: {{year_built}}<br>GLA: {{gla}}<br>Bedrooms: {{Beds}} <br> Bathrooms: {{Baths}} <br> Latitude: {{poplat}} <br> Longitude: {{lng}}</p>",
          circle_radius = list("interpolate",
                               list("linear"),
                               list("get", "sale_price"),
                               200000, 2.5,  # Circle radius 10 for sale_price 100000
                               300000, 5,
                               400000, 7.5,
                               500000, 10,
                               600000, 12.5,
                               700000, 15,
                               800000, 17.5,
                               900000, 20,
                               1000000, 22.5), # Circle radius 30 for sale_price 1000000
          circle_color = list("interpolate",
                               list("linear"),
                               list("get", "sale_price"),
                               200000, "DarkGreen",  # Circle radius 10 for sale_price 100000
                               300000, "OliveDrab",
                               400000, "DarkKhaki",
                               500000, "GoldenRod",
                               600000, "DarkOrange",
                               700000, "Tomato",
                               800000, "IndianRed",
                               900000, "FireBrick",
                               1000000, "red") # Circle radius 30 for sale_price 1000000
          
        ) %>%
        add_marker(lng = subject_lon, lat = subject_lat, popup = "Subject Property")
    
    map
  })
  
  # ADF BY SELECTED NEIGHBOURHOODS ----
  
  ## Function to read GeoJSON files ----
  get_neighborhood_names <- function() {
    folder_path <- "neighbourhoods"
    geojson_files <- list.files(path = folder_path, pattern = ".geojson", full.names = TRUE)
    
    neighborhood_names <- character(0)
    
    for (file in geojson_files) {
      geojson <- sf::st_read(file, quiet = TRUE)
      neighborhood_names <- c(neighborhood_names, unique(geojson$name))
    }
    
    neighborhood_names <- unique(neighborhood_names)
    return(neighborhood_names)
  }

  ## Initialize neighborhood choices in the UI ----
  neighborhood_choices <- get_neighborhood_names()
  updateSelectInput(session, "adf_neighborhoods", choices = neighborhood_choices)
  updateSelectInput(session, "subject_neighbourhood", choices = neighborhood_choices, selected = "")

  observeEvent(input$resetADFSearchButton, {
    # Update inputs to their default values
    updateSelectInput(session, "adfSubAreaInput", selected = "")
    updateSelectInput(session, "adfPropertySubType", selected = "")
    updateSelectInput(session, 'adfStartMonth', 'Start Month:', month.name, selected = month.name[month(Sys.Date())])
    updateSelectInput(session, 'adfStartYear', 'Start Year:', as.character(2025:2000), selected = as.character(year(Sys.Date())))
    updateSelectInput(session, 'adfEndMonth', 'End Month:', month.name, selected = month.name[month(Sys.Date())])
    updateSelectInput(session, 'adfEndYear', 'End Year:', as.character(2025:2000), selected = as.character(year(Sys.Date())))
    updateNumericInput(session, 'adfSalePriceLow', 'Price Low:', value = 0)
    updateNumericInput(session, 'adfSalePriceHigh', 'Price High:', value = 1000000000)
    updateSelectInput(session, "adf_neighborhoods", selected = "")
    updateRadioButtons(session, 'adf_waterfront', selected = "None")
    updateRadioButtons(session, 'adf_price_per_unit', selected = "Bulk")
    
    
    #reload ADF database
    master_df <- read.csv("data/ADFMaster.csv")
    master_df$Date.Pending <- as.Date(master_df$Date.Pending)
    master_df$Price.Sold <- as.numeric(gsub("[^0-9.]", "", master_df$Price.Sold))
    master_df$Price.List <- as.numeric(gsub("[^0-9.]", "", master_df$Price.List))
    master_df$Price.Original <- as.numeric(gsub("[^0-9.]", "", master_df$Price.Original))
    
    
    updateSelectInput(session, "adfSubAreaInput", choices = c("Select an option" = "", unique(master_df$`Sub.Area`), selected = NULL))
    updateSelectInput(session, "adfPropertySubType", choices = c("Select an option" = "", unique(master_df$`Property.Sub.Type`), selected = NULL))
    updateSelectInput(session, "listingsSubAreaInput", choices = c("Select an option" = "", unique(master_df$`Sub.Area`), selected = NULL))
    updateSelectInput(session, "listingsPropertyType", choices = c("Select an option" = "", unique(master_df$`Property.Sub.Type`), selected = NULL))
    
    unique_views <- unique(trimws(unlist(strsplit(master_df$View, ","))))
    unique_views <- unique_views[!is.na(unique_views)]
    updateSelectInput(session, "cms_view", choices = c("Select an option" = "", unique_views))
    
    condition_choices = c('Poor','Fair','Average','Good','Excellent')
    updateSelectInput(session, "cms_condition", choices = c("Select an option" = "", condition_choices))
    
    unique_extras <- unique(trimws(unlist(strsplit(master_df$`Other.Structures`, ","))))
    unique_extras <- unique_extras[!is.na(unique_extras)]
    updateSelectInput(session, "cms_extras", choices = c("Select an option" = "", unique_extras))
    
    # Calculate the number of observations
    num_observations <- nrow(master_df)
    output$numObservations <- renderText({
      paste("Observations:", num_observations)
    })
    
    # Calculate the minimum and maximum of Date.Pending
    min_date_pending <- min(master_df$Date.Pending, na.rm = TRUE)
    max_date_pending <- max(master_df$Date.Pending, na.rm = TRUE)
    
    output$minDatePending <- renderText({
      paste("Minimum Date Sold:", min_date_pending)
    })
    output$maxDatePending <- renderText({
      paste("Maximum Date Sold:", max_date_pending)
    })
    
    masterData(master_df)
    
    #browser()
  })
  
  observeEvent(list(input$resetCMSSearchButton,adf_data_calc()), {
    #browser()
    adfdata <- adfData()
    # Update inputs to their default values
    
    #browser()

    cms_year_built_min(min(adfdata$`Year.Built..Est.`))
    cms_year_built_max(max(adfdata$`Year.Built..Est.`))
    
    updateNumericRangeInput(
      #session = session,
      inputId = "yearBuiltRange",
      value = c(cms_year_built_min(), cms_year_built_max())
    )

    cms_gla_min(min(adfdata$`Fin.SqFt.Total`))
    cms_gla_max(max(adfdata$`Fin.SqFt.Total`))
    
    updateNumericRangeInput(
      #session = session,
      inputId = "glaRange",
      value = c(cms_gla_min(), cms_gla_max())
    )
    
    cms_lot_size_min(min(adfdata$`Lot.Size.SqFt`))
    cms_lot_size_max(max(adfdata$`Lot.Size.SqFt`))
    
    updateNumericRangeInput(
      #session = session,
      inputId = "lotSizeRange",
      value = c(cms_lot_size_min(), cms_lot_size_max())
    )
    
    cms_beds_min(min(adfdata$`Beds`))
    cms_beds_max(max(adfdata$`Beds`))
    
    updateNumericRangeInput(
      #session = session,
      inputId = "bedsRange",
      value = c(cms_beds_min(), cms_beds_max())
    )
    
    cms_baths_min(min(adfdata$`Baths`))
    cms_baths_max(max(adfdata$`Baths`))
    
    updateNumericRangeInput(
      #session = session,
      inputId = "bathsRange",
      value = c(cms_baths_min(), cms_baths_max())
    )
    
    cms_condition_min(min(adfdata$`condition`))
    cms_condition_max(max(adfdata$`condition`))
    
    updateNumericRangeInput(
      #session = session,
      inputId = "conditionRange",
      value = c(cms_condition_min(), cms_condition_max())
    )
    
    cms_garage_min(min(adfdata$`Garage.Spaces`))
    cms_garage_max(max(adfdata$`Garage.Spaces`))
    
    updateNumericRangeInput(
      #session = session,
      inputId = "garageRange",
      value = c(cms_garage_min(), cms_garage_max())
    )
    
    cms_carport_min(min(adfdata$`Carport.Spaces`))
    cms_carport_max(max(adfdata$`Carport.Spaces`))
    
    updateNumericRangeInput(
      #session = session,
      inputId = "carportRange",
      value = c(cms_carport_min(), cms_carport_max())
    )
  })

  # LISTING INVENTORY CALCULATION ----
  
  adf_listing_data <- reactive({
    req(input$adfSubAreaInput, input$adfPropertySubType, input$adfStartMonth, input$adfStartYear)
    startDate <- paste(input$adfStartYear, match(input$adfStartMonth, month.name), "01", sep = "-")
    endDate <- paste(input$adfEndYear, match(input$adfEndMonth, month.name), "01", sep = "-")
    # Adjust endDate to the last day of the selected month
    endDate <- as.Date(endDate) %m+% months(1) %m-% days(1)
    # Initialize 'data' to prevent scope issues
    aggregated_data <- data.frame(month = as.Date(character()), count = numeric())
    
    # Iterate over each combination of sub-area and property subtype
    for (sub_area in input$adfSubAreaInput) {
      for (prop_subtype in input$adfPropertySubType) {
        # Build the file name
        inventory_file_name <- paste0("data/Active Listings - ", sub_area, " - ", prop_subtype, ".csv")
        
        # Check if the file exists
        if (file.exists(inventory_file_name)) {
          # Read the data from the file
          listing_inventory <- read.csv(inventory_file_name)
          listing_inventory$month <- as.Date(listing_inventory$month, format="%Y-%m-%d")
          
          # Filter the data based on the date range
          filtered_data <- subset(listing_inventory,
                                  listing_inventory$month > as.Date(startDate) &
                                    listing_inventory$month < as.Date(endDate))
          
          # Sum up the counts by month and sub-area
          monthly_sum <- aggregate(count ~ month, data = filtered_data, sum)
          
          # Combine the results
          aggregated_data <- rbind(aggregated_data, monthly_sum)
        }
      }
    }
    
    # Aggregate counts across sub-areas for each month
    aggregated_data <- aggregate(count ~ month, data = aggregated_data, sum)
    
    return(aggregated_data)
  })
  
  # ADF GRAPHS ----
  ## ADF Graph Sale Price vs Sale Date ----
  output$adfSalePrice_SaleDatePlot <- renderPlotly(
    
    {
      #adf_data <- adf_data()
      adf_data_with_tooltips <- adfData() %>%
        mutate(tooltip_text = paste("MLS No.:", MLS..No, 
                                    "<br>Address:", Address,
                                    "<br>Sale Date", Date.Pending,
                                    "<br>Sale Price", Price.Sold
        ))
      
      adf_data_with_tooltips <- adf_data_with_tooltips %>%
        mutate(Price_Per_Unit = case_when(
          input$adf_price_per_unit == "Bulk" ~ Price.Sold,
          input$adf_price_per_unit == "Square foot" ~ Price.Sold / Lot.Size.SqFt,
          input$adf_price_per_unit == "Acre" ~ Price.Sold / Lot.Size.Acres,
          input$adf_price_per_unit == "Total Floor Area" ~ Price.Sold / Fin.SqFt.Total,
          TRUE ~ Price.Sold  # default case
        ))
      
      gg <- ggplot(adf_data_with_tooltips) +
        aes(`Date.Pending`, `Price_Per_Unit`, key = MLS..No) +
        geom_point(aes(text = tooltip_text)) +
        geom_smooth(method = "lm",
                    formula = y ~ poly(x,1),
                    se = F,
                    color = "blue"
        ) +
        geom_smooth(method = "lm",
                    formula = y ~ poly(x,3),
                    se = F,
                    color = "red"
        ) +
        scale_x_date(date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(
          #labels = scales::dollar_format(prefix = "$", big.mark = ",", scale = 1e-3, suffix = "K"),
          labels = scales::dollar_format(prefix = "$", big.mark = ","),
          expand = c(0, 0),
          breaks = scales::pretty_breaks(n = 10)
        ) +
        labs(title = "Sale Price vs Sale Date",
             x = "Reported Sale Date",
             y = "Reported Sale Price") +
        theme_bw() +
        theme(
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 10, face = "bold")
        )
      
      suppressWarnings(ggplotly(gg, tooltip = "text", source = "adfSalePrice_SaleDatePlot"))
    })
  
  observeEvent(event_data("plotly_click", source = "adfSalePrice_SaleDatePlot"), {
    click_data <- event_data("plotly_click", source = "adfSalePrice_SaleDatePlot")
    # Extract current data
    adf_data <- adfData()
    
    # Find the clicked point from adfData and add it to outliers df
    datapoint_clicked <- adf_data[adf_data$MLS..No == click_data$key, ]
    adfDataOutliers(rbind(datapoint_clicked, adfDataOutliers()))
  })
  
  # Render the outlier table
  output$ADFOutlierTable <- renderDT({
    req(adfDataOutliers())
    data_to_display <- adfDataOutliers()[, c("MLS..No", "Address", "City", "Date.Pending", "Price.Sold")]
    
    datatable(data_to_display,
              selection = 'none', 
              options = list(
                preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
              ),
              callback = JS(
                "table.on('dblclick', 'td',", 
                "  function() {",
                "    var row = table.cell(this).index().row;",
                "    var col = table.cell(this).index().column;",
                "    Shiny.setInputValue('dt_dblclick', {dt_row: row, dt_col: col});",
                "  }",
                ");"
              ))
  })

  # Handle the double-click event
  observeEvent(input$dt_dblclick, {
    if (!is.null(input$dt_dblclick)) {
      row_index <- input$dt_dblclick$dt_row + 1
      #print(paste("row index: ", row_index))
      data <- adfDataOutliers()
      mls_number <- data[row_index, 1]
      matching_row <- adfDataOutliers() %>%
        filter(`MLS..No` == mls_number)
      
      # Step 2: Extract the matching row from adfDataOutliers()
      matching_row <- as.data.frame(matching_row)
      
      # Step 3: Append or concatenate the matching row to adfData()
      adfDataNew <- rbind(adfData(), matching_row)
      
      # Step 4: Delete the matching row from adfDataOutliers()
      adfDataOutliersNew <- adfDataOutliers() %>%
        filter(`MLS..No` != mls_number)
      
      # Update the reactiveVals with the new data
      adfData(adfDataNew)
      adfDataOutliers(adfDataOutliersNew)
    }
  })
  
  observeEvent(input$reset_adf_outliers, {
    adfData(rbind(adfData(), adfDataOutliers()))
    adfDataOutliers(adfDataOutliers()[logical(0), ])
  })
  
  observeEvent(adfDataOutliers(), {
    # Ensure there are outliers to remove
    req(adfDataOutliers())
    
    # Get the IDs (or other unique identifiers) of the outlier rows
    outlier_ids <- adfDataOutliers()[["MLS..No"]]
    
    # Filter out these IDs from adfData
    updated_adf_data <- adfData() %>%
      filter(!(MLS..No %in% outlier_ids))
    
    # Update adfData with the new dataset
    adfData(updated_adf_data)
  }, ignoreNULL = FALSE)
  
  observeEvent(cmsDataOutliers(), {
    # Ensure there are outliers to remove
    req(cmsDataOutliers())
    
    # Get the IDs (or other unique identifiers) of the outlier rows
    outlier_ids <- cmsDataOutliers()[["MLS..No"]]
    
    # Filter out these IDs from adfData
    updated_cms_data <- cmsData() %>%
      filter(!(MLS..No %in% outlier_ids))
    
    # Update adfData with the new dataset
    cmsData(updated_cms_data)
  }, ignoreNULL = FALSE)
  
  
  ## ADF Graph Sale Count by Price Range ----
  
  output$adfCountByPriceRangePlot <- renderPlotly({
    adf_data <- adfData()
    MeanADF <- mean(adf_data$Price.Sold, na.rm = TRUE)
    # Calculate the histogram
    hist_data <- ggplot_build(
      ggplot(adf_data, aes(x = `Price.Sold`)) +
        geom_histogram(binwidth = 20000, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
    
    # Find the bin with the highest count
    max_count <- max(hist_data$data[[1]]$count)
    
    mean_label <- "           \u2190Mean"

    gg <- ggplot(adfData()) +
      geom_histogram(aes(x=`Price.Sold`),
                     binwidth = 20000,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      theme_bw() +
      labs(title = "Sale Count by Price Range",
           x = "Reported Sale Price",
           y = "Count") +
      scale_x_continuous(
        labels = scales::dollar_format(prefix = "$", big.mark = ",", scale = 1e-3, suffix = "K"),
        expand = c(0, 0),
        breaks = scales::pretty_breaks(n = 10)
      ) +
      geom_vline(xintercept = MeanADF, color = "blue", linetype = "solid", linewidth = 0.5) +
      geom_text(aes(x=MeanADF,
                    label=mean_label,
                    y=max_count),
                    colour="blue",
                    size = 3
                    ) +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")
      )

    ggplotly(gg)
  })
  
  ## ADF Graph Sale Count by Month ----
  
  output$adfCountByMonthPlot <- renderPlotly({
    sales_counts <- adfData()[c("Date.Pending")] %>%
      mutate(Date.Pending = as.Date(Date.Pending, format = "%Y/%m/%d")) %>%
      group_by(month = as.Date(format(Date.Pending, "%Y-%m-01"))) %>%
      summarise(Count = n())
    gg <- ggplot(data = sales_counts, aes(x = month, y = Count)) +
      geom_bar(stat = "identity",
               fill = "blue",
               alpha = 0.1,
               colour = "black",
               linewidth=0.5) + 
      xlab("Reported Sale Date") + 
      ylab("Count") +
      
      labs(title = "Sale Count by Month") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")) +
      scale_x_date(date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      #geom_smooth(method = "lm", se = FALSE) +
      geom_smooth(method = "lm",
                  formula = "y ~ poly(x, 3)",
                  se = FALSE,
                  colour = "red")
    suppressWarnings(ggplotly(gg))
  })
  
  ## ADF Graph Days on Market ----
  
  output$adfDaysOnMarket <- renderPlotly({
    # data <- adfData()[c("Date.Listed", "Date.Pending")] %>%
    #   mutate(DOM = interval(`Date.Listed`, `Date.Pending`) /days(1))
    # data$Date.Pending <- as.Date(data$Date.Pending)
    
    adf_data_with_tooltips <- adfData() %>%
      mutate(tooltip_text = paste("MLS No.:", MLS..No, 
                                  "<br>Address:", Address,
                                  "<br>Sale Date", Date.Pending,
                                  "<br>Sale Price", Price.Sold,
                                  "<br>Days on Market:", Days.On.Market
      ))
    
    gg <- ggplot(adf_data_with_tooltips) +
      aes(`Date.Pending`, `Days.On.Market`) +
      geom_point(aes(text = tooltip_text)) +
      geom_smooth(se = TRUE,
                  colour = "red") +
      scale_x_date(labels = date_format("%m-%y"),
                   date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(name="Reported Days on Market", breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Days on Market",
           x = "Reported Sale Date") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")
      )
    ggplotly(gg, tooltip = "text")
  })
  
  ## ADF Graph Sale to List Price Ratio ----
  
  output$adfSaleToListprice <- renderPlotly({
    data <- adfData()

    adf_data_with_tooltips <- data %>%
      mutate(tooltip_text = paste("MLS No.:", MLS..No, 
                                  "<br>Address:", Address,
                                  "<br>Sale Date", Date.Pending,
                                  "<br>Sale Price", Price.Sold
      ))
    
    gg <- ggplot(adf_data_with_tooltips) +
      aes(`Date.Pending`, SPAP) +
      geom_point(aes(text = tooltip_text)) +
      geom_smooth(se = FALSE,
                  colour = "red") +
      geom_hline(yintercept=1, linewidth=1) +
      scale_x_date(labels = date_format("%m-%y"),
                   date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(name="Sale to List Price Ratio", breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Sale to List Price Ratio",
           x = "Reported Sale Date") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")
      )
    ggplotly(gg, tooltip = "text")
  })
  
  ## ADF Graph Listing Inventory ----
  
  output$adfListingsInventory <- renderPlotly({
    gg <- ggplot(adf_listing_data(), aes(month, count)) +
      geom_bar(stat = "identity",
               fill = "blue",
               alpha = 0.1,
               colour = "black",
               linewidth=0.5) + 
      xlab("Reported Sale Date") + 
      ylab("Count") +
      
      geom_smooth(method = "lm",
                  formula = y ~ poly(x,3),
                  se = F,
                  color = "red"
      ) +
      scale_y_continuous(name="Count", breaks = scales::pretty_breaks(n = 10)) +
      scale_x_date(labels = date_format("%m-%y"),
                   date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Listings Inventory by Month",
           x = "Reported Sale Date",
           y = "Active Listings Count") +
      
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")
      )
    suppressWarnings(ggplotly(gg))
  })
  
  ## ADF Graph Market Price Trends ----
  
  output$adfMarketPriceTrends <- renderPlotly({
    #data <- adfData()[c("Date.Pending", "Price.Sold", "Price.List", "Price.Original")]
    data <- adfData()
    data$Date.Pending <- as.Date(data$Date.Pending)
    # Remove non-numeric characters and convert to numeric
    data$Price.Sold <- as.numeric(gsub("[^0-9.]", "", data$Price.Sold))
    data$Price.List <- as.numeric(gsub("[^0-9.]", "", data$Price.List))
    data$Price.Original <- as.numeric(gsub("[^0-9.]", "", data$Price.Original))
    
    gg <- ggplot(data) +
      geom_smooth(aes(`Date.Pending`, `Price.Original`, color = "Original List Price"),
                  method = "lm", linewidth = 1, se = FALSE) +
      geom_smooth(aes(`Date.Pending`, `Price.List`, color = "List Price"),
                  method = "lm", linewidth = 1, se = FALSE) +
      geom_smooth(aes(`Date.Pending`, `Price.Sold`, color = "Sale Price"),
                  method = "lm", linewidth = 1, se = FALSE) +
      scale_color_manual(name = "Price Trend Lines",
                         values = c("green", "blue", "red")) +
      scale_x_date(labels = date_format("%m-%y"),
                   date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(name = "Price", breaks = scales::pretty_breaks(n = 10)) +
      
      labs(title = "Market Price Trends",
           x = "Reported Sale Date") +
      theme_bw() +
      theme(
        legend.position = c(0.20, 0.20),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.75,"line"),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")
      )
    ggplotly(gg)
  })
  
  ## ADF Graph Sale Count as % of Inventory ----
  
  output$adfSalesVsInventory <- renderPlotly({
    sales_counts <- adfData()[c("Date.Pending")] %>%
      mutate(Date.Pending = as.Date(Date.Pending, format = "%Y-%m-%d")) %>%
      group_by(month = as.Date(format(Date.Pending, "%Y-%m-01"))) %>%
      summarise(count = n())
    
    listings_count <- adf_listing_data()
    
    #print(sales_counts)
    #print(listings_count)
    
    
    # Ensure that the 'month' column in listings_count is of Date type
    listings_count$month <- as.Date(listings_count$month)
    
    # Merge the two data frames by 'month'
    combined_data <- left_join(sales_counts, listings_count, by = "month")
    
    # Calculate the percentage
    # Handle the case where the denominator might be zero
    combined_data <- combined_data %>%
      mutate(percentage = count.x / ifelse(count.y == 0, NA, count.y))
    
    # Rename columns as needed
    combined_data <- rename(combined_data, date = month, sales_to_inventory_percentage = percentage)
    
    # Plot
    gg <- ggplot(combined_data) +
      geom_bar(aes(x = date, y = sales_to_inventory_percentage),
               stat = "identity",
               fill = "blue",
               alpha = 0.1,
               colour = "black",
               linewidth=0.5) +
      geom_smooth(aes(x = date, y = sales_to_inventory_percentage),
                  method = "lm",
                  formula = y ~ poly(x, 3),
                  se = FALSE,
                  color = "red") +
      xlab("Reported Sale Date") +
      scale_y_continuous(labels = scales::percent_format(),
                         limits = c(0, 1),
                         breaks = scales::pretty_breaks(n = 10)) +
      scale_x_date(labels = date_format("%m-%y"),
                   date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Absorption Rate") +
      
      # Theme settings
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")
      )
    suppressWarnings(ggplotly(gg))
  })
  
  ## ADF Graph Inventory Absorption in Months ----
  
  output$adfInventoryAbsorption <- renderPlotly({
    sales_counts <- adfData()[c("Date.Pending")] %>%
      mutate(Date.Pending = as.Date(Date.Pending, format = "%Y/%m/%d")) %>%
      group_by(month = as.Date(format(Date.Pending, "%Y-%m-01"))) %>%
      summarise(count = n())
    
    listings_count <- adf_listing_data()
    
    
    # Ensure that the 'month' column in listings_count is of Date type
    listings_count$month <- as.Date(listings_count$month)
    
    # Merge the two data frames by 'month'
    combined_data <- left_join(sales_counts, listings_count, by = "month")
    # Calculate the percentage
    # Handle the case where the denominator might be zero
    combined_data <- combined_data %>%
      mutate(months = count.y / ifelse(count.x == 0, NA, count.x))
    
    # Rename columns as needed
    combined_data <- rename(combined_data, date = month, inventory_months = months)
    # Plot
    gg <- ggplot(combined_data) +
      geom_bar(aes(x = date, y = inventory_months),
               stat = "identity",
               fill = "blue",
               alpha = 0.1,
               colour = "black",
               linewidth=0.5) +
      geom_smooth(aes(x = date, y = inventory_months),
                  method = "lm",
                  formula = y ~ poly(x, 3),
                  se = FALSE,
                  color = "red") +
      xlab("Reported Sale Date") +
      ylab("Month") +
      
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      scale_x_date(labels = date_format("%m-%y"),
                   date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Inventory Absorption in Months") +
      
      # Theme settings
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")
      )
    suppressWarnings(ggplotly(gg))
  })
  
  ## ADF Graph Inventory vs Sales ----
  
  output$adfInventoryVsSalesPlot <- renderPlotly({

    sales_counts <- adfData()[c("Date.Pending")] %>%
      mutate(Date.Pending = as.Date(Date.Pending, format = "%Y-%m-%d")) %>%
      group_by(month = as.Date(format(Date.Pending, "%Y-%m-01"))) %>%
      filter(month >= min(adf_listing_data()$month) & month <= max(adf_listing_data()$month)) %>%
      summarise(Count = n())
    
    #print(sales_counts)
    #print(adf_listing_data())
    #cat("Sales Counts Date Range: ", min(sales_counts$month), " to ", max(sales_counts$month), "\n")
    #cat("adf_listing_data Date Range: ", min(adf_listing_data()$month), " to ", max(adf_listing_data()$month), "\n")

    gg <- ggplot() +
      geom_bar(data = sales_counts,
               aes(x = month, y = Count),
               stat = "identity",
               fill = "blue",
               alpha = 0.5,
               colour = "black",
               linewidth=0.5) +
      
      geom_smooth(data = sales_counts,
                  aes(x = month, y = Count),
                  color = "red",
                  method = "lm",
                  formula = y ~ poly(x, 3),
                  linewidth = 1,
                  se = FALSE) +
      
      
      geom_bar(data = adf_listing_data(),
               aes(x = month, y = count),
               stat = "identity",
               fill = "blue",
               alpha = 0.1,
               colour = "black",
               linewidth=0.5) + 
      
      geom_smooth(data = adf_listing_data(),
                  aes(x= month, y=count),
                  color = "darkgreen",
                  method = "lm",
                  formula = y ~ poly(x, 3),
                  linewidth = 1,
                  se = FALSE) +
      
      
      xlab("Reported Sale Date") + 
      ylab("Count") +
      

      labs(title = "Inventory vs Sale Count") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")) +
      scale_x_date(date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(name="Count", breaks = scales::pretty_breaks(n = 10))
    suppressWarnings(ggplotly(gg))
  })
  
  ## ADF Graph Annual Average Price Comparison ----
  output$adfAnnualPriceTrendComparison <- renderPlotly({
    adf_data <- adfData()
    adf_data_previous <- adfDataHistorical()
    #browser()
    # Extract month and year from the Date.Pending column
    adf_data$Month <- format(as.Date(adf_data$Date.Pending), "%b")
    adf_data_previous$Month <- format(as.Date(adf_data_previous$Date.Pending), "%b")
    
    # Extract month and year from the Date.Pending column
    adf_data$Year <- format(as.Date(adf_data$Date.Pending), "%Y")
    adf_data_previous$Year <- format(as.Date(adf_data_previous$Date.Pending), "%Y")
    
    # Combine datasets into one for plotting
    combined_data <- rbind(
      transform(adf_data, Dataset = "adf_data_previous"),
      transform(adf_data_previous, Dataset = "adf_data_previous")
    )
    
    aggregated_data <- combined_data %>%
      group_by(Month, Year) %>%
      summarize(AveragePrice = mean(Price.Sold, na.rm = TRUE), .groups = 'drop')
    
    # Convert Month to factor with ordered levels based on calendar months
    aggregated_data$Month <- factor(aggregated_data$Month, levels = month.abb, ordered = TRUE)
    
    
    #print(aggregated_data, n=Inf)
      
    # Create bar plot using ggplot
    gg <- ggplot(aggregated_data, aes(x = Month, y = AveragePrice, fill = Year)) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 0.7,
               alpha = 0.5,
               colour = "black",
               linewidth=0.3) +
      geom_smooth(aes(group = Year,
                      color = Year,),
                  method = "lm",
                  formula = y ~ poly(x, 1),
                  se = FALSE,
      ) +
      guides(fill = FALSE) +
      scale_y_continuous(
            labels = scales::dollar_format(prefix = "$", big.mark = ",", scale = 1e-3, suffix = "K"),
            expand = c(0, 0),
            breaks = scales::pretty_breaks(n = 10)
          ) +
      labs(
            title = "Average Monthly Price vs Date by Year",
            x = "Month",
            y = "Average Sale Price",
            fill = "Year"
          ) +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "bottom"
      )
      #scale_fill_manual(values = c("adf_data_previous" = "blue", "adf_data_previous" = "red"))  # Custom colors for datasets

    ggplotly(gg) %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
  })

  ## ADF Graph Annual Sale Count Comparison ----
  output$adfAnnualSaleCountComparison <- renderPlotly({
    adf_data <- adfData()
    adf_data_previous <- adfDataHistorical()
    
    # Extract month and year from the Date.Pending column
    adf_data$Month <- format(as.Date(adf_data$Date.Pending), "%b")
    adf_data_previous$Month <- format(as.Date(adf_data_previous$Date.Pending), "%b")
    
    # Extract year from the Date.Pending column
    adf_data$Year <- format(as.Date(adf_data$Date.Pending), "%Y")
    adf_data_previous$Year <- format(as.Date(adf_data_previous$Date.Pending), "%Y")
    
    # Combine datasets into one for plotting
    combined_data <- rbind(
      transform(adf_data, Dataset = "adf_data"),
      transform(adf_data_previous, Dataset = "adf_data_previous")
    )
    
    # Aggregate data to count sales per month and year
    aggregated_data <- combined_data %>%
      group_by(Month, Year) %>%
      summarize(saleCount = n(), .groups = 'drop')
    
    # Convert Month to factor with ordered levels based on calendar months
    aggregated_data$Month <- factor(aggregated_data$Month, levels = month.abb, ordered = TRUE)
    
    # Create bar plot using ggplot
    gg <- ggplot(aggregated_data,
                 aes(x = Month,
                     y = saleCount,
                     fill = Year),
                    show.legend = FALSE) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 0.7,
               alpha = 0.5,
               colour = "black",
               linewidth=0.3) +
      geom_smooth(aes(group = Year,
                      color = Year,),
                  method = "lm",
                  formula = y ~ poly(x, 3),
                  se = FALSE,
                  ) +
      guides(fill = FALSE) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = scales::pretty_breaks(n = 10)
      ) +
      labs(
        title = "Monthly Sale Count Comparison by Year",
        x = "Month",
        y = "Sale Count",
        fill = "Year"
      ) +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")
      )
    
    # Convert ggplot to plotly object and adjust layout
    ggplotly(gg) %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
  })
  
  
  
  # ADF Page Title Information ----
  output$adfTitle <- renderText({
    req(input$adfSubAreaInput, input$adfPropertySubType)
    adfTitle()
  })
  
  output$adfTitlePropertyType <- renderText({
    req(input$adfSubAreaInput, input$adfPropertySubType)
    adfTitlePropertyType()
  })
  
  output$cmsTitle <- renderText({
    req(input$adfSubAreaInput, input$adfPropertySubType)
    cmsTitle()
  })
  
  output$adfDateRange <- renderText({
    req(statsADF())
    paste0("Summary of transactions from ",adfMinDate()," to ", adfMaxDate(),":")
  })
  
  output$cmsDateRange <- renderText({
    req(statsCMS())
    paste0("Summary of transactions from ",cmsMinDate()," to ", cmsMaxDate(),":")
  })
  
  # ADF STATISTICS CALCULATION ----
  statsADF <- reactive({
    req(input$adfSubAreaInput, input$adfPropertySubType,input$adfStartMonth, input$adfStartYear)
    #adf_data <- adfData()[c("Date.Pending", "Price.Sold")]
    adf_data <- adfData()
    #adfData$Date.Pending <- as.Date(adf_data$Date.Pending)
    # Remove non-numeric characters and convert to numeric
    #adf_data$Price.Sold <- as.numeric(gsub("[^0-9.]", "", adf_data$Price.Sold))
    
    #Calculate market statistics for ADF
    minDateADF <- format(min(adf_data$Date.Pending), format="%B %d, %Y")
    maxDateADF <- format(max(adf_data$Date.Pending), format="%B %d, %Y")
    adfMinDate(minDateADF)
    adfMaxDate(maxDateADF)
    
    adfSubArea <- input$adfSubAreaInput
    adfPropertyType <- input$adfPropertySubType
    
    MinimumADF <- dollar(min(adf_data$Price.Sold, na.rm = TRUE), accuracy = 1)
    MeanADF <- dollar(mean(adf_data$Price.Sold, na.rm = TRUE), accuracy = 1)
    MedianADF <- dollar(median(adf_data$Price.Sold, na.rm = TRUE), accuracy = 1)
    MaximumADF <- dollar(max(adf_data$Price.Sold, na.rm = TRUE), accuracy = 1)
    MktConditionsADF <- lm(adf_data$Price.Sold ~ adf_data$Date.Pending, adf_data = data)
    changeADF <- coef(MktConditionsADF)
    changePerDay1ADF <- changeADF[2]
    #changePerDayADF <- dollar(changeADF[2], accuracy = 1)
    changePerDayADF <- ifelse(abs(changeADF[2]) >= 1, sprintf("$%d", round(changeADF[2])), sprintf("$%.2f", changeADF[2]))
    #changePerDayADF <- ifelse(changeADF[2] < 0, paste0("-", changePerDayADF), changePerDayADF)
    
    data_count = count(adf_data)
    #Add row to ADF Summary
    statsADF <- data.frame("rowname" = "Sale Price",
                           "Count" = data_count,
                           "Minimum" = MinimumADF,
                           "Mean" = MeanADF,
                           "Median" = MedianADF,
                           "Maximum" = MaximumADF,
                           "DailyChangeADF" = changePerDayADF
    )
    row.names(statsADF) <- c(NULL)
    colnames(statsADF)[colnames(statsADF) == "rowname"] <- "Benchmark"
    colnames(statsADF)[colnames(statsADF) == "n"] <- "Sales"
    return(statsADF)
  })

  
  
  #ADF DataTable Calculations
  output$adf_data_table <- renderDT({
    data <- adfData()[, c("MLS..No", "Address", "City","Date.Pending","Price.Sold","Lot.Size.SqFt","Fin.SqFt.Total","Year.Built..Est.","condition")]
    
    data <- data %>%
      mutate(data, condition = case_when(
        condition == 1 ~ "Poor",
        condition == 2 ~ "Fair",
        condition == 3 ~ "Average",
        condition == 4 ~ "Good",
        condition == 5 ~ "Excellent",
        TRUE ~ as.character(condition)  # Keeps original value if condition is not 1-5
      ))
    
    colnames(data) <- c("MLS No.", "Address", "City", "Sale Date", "Price Sold", "Lot Size SqFt", "GLA SqFt", "Year Built","Condition")
    datatable(data, options = list(pageLength = 10, rownames = FALSE, paging = FALSE))
  })
  
  ## ADF Statistics Table Calculations ----
  output$adfDataStatsTable <- render_gt({
    statsADF <- statsADF()
    # Convert character columns to numeric
    cols_to_convert <- c("Sales", "Minimum", "Mean", "Median", "Maximum", "DailyChangeADF")
    
    # Modify the regular expression to retain negative signs
    statsADF[cols_to_convert] <- lapply(statsADF[cols_to_convert], function(x) {
      # Use a regular expression to extract numbers and decimals
      numeric_value <- as.numeric(gsub("[^0-9.-]", "", x))
      # If the original value starts with a negative sign, make the numeric value negative
      if (grepl("^-", x)) {
        numeric_value <- -numeric_value
      }
      return(numeric_value)
    })
    
    # Modify the label of the "DailyChangeADF" column to "Daily Change"
    table <- statsADF %>%
      gt() %>%
      cols_label(DailyChangeADF = "Daily Change") %>% 
      fmt_currency(
        columns = c("Minimum", "Mean", "Median", "Maximum", "DailyChangeADF"),
        decimals = 0,
      ) %>%
      data_color(
        columns = DailyChangeADF,
        method = "bin",
        bins=c(-100000,0,100000),
        palette = c("red","#008000"),
        apply_to = c("text")
      ) %>% 
      tab_style(
        style = list(
          cell_text(weight = "bold")),
        locations = cells_body(
        )
      ) %>% 
      tab_options(
        table.width = pct(100)
      )
  })
  
  #Attribute stats table
  attributeStatsADF <- reactive({
    req(input$adfSubAreaInput, input$adfPropertySubType, input$adfStartMonth, input$adfStartYear)
    adf_data <- adfData()
    
    # Lot size
    minimumLotSize <- round(min(adf_data$Lot.Size.SqFt, na.rm = TRUE))
    meanLotSize <- round(mean(adf_data$Lot.Size.SqFt, na.rm = TRUE))
    medianLotSize <- round(median(adf_data$Lot.Size.SqFt, na.rm = TRUE))
    maximumLotSize <- round(max(adf_data$Lot.Size.SqFt, na.rm = TRUE))
    
    # Format numbers with commas for thousands and rounded to the nearest integer
    minimumLotSize <- format(minimumLotSize, big.mark = ",", scientific = FALSE)
    meanLotSize <- format(meanLotSize, big.mark = ",", scientific = FALSE)
    medianLotSize <- format(medianLotSize, big.mark = ",", scientific = FALSE)
    maximumLotSize <- format(maximumLotSize, big.mark = ",", scientific = FALSE)
    
    # GLA size
    minimumGLA <- round(min(adf_data$Fin.SqFt.Total, na.rm = TRUE))
    meanGLA <- round(mean(adf_data$Fin.SqFt.Total, na.rm = TRUE))
    medianGLA <- round(median(adf_data$Fin.SqFt.Total, na.rm = TRUE))
    maximumGLA <- round(max(adf_data$Fin.SqFt.Total, na.rm = TRUE))
    
    # Format numbers with commas for thousands and rounded to the nearest integer
    minimumGLA <- format(minimumGLA, big.mark = ",", scientific = FALSE)
    meanGLA <- format(meanGLA, big.mark = ",", scientific = FALSE)
    medianGLA <- format(medianGLA, big.mark = ",", scientific = FALSE)
    maximumGLA <- format(maximumGLA, big.mark = ",", scientific = FALSE)
    
    # Year Built
    minimumYB <- round(min(adf_data$Year.Built..Est., na.rm = TRUE))
    meanYB <- round(mean(adf_data$Year.Built..Est., na.rm = TRUE))
    medianYB <- round(median(adf_data$Year.Built..Est., na.rm = TRUE))
    maximumYB <- round(max(adf_data$Year.Built..Est., na.rm = TRUE))
    
    # Year Built
    minimumCond <- round(min(adf_data$condition, na.rm = TRUE))
    meanCond <- round(mean(adf_data$condition, na.rm = TRUE))
    medianCond <- round(median(adf_data$condition, na.rm = TRUE))
    maximumCond <- round(max(adf_data$condition, na.rm = TRUE))

    # Create data frames for lot size and GLA
    lot_size_row <- data.frame("Attribute" = "Lot Size SqFt",
                               "Minimum" = minimumLotSize,
                               "Mean" = meanLotSize,
                               "Median" = medianLotSize,
                               "Maximum" = maximumLotSize)
    
    GLA_row <- data.frame("Attribute" = "GLA SqFt",
                          "Minimum" = minimumGLA,
                          "Mean" = meanGLA,
                          "Median" = medianGLA,
                          "Maximum" = maximumGLA)
    
    year_built_row <- data.frame("Attribute" = "Year Built",
                          "Minimum" = minimumYB,
                          "Mean" = meanYB,
                          "Median" = medianYB,
                          "Maximum" = maximumYB)
    
    # Bind the rows together
    attributeStatsADF <- rbind(year_built_row, GLA_row, lot_size_row)
    
    row.names(attributeStatsADF) <- NULL
    return(attributeStatsADF)
  })
  

  
  output$adfAttributeStatsTable <- render_gt({
    attributeStatsADF <- attributeStatsADF()
    
    # Modify the label of the "DailyChangeADF" column to "Daily Change"
    table <- attributeStatsADF %>%
      gt() %>%
      
      tab_options(
        table.width = pct(50),
        column_labels.font.weight = "bold",
        table.align = "left"
      )
  })

  ## ADF Coefficient Calculations ----
  observe({
    master_df <- masterData()
    numeric_column_names <- names(master_df)[sapply(master_df, function(x) any(grepl("^-?\\d*\\.?\\d+$", x)))]
    numeric_column_names <- sort(numeric_column_names)
    updateSelectInput(session, "variable_1", choices = numeric_column_names)
  })
  
  observe({
    master_df <- masterData()
    numeric_column_names <- names(master_df)[sapply(master_df, function(x) any(grepl("^-?\\d*\\.?\\d+$", x)))]
    numeric_column_names <- sort(numeric_column_names)
    updateSelectInput(session, "variable_2", choices = numeric_column_names)
  })
  
  observeEvent(c(input$variable_1, input$variable_2), {
    adf_data <- adfData()
    
    # Extract variable names from input
    var1_name <- input$variable_1
    var2_name <- input$variable_2
    
    # Extract variables from master_df
    var1 <- as.numeric(adf_data[[var1_name]])
    var2 <- as.numeric(adf_data[[var2_name]])
    
    # Remove NA values
    na_indices <- !is.na(var1) & !is.na(var2)
    var1 <- var1[na_indices]
    var2 <- var2[na_indices]
    
    # Check if variables are numeric
    if (all(!is.na(var1)) && all(!is.na(var2))) {
      # Calculate correlation coefficient
      co <- cor(var1, var2)
      
      #updateTextInput(session, "coefText", value = as.character(co))
      
      output$coefText <- renderText({
        as.character(co)
      })
      
      #print(co)
    } else {
      print("One or both variables contain non-numeric values.")
    }
  })
  
  #Coefficient table
  observeEvent(adf_data_calc(), {
    adf_data <- adfData()
    
    adf_data <- select(adf_data,-SPAP,-SPOAP,-year,-month,-lat,-lon,-MLS..No,-Baths.5pce,-Baths.4pce,-Baths.3pce,-Baths.2pce,)
    if (nrow(adf_data) > 0) {
      # Plot correlation matrix using ggcorr() function
      output$correlation_matrix_plot <- renderPlot({
        # Plot correlation matrix
        ggcorr(adf_data)
      })
      
      #browser()
      
      # Filter numeric columns
      numeric_column_names <- names(adf_data)[sapply(adf_data, function(x) any(grepl("^-?\\d*\\.?\\d+$", x)))]
      
      # Initialize an empty data frame to store the results
      correlation_table <- data.frame(variable_name = character(), correlation_coefficient = numeric(), stringsAsFactors = FALSE)
      
      # Loop through each numeric column
      for (col_name in numeric_column_names) {
        # Calculate correlation coefficient
        corr <- cor(adf_data$Price.Sold, adf_data[[col_name]])
        
        # Add the results to the data frame
        correlation_table <- rbind(correlation_table, data.frame(variable_name = col_name, correlation_coefficient = corr))
      }
      
      # Sort the results by correlation coefficient in descending order
      correlation_table <- correlation_table[order(-correlation_table$correlation_coefficient), ]
      
      
      # Print the results
      #print(correlation_table)

      
      output$adf_coefficient_table <- renderDT({
        data <- correlation_table
        #print(data)

        # Render the datatable
        datatable(data, 
                  rownames = FALSE,
                  options = list(paging = FALSE))
      })

    }
  })

  
  # ADF Download Buttons ----
  
  output$downloadAdfStatsReport <- downloadHandler(
    filename = function() {
      paste('ADF-Stats', sep = '.', switch(
        input$adf_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('reports/ADF-Stats.Rmd')
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'ADF-Stats.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('ADF-Stats.Rmd', switch(
        input$adf_format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document(),
      ),
      params = list(adf_data = adfData(),
                    adf_listing_data = adf_listing_data(),
                    adf_stats = statsADF(),
                    adfMinDate = adfMinDate(),
                    adfMaxDate = adfMaxDate(),
                    adfTitle = adfTitle(),
                    adfTitlePropertyType = adfTitlePropertyType()
      )
      )
      file.copy(out, file)
    }
  )
  
  output$downloadAdfCSV <- downloadHandler(
    filename = "ADF-Data.csv",
    content = function(file) {
      write.csv(adfData(), file)
    }
  )

  # CMS STATISTICS CALCULATION ----
  statsCMS <- reactive({
    #browser()
    req(input$adfSubAreaInput, input$adfPropertySubType,input$adfStartMonth, input$adfStartYear)
    cmsTitle(paste0(sub("^PA ", "", input$adfSubAreaInput)," - ", input$adfPropertySubType))
    
    cms_data <- cmsData()

    #Calculate market statistics for ADF
    minDateCMS <- format(min(cms_data$Date.Pending), format="%B %d, %Y")
    maxDateCMS <- format(max(cms_data$Date.Pending), format="%B %d, %Y")
    cmsMinDate(minDateCMS)
    cmsMaxDate(maxDateCMS)
    
    cmsSubArea <- input$adfSubAreaInput
    cmsPropertyType <- input$adfPropertySubType
    
    MinimumCMS <- dollar(min(cms_data$Price.Sold, na.rm = TRUE), accuracy = 1)
    MeanCMS <- dollar(mean(cms_data$Price.Sold, na.rm = TRUE), accuracy = 1)
    MedianCMS <- dollar(median(cms_data$Price.Sold, na.rm = TRUE), accuracy = 1)
    MaximumCMS <- dollar(max(cms_data$Price.Sold, na.rm = TRUE), accuracy = 1)
    
    if(nrow(cms_data) > 0) {
      MktConditionsCMS <- lm(cms_data$Price.Sold ~ cms_data$Date.Pending, data = cms_data)
      changeCMS <- coef(MktConditionsCMS)
      changePerDayCMS(ifelse(abs(changeCMS[2]) >= 1, sprintf("$%d", round(changeCMS[2])), sprintf("$%.2f", changeCMS[2])))
      data_count = count(cms_data)
      #Add row to ADF Summary
      statsCMS <- data.frame("rowname" = "Sale Price",
                             "Count" = data_count,
                             "Minimum" = MinimumCMS,
                             "Mean" = MeanCMS,
                             "Median" = MedianCMS,
                             "Maximum" = MaximumCMS,
                             "DailyChangeADF" = changePerDayCMS())
    }
    else {
      statsCMS <- data.frame("rowname" = "Sale Price",
                             "Count" = 0,
                             "Minimum" = 0,
                             "Mean" = "",
                             "Median" = "",
                             "Maximum" = 0,
                             "DailyChangeADF" = 0)
    }

    row.names(statsCMS) <- c(NULL)
    colnames(statsCMS)[colnames(statsCMS) == "rowname"] <- "Benchmark"
    colnames(statsCMS)[colnames(statsCMS) == "n"] <- "Sales"
    print(statsCMS)
    return(statsCMS)
  })
  
  ## CMS Statistics Table Calculations ----
  output$cmsDataStatsTable <- render_gt({
    statsCMS <- statsCMS()
    # Convert character columns to numeric
    cols_to_convert <- c("Sales", "Minimum", "Mean", "Median", "Maximum", "DailyChangeADF")
    
    if(nrow(cmsData()) > 0) {
      # Modify the regular expression to retain negative signs
      statsCMS[cols_to_convert] <- lapply(statsCMS[cols_to_convert], function(x) {
        # Use a regular expression to extract numbers and decimals
        numeric_value <- as.numeric(gsub("[^0-9.-]", "", x))
        # If the original value starts with a negative sign, make the numeric value negative
        if (grepl("^-", x)) {
          numeric_value <- -numeric_value
        }
        return(numeric_value)
      })
    }
    
    # Modify the label of the "DailyChangeADF" column to "Daily Change"
    table <- statsCMS %>%
      gt() %>%
      cols_label(DailyChangeADF = "Daily Change") %>% 
      fmt_currency(
        columns = c("Minimum", "Mean", "Median", "Maximum", "DailyChangeADF"),
        decimals = 0,
      ) %>%
      data_color(
        columns = DailyChangeADF,
        method = "bin",
        bins=c(-100000,0,100000),
        palette = c("red","#008000"),
        apply_to = c("text")
      ) %>% 
      tab_style(
        style = list(
          cell_text(weight = "bold")),
        locations = cells_body(
        )
      ) %>% 
      tab_options(
        table.width = pct(100)
      )
  })
  
  
  
  ## CMS Datatable calculations ----

  cmsDataTableSummary <- reactive({
    #browser()
    req(input$adfSubAreaInput, input$adfPropertySubType,input$adfStartMonth, input$adfStartYear)

    cms_data <- cmsData()
    
    #print(round(median(cms_data$condition)))
    
    if(round(median(cms_data$condition)) == 1){
      cms_condition <- "Poor"
    }
    else if(round(median(cms_data$condition)) == 2) {
      cms_condition <- "Fair"
    }
    else if(round(median(cms_data$condition)) == 3) {
      cms_condition <- "Average"
    }
    else if(round(median(cms_data$condition)) == 4) {
      cms_condition <- "Good"
    }
    else if(round(median(cms_data$condition)) == 5) {
      cms_condition <- "Excellent"
    }
    
    MktConditionsCMS <- lm(cms_data$Price.Sold ~ cms_data$Date.Pending, cms_data = data)
    changeCMS <- coef(MktConditionsCMS)
    changePerDayCMS <- ifelse(abs(changeCMS[2]) >= 1, round(changeCMS[2]))
    cms_data$time_adjusted_sale_price <- cms_data$Price.Sold + (cms_data$days_since_sale * as.numeric(changePerDayCMS))
    #print(str(cms_data))

    # print(input$SubjectLotSF)
    # print(input$SubjectGLA)
    # print(input$SubYB)
    # print(input$SubjectCondition)
    # print(input$SubjectBeds)
    # print(input$SubjectBaths)
    # print(input$SubjectBasement)
    # print(input$SubjectGarageSpaces)
    # print(input$SubjectCarportSpaces)
    # print(input$SubjectSalePrice)
    
    
    
    #print(str(subject_data))

    summary_data <- data.frame(
      `Lot.Size.SqFt` = round(mean(cms_data$`Lot.Size.SqFt`, na.rm = TRUE)),
      `Fin.SqFt.Total` = mean(cms_data$`Fin.SqFt.Total`, na.rm = TRUE),
      `Year.Built..Est.` = round(mean(cms_data$`Year.Built..Est.`, na.rm = TRUE)),
      `Condition` = cms_condition,
      Beds = round(mean(cms_data$Beds, na.rm = TRUE)),
      Baths = round(mean(cms_data$Baths, na.rm = TRUE)),
      Basement = "",
      GarageSpaces = mean(cms_data$`Garage.Spaces`, na.rm = TRUE),
      CarportSpaces = mean(cms_data$`Carport.Spaces`, na.rm = TRUE),
      `Price.Sold` = mean(cms_data$`Price.Sold`, na.rm = TRUE),
      `time_adjusted_sale_price` = mean(cms_data$`time_adjusted_sale_price`, na.rm = TRUE)
    ) %>%
      mutate(benchmark = "CMS Summary") %>%
      mutate(observations = nrow(cms_data)) %>%
      select(benchmark, observations, everything())
    
    subject_data <- data.frame(
      benchmark = "Subject",
      observations = NA,
      `Lot.Size.SqFt` = input$SubjectLotSF,
      `Fin.SqFt.Total` = input$SubjectGLA,
      `Year.Built..Est.` = input$SubYB,
      `Condition` = input$SubjectCondition,
      Beds = input$SubjectBeds,
      Baths = input$SubjectBaths,
      Basement = input$SubjectBasement,
      GarageSpaces = input$SubjectGarageSpaces,
      CarportSpaces = input$SubjectCarportSpaces,
      `Price.Sold` = ifelse(input$SubjectSalePrice == 0, NA, input$SubjectSalePrice),
      `time_adjusted_sale_price` = NA
    )
    #print(str(summary_data))
    
    data <- rbind(summary_data, subject_data)
    
    #print(str(data))
    
    return(data)
  })
  
  output$cmsDataSummaryTable <- render_gt({
    cmsDataTableSummary <- cmsDataTableSummary()
    
    cmsDataTableSummary %>%
      gt() %>%
      cols_label(
        benchmark = "CMS Data",
        observations = "Count",
        `Lot.Size.SqFt` = "Lot Size SF",
        `Fin.SqFt.Total` = "Finished SF Total",
        `Year.Built..Est.` = "Year Built",
        Beds = "Beds",
        Baths = "Baths",
        `Price.Sold` = "Price Sold",
        `time_adjusted_sale_price` = "Time Adjusted Price"
      ) %>%
      fmt_number(
        columns = vars(`Lot.Size.SqFt`, `Fin.SqFt.Total`, GarageSpaces, CarportSpaces),
        decimals = 0,
        use_seps = TRUE
      ) %>%
      fmt_currency(
        columns = vars(`Price.Sold`, `time_adjusted_sale_price`),
        currency = "USD",
        decimals = 0
      ) %>%
      fmt_missing(
        columns = everything(),
        missing_text = ""  # Replace NAs with an empty string
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(columns = everything())
      ) %>%
      tab_options(
        table.width = pct(100)
      )
  })
  
  output$cms_data_table <- renderDT({
    cms_data <- cmsData()
    MktConditionsCMS <- lm(cms_data$Price.Sold ~ cms_data$Date.Pending, cms_data = data)
    changeCMS <- coef(MktConditionsCMS)
    changePerDayCMS <- ifelse(abs(changeCMS[2]) >= 1, round(changeCMS[2]))
    cms_data$time_adjusted_sale_price <- cms_data$Price.Sold + (cms_data$days_since_sale * as.numeric(changePerDayCMS))
    data <- cms_data[, c("MLS..No", "Address", "City","Lot.Size.SqFt","Fin.SqFt.Total","Year.Built..Est.","Beds","Baths","condition","Date.Pending","Price.Sold","time_adjusted_sale_price")]
    
    data <- data %>%
      mutate(data, condition = case_when(
        condition == 1 ~ "Poor",
        condition == 2 ~ "Fair",
        condition == 3 ~ "Average",
        condition == 4 ~ "Good",
        condition == 5 ~ "Excellent",
        TRUE ~ as.character(condition)  # Keeps original value if condition is not 1-5
      ))
    
    colnames(data) <- c("MLS No.", "Address", "City", "Lot Size SqFt", "GLA SqFt", "Year Built","Beds","Baths", "Condition","Sale Date", "Price Sold","Time Adjusted")
    datatable(data, options = list(pageLength = 10, rownames = FALSE, paging = FALSE))
  })
  
  ## CMS Download Buttons ----
  output$downloadCMSReport <- downloadHandler(
    filename = function() {
      paste('CMS-Report', sep = '.', switch(
        input$cms_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('reports/CMS-Report.Rmd')
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'CMS-Report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('CMS-Report.Rmd', switch(
        input$adf_format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document(),
      ),
      params = list(adf_data = adfData(),
                    cms_data = cmsData(),
                    cms_stats = statsCMS(),
                    cmsMinDate = cmsMinDate(),
                    cmsMaxDate = cmsMaxDate(),
                    adfTitle = adfTitle(),
                    adfTitlePropertyType = adfTitlePropertyType(),
                    SubjectAddress = input$SubjectAddress,
                    SubYB = input$SubYB,
                    SubjectLotSF = input$SubjectLotSF,
                    SubjectGLA = input$SubjectGLA,
                    SubjectBeds = input$SubjectBeds,
                    SubjectBaths = input$SubjectBaths,
                    SubjectBasement = input$SubjectBasement,
                    SubjectSalePrice = input$SubjectSalePrice,
                    SubjectCondition = input$SubjectCondition,
                    SubjectGarageSpaces = input$SubjectGarageSpaces,
                    SubjectCarportSpaces = input$SubjectCarportSpaces,
                    
                    yearBuiltRangeLow = input$yearBuiltRange[1],
                    yearBuiltRangeHigh = input$yearBuiltRange[2],
                    lotSizeRangeLow = input$lotSizeRange[1],
                    lotSizeRangeHigh = input$lotSizeRange[2],
                    glaRangeLow = input$glaRange[1],
                    glaRangeHigh = input$glaRange[2],
                    bedsRangeLow = input$bedsRange[1],
                    bedsRangeHigh = input$bedsRange[2],
                    bathsRangeLow = input$bathsRange[1],
                    bathsRangeHigh = input$bathsRange[2],
                    conditionRangeLow = input$conditionRange[1],
                    conditionRangeHigh = input$conditionRange[2],
                    garageSpacesRangelow = input$garageRange[1],
                    garageSpacesRangeHigh = input$garageRange[2],
                    carportSpacesRangeLow = input$carportRange[1],
                    carportSpacesRangeHigh = input$carportRange[2]
      )
      )
      file.copy(out, file)
    }
  )

  # CMS FUNCTIONS ----
  # CMS CALCULATION BASED ON INPUTS ----
  cms_data_calc <- reactive({
    #browser()

    req(adfData())

    adfdata <- adfData() # Initialize 'data' with the entire data frame
    
    cms_basement <- input$cms_basement
    #browser()
    # Define a filter condition based on 'cms_basement'
    filter_basement <- 
      if (cms_basement == "Yes") {
        adfdata$`Basement.` == "Yes"
      } else if (cms_basement == "No") {
        adfdata$`Basement.` == "No"
      } else {
        TRUE # No filter for 'All'
      }

    # Apply the filter condition
    cmsdata <- subset(adfdata, `Lot.Size.SqFt` >= cms_lot_size_values()[1] &
                     `Lot.Size.SqFt` <= cms_lot_size_values()[2] &
                     `Year.Built..Est.` >= cms_year_built_values()[1] &
                     `Year.Built..Est.` <= cms_year_built_values()[2] &
                     `Fin.SqFt.Total` >= cms_gla_values()[1] &
                     `Fin.SqFt.Total` <= cms_gla_values()[2] &
                     `Beds` >= cms_beds_values()[1] &
                     `Beds` <= cms_beds_values()[2] &
                     `Baths` >= cms_baths_values()[1] &
                     `Baths` <= cms_baths_values()[2] &
                     `condition` >= cms_condition_values()[1] &
                     `condition` <= cms_condition_values()[2] &
                     `Garage.Spaces` >= cms_garage_values()[1] &
                     `Garage.Spaces` <= cms_garage_values()[2] &
                     `Carport.Spaces` >= cms_carport_values()[1] &
                     `Carport.Spaces` <= cms_carport_values()[2] &
                     filter_basement)
    
    
    if (!is.null(input$cms_view)) {
      # Apply subset only if input$cms_view is not Null
      cmsdata <- subset(cmsdata, `View` %in% input$cms_view)
    }

    if (!is.null(input$cms_extras)) {
      # Apply subset only if input$cms_view is not NA and has elements
      cmsdata <- subset(cmsdata, `Other.Structures` %in% input$cms_extras)
    }
  

    cmsdata$days_since_sale <- as.integer(difftime(input$effectiveDate, cmsdata$Date.Pending, units = "days"))
    
    return(cmsdata)
  })
  
  observe({
    # req ensures that cms_data_calc is not NULL
    req(adfData())
    # Update cmsData with the value from cms_data_calc
    cmsData(cms_data_calc())
  })
  
  ## CMS Graph Reported Sale Price ----
  
  output$cmsSalePriceDistribution <- renderPlotly({
    adf_data <- adfData()
    cms_data <- cmsData()
    MeanADF <- mean(adf_data$Price.Sold, na.rm = TRUE)
    MeanCMS <- mean(cms_data$Price.Sold, na.rm = TRUE)
    # Calculate the histogram
    hist_data <- ggplot_build(
      ggplot(adf_data, aes(x = `Price.Sold`)) +
        geom_histogram(binwidth = 20000, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)
    
    mean_label <- "                  \u2190CMS Mean"
    
    adf_price_min <- min(adf_data$Price.Sold, na.rm = TRUE)
    adf_price_max <- max(adf_data$Price.Sold, na.rm = TRUE)
    
    graph_middle <- adf_price_min + ((adf_price_max - adf_price_min) / 2)
    
    if (MeanCMS <= graph_middle ) {
      label_mean <- "\u2190CMS Mean"
      label_position <- MeanCMS + ((adf_price_max - adf_price_min) * 0.2)
    }
    else {
      label_mean <- "CMS Mean\u2192"
      label_position <- MeanCMS - ((adf_price_max - adf_price_min) * 0.2)
    }
    
    gg <- ggplot() +
      geom_histogram(data = adf_data,
                     aes(`Price.Sold`),
                     binwidth = 20000,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5) +
      geom_histogram(data = cms_data,
                     aes(`Price.Sold`),
                     binwidth = 20000,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      geom_vline(xintercept = MeanCMS, color = "blue", linetype = "solid", linewidth = 0.5) +
      geom_text(aes(x=label_position,
                    label=label_mean,
                    y=max_count),
                colour="blue",
                size = 3
      ) +
      labs(title = "CMS vs ADF",
           x = "Reported Sale Price",
           y = "Count") +
      scale_x_continuous(
        labels = scales::dollar_format(prefix = "$", big.mark = ",", scale = 1e-3, suffix = "K"),
        expand = c(0, 0),
        breaks = scales::pretty_breaks(n = 10)
      ) +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sales by Year Built Distribution ----

  observeEvent(input$yearBuiltRange, {
    cms_year_built_values(input$yearBuiltRange)
  })

  output$cmsSalesByYearBuilt <- renderPlotly({
    #browser()
    adf_data <- adfData()
    cms_data <- cmsData()
    MeanYB <- mean(adf_data$`Year.Built..Est.`, na.rm = TRUE)
    # Calculate the histogram
    hist_data <- ggplot_build(
      ggplot(adf_data, aes(x = `Year.Built..Est.`)) +
        geom_histogram(binwidth = 5, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
    
    # Find the bin with the highest count
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)

    graph_middle <- cms_year_built_min() + ((cms_year_built_max() - cms_year_built_min()) / 2)

    if (input$SubYB <= graph_middle ) {
      label_subject <- paste0("\u2190Subject", " (", input$SubYB, ")")
      label_position <- input$SubYB + ((cms_year_built_max() - cms_year_built_min()) * 0.3)
    }
    else {
      label_subject <- paste0("Subject", " (", input$SubYB, ")", "\u2192")
      label_position <- input$SubYB - ((cms_year_built_max() - cms_year_built_min()) * 0.3)
    }

    gg <- ggplot() +
      geom_histogram(data = adfData(),
                     aes(`Year.Built..Est.`),
                     binwidth = 5,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5,) +
      geom_histogram(data = cms_data,
                     aes(`Year.Built..Est.`),
                     binwidth = 5,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      scale_x_continuous(limits = c(cms_year_built_min(), cms_year_built_max())) +
      geom_vline(xintercept = input$SubYB, color = "red", linetype = "solid", linewidth = 0.5) +
      labs(
        title = "Sales by Year Built",
        x = "Year Built",
        y = "Count") +
      geom_text(aes(x=label_position,
                    label=label_subject,
                    y=max_count),
                colour="red",
                size = 3) +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sales by GLA Distribution ----
  observeEvent(input$glaRange, {
    cms_gla_values(input$glaRange)
  })
  
  output$cmsSalesByGLA <- renderPlotly({
    adf_data <- adfData()
    cms_data <- cmsData()
    #print(str(cms_data$`Fin.SqFt.Total`))
    MeanGLA <- mean(adf_data$`Fin.SqFt.Total`, na.rm = TRUE)
    # Calculate the histogram
    hist_data <- ggplot_build(
     ggplot(adf_data, aes(x = `Fin.SqFt.Total`)) +
       geom_histogram(binwidth = 100, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
     
    # Find the bin with the highest count
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)
     
    graph_middle <- cms_gla_min() + ((cms_gla_max() - cms_gla_min()) / 2)

    
    if (input$SubjectGLA <= graph_middle ) {
     #label_subject <- paste0("\u2190Subject", " (", input$SubjectGLA, ")")
     label_subject <- paste0(
       "\u2190Subject", 
       " (", 
       ifelse(as.numeric(input$SubjectGLA) > 999, format(as.numeric(input$SubjectGLA), big.mark = ","), input$SubjectGLA), 
       ")"
     )
     label_position <- input$SubjectGLA + ((cms_gla_max() - cms_gla_min()) * 0.3)
    }
    else {
     label_subject <- paste0(
       "Subject", 
       " (", 
       ifelse(as.numeric(input$SubjectGLA) > 999, format(as.numeric(input$SubjectGLA), big.mark = ","), input$SubjectGLA), 
       ")", "\u2192"
     )
     label_position <- input$SubjectGLA - ((cms_gla_max() - cms_gla_min()) * 0.3)
    }
    
    gg <- ggplot() +
      geom_histogram(data = adf_data,
                     aes(`Fin.SqFt.Total`),
                     binwidth = 100,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5,) +
      geom_histogram(data = cms_data,
                     aes(`Fin.SqFt.Total`),
                     binwidth = 100,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      scale_x_continuous(limits = c(cms_gla_min(), cms_gla_max())) +
      geom_vline(xintercept = input$SubjectGLA, color = "red", linetype = "solid", linewidth = 0.5) +
      labs(
        title = "Sales by Gross Living Area (GLA)",
        x = "GLA SqFt",
        y = "Count") +
      geom_text(aes(x=label_position,
                    label=label_subject,
                    y=max_count),
                colour="red",
                size = 3) +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sales by Lot Size Distribution ----

  observeEvent(input$lotSizeRange, {
    cms_lot_size_values(input$lotSizeRange)
  })

  output$cmsSalesByLotSize <- renderPlotly({
    adf_data <- adfData()
    cms_data <- cmsData()
    # Calculate the histogram
    hist_data <- ggplot_build(
      ggplot(adf_data, aes(x = `Lot.Size.SqFt`)) +
        geom_histogram(binwidth = 2000, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)
    
    graph_middle <- cms_lot_size_min() + ((cms_lot_size_max() - cms_lot_size_min()) / 2)
    
    if (input$SubjectLotSF <= graph_middle ) {
      label_subject <- paste0(
        "\u2190Subject", 
        " (", 
        ifelse(as.numeric(input$SubjectLotSF) > 999, format(as.numeric(input$SubjectLotSF), big.mark = ","), input$SubjectLotSF), 
        ")"
      )
      label_position <- input$SubjectLotSF + ((cms_lot_size_max() - cms_lot_size_min()) * 0.3)
    }
    else {
      label_subject <- paste0(
        "Subject", 
        " (", 
        ifelse(as.numeric(input$SubjectLotSF) > 999, format(as.numeric(input$SubjectLotSF), big.mark = ","), input$SubjectLotSF), 
        ")", "\u2192"
      )
      label_position <- input$SubjectLotSF - ((cms_lot_size_max() - cms_lot_size_min()) * 0.3)
    }

    gg <- ggplot() +
      geom_histogram(data = adf_data,
                     aes(`Lot.Size.SqFt`),
                     binwidth = 2000,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5,) +
      geom_histogram(data = cms_data,
                     aes(`Lot.Size.SqFt`),
                     binwidth = 2000,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      geom_vline(xintercept = input$SubjectLotSF, color = "red", linetype = "solid", linewidth = 0.5) +
      geom_text(aes(x=label_position,
                    label=label_subject,
                    y=max_count),
                colour="red",
                size = 3
      ) +
      scale_x_continuous(limits = c(cms_lot_size_min(), cms_lot_size_max()), labels = comma_format()) +
      labs(
        title = "Sales by Lot Size",
        x = "Lot Size SqFt.",
        y = "Count") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sales by Bedrooms Distribution ----

  # Define an observer to update the reactive value when the slider changes
  observeEvent(input$bedsRange, {
    cms_beds_values(input$bedsRange)
  })

  output$cmsSalesByBeds <- renderPlotly({
    adf_data <- adfData()
    cms_data <- cmsData()
    # Calculate the histogram
    hist_data <- ggplot_build(
      ggplot(adf_data, aes(x = Beds)) +
        geom_histogram(binwidth = 1, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)
    
    cms_beds_min(min(adf_data$`Beds`))
    cms_beds_max(max(adf_data$`Beds`))
    
    graph_middle <- cms_beds_min() + ((cms_beds_max() - cms_beds_min()) / 2)
    
    if (input$SubjectBeds <= graph_middle ) {
      label_subject <- "\u2190Subject"
      label_position <- input$SubjectBeds + ((cms_beds_max() - cms_beds_min()) * 0.2)
    }
    else {
      label_subject <- "Subject\u2192"
      label_position <- input$SubjectBeds - ((cms_beds_max() - cms_beds_min()) * 0.2)
    }
    
    gg <- ggplot() +
      geom_histogram(data = adf_data,
                     aes(`Beds`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5,) +
      geom_histogram(data = cms_data,
                     aes(`Beds`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      geom_vline(xintercept = input$SubjectBeds, color = "red", linetype = "solid", linewidth = 0.5) +
      geom_text(aes(x=label_position,
                    label=label_subject,
                    y=max_count),
                colour="red",
                size = 3
      ) +
      scale_x_continuous(limits = c(cms_beds_min(),
                                    cms_beds_max()),
                         labels = function(x) round(x),
                         breaks = seq(cms_beds_min(), cms_beds_max(), by = 1)
                         ) +
      labs(
        title = "Sales by Bedrooms",
        x = "Bedrooms",
        y = "Count") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sales by Baths Distribution ----

  # Define an observer to update the reactive value when the slider changes
  observeEvent(input$bathsRange, {
    cms_baths_values(input$bathsRange)
    #print(cms_baths_values())
  })

  output$cmsSalesByBaths <- renderPlotly({
    adf_data <- adfData()
    cms_data <- cmsData()
    # Calculate the histogram
    hist_data <- ggplot_build(
      ggplot(adf_data, aes(x = Baths)) +
        geom_histogram(binwidth = 1, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)
    
    cms_baths_min(min(adf_data$`Baths`))
    cms_baths_max(max(adf_data$`Baths`))
    
    graph_middle <- cms_baths_min() + ((cms_baths_max() - cms_baths_min()) / 2)
    
    if (input$SubjectBaths <= graph_middle ) {
      label_subject <- "\u2190Subject"
      label_position <- input$SubjectBaths + ((cms_baths_max() - cms_baths_min()) * 0.2)
    }
    else {
      label_subject <- "Subject\u2192"
      label_position <- input$SubjectBaths - ((cms_baths_max() - cms_baths_min()) * 0.2)
    }
    
    gg <- ggplot() +
      geom_histogram(data = adf_data,
                     aes(`Baths`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5,) +
      geom_histogram(data = cms_data,
                     aes(`Baths`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      geom_vline(xintercept = input$SubjectBaths, color = "red", linetype = "solid", linewidth = 0.5) +
      geom_text(aes(x=label_position,
                    label=label_subject,
                    y=max_count),
                colour="red",
                size = 3
      ) +
      #scale_x_continuous(limits = c(cms_baths_min(), cms_baths_max())) +
      labs(
        title = "Sales by Bathrooms",
        x = "Bathrooms",
        y = "Count") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sales by Condition Distribution ----
  
  # Define an observer to update the reactive value when the slider changes
  observeEvent(input$conditionRange, {
    cms_condition_values(input$conditionRange)
  })
  
  output$cmsSalesByCondition <- renderPlotly({
    #browser()
    #print(input$subjectCondition)
    adf_data <- adfData()
    cms_data <- cmsData()
    
    #print(str(cms_data$condition))

    text_to_number <- setNames(1:5, c('Poor','Fair','Average','Good','Excellent'))
    subject_condition_number <- text_to_number[input$SubjectCondition]
    
    # Calculate the histogram
    hist_data <- ggplot_build(
       ggplot(adf_data, aes(x = condition)) +
         geom_histogram(binwidth = 1, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
     )
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)
    
    cms_condition_min(min(adf_data$`condition`))
    cms_condition_max(max(adf_data$`condition`))
     
    graph_middle <- cms_condition_min() + ((cms_condition_max() - cms_condition_min()) / 2)
    
    
     
    if (subject_condition_number <= graph_middle ) {
      label_subject <- "\u2190Subject"
      label_position <- subject_condition_number + ((cms_condition_max() - cms_condition_min()) * 0.2)
    }
    else {
      label_subject <- "Subject\u2192"
      label_position <- subject_condition_number - ((cms_condition_max() - cms_condition_min()) * 0.2)
    }

    gg <- ggplot() +
      geom_histogram(data = adf_data,
                     aes(`condition`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5,) +
      geom_histogram(data = cms_data,
                     aes(`condition`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      geom_vline(xintercept = subject_condition_number, color = "red", linetype = "solid", linewidth = 0.5) +
      geom_text(aes(x=label_position,
                    label=label_subject,
                    y=max_count),
                colour="red",
                size = 3
      ) +
      scale_x_continuous(
        #limits = c(cms_condition_min(), cms_condition_max()),
        breaks = 1:5, 
        labels = c('Poor', 'Fair', 'Average', 'Good', 'Excellent')
      ) +
      labs(
        title = "Sales by Condition",
        x = "Condition",
        y = "Count") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sales by Garage Spaces Distribution ----

  # Define an observer to update the reactive value when the slider changes
  observeEvent(input$garageRange, {
    cms_garage_values(input$garageRange)
  })
  
  output$cmsSalesByGarage <- renderPlotly({
    adf_data <- adfData()
    cms_data <- cmsData()
    # Calculate the histogram
    hist_data <- ggplot_build(
      ggplot(adf_data, aes(x = `Garage.Spaces`)) +
        geom_histogram(binwidth = 1, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)
    
    cms_garage_min(min(adf_data$`Garage.Spaces`))
    cms_garage_max(max(adf_data$`Garage.Spaces`))
    
    graph_middle <- cms_garage_min() + ((cms_garage_max() - cms_garage_min()) / 2)
    
    if (input$SubjectGarageSpaces <= graph_middle ) {
      label_subject <- "\u2190Subject"
      label_position <- input$SubjectGarageSpaces + ((cms_garage_max() - cms_garage_min()) * 0.2)
    }
    else {
      label_subject <- "Subject\u2192"
      label_position <- input$SubjectGarageSpaces - ((cms_garage_max() - cms_garage_min()) * 0.2)
    }
    
    gg <- ggplot() +
      geom_histogram(data = adf_data,
                     aes(`Garage.Spaces`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5,) +
      geom_histogram(data = cms_data,
                     aes(`Garage.Spaces`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      geom_vline(xintercept = input$SubjectGarageSpaces, color = "red", linetype = "solid", linewidth = 0.5) +
      geom_text(aes(x=label_position,
                    label=label_subject,
                    y=max_count),
                colour="red",
                size = 3
      ) +
      scale_x_continuous(limits = c(cms_garage_min(), cms_garage_max())) +
      labs(
        title = "Sales by Garage Spaces",
        x = "Garage Spaces",
        y = "Count") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sales by Carport Spaces Distribution ----

  # Define an observer to update the reactive value when the slider changes
  observeEvent(input$carportRange, {
    cms_carport_values(input$carportRange)
    # cms_carport_range_low <- cms_carport_slider_values()[1]
    # cms_carport_range_high <- cms_carport_slider_values()[2]
  })
  
  output$cmsSalesByCarport <- renderPlotly({
    
    adf_data <- adfData()
    cms_data <- cmsData()
    # Calculate the histogram
    hist_data <- ggplot_build(
      ggplot(adf_data, aes(x = `Carport.Spaces`)) +
        geom_histogram(binwidth = 1, fill = "blue", alpha = 0.1, colour = "black", linewidth = 0.5)
    )
    max_count <- max(hist_data$data[[1]]$count)
    max_count <- round(max_count * 1.15)
    
    cms_carport_min(min(adf_data$`Carport.Spaces`))
    cms_carport_max(max(adf_data$`Carport.Spaces`))
    
    graph_middle <- cms_carport_min() + ((cms_carport_max() - cms_carport_min()) / 2)
    
    if (input$SubjectCarportSpaces <= graph_middle ) {
      label_subject <- "\u2190Subject"
      label_position <- input$SubjectCarportSpaces + ((cms_carport_max() - cms_carport_min()) * 0.2)
    }
    else {
      label_subject <- "Subject\u2192"
      label_position <- input$SubjectCarportSpaces - ((cms_carport_max() - cms_carport_min()) * 0.2)
    }
    
    gg <- ggplot() +
      geom_histogram(data = adf_data,
                     aes(`Carport.Spaces`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.1,
                     colour = "black",
                     linewidth=0.5,) +
      geom_histogram(data = cms_data,
                     aes(`Carport.Spaces`),
                     binwidth = 1,
                     fill = "blue",
                     alpha = 0.5,
                     colour = "black",
                     linewidth=0.5) +
      scale_y_continuous(limits=c(0, max_count)) +
      geom_vline(xintercept = input$SubjectCarportSpaces, color = "red", linetype = "solid", linewidth = 0.5) +
      geom_text(aes(x=label_position,
                    label=label_subject,
                    y=max_count),
                colour="red",
                size = 3
      ) +
      scale_x_continuous(limits = c(cms_carport_min(), cms_carport_max())) +
      labs(
        title = "Sales by Carport Spaces",
        x = "Carport Spaces",
        y = "Count") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 8, face = "bold")
      )
    # Suppress specific warning message
    suppressWarnings(ggplotly(gg))
  })
  
  ## CMS Graph Sale Price vs Sale Date ----
  output$cmsSalesBySaleDatePlot <- renderPlotly(
    
    {
      #adf_data <- adf_data()
      cms_data_with_tooltips <- cmsData() %>%
        mutate(tooltip_text = paste("MLS No.:", MLS..No, 
                                    "<br>Address:", Address,
                                    "<br>Sale Date", Date.Pending,
                                    "<br>Sale Price", Price.Sold
        ))
      gg <- ggplot(cms_data_with_tooltips) +
        aes(`Date.Pending`, `Price.Sold`, key = MLS..No) +
        geom_point(aes(text = tooltip_text)) +
        geom_smooth(method = "lm",
                    formula = y ~ poly(x,1),
                    se = F,
                    color = "blue"
        ) +
        geom_smooth(method = "lm",
                    formula = y ~ poly(x,3),
                    se = F,
                    color = "red"
        ) +
        scale_x_date(date_labels = "%b %y", breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(
          labels = scales::dollar_format(prefix = "$", big.mark = ",", scale = 1e-3, suffix = "K"),
          expand = c(0, 0),
          breaks = scales::pretty_breaks(n = 10)
        ) +
        labs(title = "CMS Sale Price vs Sale Date",
             x = "Reported Sale Date",
             y = "Reported Sale Price") +
        theme_bw() +
        theme(
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 10, face = "bold")
        )
      
      suppressWarnings(ggplotly(gg, tooltip = "text", source = "cmsSalesBySaleDatePlot"))
    })
  
  observeEvent(event_data("plotly_click", source = "cmsSalesBySaleDatePlot"), {
    click_data <- event_data("plotly_click", source = "cmsSalesBySaleDatePlot")
    # Extract current data
    cms_data <- cmsData()
    
    # Find the clicked point from cmsData and add it to outliers df
    datapoint_clicked <- cms_data[cms_data$MLS..No == click_data$key, ]
    cmsDataOutliers(rbind(datapoint_clicked, adfDataOutliers()))
  })
  
  # CMS LOCATION MAP ----
observeEvent(cmsData(),{

  output$cms_data_map <- renderMapboxer({
    LAYER_ID <- "sales"
    cms_data <- cmsData()
    
    colnames(cms_data)[colnames(cms_data) == "lon"] <- "lng"
    colnames(cms_data)[colnames(cms_data) == "MLS..No"] <- "mls"
    colnames(cms_data)[colnames(cms_data) == "Date.Pending"] <- "sale_date"
    colnames(cms_data)[colnames(cms_data) == "Price.Sold"] <- "sale_price"
    colnames(cms_data)[colnames(cms_data) == "Year.Built..Est."] <- "year_built"
    colnames(cms_data)[colnames(cms_data) == "Lot.Size.SqFt"] <- "lot_size"
    colnames(cms_data)[colnames(cms_data) == "Fin.SqFt.Total"] <- "gla"
    
    cms_data$mls <- as.character(cms_data$mls)
    cms_data$lot_size <- as.character(cms_data$lot_size)
    
    #print(str(cms_data))
    
    my_center <- c(lng = mean(cms_data$lng, na.rm = TRUE), lat = mean(cms_data$lat, na.rm = TRUE))
    
    map <- as_mapbox_source(cms_data,
                            lng = "lng",
                            lat = "lat"
    ) %>%
      mapboxer(center = my_center,
               zoom = 11,
               style = "mapbox://styles/mapbox/outdoors-v12", token = MAPBOX_TOKEN) %>%
      add_navigation_control() %>%
      # Adding markers for each point
      # for(i in 1:nrow(adf_data)) {
      #   map <- add_marker(map, lng = adf_data$lon[i], lat = adf_data$lat[i], popup = paste("Point", i))
      # }
      add_circle_layer(
        id = LAYER_ID,
        popup = "<p>MLS No.: {{mls}}<br>{{location}}<br>Date of Sale: {{sale_date}}<br>Sale Price: {{sale_price}} <br>Lot size: {{lot_size}}<br>Year Built: {{year_built}}<br>GLA: {{gla}}<br>Bedrooms: {{Beds}} <br> Bathrooms: {{Baths}}</p>",
        #filter = c("has", "point_count"),
        #circle_color = "blue",
        circle_radius = list("interpolate",
                             list("linear"),
                             list("get", "sale_price"),
                             200000, 2.5,  # Circle radius 10 for sale_price 100000
                             300000, 5,
                             400000, 7.5,
                             500000, 10,
                             600000, 12.5,
                             700000, 15,
                             800000, 17.5,
                             900000, 20,
                             1000000, 22.5), # Circle radius 30 for sale_price 1000000
        circle_color = list("interpolate",
                            list("linear"),
                            list("get", "sale_price"),
                            200000, "DarkGreen",  # Circle radius 10 for sale_price 100000
                            300000, "OliveDrab",
                            400000, "DarkKhaki",
                            500000, "GoldenRod",
                            600000, "DarkOrange",
                            700000, "Tomato",
                            800000, "IndianRed",
                            900000, "FireBrick",
                            1000000, "red") # Circle radius 30 for sale_price 1000000
        
      ) %>%
      add_marker(lng = mean(cms_data$lng), lat = mean(cms_data$lat), popup = "Subject Property")
    
    map
  })
  
})
  
  # WINTOTAL SUBJECT SECTION IMPORT ----
  
  observeEvent(input$import_wintotal, {
    df <- read_excel(wintotal_excel_path, skip = 17, n_max = 12)
    if (ncol(df) != 2) {
      return(NULL)
    }
    colnames(df) <- c("Variable_Name", "Variable_Value")
    # Removing the trailing colon from the variable names
    df$Variable_Name <- gsub(":$", "", df$Variable_Name)
    # Remove escaped quotes
    df$Variable_Value <- gsub("\\\"", "", df$Variable_Value)
    # Store the data in a reactive variable
    excel_data(df)
    # Make the variables globally accessible in the app (use with caution)
    for (i in 1:nrow(df)) {
      assign(trimws(as.character(df$Variable_Name[i])), df$Variable_Value[i], envir = .GlobalEnv)
      # print(paste(
      #   "Variable:",
      #   trimws(as.character(df$Variable_Name[i])),
      #   "Value:",
      #   df$Variable_Value[i]
      # ))
    }
    # Update each control
    if ("SubjectAddress" %in% df$Variable_Name) {
      updateTextInput(session, "SubjectAddress", value = SubjectAddress)
    }
    if ("propertyType" %in% df$Variable_Name) {
      updateSelectInput(session, "propertyType", selected = propertyType)
    }
    if ("SubYB" %in% df$Variable_Name) {
      updateNumericInput(session, "SubYB", value = as.numeric(SubYB))
    }
    if ("SubjectLotSF" %in% df$Variable_Name) {
      updateNumericInput(session, "SubjectLotSF", value = as.numeric(SubjectLotSF))
    }
    if ("SubjectGLA" %in% df$Variable_Name) {
      updateNumericInput(session, "SubjectGLA", value = as.numeric(SubjectGLA))
    }
    if ("SubjectBeds" %in% df$Variable_Name) {
      updateNumericInput(session, "SubjectBeds", value = as.numeric(SubjectBeds))
    }
    if ("SubjectBaths" %in% df$Variable_Name) {
      updateNumericInput(session, "SubjectBaths", value = as.numeric(SubjectBaths))
    }
    if ("SubjectBasement" %in% df$Variable_Name) {
      updateRadioButtons(session, "SubjectBasement", selected = SubjectBasement)
    }
    if ("SubjectSalePrice" %in% df$Variable_Name) {
      updateNumericInput(session, "SubjectSalePrice", value = as.numeric(SubjectSalePrice))
    }
    if ("SubjectCondition" %in% df$Variable_Name) {
      updateSelectInput(session, "SubjectCondition", selected = SubjectCondition)
    }
    if ("SubjectGarage" %in% df$Variable_Name) {
      updateNumericInput(session, "SubjectGarageSpaces", value = as.numeric(SubjectGarage))
    }
    if ("SubjectCarport" %in% df$Variable_Name) {
      updateNumericInput(session, "SubjectCarportSpaces", value = as.numeric(SubjectCarport))
    }
    
    #GeoCode address function
    geocode_subject_address <- geocode(SubjectAddress, output = "latlona", source = "google")
    
    # Update reactive values with latitude and longitude
    subjectLat(geocode_subject_address$lat)
    subjectLon(geocode_subject_address$lon)
    
    #print(subjectLat())
    #print(subjectLon())
    
    
  })
  
  observeEvent(input$copyMLSButton, {
    mls_numbers <- cmsData()$`MLS..No`
    clipr::write_clip(mls_numbers)
  })
  
  # Trigger reactivity on click of reload button
  observeEvent(input$reload, {
    session$reload()  # Simulates refreshing by invalidating after 1 millisecond
  })


}
shinyApp(ui, server)