## --------------------------------
##
## FILE NAME: Context360 (Search)
## AUTHOR: Ariya Hagh
## CREATED: 2025-06-08
## EMAIL: ahagh@worldbank.org
##
## --------------------------------
## NOTES: Version 2 app based on
## Data360 API   
##
## --------------------------------
##
## PREAMBLE:
##
setwd("C:/Users/wb519588/OneDrive - WBG/Documents/Data Work/Data360")
#options(scipen = 6, digits = 4)     # Remove for Scientific Notation
##
library(dplyr)
library(shiny)
library(httr)
library(jsonlite)
library(DT)           # for DT::datatable
library(htmlwidgets)  # required for rendering DT tables
library(crosstalk)    # fixes 'crosstalkOptions' error
library(countrycode)  # adds country list
## --------------------------------

# ---- THEME ----
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "superhero",     # Use the new base theme
  bg = "#ffffff",               # Force white background
  fg = "#000000",               # All text black
  base_font = font_google("Montserrat"),
  primary = "#ffc700"           # Accent color for buttons
)


# ---- UI ----
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
    div("Context360", style = "font-size: 1.5rem; padding-right: 2rem; color: black;")
  ),
  
  theme = custom_theme,
  
  header = tagList(
    tags$head(
      tags$style(HTML("
  .navbar {
    background-color: #ffc700 !important;
  }
  body {
    background-color: #ffffff !important;
  }
  .navbar-nav > li > a {
    font-size: 1.1rem;
  }
  input.form-control, 
  input[type='number'] {
    background-color: #61bcff !important;
    border: 2px solid #000 !important;
  }
  /* NEW: Highlight selected DT row */
  table.dataTable tbody tr.selected {
    background-color: #61bcff !important;
    color: #000000 !important;
  }
"))
    ),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
  ),
  
  tabPanel("Topics",
           sidebarLayout(
             sidebarPanel(
               textInput("searchTerm", "Enter Topic:", placeholder = "e.g., literacy"),
               numericInput("topN", "Number of results:", value = 5, min = 1, max = 100),
               actionButton("search_btn", "Search",
                            class = "btn btn-primary",
                            style = "background-color: #ffc700; color: black;"),
               br(), br(),
               downloadButton("download_data", "Download Table", class = "btn btn-primary")
             ),
             mainPanel(
               h4("Search Results"),
               DT::dataTableOutput("results_table")
             )
           )
  ),
  
  tabPanel("Data",
           fluidPage(
             h4("Step 1: Search for Indicators"),
             textInput("data_search_term", "Enter Topic:", placeholder = "e.g., education"),
             numericInput("data_top_n", "Number of results:", value = 5, min = 1, max = 100),
             actionButton("data_search_btn", "Search", class = "btn btn-lg", style = "background-color: #ffc700; color: black;"),
             br(), br(),
             DT::dataTableOutput("data_search_results"),
             
             hr(),
             h4("Step 2: Get Data by Country and Year"),
             
             selectInput("selected_country", "Select Country:", choices = NULL),
             numericInput("year_from", "From Year:", value = 2015, min = 1960, max = 2100),
             numericInput("year_to", "To Year:", value = 2025, min = 1960, max = 2100),
             
             actionButton("fetch_data_btn", "Fetch Data",
                          class = "btn btn-lg", 
                          style = "background-color: #ffc700; color: black;"),
             
             br(), br(),
             DT::dataTableOutput("data_api_results")
           ),
          # downloadButton("download_step2_data", "Download Results", class = "btn btn-warning", style = "margin-top: 15px; color: black;")
           uiOutput("download_step2_button")
           
  )
)

#-----SERVER-----
server <- function(input, output, session) {
  
  # -- Reactive values --
  data_search_results <- reactiveVal(NULL)
  results_data <- reactiveVal(NULL)
  
  # -- Topic Search Tab --
  observeEvent(input$search_btn, {
    req(input$searchTerm, input$topN)
    
    body <- list(
      count = FALSE,
      orderby = "series_description/name",
      select = "series_description/idno, series_description/name, series_description/database_id, series_description/periodicity, series_description/measurement_unit, series_description/definition_long, series_description/statistical_concept, series_description/limitation, series_description/csv_link",
      search = input$searchTerm,
      top = input$topN,
      skip = 0
    )
    
    res <- POST(
      url = "https://data360api.worldbank.org/data360/search",
      body = body,
      encode = "json",
      add_headers(`Content-Type` = "application/json")
    )
    
    if (status_code(res) == 200) {
      res_content <- content(res, as = "text", encoding = "UTF-8")
      parsed <- fromJSON(res_content, flatten = TRUE)
      
      if (length(parsed$value) == 0) {
        output$results_table <- renderDataTable({
          data.frame(Message = "No results found.")
        })
        results_data(NULL)
      } else {
        results <- parsed$value
        df <- results[, c(
          "@search.score", "series_description.idno", "series_description.name", "series_description.database_id",
          "series_description.measurement_unit", "series_description.periodicity", "series_description.definition_long",
          "series_description.statistical_concept", "series_description.limitation", "series_description.csv_link"
        )]
        
        colnames(df) <- c(
          "Score", "ID", "Name", "Database", "Unit", "Periodicity",
          "Definition", "Statistical Concept", "Limitations", "CSV Link"
        )
        
        df <- df[order(-as.numeric(df$Score)), ]
        
        df$`CSV Link` <- ifelse(
          is.na(df$`CSV Link`) | df$`CSV Link` == "",
          NA,
          paste0("<a href='", df$`CSV Link`, "' target='_blank'><i class='fas fa-download' style='color: #d9534f;'></i></a>")
        )
        
        results_data(df)
        
        output$results_table <- renderDataTable({
          datatable(
            df,
            escape = FALSE,
            rownames = FALSE,
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              autoWidth = TRUE,
              columnDefs = list(
                list(width = '350px', targets = which(colnames(df) == "Definition") - 1),
                list(width = '275px', targets = which(colnames(df) == "Statistical Concept") - 1),
                list(width = '275px', targets = which(colnames(df) == "Limitations") - 1),
                list(className = 'dt-left', targets = "_all")
              )
            )
          )
        })
      }
    } else {
      output$results_table <- renderDataTable({
        data.frame(Message = paste("Error from API:", status_code(res)))
      })
      results_data(NULL)
    }
  })
  
  # -- CSV Download --
  output$download_data <- downloadHandler(
    filename = function() paste0("data360_results_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- results_data()
      if (!is.null(df)) {
        df$`CSV Link` <- gsub(".*href='(.*?)'.*", "\\1", df$`CSV Link`)
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  # -- Data Tab: Step 1 (Search for indicators) --
  observeEvent(input$data_search_btn, {
    req(input$data_search_term, input$data_top_n)
    
    body <- list(
      count = FALSE,
      orderby = "series_description/name",
      select = "series_description/idno, series_description/name, series_description/database_id",
      search = input$data_search_term,
      top = input$data_top_n,
      skip = 0
    )
    
    res <- POST(
      url = "https://data360api.worldbank.org/data360/search",
      body = body,
      encode = "json",
      add_headers(`Content-Type` = "application/json")
    )
    
    if (status_code(res) == 200) {
      res_text <- content(res, as = "text", encoding = "UTF-8")
      parsed <- fromJSON(res_text, flatten = TRUE)
      
      if (length(parsed$value) == 0) {
        output$data_search_results <- renderDataTable({
          data.frame(Message = "No results found.")
        })
      } else {
        results <- parsed$value
        df <- as.data.frame(results)[, c(
          "@search.score",
          "series_description.idno",
          "series_description.name",
          "series_description.database_id"
        )]
        colnames(df) <- c("Score", "Indicator ID", "Indicator Name", "Database ID")
        df <- df[order(-df$Score), ]
        data_search_results(df)
        
        output$data_search_results <- renderDataTable({
          datatable(
            df, selection = "multiple", rownames = FALSE,
            options = list(pageLength = 5)
          )
        })
      }
    } else {
      output$data_search_results <- renderDataTable({
        data.frame(Message = paste("API Error:", status_code(res)))
      })
    }
  })
  
  # -- Full ISO3 Country Dropdown via countrycode --
  iso3_lookup <- na.omit(countrycode::codelist[, c("country.name.en", "iso3c")])
  colnames(iso3_lookup) <- c("country", "iso3")
  
  observe({
    updateSelectInput(session, "selected_country", choices = iso3_lookup$country)
  })
  
  # -- Data Tab: Step 2 (Fetch data from selected indicators) --
  step2_data <- reactiveVal(NULL)
  
  observeEvent(input$fetch_data_btn, {
    req(input$selected_country, input$year_from, input$year_to)
    df_step1 <- data_search_results()
    req(df_step1)
    
    ref_area <- iso3_lookup$iso3[iso3_lookup$country == input$selected_country]
    full_data <- list()
    
    for (i in 1:nrow(df_step1)) {
      indicator_id <- df_step1$`Indicator ID`[i]
      database_id <- df_step1$`Database ID`[i]
      
      data_url <- paste0(
        "https://data360api.worldbank.org/data360/data?",
        "DATABASE_ID=", database_id,
        "&INDICATOR=", indicator_id,
        "&REF_AREA=", ref_area,
        "&timePeriodFrom=", input$year_from,
        "&timePeriodTo=", input$year_to,
        "&skip=0"
      )
      
      res <- httr::GET(data_url)
      if (status_code(res) == 200) {
        parsed <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
        if (!is.null(parsed$value) && length(parsed$value) > 0) {
          subdf <- parsed$value[, c("INDICATOR", "COMMENT_TS", "REF_AREA", "SEX", "TIME_PERIOD", "OBS_VALUE", "LATEST_DATA")]
          
          # Translate SEX codes
          subdf$SEX <- recode(subdf$SEX, `_T` = "Total", F = "Female", M = "Male", `_Z` = "NA")
          
          full_data[[i]] <- subdf
        }
      }
    }
    
    if (length(full_data) == 0) {
      output$data_api_results <- DT::renderDataTable({
        data.frame(Message = "No data found for selected indicators and time range.")
      })
      step2_data(NULL)
    } else {
      combined <- do.call(rbind, full_data)
      colnames(combined) <- c("Indicator ID", "Name", "Country", "Sex", "Year", "Value", "Latest Data?")
      
      step2_data(combined)  
      
      # Make value column red
      output$data_api_results <- DT::renderDataTable({
        DT::datatable(
          combined,
          escape = FALSE,
          rownames = FALSE,
          options = list(pageLength = 10),
          callback = JS(
            "table.rows().every(function(rowIdx, tableLoop, rowLoop) {",
            "  var data = this.data();",
            "  data[5] = '<span style=\"color:#d9534f;\">' + data[5] + '</span>';",
            "  this.invalidate();",
            "});"
          )
        )
      })
    }
  })
  # -- Step 2 Download Handler --
  output$download_step2_data <- downloadHandler(
    filename = function() {
      paste0("step2_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- step2_data()
      if (!is.null(df)) {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  # -- Conditionally render the download button --
  output$download_step2_button <- renderUI({
    df <- step2_data()
    if (!is.null(df) && nrow(df) > 0) {
      downloadButton("download_step2_data", "Download Results", class = "btn btn-primary")
    }
  })
}

# ---- LAUNCH ----
shinyApp(ui = ui, server = server)
