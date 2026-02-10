library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(reactable)
library(echarts4r)
library(scales)
library(fresh)
library(tidyquant)
library(PerformanceAnalytics)
library(xts)

# Create custom theme - standard navy/white
app_theme <- create_theme(
  bs4dash_vars(
    navbar_dark_color = "#fff",
    navbar_dark_active_color = "#fff",
    navbar_dark_hover_color = "#a8c5dd"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#000080", 
    text_light = "#FFF"
  ),
  bs4dash_layout(
    main_bg = "#f4f6f9"
  ),
  bs4dash_sidebar_dark(
    bg = "#000080",
    color = "#fff",
    hover_color = "#a8c5dd",
    submenu_bg = "#000066", 
    submenu_color = "#FFF", 
    submenu_hover_color = "#a8c5dd"
  ),
  bs4dash_status(
    primary = "#000080",
    secondary = "#0066cc",
    info = "#17a2b8",
    success = "#28a745",
    warning = "#ffc107",
    danger = "#dc3545"
  ),
  bs4dash_color(
    gray_x_light = "#e9ecef",
    gray_600 = "#6c757d",
    white = "#FFF",
    gray_800 = "#343a40"
  )
)

# UI Definition
ui <- dashboardPage(
  dark = FALSE,
  freshTheme = app_theme,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Index Performance",
      color = "primary",
      image = NULL
    ),
    skin = "dark",
    rightUi = tagList(
      dropdownMenu(
        type = "messages",
        badgeStatus = "primary",
        icon = icon("filter"),
        headerText = "Quick Filters",
        messageItem(
          from = "Asset Classes",
          message = "Click sidebar to filter",
          icon = icon("chart-pie")
        )
      )
    )
  ),
  
  sidebar = dashboardSidebar(
    skin = "dark",
    status = "primary",
    elevation = 3,
    sidebarUserPanel(
      image = NULL,
      name = "Dashboard Controls"
    ),
    sidebarMenu(
      id = "sidebar",
      sidebarHeader("Navigation"),
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      menuItem(
        "Performance Table",
        tabName = "table",
        icon = icon("table")
      ),
      menuItem(
        "Index Details",
        tabName = "charts",
        icon = icon("chart-line")
      ),
      menuItem(
        "Heatmap",
        tabName = "heatmap",
        icon = icon("th")
      ),
      menuItem(
        "Economic Data",
        tabName = "economic",
        icon = icon("chart-area")
      )
    ),
    
    hr(),
    
    sidebarHeader("Filters"),
    
    # Date Selection
    dateInput("as_of_date", 
              "As of Date:", 
              value = Sys.Date(),
              max = Sys.Date(),
              width = "100%"),
    
    # Asset Class Filter
    pickerInput(
      "asset_class_filter",
      "Asset Class:",
      choices = c("Equity", "Fixed Income", "Commodity", "Currency", "Alternative"),
      selected = c("Equity", "Fixed Income", "Commodity", "Currency", "Alternative"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} classes selected"
      )
    ),
    
    # Type Filter
    pickerInput(
      "type_filter",
      "Type:",
      choices = c("Broad Market", "Sector", "Factor", "Style", "Geographic"),
      selected = c("Broad Market", "Sector", "Factor", "Style", "Geographic"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} types selected"
      )
    ),
    
    hr(),
    
    actionButton("refresh", 
                 "Refresh Data", 
                 icon = icon("sync"),
                 class = "btn-primary btn-block",
                 style = "margin: 10px;")
  ),
  
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Navy theme styling */
        .main-header .navbar {
          background-color: #000080 !important;
        }
        .brand-link {
          background-color: #000080 !important;
          color: #fff !important;
        }
        .brand-link:hover {
          color: #a8c5dd !important;
        }
        /* Card headers */
        .card-primary:not(.card-outline) > .card-header {
          background-color: #000080 !important;
          color: #fff !important;
        }
        .card-success:not(.card-outline) > .card-header {
          background-color: #28a745 !important;
          color: #fff !important;
        }
        .card-info:not(.card-outline) > .card-header {
          background-color: #17a2b8 !important;
          color: #fff !important;
        }
        .card-warning:not(.card-outline) > .card-header {
          background-color: #ffc107 !important;
          color: #000 !important;
        }
      "))
    ),
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("best_performer", width = 3),
          valueBoxOutput("worst_performer", width = 3),
          valueBoxOutput("avg_return", width = 3),
          valueBoxOutput("total_indices", width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "YTD Performance Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            echarts4rOutput("ytd_dist_chart", height = "350px")
          ),
          bs4Card(
            title = "Top 10 Performers (YTD)",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            echarts4rOutput("top_performers_chart", height = "350px")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Asset Class Performance",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            echarts4rOutput("asset_class_chart", height = "350px")
          )
        )
      ),
      
      # Performance Table Tab
      tabItem(
        tabName = "table",
        fluidRow(
          bs4Card(
            title = "Index Returns (%)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            closable = FALSE,
            maximizable = TRUE,
            reactableOutput("performance_table")
          )
        )
      ),
      
      # Charts Tab
      tabItem(
        tabName = "charts",
        fluidRow(
          bs4Card(
            width = 12,
            status = "primary",
            pickerInput("chart_indices", 
                       "Select Indices (Multi-Select):", 
                       choices = NULL,
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         `selected-text-format` = "count > 3",
                         `count-selected-text` = "{0} indices selected",
                         `live-search` = TRUE
                       ))
          )
        ),
        fluidRow(
          bs4Card(
            title = "Trailing Period Returns Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            maximizable = TRUE,
            echarts4rOutput("returns_chart", height = "400px")
          ),
          bs4Card(
            title = "Return vs Volatility",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            maximizable = TRUE,
            fluidRow(
              column(12,
                selectInput("volatility_period",
                           "Return Period:",
                           choices = c("1 Week", "MTD", "QTD", 
                                     "3 Month", "6 Month", "YTD", "1 Year"),
                           selected = "YTD")
              )
            ),
            echarts4rOutput("volatility_scatter", height = "350px")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Price History (1 Year)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            maximizable = TRUE,
            echarts4rOutput("price_chart", height = "400px")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Return Scatter Plot",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            maximizable = TRUE,
            fluidRow(
              column(6,
                selectInput("scatter_x_axis",
                           "X-Axis Period:",
                           choices = c("1 Day", "1 Week", "MTD", "QTD", 
                                     "3 Month", "6 Month", "YTD", "1 Year", 
                                     "2 Year", "3 Year"),
                           selected = "YTD")
              ),
              column(6,
                selectInput("scatter_y_axis",
                           "Y-Axis Period:",
                           choices = c("1 Day", "1 Week", "MTD", "QTD", 
                                     "3 Month", "6 Month", "YTD", "1 Year", 
                                     "2 Year", "3 Year"),
                           selected = "1 Year")
              )
            ),
            echarts4rOutput("scatter_chart", height = "400px")
          )
        )
      ),
      
      # Heatmap Tab
      tabItem(
        tabName = "heatmap",
        fluidRow(
          bs4Card(
            width = 12,
            status = "primary",
            selectInput("heatmap_period", 
                       "Select Period:", 
                       choices = c("1 Day", "1 Week", "MTD", "QTD", 
                                 "3 Month", "6 Month", "YTD", "1 Year", 
                                 "2 Year", "3 Year"),
                       selected = "YTD",
                       width = "100%")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Returns Heatmap",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            maximizable = TRUE,
            echarts4rOutput("heatmap", height = "700px")
          )
        )
      ),
      
      # Economic Data Tab
      tabItem(
        tabName = "economic",
        fluidRow(
          bs4Card(
            title = "Key Economic Indicators",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            reactableOutput("economic_table")
          )
        ),
        fluidRow(
          bs4Card(
            title = "US Treasury Yield Curve",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            maximizable = TRUE,
            echarts4rOutput("yield_curve_chart", height = "400px")
          ),
          bs4Card(
            title = "Yield Curve Data",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            reactableOutput("yield_curve_table")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Credit Spreads",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            maximizable = TRUE,
            echarts4rOutput("credit_spreads_chart", height = "400px")
          ),
          bs4Card(
            title = "Credit Spread Data",
            status = "secondary",
            solidHeader = TRUE,
            width = 6,
            reactableOutput("credit_spreads_table")
          )
        )
      )
    )
  ),
  
  controlbar = dashboardControlbar(
    skin = "light",
    title = "Settings",
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        title = "Display Options",
        sliderInput(
          "decimal_places",
          "Decimal Places:",
          min = 0,
          max = 4,
          value = 2
        ),
        switchInput(
          "show_percentages",
          "Show % Symbol",
          value = TRUE
        ),
        switchInput(
          "color_coding",
          "Color Coding",
          value = TRUE
        )
      )
    )
  ),
  
  footer = dashboardFooter(
    left = "Index Performance Dashboard",
    right = paste("Last Updated:", Sys.time())
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # NEPC Color Palette (for charts only)
  nepc_colors <- c(
    `NEPC Blue`        = "#002060",
    `Middle Blue`      = "#16709E",
    `Sky Blue`         = "#6ED0F7",
    `Key Lime`         = "#8CC94A",
    `Petal`            = "#D6F28C",
    `Custard`          = "#F2CC73",
    `Grey Text`        = "#6B6B6B",
    `Light Grey`       = "#C9D4DE",
    `Dark Grey`        = "#43505E",
    `Middle Grey`      = "#8596A8"
  )
  
  # Helper function to get colors
  nepc_pal <- function(names) {
    unname(nepc_colors[names])
  }
  
  # ========== TICKER CONFIGURATION ==========
  # Define your tickers and index names
  # Replace these with your actual tickers
  ticker_config <- data.frame(
    Index = c("S&P 500", "NASDAQ", "Russell 2000", "MSCI World", 
              "Energy Sector", "Technology Sector", "Healthcare Sector",
              "Value Factor", "Momentum Factor", "Quality Factor",
              "US Agg Bond", "High Yield", "Gold", "USD Index"),
    Ticker = c("^GSPC", "^IXIC", "^RUT", "URTH", 
               "XLE", "XLK", "XLV",
               "VTV", "MTUM", "QUAL",
               "AGG", "HYG", "GLD", "UUP"),
    Asset_Class = c("Equity", "Equity", "Equity", "Equity",
                   "Equity", "Equity", "Equity",
                   "Equity", "Equity", "Equity",
                   "Fixed Income", "Fixed Income", "Commodity", "Currency"),
    Type = c("Broad Market", "Broad Market", "Broad Market", "Geographic",
            "Sector", "Sector", "Sector",
            "Factor", "Factor", "Factor",
            "Broad Market", "Sector", "Broad Market", "Broad Market"),
    stringsAsFactors = FALSE
  )
  
  # Load and process data using tidyquant
  index_data <- reactive({
    # Trigger refresh
    input$refresh
    
    # Date range for data
    start_date <- as.Date("2021-01-01")
    end_date <- Sys.Date()
    
    # Fetch price data for all tickers
    withProgress(message = 'Fetching ticker data...', value = 0, {
      
      price_data_list <- list()
      
      for(i in 1:nrow(ticker_config)) {
        incProgress(1/nrow(ticker_config), detail = ticker_config$Index[i])
        
        tryCatch({
          ticker_data <- tq_get(
            ticker_config$Ticker[i],
            get = "stock.prices",
            from = start_date,
            to = end_date
          ) %>%
            select(date, adjusted) %>%
            rename(Date = date, Price = adjusted) %>%
            mutate(
              Index = ticker_config$Index[i],
              Ticker = ticker_config$Ticker[i],
              Asset_Class = ticker_config$Asset_Class[i],
              Type = ticker_config$Type[i]
            )
          
          price_data_list[[i]] <- ticker_data
        }, error = function(e) {
          # If ticker fails, create empty data frame
          showNotification(
            paste("Failed to fetch:", ticker_config$Index[i]),
            type = "warning",
            duration = 3
          )
        })
      }
      
      # Combine all data
      if(length(price_data_list) > 0) {
        price_data <- bind_rows(price_data_list)
      } else {
        price_data <- data.frame()
      }
    })
    
    return(price_data)
  })
  
  # Fetch Economic Data using tidyquant/FRED
  economic_data <- reactive({
    # Trigger refresh
    input$refresh
    
    withProgress(message = 'Fetching economic data...', value = 0, {
      
      # Define key economic indicators with FRED series IDs
      indicators <- c(
        "GDP" = "GDP",
        "Unemployment Rate (%)" = "UNRATE",
        "CPI (All Items)" = "CPIAUCSL",
        "Federal Funds Rate (%)" = "FEDFUNDS",
        "10-Year Treasury Rate (%)" = "DGS10",
        "10Y-2Y Spread (bps)" = "T10Y2Y",
        "USD/EUR Exchange Rate" = "DEXUSEU",
        "VIX Index" = "VIXCLS"
      )
      
      result <- map_dfr(names(indicators), function(name) {
        incProgress(1/length(indicators), detail = name)
        
        tryCatch({
          data <- tq_get(
            indicators[[name]],
            get = "economic.data",
            from = Sys.Date() - years(1)
          ) %>%
            slice_max(date, n = 1) %>%
            select(Date = date, Value = price) %>%
            mutate(Indicator = name)
          
          return(data)
        }, error = function(e) {
          return(data.frame(
            Indicator = name,
            Value = NA,
            Date = Sys.Date()
          ))
        })
      })
    })
    
    result %>% select(Indicator, Value, Date)
  })
  
  # Fetch Yield Curve Data using tidyquant
  yield_curve_data <- reactive({
    # Trigger refresh
    input$refresh
    
    withProgress(message = 'Fetching yield curve...', value = 0, {
      
      # Treasury yield series
      yields <- c(
        "1 Mo" = "DGS1MO",
        "3 Mo" = "DGS3MO",
        "6 Mo" = "DGS6MO",
        "1 Yr" = "DGS1",
        "2 Yr" = "DGS2",
        "3 Yr" = "DGS3",
        "5 Yr" = "DGS5",
        "7 Yr" = "DGS7",
        "10 Yr" = "DGS10",
        "20 Yr" = "DGS20",
        "30 Yr" = "DGS30"
      )
      
      result <- map_dfr(names(yields), function(maturity) {
        incProgress(1/length(yields), detail = maturity)
        
        tryCatch({
          data <- tq_get(
            yields[[maturity]],
            get = "economic.data",
            from = Sys.Date() - days(30)
          ) %>%
            slice_max(date, n = 1) %>%
            select(Date = date, Rate = price) %>%
            mutate(Maturity = maturity)
          
          return(data)
        }, error = function(e) {
          return(data.frame(
            Maturity = maturity,
            Rate = NA,
            Date = Sys.Date()
          ))
        })
      })
      
      # Order by maturity
      result$Maturity <- factor(result$Maturity, levels = names(yields))
      result <- result %>% arrange(Maturity) %>% select(Maturity, Rate, Date)
    })
    
    return(result)
  })
  
  # Fetch Credit Spreads Data using tidyquant
  credit_spreads_data <- reactive({
    # Trigger refresh
    input$refresh
    
    withProgress(message = 'Fetching credit spreads...', value = 0, {
      
      # Credit spread series
      spreads <- c(
        "AAA Corporate" = "DAAA",
        "BAA Corporate" = "DBAA",
        "High Yield (BAA-AAA)" = "BAA10Y",
        "ICE BofA High Yield" = "BAMLH0A0HYM2"
      )
      
      result <- map_dfr(names(spreads), function(name) {
        incProgress(1/length(spreads), detail = name)
        
        tryCatch({
          data <- tq_get(
            spreads[[name]],
            get = "economic.data",
            from = Sys.Date() - days(30)
          ) %>%
            slice_max(date, n = 1) %>%
            select(Date = date, Rate = price) %>%
            mutate(Spread = name)
          
          return(data)
        }, error = function(e) {
          return(data.frame(
            Spread = name,
            Rate = NA,
            Date = Sys.Date()
          ))
        })
      })
    })
    
    result %>% select(Spread, Rate, Date)
  })
  
  # Function to calculate return
  calc_return <- function(data, start_date, end_date, annualize = FALSE, years = 1) {
    tryCatch({
      end_price <- data %>%
        filter(Date <= end_date) %>%
        slice_max(Date, n = 1) %>%
        pull(Price)
      
      start_price <- data %>%
        filter(Date <= start_date) %>%
        slice_max(Date, n = 1) %>%
        pull(Price)
      
      if(length(end_price) == 0 || length(start_price) == 0) {
        return(NA_real_)
      }
      
      if(annualize) {
        return(((end_price / start_price)^(1/years) - 1) * 100)
      } else {
        return((end_price / start_price - 1) * 100)
      }
    }, error = function(e) {
      return(NA_real_)
    })
  }
  
  # Function to calculate volatility using PerformanceAnalytics
  calc_volatility <- function(data, start_date, end_date) {
    tryCatch({
      period_data <- data %>%
        filter(Date >= start_date, Date <= end_date) %>%
        arrange(Date)
      
      if(nrow(period_data) < 2) return(NA_real_)
      
      # Convert to xts and calculate returns
      idx_xts <- xts(period_data$Price, order.by = period_data$Date)
      returns <- Return.calculate(idx_xts, method = "log")
      returns <- na.omit(returns)
      
      if(nrow(returns) < 2) return(NA_real_)
      
      # Annualized volatility
      volatility <- StdDev.annualized(returns, scale = 252) * 100
      
      return(as.numeric(volatility))
    }, error = function(e) {
      return(NA_real_)
    })
  }
  
  # Calculate returns using PerformanceAnalytics
  calculate_returns <- reactive({
    req(input$as_of_date)
    
    data <- index_data() %>%
      filter(Date <= input$as_of_date)
    
    req(nrow(data) > 0)
    
    as_of <- input$as_of_date
    
    one_day_ago <- as_of - days(1)
    one_week_ago <- as_of - weeks(1)
    mtd_start <- floor_date(as_of, "month")
    qtd_start <- floor_date(as_of, "quarter")
    three_month_ago <- as_of - months(3)
    six_month_ago <- as_of - months(6)
    ytd_start <- floor_date(as_of, "year")
    one_year_ago <- as_of - years(1)
    two_year_ago <- as_of - years(2)
    three_year_ago <- as_of - years(3)
    
    returns_list <- list()
    unique_indices <- unique(data$Index)
    
    withProgress(message = 'Calculating performance metrics...', value = 0, {
      
      for(idx in unique_indices) {
        incProgress(1/length(unique_indices), detail = idx)
        
        idx_data <- data %>% 
          filter(Index == idx) %>%
          arrange(Date)
        
        asset_class <- idx_data$Asset_Class[1]
        type <- idx_data$Type[1]
        
        # Convert to xts for PerformanceAnalytics
        idx_xts <- xts(idx_data$Price, order.by = idx_data$Date)
        daily_returns <- Return.calculate(idx_xts, method = "log")
        daily_returns <- na.omit(daily_returns)
        
        # Calculate annualized metrics (1 year)
        one_year_returns <- daily_returns[paste0(one_year_ago, "/", as_of)]
        
        if(nrow(one_year_returns) > 20) {
          ann_return <- Return.annualized(one_year_returns, scale = 252) * 100
          ann_vol <- StdDev.annualized(one_year_returns, scale = 252) * 100
          sharpe <- SharpeRatio.annualized(one_year_returns, Rf = 0.02/252, scale = 252)
          max_dd <- maxDrawdown(one_year_returns) * 100
        } else {
          ann_return <- NA_real_
          ann_vol <- NA_real_
          sharpe <- NA_real_
          max_dd <- NA_real_
        }
        
        # Calculate beta (using S&P 500 as benchmark)
        beta_val <- NA_real_
        if(idx != "S&P 500" && "S&P 500" %in% unique_indices) {
          benchmark_data <- data %>% 
            filter(Index == "S&P 500", Date >= one_year_ago, Date <= as_of) %>%
            arrange(Date)
          
          if(nrow(benchmark_data) > 20 && nrow(one_year_returns) > 20) {
            benchmark_xts <- xts(benchmark_data$Price, order.by = benchmark_data$Date)
            benchmark_returns <- Return.calculate(benchmark_xts, method = "log")
            benchmark_returns <- na.omit(benchmark_returns)
            
            # Align dates
            common_dates <- index(one_year_returns)[index(one_year_returns) %in% index(benchmark_returns)]
            if(length(common_dates) > 20) {
              idx_aligned <- one_year_returns[common_dates]
              bench_aligned <- benchmark_returns[common_dates]
              beta_val <- CAPM.beta(idx_aligned, bench_aligned)
            }
          }
        }
        
        returns_list[[idx]] <- data.frame(
          Index = idx,
          Asset_Class = asset_class,
          Type = type,
          `1 Day` = calc_return(idx_data, one_day_ago, as_of),
          `1 Week` = calc_return(idx_data, one_week_ago, as_of),
          `MTD` = calc_return(idx_data, mtd_start, as_of),
          `QTD` = calc_return(idx_data, qtd_start, as_of),
          `3 Month` = calc_return(idx_data, three_month_ago, as_of),
          `6 Month` = calc_return(idx_data, six_month_ago, as_of),
          `YTD` = calc_return(idx_data, ytd_start, as_of),
          `1 Year` = calc_return(idx_data, one_year_ago, as_of),
          `2 Year` = calc_return(idx_data, two_year_ago, as_of, annualize = TRUE, years = 2),
          `3 Year` = calc_return(idx_data, three_year_ago, as_of, annualize = TRUE, years = 3),
          `Ann. Return` = as.numeric(ann_return),
          `Ann. Volatility` = as.numeric(ann_vol),
          `Sharpe Ratio` = as.numeric(sharpe),
          `Max Drawdown` = as.numeric(max_dd),
          `Beta` = as.numeric(beta_val),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    })
    
    bind_rows(returns_list)
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(input$asset_class_filter, input$type_filter)
    
    calculate_returns() %>%
      filter(Asset_Class %in% input$asset_class_filter,
             Type %in% input$type_filter)
  })
  
  # Update chart index choices
  observe({
    req(nrow(filtered_data()) > 0)
    choices <- filtered_data()$Index
    updatePickerInput(session, "chart_indices", choices = choices, selected = choices[1])
  })
  
  # Value Boxes
  output$best_performer <- renderValueBox({
    req(nrow(filtered_data()) > 0)
    
    best <- filtered_data() %>%
      slice_max(YTD, n = 1)
    
    valueBox(
      value = paste0(round(best$YTD, 2), "%"),
      subtitle = paste("Best Performer:", best$Index),
      icon = icon("arrow-up"),
      color = "success"
    )
  })
  
  output$worst_performer <- renderValueBox({
    req(nrow(filtered_data()) > 0)
    
    worst <- filtered_data() %>%
      slice_min(YTD, n = 1)
    
    valueBox(
      value = paste0(round(worst$YTD, 2), "%"),
      subtitle = paste("Worst Performer:", worst$Index),
      icon = icon("arrow-down"),
      color = "danger"
    )
  })
  
  output$avg_return <- renderValueBox({
    req(nrow(filtered_data()) > 0)
    
    avg <- mean(filtered_data()$YTD, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(avg, 2), "%"),
      subtitle = "Average YTD Return",
      icon = icon("chart-bar"),
      color = "primary"
    )
  })
  
  output$total_indices <- renderValueBox({
    req(nrow(filtered_data()) > 0)
    
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Indices",
      icon = icon("list"),
      color = "info"
    )
  })
  
  # Performance Table
  output$performance_table <- renderReactable({
    req(nrow(filtered_data()) > 0)
    
    data <- filtered_data() %>%
      arrange(Asset_Class, Type, Index)
    
    reactable(
      data,
      defaultPageSize = 25,
      searchable = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      compact = FALSE,
      defaultSorted = "YTD",
      defaultSortOrder = "desc",
      groupBy = c("Asset_Class", "Type"),
      columns = list(
        Index = colDef(
          name = "Index",
          minWidth = 150,
          style = list(fontWeight = "bold")
        ),
        Asset_Class = colDef(
          name = "Asset Class",
          minWidth = 120,
          style = list(
            fontWeight = "bold",
            fontSize = "14px",
            color = "#002060"  # NEPC Blue
          ),
          aggregate = "unique"
        ),
        Type = colDef(
          name = "Type",
          minWidth = 120,
          style = list(
            fontWeight = "600",
            fontSize = "13px",
            color = "#43505E"  # Dark Grey
          ),
          aggregate = "unique"
        ),
        `1 Day` = colDef(
          name = "1 Day",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        ),
        `1 Week` = colDef(
          name = "1 Week",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        ),
        MTD = colDef(
          name = "MTD",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        ),
        QTD = colDef(
          name = "QTD",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        ),
        `3 Month` = colDef(
          name = "3 Month",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        ),
        `6 Month` = colDef(
          name = "6 Month",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        ),
        YTD = colDef(
          name = "YTD",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "bold")
          },
          minWidth = 100
        ),
        `1 Year` = colDef(
          name = "1 Year",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        ),
        `2 Year` = colDef(
          name = "2 Year",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        ),
        `3 Year` = colDef(
          name = "3 Year",
          format = colFormat(digits = 2, suffix = "%"),
          aggregate = "mean",
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 0) "#28a745" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "500")
          }
        )
      ),
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5ff",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%"),
        groupHeaderStyle = list(
          fontWeight = "bold",
          fontSize = "15px",
          background = "#f8f9fa",
          borderBottom = "2px solid #002060"  # NEPC Blue
        )
      )
    )
  })
  
  # YTD Distribution Chart
  output$ytd_dist_chart <- renderEcharts4r({
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      e_charts() %>%
      e_histogram(YTD, name = "Frequency", breaks = 20) %>%
      e_color(nepc_pal("Middle Blue")) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("YTD Return Distribution") %>%
      e_x_axis(name = "Return (%)", nameLocation = "center", nameGap = 30) %>%
      e_y_axis(name = "Count", nameLocation = "center", nameGap = 50) %>%
      e_legend(show = FALSE)
  })
  
  # Top Performers Chart
  output$top_performers_chart <- renderEcharts4r({
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      arrange(desc(YTD)) %>%
      head(10) %>%
      e_charts(Index) %>%
      e_bar(YTD, name = "YTD Return") %>%
      e_flip_coords() %>%
      e_tooltip(trigger = "axis") %>%
      e_color(nepc_pal("Key Lime")) %>%
      e_y_axis(inverse = TRUE) %>%
      e_x_axis(formatter = e_axis_formatter("decimal", digits = 2)) %>%
      e_legend(show = FALSE)
  })
  
  # Asset Class Performance Chart
  output$asset_class_chart <- renderEcharts4r({
    req(nrow(filtered_data()) > 0)
    
    asset_data <- filtered_data() %>%
      group_by(Asset_Class) %>%
      summarise(
        `1 Month` = mean(`MTD`, na.rm = TRUE),
        `3 Month` = mean(`3 Month`, na.rm = TRUE),
        `6 Month` = mean(`6 Month`, na.rm = TRUE),
        YTD = mean(YTD, na.rm = TRUE),
        `1 Year` = mean(`1 Year`, na.rm = TRUE),
        .groups = "drop"
      )
    
    asset_data %>%
      e_charts(Asset_Class) %>%
      e_bar(`1 Month`, stack = "grp", name = "1 Month") %>%
      e_bar(`3 Month`, stack = "grp", name = "3 Month") %>%
      e_bar(`6 Month`, stack = "grp", name = "6 Month") %>%
      e_bar(YTD, stack = "grp", name = "YTD") %>%
      e_bar(`1 Year`, stack = "grp", name = "1 Year") %>%
      e_tooltip(trigger = "axis") %>%
      e_legend(top = "top") %>%
      e_color(nepc_pal(c("NEPC Blue", "Middle Blue", "Sky Blue", "Key Lime", "Custard")))
  })
  
  # Returns Chart - Multi-Index Comparison (colored by index)
  output$returns_chart <- renderEcharts4r({
    req(input$chart_indices)
    req(all(input$chart_indices %in% filtered_data()$Index))
    
    data <- filtered_data() %>%
      filter(Index %in% input$chart_indices) %>%
      select(Index, `1 Day`, `1 Week`, `MTD`, `QTD`, `3 Month`, `6 Month`, 
             `YTD`, `1 Year`, `2 Year`, `3 Year`) %>%
      pivot_longer(cols = -Index, names_to = "Period", values_to = "Return") %>%
      mutate(Period = factor(Period, levels = c("1 Day", "1 Week", "MTD", "QTD", 
                                                "3 Month", "6 Month", "YTD", 
                                                "1 Year", "2 Year", "3 Year")))
    
    # Get unique indices for consistent coloring
    unique_indices <- unique(data$Index)
    color_palette <- nepc_pal(c("NEPC Blue", "Middle Blue", "Sky Blue", "Key Lime", "Custard", 
                                "Petal", "Grey Text", "Dark Grey", "Middle Grey", "Light Grey"))
    
    chart <- data %>%
      group_by(Index) %>%
      e_charts(Period, timeline = FALSE) %>%
      e_bar(Return) %>%
      e_tooltip(trigger = "axis") %>%
      e_color(color_palette[1:length(unique_indices)]) %>%
      e_y_axis(
        name = "Return (%)",
        nameLocation = "center",
        nameGap = 50,
        axisLabel = list(formatter = '{value}%')
      ) %>%
      e_x_axis(
        axisLabel = list(rotate = 45)
      ) %>%
      e_legend(top = "top")
    
    chart
  })
  
  # Return vs Volatility Scatter
  output$volatility_scatter <- renderEcharts4r({
    req(input$chart_indices, input$volatility_period)
    req(all(input$chart_indices %in% filtered_data()$Index))
    
    as_of <- input$as_of_date
    
    # Calculate lookback period
    period_map <- list(
      "1 Week" = weeks(1),
      "MTD" = floor_date(as_of, "month"),
      "QTD" = floor_date(as_of, "quarter"),
      "3 Month" = months(3),
      "6 Month" = months(6),
      "YTD" = floor_date(as_of, "year"),
      "1 Year" = years(1)
    )
    
    if(input$volatility_period %in% c("MTD", "QTD", "YTD")) {
      start_date <- period_map[[input$volatility_period]]
    } else {
      start_date <- as_of - period_map[[input$volatility_period]]
    }
    
    # Calculate return and volatility for each selected index
    vol_data <- map_dfr(input$chart_indices, function(idx) {
      idx_data <- index_data() %>%
        filter(Index == idx, Date <= as_of)
      
      return_val <- calc_return(idx_data, start_date, as_of)
      vol_val <- calc_volatility(idx_data, start_date, as_of)
      
      data.frame(
        Index = idx,
        Return = return_val,
        Volatility = vol_val,
        stringsAsFactors = FALSE
      )
    })
    
    req(nrow(vol_data) > 0)
    
    # Get consistent colors for indices
    unique_indices <- vol_data$Index
    color_palette <- nepc_pal(c("NEPC Blue", "Middle Blue", "Sky Blue", "Key Lime", "Custard", 
                                "Petal", "Grey Text", "Dark Grey", "Middle Grey", "Light Grey"))
    
    vol_data %>%
      group_by(Index) %>%
      e_charts(Volatility) %>%
      e_scatter(Return, symbol_size = 12) %>%
      e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS("
          function(params){
            return '<strong>' + params.seriesName + '</strong><br/>' +
                   'Volatility: ' + params.value[0].toFixed(2) + '%<br/>' +
                   'Return: ' + params.value[1].toFixed(2) + '%';
          }
        ")
      ) %>%
      e_x_axis(
        name = "Volatility (%)",
        nameLocation = "center",
        nameGap = 30,
        axisLabel = list(formatter = '{value}%')
      ) %>%
      e_y_axis(
        name = paste(input$volatility_period, "Return (%)"),
        nameLocation = "center",
        nameGap = 50,
        axisLabel = list(formatter = '{value}%')
      ) %>%
      e_legend(top = "top") %>%
      e_color(color_palette[1:length(unique_indices)])
  })
  
  # New Scatter Chart (Period vs Period) - colored by index
  output$scatter_chart <- renderEcharts4r({
    req(input$scatter_x_axis, input$scatter_y_axis)
    req(input$chart_indices)
    req(all(input$chart_indices %in% filtered_data()$Index))
    
    data <- filtered_data() %>%
      filter(Index %in% input$chart_indices) %>%
      select(Index, x = !!sym(input$scatter_x_axis), y = !!sym(input$scatter_y_axis))
    
    # Get consistent colors for indices
    unique_indices <- unique(data$Index)
    color_palette <- nepc_pal(c("NEPC Blue", "Middle Blue", "Sky Blue", "Key Lime", "Custard", 
                                "Petal", "Grey Text", "Dark Grey", "Middle Grey", "Light Grey"))
    
    data %>%
      group_by(Index) %>%
      e_charts(x) %>%
      e_scatter(y, symbol_size = 12) %>%
      e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS("
          function(params){
            return '<strong>' + params.seriesName + '</strong><br/>' +
                   params.value[0].toFixed(2) + '% vs ' + 
                   params.value[1].toFixed(2) + '%';
          }
        ")
      ) %>%
      e_x_axis(
        name = input$scatter_x_axis,
        nameLocation = "center",
        nameGap = 30,
        axisLabel = list(formatter = '{value}%')
      ) %>%
      e_y_axis(
        name = input$scatter_y_axis,
        nameLocation = "center",
        nameGap = 50,
        axisLabel = list(formatter = '{value}%')
      ) %>%
      e_legend(top = "top") %>%
      e_color(color_palette[1:length(unique_indices)]) %>%
      e_datazoom(x_index = 0, type = "slider") %>%
      e_datazoom(y_index = 0, type = "slider")
  })
  
  # Price Chart - Multi-Index (colored by index)
  output$price_chart <- renderEcharts4r({
    req(input$chart_indices)
    
    one_year_ago <- input$as_of_date - years(1)
    
    data <- index_data() %>%
      filter(Index %in% input$chart_indices,
             Date >= one_year_ago,
             Date <= input$as_of_date)
    
    req(nrow(data) > 0)
    
    # Get consistent colors for indices
    unique_indices <- unique(data$Index)
    color_palette <- nepc_pal(c("NEPC Blue", "Middle Blue", "Sky Blue", "Key Lime", "Custard", 
                                "Petal", "Grey Text", "Dark Grey", "Middle Grey", "Light Grey"))
    
    chart <- data %>%
      group_by(Index) %>%
      e_charts(Date, timeline = FALSE) %>%
      e_line(Price, smooth = TRUE) %>%
      e_tooltip(trigger = "axis") %>%
      e_datazoom(type = "slider") %>%
      e_color(color_palette[1:length(unique_indices)]) %>%
      e_y_axis(
        name = "Price",
        nameLocation = "center",
        nameGap = 50
      ) %>%
      e_legend(top = "top") %>%
      e_toolbox_feature(feature = "dataZoom") %>%
      e_toolbox_feature(feature = "restore")
    
    chart
  })
  
  # Heatmap
  output$heatmap <- renderEcharts4r({
    req(input$heatmap_period)
    req(nrow(filtered_data()) > 0)
    
    data <- filtered_data() %>%
      select(Index, Asset_Class, all_of(input$heatmap_period)) %>%
      arrange(Asset_Class, Index) %>%
      mutate(row_id = row_number() - 1)
    
    heatmap_data <- data.frame(
      x = 0,
      y = data$row_id,
      value = data[[input$heatmap_period]],
      Index = data$Index
    )
    
    heatmap_data %>%
      e_charts(y) %>%
      e_heatmap(x, value) %>%
      e_visual_map(
        value,
        inRange = list(color = c(nepc_pal("Dark Grey"), 
                                 nepc_pal("Grey Text"), 
                                 nepc_pal("Light Grey"), 
                                 nepc_pal("Sky Blue"), 
                                 nepc_pal("Key Lime"))),
        min = min(heatmap_data$value, na.rm = TRUE),
        max = max(heatmap_data$value, na.rm = TRUE)
      ) %>%
      e_y_axis(
        type = "category",
        data = data$Index,
        axisLabel = list(interval = 0)
      ) %>%
      e_x_axis(show = FALSE) %>%
      e_tooltip(
        formatter = htmlwidgets::JS("
          function(params){
            return params.name + '<br/>' + 
                   'Return: ' + params.value[2].toFixed(2) + '%';
          }
        ")
      )
  })
  
  # Refresh notification
  observeEvent(input$refresh, {
    showNotification(
      "Data refreshed successfully!",
      type = "message",
      duration = 3
    )
  })
  
  # Economic Indicators Table
  # Economic Indicators Table
  output$economic_table <- renderReactable({
    data <- economic_data()
    
    reactable(
      data,
      defaultPageSize = 10,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        Indicator = colDef(
          name = "Indicator",
          minWidth = 200,
          style = list(fontWeight = "bold")
        ),
        Value = colDef(
          name = "Value",
          format = colFormat(digits = 2),
          minWidth = 100
        ),
        Date = colDef(
          name = "As of Date",
          format = colFormat(date = TRUE),
          minWidth = 120
        )
      ),
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5ff",
        cellPadding = "8px 12px"
      )
    )
  })
  
  # Yield Curve Chart
  output$yield_curve_chart <- renderEcharts4r({
    data <- yield_curve_data()
    
    req(nrow(data) > 0)
    
    data %>%
      e_charts(Maturity) %>%
      e_line(Rate, name = "Yield (%)", smooth = TRUE, symbol_size = 8) %>%
      e_tooltip(trigger = "axis") %>%
      e_color(nepc_pal("NEPC Blue")) %>%
      e_y_axis(
        name = "Yield (%)",
        nameLocation = "center",
        nameGap = 50,
        axisLabel = list(formatter = '{value}%')
      ) %>%
      e_x_axis(
        name = "Maturity",
        nameLocation = "center",
        nameGap = 30,
        axisLabel = list(rotate = 45)
      ) %>%
      e_legend(show = FALSE) %>%
      e_grid(bottom = "20%")
  })
  
  # Yield Curve Table
  output$yield_curve_table <- renderReactable({
    data <- yield_curve_data()
    
    reactable(
      data,
      defaultPageSize = 11,
      bordered = TRUE,
      striped = TRUE,
      columns = list(
        Maturity = colDef(
          name = "Maturity",
          minWidth = 80,
          style = list(fontWeight = "bold")
        ),
        Rate = colDef(
          name = "Rate (%)",
          format = colFormat(digits = 2),
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 4) "#dc3545" else if (value > 3) "#ffc107" else "#28a745"
            list(color = color, fontWeight = "500")
          }
        ),
        Date = colDef(
          name = "Date",
          format = colFormat(date = TRUE),
          show = FALSE
        )
      ),
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        cellPadding = "6px 8px"
      )
    )
  })
  
  # Credit Spreads Chart
  output$credit_spreads_chart <- renderEcharts4r({
    data <- credit_spreads_data()
    
    req(nrow(data) > 0)
    
    data %>%
      e_charts(Spread) %>%
      e_bar(Rate, name = "Rate/Spread (%)") %>%
      e_tooltip(trigger = "axis") %>%
      e_color(nepc_pal(c("NEPC Blue", "Middle Blue", "Sky Blue", "Key Lime"))) %>%
      e_y_axis(
        name = "Rate (%)",
        nameLocation = "center",
        nameGap = 50,
        axisLabel = list(formatter = '{value}%')
      ) %>%
      e_x_axis(
        axisLabel = list(rotate = 45)
      ) %>%
      e_legend(show = FALSE) %>%
      e_grid(bottom = "25%")
  })
  
  # Credit Spreads Table
  output$credit_spreads_table <- renderReactable({
    data <- credit_spreads_data()
    
    reactable(
      data,
      defaultPageSize = 10,
      bordered = TRUE,
      striped = TRUE,
      columns = list(
        Spread = colDef(
          name = "Credit Spread",
          minWidth = 180,
          style = list(fontWeight = "bold")
        ),
        Rate = colDef(
          name = "Rate (%)",
          format = colFormat(digits = 2),
          style = function(value) {
            if (is.na(value)) return(NULL)
            color <- if (value > 5) "#dc3545" else if (value > 3) "#ffc107" else "#28a745"
            list(color = color, fontWeight = "500")
          }
        ),
        Date = colDef(
          name = "As of Date",
          format = colFormat(date = TRUE)
        )
      ),
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        cellPadding = "8px 12px"
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
