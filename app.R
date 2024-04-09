#-------------------------------------------------
# Rnssp Anomaly Detection Algorithms 
# Serverless WebAssembly App using Rnssp-wasm
#
# Atlanta, April 2024
# Author: G. Roseric Azondekon 
#
#-------------------------------------------------

# Install the Rnssp package for webr
webr::install(
  "Rnssp",
  repos = c("https://cdcgov.github.io/Rnssp-wasm", "https://repo.r-wasm.org")
)

library(Rnssp)

library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(shinycssloaders)
library(stringr)
library(ggplot2)


# UI widgets
ui <- fluidPage(
  tags$head(tags$style("#tbl {white-space: nowrap;}")),
  # Application title
  titlePanel("Rnssp Anomaly Detection Algorithms"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "detector", 
        "Select a Detector:", 
        c("EWMA", "Adaptive Multiple Regression", "EWMA/Regression Switch",
          "Negative Binomial", "Serfling", "Farrington"), 
        "EWMA"
      ),
      conditionalPanel(
        condition = "input.detector == 'EWMA' | input.detector == 'EWMA/Regression Switch'",
        selectInput("a_column", "Select a count or percent", c("count", "percent"), "count"),
        numericInput("a_B", "Number of days used to calculate rolling averages (B):", 28, min = 7),
        numericInput("a_g", "Number of guardband days (g):", 2, min = 1),
        numericInput("a_w1", "Smoothing coefficient for sensitivity to gradual events (w1):", 0.4, min = 0, max = 1, step = 0.1),
        numericInput("a_w2", "Smoothed coefficient for sensitivity to sudden events (w2):", 0.9, min = 0, max = 1, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.detector == 'Adaptive Multiple Regression'",
        selectInput("b_column", "Select a count or percent", c("count", "percent"), "count"),
        numericInput("b_B", "Number of days used to calculate rolling averages (B):", 28, min = 7),
        numericInput("b_g", "Number of guardband days (g):", 2, min = 1)
      ),
      conditionalPanel(
        condition = "input.detector == 'Serfling'",
        selectInput("s_scenario", "Select a scenario", unique(simulated_data$id), "Scenario #1"),
        dateInput("s_baseline_end", "End of the baseline/training period", value = as.Date("2021-12-26"), min = as.Date("2017-01-01"), max = as.Date("2022-12-25"))
      ),
      conditionalPanel(
        condition = "input.detector == 'Negative Binomial'",
        selectInput("nb_scenario", "Select a scenario", unique(simulated_data$id), "Scenario #1"),
        dateInput("nb_baseline_end", "End of the baseline/training period", value = as.Date("2021-12-26"), min = as.Date("2017-01-01"), max = as.Date("2022-12-25")),
        checkboxInput("includeTime", label = "Include Time Term?" , value = TRUE)
      ),
      conditionalPanel(
        condition = "input.detector == 'Farrington'",
        selectInput("f_scenario", "Select a scenario", unique(simulated_data$id), "Scenario #1"),
        radioButtons("method", "Farring Method:", c("Original" = "original", "Modified" = "modified")),
        numericInput("f_B", "Number of years to include in baseline (B):", 4, min = 1),
        numericInput("f_g", "Number of guardband weeks (g):", 27, min = 2),
        numericInput("f_w", "Number of weeks included in reference window (w):", 3, min = 1),
        numericInput("f_p", "Number of seasonal periods for each year in baseline (p):", 10, min = 1),
      ),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Plot",
          value = "plot",
          useShinyjs(),
          fluidRow(
            column(
              width = 12,
              withSpinner(plotOutput("outputPlot"))
            )
          )
        ),
        tabPanel(
          title = "Original Data",
          value = "originalData",
          useShinyjs(),
          fluidRow(
            column(
              width = 12,
              withSpinner(dataTableOutput("originalTable"))
            )
          )
        ),
        tabPanel(
          title = "Processed Data",
          value = "processedData",
          useShinyjs(),
          fluidRow(
            column(
              width = 12,
              withSpinner(dataTableOutput("processedTable"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  useShinyjs()
  
  data_list <- reactiveValues()
  
  # Get selected data
  selected_data <- reactive({
    if(input$detector %in% c("EWMA", "Adaptive Multiple Regression", "EWMA/Regression Switch")){
      date <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1)
      count <- pmax(0, round(seq(30, 70, length.out = length(date))) + rnorm(length(date), mean = 0, sd = 10))
      # count[count < 0] <- 0 # Ensure counts are non-negative
      percent <- pmax(0, pmin(1, seq(0.2, 0.8, length.out = length(date)) + rnorm(length(date), mean = 0, sd = 0.1)))
      data.frame(date = date, count = count, percent = percent)
    } else if(input$detector == "Serfling") {
      simulated_data %>% 
        filter(id == input$s_scenario)
    } else if(input$detector == "Negative Binomial") {
      simulated_data %>% 
        filter(id == input$nb_scenario)
    } else {
      simulated_data %>% 
        filter(id == input$f_scenario)
    }
  })
  
  # Send data to UI
  output$originalTable <- renderDT(
    selected_data() %>% 
      mutate_if(is.numeric, round, digits = 4),
    # extensions = c("Buttons", "KeyTable"),
    options = list(
      keys = FALSE,
      paging = FALSE,
      pageLength = 200,
      scrollX = TRUE,
      scrollY = "400px",
      searching = FALSE,
      # fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = "Bfrltip"
      # ,buttons = c("copy", "csv", "excel")
    )
  )
  
  
  processed_data <- reactive(
    switch(
      input$detector,
      "EWMA" = alert_ewma(
        selected_data(), 
        y = input$a_column, 
        B = input$a_B, 
        g = input$a_g, 
        w1 = input$a_w1, 
        w2 = input$a_w2
      ),
      "EWMA/Regression Switch" = alert_switch(
        selected_data(), 
        y = input$a_column, 
        B = input$a_B, 
        g = input$a_g, 
        w1 = input$a_w1, 
        w2 = input$a_w2
      ),
      "Adaptive Multiple Regression" = alert_regression(
        selected_data(), 
        y = input$b_column, 
        B = input$b_B, 
        g = input$b_g
      ),
      "Serfling" = alert_serfling(
        selected_data(),
        y = cases,
        baseline_end = input$s_baseline_end
      ),
      "Negative Binomial" = alert_nbinom(
        selected_data(),
        y = cases,
        include_time = input$includeTime,
        baseline_end = input$nb_baseline_end
      ),
      "Farrington" = alert_farrington(
        selected_data(), 
        y = cases, 
        B = input$f_B, 
        g = input$f_g, 
        w = input$f_w, 
        p = input$f_p, 
        method = input$method
      )
    )
  )
  
  output$processedTable <- renderDT(
    processed_data() %>% 
      mutate_if(is.numeric, round, digits = 4),
    # extensions = c("Buttons", "KeyTable"),
    options = list(
      keys = FALSE,
      paging = FALSE,
      pageLength = 200,
      scrollX = TRUE,
      scrollY = "400px",
      searching = FALSE,
      # fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = "Bfrltip"
      # ,buttons = c("copy", "csv", "excel")
    ) 
  )
  
  
  output$outputPlot <- renderPlot({
    switch(
      input$detector,
      "EWMA" = processed_data() %>% 
        ggplot() +
        geom_line(aes(x = date, y = !!as.symbol(input$a_column)), color = "grey70") +
        geom_line(
          data = subset(processed_data(), alert != "grey"),
          aes(x = date, y = !!as.symbol(input$a_column)), color = "navy"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "blue"),
          aes(x = date, y = !!as.symbol(input$a_column)), color = "navy"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "yellow"),
          aes(x = date, y = !!as.symbol(input$a_column)), color = "yellow"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "red"),
          aes(x = date, y = !!as.symbol(input$a_column)), color = "red"
        ) +
        theme_bw() +
        labs(
          x = "Date",
          y = str_to_title(input$a_column)
        ),
      "EWMA/Regression Switch" = processed_data() %>% 
        ggplot() +
        geom_line(aes(x = date, y = !!as.symbol(input$a_column)), color = "grey70") +
        geom_line(
          data = subset(processed_data(), alert != "grey"),
          aes(x = date, y = !!as.symbol(input$a_column)), color = "navy"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "blue"),
          aes(x = date, y = !!as.symbol(input$a_column)), color = "navy"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "yellow"),
          aes(x = date, y = !!as.symbol(input$a_column)), color = "yellow"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "red"),
          aes(x = date, y = !!as.symbol(input$a_column)), color = "red"
        ) +
        theme_bw() +
        labs(
          x = "Date",
          y = str_to_title(input$a_column)
        ),
      "Adaptive Multiple Regression" = processed_data() %>% 
        ggplot() +
        geom_line(aes(x = date, y = !!as.symbol(input$b_column)), color = "grey70") +
        geom_line(
          data = subset(processed_data(), alert != "grey"),
          aes(x = date, y = !!as.symbol(input$b_column)), color = "navy"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "blue"),
          aes(x = date, y = !!as.symbol(input$b_column)), color = "navy"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "yellow"),
          aes(x = date, y = !!as.symbol(input$b_column)), color = "yellow"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "red"),
          aes(x = date, y = !!as.symbol(input$b_column)), color = "red"
        ) +
        theme_bw() +
        labs(
          x = "Date",
          y = str_to_title(input$b_column)
        ),
      "Serfling" = processed_data() %>% 
        ggplot() +
        theme_classic() +
        geom_line(aes(x = date, y = cases), linewidth = 0.3) +
        geom_line(aes(x = date, y = estimate), color = "blue", linewidth = 0.3) +
        geom_line(aes(x = date, y = threshold), color = "red", linewidth = 0.3,
                  linetype = "dashed") +
        geom_point(data = subset(processed_data(), alarm), aes(x = date, y = cases),
                   color = "red", shape = 21, size = 2.5) +
        geom_vline(xintercept = input$s_baseline_end, linewidth = 0.2) +
        annotate(geom = "text", x = input$s_baseline_end - round((input$s_baseline_end - min(processed_data()$date))/2), y = round(max(processed_data()$cases) * 0.95), label = "Baseline Data") +
        scale_color_manual(
          values = c("blue", "red"),
          name = "Series"
        ) +
        scale_y_continuous(
          # limits = c(0, 80),
          # expand = c(0, 0),
          name = "Weekly Count"
        ) +
        scale_x_date(
          breaks = seq.Date(from = min(processed_data()$date), to = max(processed_data()$date), by = "4 month"),
          name = "MMWR Week Date"
        ) +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.ticks.length = unit(0.25, "cm")
        ) +
        theme(
          legend.position = "bottom",
          axis.ticks.length = unit(0.25, "cm"),
          legend.background = element_rect(color = "black", linewidth = 0.1)
        ) +
        labs(
          title = paste("Original Serfling Method Results for Simulated Time Series", gsub("Scenario ", "", input$s_scenario)),
          subtitle = "Annual seasonality with moderate counts"
        ),
      "Negative Binomial" = processed_data() %>% 
        ggplot() +
        theme_classic() +
        geom_line(aes(x = date, y = cases), linewidth = 0.3) +
        geom_line(aes(x = date, y = estimate), color = "blue", linewidth = 0.3) +
        geom_line(aes(x = date, y = threshold), color = "red", linewidth = 0.3,
                  linetype = "dashed") +
        geom_point(data = subset(processed_data(), alarm), aes(x = date, y = cases),
                   color = "red", shape = 21, size = 2.5) +
        geom_vline(xintercept = input$nb_baseline_end, linewidth = 0.2) +
        annotate(geom = "text", x = input$nb_baseline_end - round((input$nb_baseline_end - min(processed_data()$date))/2), y = round(max(processed_data()$cases) * 0.95), label = "Baseline Data") +
        scale_y_continuous(
          # limits = c(0, 80),
          # expand = c(0, 0),
          name = "Weekly Count"
        ) +
        scale_x_date(
          breaks = seq.Date(from = min(processed_data()$date), to = max(processed_data()$date), by = "4 month"),
          name = "MMWR Week Date"
        ) +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.ticks.length = unit(0.25, "cm")
        ) +
        labs(
          title = paste("Negative Binomial Regression Algorithm Results for Simulated Time Series", gsub("Scenario ", "", input$nb_scenario)),
          subtitle = "Annual seasonality with moderate counts"
        ),
      "Farrington" = processed_data() %>% 
        ggplot() +
        geom_line(aes(x = date, y = cases), linewidth = 0.4, color = "grey70") +
        geom_line(
          data = subset(processed_data(), alert != "grey"),
          aes(x = date, y = cases), color = "navy"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "blue"),
          aes(x = date, y = cases), color = "navy"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "yellow"),
          aes(x = date, y = cases), color = "yellow"
        ) +
        geom_point(
          data = subset(processed_data(), alert == "red"),
          aes(x = date, y = cases), color = "red"
        ) +
        theme_bw() +
        labs(
          x = "Date",
          y = "Weekly ED Visits"
        )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)  #%>% suppressWarnings()
