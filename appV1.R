# LEARNING LAB 38: INTRO TO MODELTIME ----
# BONUS - SHINY APP - FORECAST GOOGLE TRENDS

# LIBRARIES ----
library(shiny)
library(shinythemes)
library(gtrendsR)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(lubridate)
library(plotly)
library(reactable)
library(dplyr)
library(readr)

# Initial Inputs
search_term <- "data science"
location    <- "US"
time        <- "all"

# ---- 1.0 UI ----
ui <- navbarPage(
    title = "Forecast Google Trends",
    collapsible = TRUE,
    inverse     = TRUE, 
    theme       = shinytheme("darkly"),
    
    shiny::tabPanel(
        title = "Trendy",
        includeCSS("css/styles.css"),
        sidebarLayout(
            sidebarPanel(
                h3("What's trending now?"),
                HTML("<p>Companies can use Google Trends data to <em>Now-Cast</em>.
                     With <Code>Modeltime</code>, we can quickly make many <strong>Forecasting Models</strong> 
                     to forecast Google Trends Search Terms. 
                     We can <strong>automate the forecasting process</strong> using <code>R Shiny</code><p>"),
                hr(),
                shiny::textInput(
                    inputId = "search_term", 
                    label   = "Google Trends Search Term", 
                    value   = search_term
                ),
                shiny::textInput(inputId = "location", label = "Location", value = location),
                shiny::numericInput(inputId = "n_models", label = "Use Top N Models", value = 10, min = 1, max = 10),
                shiny::actionButton(inputId = "submit", "Submit", class = "btn-primary"),
                hr(),
                h5("Accuracy Table"),
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        # div(class = "panel-heading", h5("Average of Models")),
                        div(
                            class = "panel-body",
                            reactableOutput("reactable_accuracy")
                        )
                    )
                ),
                
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Average of Models")),
                        div(
                            class = "panel-body",
                            plotlyOutput("plotly_model_average")
                        )
                    )
                ),
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Sub-Model Forecasts")),
                        div(class = "panel-body", 
                            # verbatimTextOutput(outputId = "print")
                            plotlyOutput("plotly_submodel")
                        )
                    )
                ),
                
                # # Used for debugging
                # verbatimTextOutput(outputId = "code"),
                # div(
                #     class = "col-sm-12",
                #     leafletOutput(outputId = "leaflet", height = "100%")
                # )
                
                
            )
        )
        
    )
    
)

# ---- 2.0 SERVER ----
server <- function(session, input, output) {
    
    # Setup Reactive Values ----
    rv <- reactiveValues()
    
    observeEvent(input$submit, {
        
        # Process data
        rv$gtrends_list <- gtrendsR::gtrends(
            keyword = input$search_term, 
            geo     = input$location, 
            time    = time
        )
        
        rv$interest_over_time_tbl <- rv$gtrends_list %>%
            pluck("interest_over_time") %>%
            as_tibble() %>%
            select(date, hits) %>%
            mutate(hits = ifelse(hits == 0, NA, hits)) %>%
            mutate(hits = ts_clean_vec(hits, period = 12)) %>%
            rename(value = hits) %>%
            as_tibble()
        
        rv$interest_by_region_tbl <- rv$gtrends_list %>%
            pluck("interest_by_region") %>%
            as_tibble() %>%
            select(location, hits) %>%
            rename(value = hits) %>%
            as_tibble()
        
        # Models ----
        
        urlfile="https://raw.githubusercontent.com/deltick/MSDS692b/main/Data/USBPTS.csv"
        TestV1<-read_csv(url(urlfile))
        t1 <- dplyr::select(TestV1, xDate, "Southwest Border")
        t1 <- dplyr::rename(t1, date=xDate)
        t1 <- dplyr::rename(t1, value="Southwest Border")
        data_prepared_tbl <- t1
        
        # * Train/Test ----
        #splits <- time_series_split(rv$interest_over_time_tbl, assess = "1 year", cumulative = TRUE)
        splits <- time_series_split(t1, assess = "1 year", cumulative = TRUE)
        splits %>%
            tk_time_series_cv_plan() %>%
            plot_time_series_cv_plan(date, value) 
        
        # * ARIMA ----
        model_fit_arima <- arima_reg() %>%
            set_engine("auto_arima") %>%
            fit(
                value ~ date, 
                data = training(splits)
            )
        
        # * LINEAR REGRESSION ----
        model_fit_lm <- linear_reg() %>%
            set_engine("lm") %>%
            fit(
                value ~ as.numeric(date) + month(date, label = TRUE), 
                data = training(splits)
            )
        
        # # * LINEAR REGRESSION - NO TREND ----
        # model_fit_lm_no_trend <- linear_reg() %>%
        #     set_engine("lm") %>%
        #     fit(
        #         value ~ month(date, label = TRUE), 
        #         data = training(splits)
        #     )
        
        # * PROPHET ----
        model_fit_prophet <- prophet_reg() %>%
            set_engine("prophet") %>%
            fit(
                value ~ date, 
                data = training(splits)
            )
        
        # * RANDOM FOREST ----
        model_fit_rf <- rand_forest(mode = "regression") %>%
            set_engine("randomForest") %>%
            fit(
                value ~ as.numeric(date) + month(date, label = TRUE), 
                data = training(splits)
            )
        
        # * XGBOOST ----
        model_fit_xgboost <- boost_tree(mode = "regression") %>%
            set_engine("xgboost") %>%
            fit(
                value ~ as.numeric(date) + month(date, label = TRUE), 
                data = training(splits)
            )
        
        # * SVM - Polynomial ----
        model_fit_svm_poly <- svm_poly(mode = "regression") %>%
            set_engine("kernlab") %>%
            fit(
                value ~ as.numeric(date) + month(date, label = TRUE), 
                data = training(splits)
            )
        
        # * SVM - RBF ----
        model_fit_svm_rbf <- svm_rbf(mode = "regression") %>%
            set_engine("kernlab") %>%
            fit(
                value ~ as.numeric(date) + month(date, label = TRUE), 
                data = training(splits)
            )
        
        # * PROPHET BOOST ----
        model_fit_prophet_boost <- prophet_boost() %>%
            set_engine("prophet_xgboost") %>%
            fit(
                value ~ date + as.numeric(date) + month(date, label = TRUE), 
                data = training(splits)
            )
        
        # * ARIMA BOOST ----
        model_fit_arima_boost <- arima_boost() %>%
            set_engine("auto_arima_xgboost") %>%
            fit(
                value ~ date + as.numeric(date) + month(date, label = TRUE), 
                data = training(splits)
            )
        
        # Calibration -----
        rv$calibration_tbl <- modeltime_table(
            model_fit_arima,
            model_fit_lm,
            # model_fit_lm_no_trend,
            model_fit_prophet,
            model_fit_rf,
            model_fit_xgboost,
            model_fit_svm_poly,
            model_fit_svm_rbf,
            model_fit_prophet_boost,
            model_fit_arima_boost
        ) %>%
            modeltime_calibrate(testing(splits))
        
        top_n_model_id <- rv$calibration_tbl %>%
            modeltime_accuracy() %>%
            arrange(rmse) %>%
            slice(1:input$n_models)%>%
            pull(.model_id)
        
        # Refit ----
        rv$refit_tbl <- rv$calibration_tbl %>%
            modeltime_refit(data = t1 ) #rv$interest_over_time_tbl) 
        
        # Forecast Submodels ----
        rv$forecast_submodels_tbl <- rv$refit_tbl %>%
            modeltime_forecast(
                h = "1 year",
                actual_data = rv$interest_over_time_tbl,
                conf_interval = 0.80
            ) 
        
        # Forecast Average ----
        mean_forecast_tbl <- rv$forecast_submodels_tbl %>%
            filter(.key != "actual") %>%
            filter(.model_id %in% top_n_model_id) %>%
            group_by(.key, .index) %>%
            summarise(across(.value:.conf_hi, mean)) %>%
            mutate(
                .model_id   = 100,
                .model_desc = str_glue("AVERAGE OF TOP {input$n_models} MODELS")
            )
        
        rv$mean_forecast_tbl <- rv$forecast_submodels_tbl %>%
            filter(.key == "actual") %>%
            bind_rows(mean_forecast_tbl) 
        
        
    }, ignoreNULL = FALSE)
    
    # Debugging -----
    output$print <- renderPrint({
        
        req(rv$gtrends_list)
        
        list(
            # gtrends_list = rv$gtrends_list,
            # interest_over_time = rv$interest_over_time_tbl,
            # interest_by_region = rv$interest_by_region_tbl,
            # calibration_tbl    = rv$calibration_tbl,
            # refit_tbl          = rv$refit_tbl,
            forecast_tbl       = rv$forecast_submodels_tbl,
            mean_forecast_tbl  = rv$mean_forecast_tbl
            
        )
    })
    
    # Plotly Model Average ----
    output$plotly_model_average <- renderPlotly({
        
        req(rv$mean_forecast_tbl)
        
        rv$mean_forecast_tbl %>%
            plot_modeltime_forecast()

    })
    
    # Plotly Sub-Model ----
    output$plotly_submodel <- renderPlotly({
        
        req(rv$forecast_submodels_tbl)
        
        rv$forecast_submodels_tbl %>%
            plot_modeltime_forecast()
        
    })
    
    # Sub-Model Accuracy ----
    output$reactable_accuracy <- renderReactable({
        req(rv$calibration_tbl)
        
        rv$calibration_tbl %>%
            modeltime_accuracy() %>%
            table_modeltime_accuracy(resizable = TRUE, bordered = TRUE)
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
