#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(fpp2)
library(forecast)
library(ggplot2)
library(plotly)
library(ggplotify)
library(lubridate)
library(DT)
library(scales)
library(dplyr)

# App to track covid-19 cases and deaths in the uk

data <- read.csv("covid_19_countries_cases.csv", header = TRUE)
daily <- read.csv("covid_19_daily.csv", header = TRUE)
uk_deaths <- read.csv("covid_19_deaths.csv", header = TRUE)
cumulative <- read.csv("covid19_cases_cumulative.csv", header = TRUE)
cumulative_d <- read.csv("covid19_cumulative_deaths.csv", header = TRUE)

daily[,1] <- as.Date(daily[,1], format = "%d/%m/%Y")
uk_deaths[,1] <- as.Date(uk_deaths[,1], format = "%d/%m/%Y")


growth_d = cumulative_d[,c(1,3,4)]
growth_d[,1] <- as.Date(growth_d[,1], format = "%d/%m/%Y")
growth_d <- growth_d[2:length(t(growth_d[,1])),]


uk_d <- ts(uk_deaths[,2], start = decimal_date(as.Date("2020-03-09")), frequency = 365)
uk <- ts(data[,6], start = decimal_date(as.Date("2020-03-09")), frequency = 365)
uk_2 <- data[,c(1,6)]
uk_2[,1] <- as.Date(uk_2[,1], format = "%d/%m/%Y")
england <- ts(data[,2], start = decimal_date(as.Date("2020-03-09")), frequency = 365)
scotland <- ts(data[,3], start = decimal_date(as.Date("2020-03-09")), frequency = 365)
wales <- ts(data[,4], start = decimal_date(as.Date("2020-03-09")), frequency = 365)
ni <- ts(data[,5], start = decimal_date(as.Date("2020-03-09")), frequency = 365)
choices = c("UK", "England", "Scotland", "Wales", "N. Ireland")
choices2 = c("Cases", "Deaths")
choices3 = c("Cases", "Deaths*")
grow = cumulative[,c(1,3,4)]
grow[,1] <- as.Date(grow[,1], format = "%d/%m/%Y")
grow <- grow[2:length(t(grow[,1])),]
na.pad <- function(x,len){
    x[1:len]
}

makePaddedDataFrame <- function(l,...){
    maxlen <- max(sapply(l,length))
    data.frame(lapply(l,na.pad,len=maxlen),...)
}

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                chooseSliderSkin(color = "#009E73"),
        navbarPage(
            "COVID-19 Case tracker",
            # theme = "cerulean",  # <--- To use a theme, uncomment this
                tabPanel("Cases/Deaths Data",

                         sidebarPanel(
                             selectInput("tab1cd", "Choose Cases/deaths", choices = choices2),
                             submitButton("Apply"),
                             br(),
                             p("All data in this dashboard is sourced from Public Health England."),
                             br(),
                             p("More graphs will be added in future along with more data sources. 
                               New graphs will include a map of the UK to explore how the virus has spread.
                               The dashboard will also be updated daily so tomorrow will have different, 
                               more accurate results"),
                             br(),
                             p("The Second Chart on this page shows a Logarithmic scaled version of the first. 
                               These kinds of charts are useful as we are able to see the exponential growth as 
                               a straight line rather than a curve. A steeper straight line shows faster growth.
                               We can also use these charts to estimate how quickly cases/deaths will jump an order of magnitude,
                               e.g. from 100 cases to 1000 cases as the scale is not linear."),
                             br(),
                             p("For the growth rate charts give us an idea of how quickly the virus is spreading and is
                               calculated by Number of confirmed cases today divided by the number of confirmed cases
                               yesterday. 1 is the baseline so if we find that growth rate is below 1 then the rate of spread
                               is actually slowing."),
                             p("If you see any bugs or have any suggestions or questions please contact me via my twitter:"),
                             strong("@JoshwaBail"),
                             br()

                             ),
                
                         mainPanel(
                             h4("Daily UK Cases/Deaths"),
                             plotlyOutput("uk"),
                             #plotOutput("ukdeaths"),
                             #plotOutput("ukdailycases"),
                             #plotOutput("ukdailydeaths"),
                             h4("Daily UK Cases/Deaths with Logarithmic Scale"),
                             plotlyOutput("uklog"),
                             h4("Growth Rate of Cases/Deaths"),
                             plotlyOutput("growth")
                             #plotOutput("ukdeathslog"),
                             
                         )
                         ),
                tabPanel("Forecasts by Country",
                        sidebarPanel(
                            selectInput("country", "Choose Country/Region", choices = choices),
                            sliderInput("horizon", 
                                        "Number of days to Forecast",
                                        min = 1,
                                        max = 30,
                                        value = 14),
                            selectInput("cases", "Choose Cases/Deaths", choices = choices3),
                            submitButton("Apply"),
                            br(),
                            strong("* When forecasting new deaths we show only the predictions for the whole of the UK."),
                        ),
                        mainPanel(
                            h4("Predicting Future Daily UK Cases/Deaths"),
                            plotlyOutput("forecast"),
                            dataTableOutput("table")
                         )
                        )
            #tabPanel("NHS Region"),
            #tabPanel("UTLAs")
        )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    getDataset <- reactive({
        if (input$cases == "Cases"){
            if (input$country=="UK"){
                return(uk)
            }
            else if (input$country=="England")
            {
                return(england)
            }
            else if (input$country=="Scotland")
            {
                return(scotland)
            }
            else if (input$country=="Wales")
            {
                return(wales)
            }
            else
            {
                return(ni)
        }} else{
            return(uk_d)
        }
    })
    
    output$forecast <- renderPlotly({
        # 80% conf int
        fit <- auto.arima(getDataset())
        forecast_var <- forecast(fit, h=input$horizon)
        # 80% conf int
        lower80 <- round(as.numeric(forecast_var$lower[,1]))
        max_val = max(lower80)
        lower80[which.max(lower80):nrow(as.data.frame(lower80))] <- max_val
        
        upper80 <- round(as.numeric(forecast_var$upper[,1]))
        
        # 95% conf int
        lower95 <- round(as.numeric(forecast_var$lower[,2]))
        max_val = max(lower95)
        lower95[which.max(lower95):nrow(as.data.frame(lower95))] <- max_val
        
        upper95 <- round(as.numeric(forecast_var$upper[,2]))
        
        # values
        daterange <- as.Date(data[,1], format = "%d/%m/%Y")
        values <- as.numeric(forecast_var$x)
        new_df <- as.data.frame(list(daterange, values), col.names = c("date", "number of cases"))
        plot(new_df)
        
        forecast_data <- as.numeric(forecast_var$mean)
        forecast_dates <- format(seq(as.Date("2020-04-12"), as.Date("2020-05-20"), by="days"), format="%Y/%m/%d")
        
        new_forecast_dataframe <- cbind(forecast_dates,forecast_data)
        new_forecast_dataframe <- new_forecast_dataframe[1:length(forecast_data),]
        new_forecast_dataframe <- as.data.frame(new_forecast_dataframe)
        colnames(new_forecast_dataframe) <- c("date", "number.of.cases")
        new_forecast_dataframe[,1] <- as.Date(new_forecast_dataframe[,1], format = "%Y/%m/%d")
        new_forecast_dataframe[,2] <- as.numeric(as.character(new_forecast_dataframe[,2]))
        
        
        new_forecast_dataframe_lower80 <- cbind(forecast_dates,lower80)
        new_forecast_dataframe_lower80 <- new_forecast_dataframe_lower80[1:length(forecast_data),]
        new_forecast_dataframe_lower80 <- as.data.frame(new_forecast_dataframe_lower80)
        colnames(new_forecast_dataframe_lower80) <- c("date", "lower80")
        new_forecast_dataframe_lower80[,1] <- as.Date(new_forecast_dataframe_lower80[,1], format = "%Y/%m/%d")
        new_forecast_dataframe_lower80[,2] <- as.numeric(as.character(new_forecast_dataframe_lower80[,2]))
        
        new_forecast_dataframe_upper80 <- cbind(forecast_dates,upper80)
        new_forecast_dataframe_upper80 <- new_forecast_dataframe_upper80[1:length(forecast_data),]
        new_forecast_dataframe_upper80 <- as.data.frame(new_forecast_dataframe_upper80)
        colnames(new_forecast_dataframe_upper80) <- c("date", "upper80")
        new_forecast_dataframe_upper80[,1] <- as.Date(new_forecast_dataframe_upper80[,1], format = "%Y/%m/%d")
        new_forecast_dataframe_upper80[,2] <- as.numeric(as.character(new_forecast_dataframe_upper80[,2]))
        
        new_forecast_dataframe_lower95 <- cbind(forecast_dates,lower95)
        new_forecast_dataframe_lower95 <- new_forecast_dataframe_lower95[1:length(forecast_data),]
        new_forecast_dataframe_lower95 <- as.data.frame(new_forecast_dataframe_lower95)
        colnames(new_forecast_dataframe_lower95) <- c("date", "lower95")
        new_forecast_dataframe_lower95[,1] <- as.Date(new_forecast_dataframe_lower95[,1], format = "%Y/%m/%d")
        new_forecast_dataframe_lower95[,2] <- as.numeric(as.character(new_forecast_dataframe_lower95[,2]))
        
        new_forecast_dataframe_upper95 <- cbind(forecast_dates,upper95)
        new_forecast_dataframe_upper95 <- new_forecast_dataframe_upper95[1:length(forecast_data),]
        new_forecast_dataframe_upper95 <- as.data.frame(new_forecast_dataframe_upper95)
        colnames(new_forecast_dataframe_upper95) <- c("date", "upper95")
        new_forecast_dataframe_upper95[,1] <- as.Date(new_forecast_dataframe_upper95[,1], format = "%Y/%m/%d")
        new_forecast_dataframe_upper95[,2] <- as.numeric(as.character(new_forecast_dataframe_upper95[,2]))
        
        full <- rbind(new_df, new_forecast_dataframe)
        full[,1] <- as.Date(full[,1], format = "%d/%m/%Y")
        
        full_1 <- left_join(full, new_forecast_dataframe_lower80 , by = "date")
        full_2 <- left_join(full_1, new_forecast_dataframe_upper80 , by = "date")
        full_3 <- left_join(full_2, new_forecast_dataframe_lower95 , by = "date")
        full_4 <- left_join(full_3, new_forecast_dataframe_upper95 , by = "date")
        full_4 <- as.data.frame(full_4)
        
        # Original data with Forecated data here, now to add in the rest of the data for the forecast
        colors <- c("Actual Values" = "grey45", 
                    "Predicted Values" = "#009E73", 
                    "80% confidence interval" = "grey45",
                    "95% confidence interval" = "grey45")
        p <- ggplot(full_4, x = date) + 
            geom_line(data = full_4[1:length(uk),c(1,2)], aes(x=date, y=number.of.cases, color='Actual Values')) +
            geom_line(data = full_4[length(uk)+1:nrow(full_4), c(1,2)], aes(x=date, y=number.of.cases, color='Predicted Values'))+
            geom_line(aes(x=date, y=upper80, color='80% confidence interval'), alpha = 0.2) +
            geom_line(aes(x=date, y=lower80, color='80% confidence interval'), alpha = 0.2) +
            geom_line(aes(x=date, y=upper80, color='95% confidence interval'), alpha = 0.1) +
            geom_line(aes(x=date, y=lower80, color='95% confidence interval'), alpha = 0.1) +
            geom_ribbon(aes(x = date, ymin=lower80, ymax=upper80), alpha=0.2) +
            geom_ribbon(aes(x = date, ymin=lower95, ymax=upper95), alpha=0.1) +
            theme(axis.title = element_blank(), axis.text.x = element_text(), axis.text.y = element_text())+
            labs(x = "Date",
                 y = "Number of Cases",
                 color = "Legend") +
            scale_color_manual(values = colors) +
            scale_x_date(labels = date_format("%d-%m"))
        ggplotly(p)
    })
    
    output$table <- renderDataTable({
        fit <- auto.arima(getDataset())
        forecast_var <- forecast(fit, h=input$horizon)
        df <- round(as.data.frame(forecast_var))
        rownames(df) <- 1:nrow(df)
        forecast_dates <- format(seq(as.Date("2020-04-12"), as.Date("2020-05-20"), by="days"), format="%Y/%m/%d")
        df <- cbind(forecast_dates[1:nrow(df)], df)
        df <- as.data.frame(df)
        rownames(df) <- df[,1]
        df <- df[,c(-1)]
        df[which.max(df[,4]):nrow(as.data.frame(df)),4] <- max(df[,4])
        df[which.max(df[,2]):nrow(as.data.frame(df)),2] <- max(df[,2])
        datatable(df)
    })
    output$uk <- renderPlotly({
        if(input$tab1cd == "Cases"){
            p <- ggplot(uk_2) + 
                geom_line(aes(x=Date, y=UK), color='#009E73') +
                geom_point(aes(x=Date, y=UK), color='#009E73') +
                ylab("Number of Cases")+
                theme(axis.title = element_text(), axis.text.x = element_text(), axis.text.y = element_text())+
                scale_x_date(labels = date_format("%d-%m"))
            ggplotly(p)
        } else {
            p <- ggplot(uk_deaths) + 
                geom_line(aes(x=Date, y=UK), color='#009E73') +
                geom_point(aes(x=Date, y=UK), color='#009E73') +
                ylab("Number of Deaths")+
                theme(axis.title = element_text(), axis.text.x = element_text(), axis.text.y = element_text())+
                scale_x_date(labels = date_format("%d-%m"))
            ggplotly(p)
        }
        
    })
    output$growth <- renderPlotly({
        if(input$tab1cd == "Cases"){
            p <- ggplot(grow) +
                geom_line(aes(x = Date, y = growth), color = '#009E73') +
                geom_line(aes(x = Date, y = base), color = 'firebrick3', linetype = "twodash") +
                theme(axis.title = element_text(), axis.text.x = element_text(), axis.text.y = element_text())+
                scale_x_date(labels = date_format("%d-%m"))+
                ylab("Growth Rate - Cases")
            ggplotly(p)
        } else{
            p <- ggplot(growth_d) +
            geom_line(aes(x = Date, y = growth), color = '#009E73') +
            geom_line(aes(x = Date, y = base), color = 'firebrick3', linetype = "twodash") +
            theme(axis.title = element_text(), axis.text.x = element_text(), axis.text.y = element_text())+
            scale_x_date(labels = date_format("%d-%m"))+
            ylab("Growth Rate - Deaths")
        ggplotly(p)
            
        }
    })
    
    
    
    output$uklog <- renderPlotly({
        if(input$tab1cd == "Cases"){
            p <- ggplot(uk_2) + 
                geom_line(aes(x=Date, y=UK), color='firebrick3') +
                geom_point(aes(x=Date, y=UK), color='firebrick3') +
                ylab("Number of Cases")+
                theme(axis.title = element_text(), axis.text.x = element_text(), axis.text.y = element_text())+
                scale_x_date(labels = date_format("%d-%m"))+
                scale_y_continuous(trans = "log", breaks = 10**(1:10))
            ggplotly(p)
        } else {
            p <- ggplot(uk_deaths) + 
                geom_line(aes(x=Date, y=UK), color='firebrick3') +
                geom_point(aes(x=Date, y=UK), color='firebrick3') +
                ylab("Number of Deaths")+
                theme(axis.title = element_text(), axis.text.x = element_text(), axis.text.y = element_text())+
                scale_x_date(labels = date_format("%d-%m"))+
                scale_y_continuous(trans = "log", breaks = 10**(1:10))
            ggplotly(p)
        }
        
    })
    
    
}
# Run the application 
shinyApp(ui = ui, server = server)
