data <- read.csv("covid_19_countries_cases.csv", header = TRUE)
uk <- ts(data[,6], start = decimal_date(as.Date("2020-03-09", format = "%Y-%m-%d")), frequency = 365)
uengland <- ts(data[,2], start = 1, frequency = 1)
scotland <- ts(data[,3], start = 1, frequency = 1)
wales <- ts(data[,4], start = 1, frequency = 1)
ni <- ts(data[,5], start = 1, frequency = 1)

fit <- auto.arima(uk)
forecast <- forecast(fit, h=5)
# 80% conf int
lower80 <- as.numeric(forecast$lower[,1])
lower80
upper80 <- as.numeric(forecast$upper[,1])

# 95% conf int
lower95 <- as.numeric(forecast$lower[,2])
upper95 <- as.numeric(forecast$upper[,2])

# values
daterange <- as.Date(data[,1], format = "%d/%m/%Y")
values <- as.numeric(forecast$x)
new_df <- as.data.frame(list(daterange, values), col.names = c("date", "number of cases"))
plot(new_df)

forecast_data <- as.numeric(forecast$mean)
forecast_dates <- format(seq(as.Date("2020-04-07"), as.Date("2020-05-20"), by="days"), format="%Y/%m/%d")


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
  geom_line(data = full_4[1:29, c(1,2)], aes(x=date, y=number.of.cases, color='Actual Values')) +
  geom_line(data = full_4[30:nrow(full_4), c(1,2)], aes(x=date, y=number.of.cases, color='Predicted Values'))+
  geom_line(aes(x=date, y=upper80, color='80% confidence interval'), alpha = 0.2)+
  geom_line(aes(x=date, y=lower80, color='80% confidence interval'), alpha = 0.2)+
  geom_line(aes(x=date, y=upper80, color='95% confidence interval'), alpha = 0.1)+
  geom_line(aes(x=date, y=lower80, color='95% confidence interval'), alpha = 0.1)+
  geom_ribbon(data = full_4, aes(x = as.Date(date), ymin=lower80, ymax=upper80), alpha=0.2) +
  geom_ribbon(data = full_4, aes(x = as.Date(date), ymin=lower95, ymax=upper95), alpha=0.1) +
  theme(axis.title = element_text(), axis.text.x = element_text(), axis.text.y = element_text())+
  labs(x = "Date",
       y = "Number of Cases",
       title = "Forecasted Daily Cumumlative Cases",
       color = "Legend") +
  scale_color_manual(values = colors) 
ggplotly(p)



write.csv(full_4, "covid_19_data.csv")


