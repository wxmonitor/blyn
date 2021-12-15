library(tidyverse)
library(jsonlite)
library(shiny)
library(scales)
library(ggtext)
library(curl)


# Blyn 48 hour forecast

wind.rose <- function(x) {
  upper <- seq(from = 11.25, by = 22.5, length.out = 17)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')
  ifelse(x>360 | x<0,NA,card1[findInterval(x,upper,rightmost.closed = T)+1])
}

Sys.setenv(TZ="America/Los_Angeles")

url <- "https://api.openweathermap.org/data/2.5/onecall?lat=48.02307&lon=-122.999698&exclude=minutely&units=imperial&appid=8d5cf85099c375dcad074eff91b0d5d9"
weather.page <- fromJSON(url, flatten = TRUE)
hourly.forecast <- data.frame(weather.page$hourly)
hourly.forecast$dt <- as.POSIXct(hourly.forecast$dt, origin="1970-01-01")
current <- data.frame(weather.page$current)
current$dt <- as.POSIXct(current$dt, origin="1970-01-01")
current$sunrise <- as.POSIXct(current$sunrise, origin="1970-01-01")
current$sunset <- as.POSIXct(current$sunset, origin="1970-01-01")

hourly.forecast <- hourly.forecast %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_gust = wind_gust * 0.868976)

current <- current %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_gust = wind_gust * 0.868976)

shade <- data.frame(dusk = seq.POSIXt(current$sunset, by = 'day', length.out = 3), 
                    dawn = seq.POSIXt(current$sunrise+86400, by = 'day', length.out = 3),
                    top = Inf,
                    bottom = -Inf)

shade <- shade %>% 
  mutate_at(vars(dusk, dawn), ~ replace(., which(. > tail(hourly.forecast$dt, 1)), NA))
  

dir.plot <- ggplot() +
  geom_rect(data = shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_point(data = hourly.forecast, aes(x = dt, y = wind.rose(wind_deg)), size = 1) +
  theme_bw() +
  labs(title = "**Wind Direction**") +
  theme(plot.title = element_markdown()) +
  ylab("") +
  xlab("") +
  scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N'))

bar.plot <- ggplot() + 
  geom_rect(data = shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = hourly.forecast, aes(x = dt, y = pressure), size = 1) +
  geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
  theme_bw() +
  labs(title = "**Barometric Pressure**") +
  theme(plot.title = element_markdown()) +
  ylab("Millibars") +
  xlab("")  


weather.plot <- ggplot() +
  geom_rect(data = shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = hourly.forecast, aes(x = dt, y = wind_speed), size = 1) +
  geom_line(data = hourly.forecast, aes(x = dt, y = wind_gust), color = "#FF0000") +
  theme_bw() +
  labs(
    title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
  theme(plot.title = element_markdown()) +
  ylab("Knots") +
  xlab("") + 
  xlim(c(min(hourly.forecast$dt), max(hourly.forecast$dt)))


rain.plot <- ggplot() +
  geom_rect(data = shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = hourly.forecast, aes(x = dt, y = pop), size = 1) +
  geom_col(data = hourly.forecast, aes(x = dt, y = rain.1h/5), color = "darkgrey", fill = "#28d0eb") +
  geom_text(data = hourly.forecast, aes(x = dt, y = rain.1h/5, label = rain.1h), size = 2.5, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm/hr)") +
  theme(plot.title = element_markdown()) +
  ylab("Percent") + 
  xlab("") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1))


# Construct UI
ui <- fluidPage(
  h1("Blyn Weather", align = "center"),
  h3("48 hour forecast", align = "center"),
  h4(textOutput("time.current"), align = "center"),
  
  mainPanel(
    plotOutput(outputId = "weather.plot", width = "100%", height = "400px"),
    plotOutput(outputId = "dir.plot", width = "100%", height = "400px"),
    plotOutput(outputId = "rain.plot", width = "100%", height = "400px"),
    plotOutput(outputId = "bar.plot", width = "100%", height = "400px")
  )
)


#Run r code on server
server <- function(input, output) {
  
  output$weather.plot <- renderPlot({
    weather.plot
  }) 
  
  output$time.current <- renderText({
    paste("",Sys.time())
  })
  
  
  output$dir.plot <- renderPlot({
    dir.plot
  })
  
  
  output$rain.plot <- renderPlot({
    rain.plot
  })
  
  
  output$bar.plot <- renderPlot({
    bar.plot
  }) 
  
}


# Execute app
shinyApp(ui, server)







