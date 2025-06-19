library(tidyverse)
library(jsonlite)
library(shiny)
library(scales)
library(ggtext)


##### Blyn 48 hour forecast app ####

# Wind rose function to convert wind direction degrees to compass points
wind.rose <- function(x) {
  upper <- seq(from = 11.25, by = 22.5, length.out = 17)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')
  ifelse(x>360 | x<0,NA,card1[findInterval(x,upper,rightmost.closed = T)+1])
}

# Force local time zone
Sys.setenv(TZ="America/Los_Angeles")

#  Call API and decode JSON
url <- "https://api.openweathermap.org/data/3.0/onecall?lat=48.02307&lon=-122.99969&exclude=minutely&units=imperial&appid=8d5cf85099c375dcad074eff91b0d5d9"
weather.page <- fromJSON(url, flatten = TRUE)

# Strip and format hourly data
hourly.forecast <- data.frame(weather.page$hourly) %>%
  mutate(dt = as.POSIXct(dt, origin="1970-01-01")) %>%
  mutate_at(vars(wind_speed, wind_gust), ~ . * 0.868976)

# Strip and format current data
current <- data.frame(weather.page$current) %>%
  mutate_at(vars(dt, sunrise, sunset), ~ as.POSIXct(., origin="1970-01-01")) %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_deg = as.integer(wind_deg)) %>%
  mutate(mod_deg = case_when(wind_deg > 352 && wind_deg < 356 ~ 352L,
                             wind_deg >= 356 && wind_deg <= 360 ~ 0L,
                             TRUE ~ wind_deg))

# Create night-time shade limits
shade <- data.frame(dusk = seq.POSIXt(current$sunset, by = 'day', length.out = 3), 
                    dawn = seq.POSIXt(current$sunrise+86400, by = 'day', length.out = 3),
                    top = Inf,
                    bottom = -Inf)

shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(hourly.forecast$dt, 1)), tail(hourly.forecast$dt, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(hourly.forecast$dt, 1)), head(hourly.forecast$dt, 1)))
  
# Wind rose plot
rose <- ggplot(current, aes(x = mod_deg)) +
  coord_polar(theta = "x", start = -pi/45, direction = 1) +
  geom_bar(width = 7, color = "gray10", fill = "red") +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())

# Wind direction plot
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
  scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))

# Barometric pressure plot
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
  xlab("") +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))

# Wind speed plot
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
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))

# Rain plot
if ("rain.1h" %in% colnames(hourly.forecast)) {
  
rain.plot <- ggplot() +
  geom_rect(data = shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = hourly.forecast, aes(x = dt, y = pop), size = 1) +
  geom_col(data = hourly.forecast, aes(x = dt, y = rain.1h/5), color = "darkgrey", fill = "#28d0eb") +
  geom_text(data = hourly.forecast, aes(x = dt, y = rain.1h/5, label = rain.1h), size = 2, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm)") +
  theme(plot.title = element_markdown()) +
  ylab("Percent") + 
  xlab("") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))

} else (
rain.plot <- ggplot() +
  geom_rect(data = shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = hourly.forecast, aes(x = dt, y = pop), size = 1) +
  theme_bw() +
  labs(
    title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm)") +
  theme(plot.title = element_markdown()) +
  ylab("Percent") + 
  xlab("") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))
)

# Shiny UI
ui <- fluidPage(
  h5("WX Monitor", align = "center"),
  h3("Blyn 48 hour forecast", align = "center"),
  h4(textOutput("time.current"), align = "center"),
  h4(textOutput("weather.label"), align = "center"),
  
    fluidRow(column(12, align = "center",
    plotOutput(outputId = "rose", width = "50%", height = "200px"))),
    plotOutput(outputId = "weather.plot", width = "100%", height = "400px"),
    plotOutput(outputId = "dir.plot", width = "100%", height = "400px"),
    plotOutput(outputId = "rain.plot", width = "100%", height = "400px"),
    plotOutput(outputId = "bar.plot", width = "100%", height = "400px")
)


# Shiny server
server <- function(input, output) {
  
  # Current weather label output
  output$weather.label <- renderText({
    paste0(wind.rose(current$wind_deg), " ",
           round(current$wind_speed, 0), " knots ",
           "(", current$wind_deg, "°)")
    
  }) 
  
  # Wind plot output
  output$weather.plot <- renderPlot({
    weather.plot
  }) 
  
  # Current time label output
  output$time.current <- renderText({
    paste("",format(Sys.time(), "%a %m-%d %H:%M"))
  })
  
  # Wind direction plot output
  output$dir.plot <- renderPlot({
    dir.plot
  })
  
  # Rain plot output
  output$rain.plot <- renderPlot({
    rain.plot
  })
  
  # Barometer plot output
  output$bar.plot <- renderPlot({
    bar.plot
  }) 
  
  # Wind rose output
  output$rose <- renderPlot({
    rose
  }) 
  
}


# Execute app
shinyApp(ui, server)







