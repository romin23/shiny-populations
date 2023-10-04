library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(forcats)
library(reshape)

server <- function(input, output) {
  db <- read.csv("nst-est2019-alldata.csv")
  head(db)
  
  region_column <- db$REGION
  name_column <- db$NAME
  census2010pop_column <- db$CENSUS2010POP
  popestimate2019_column <- db$POPESTIMATE2019
  population_change <- popestimate2019_column - census2010pop_column
  db$Proportion2019 <- (db$POPESTIMATE2019 / 328239523)*100
  
  df <- data.frame(
    Region = region_column,
    Name = name_column,
    Census2010Pop = census2010pop_column,
    PopEstimate2019 = popestimate2019_column,
    PopulationChange = population_change,
    Proportion2019 = db$Proportion2019,
    births_2010 = db$BIRTHS2010,
    births_2019 = db$BIRTHS2019,
    deaths_2010 = db$DEATHS2010,
    deaths_2019 = db$DEATHS2019
  )
  
  df <- df[-c(1:5), ]
  length(df$Name)
  df$Region <- factor(df$Region)
  # ===================================================================================
  #Maps
  df_map = data.frame(
    Region = db$REGION,
    Division = db$DIVISION,
    State = db$STATE,
    Name = tolower(db$NAME),
    Census2010Pop = census2010pop_column,
    PopEstimate2019 = popestimate2019_column,
    PopulationChange = population_change,
    percent_change = round((((db$POPESTIMATE2019 - db$CENSUS2010POP)/db$CENSUS2010POP)*100), 2),
    proportion_perc = round(db$Proportion2019,2)
  )
  df_map <- df_map[-c(1:5), ]
  
  
  # write.csv(df_map, "df_map.csv", )
  
  
  usa_tbl <- map_data("state") %>% tibble::as_tibble()
  pop_map <- usa_tbl %>% 
    left_join(df_map, by=c("region" = "Name"))
  green <- colorRampPalette(c("darkseagreen1","darkgreen"))(200)
  yellow <- colorRampPalette(c("yellow"))(200)  
  
  #Proportion Change
  # ==============================================================================================
  #Sweta
  
  df_s = df
  df_plot <- data.frame(
    NAME = factor(df_s$Name, levels = df_s$Name),  # Ensure the order of state names
    Year = rep(c("2010", "2019"), each = nrow(df_s)),
    Population_Millions = round(c(df_s$Census2010Pop / 1e6, df_s$PopEstimate2019 / 1e6), 2),
    Region = df_s$Region
  )
  
  df_plot$NAME <- fct_reorder(df_plot$NAME, df_plot$Region, .fun = function(x) min(as.numeric(x)))
  
  # Add the Region variable to the text aesthetic
  
  # ===============================================================================================
  # Krithi
  
  df_birth_death = data.frame(
    NAME = factor(df$Name, levels = df$Name),  # Ensure the order of state names
    births_2010 = df$births_2010,
    births_2019 = df$births_2019,
    deaths_2010 = df$deaths_2010,
    deaths_2019 = df$deaths_2019
  )
  df_long <- melt(df_birth_death, id.vars = "NAME", variable.name = "year")
  df_long <- separate(df_long, variable, into = c("event", "year"), sep = "_")
  df_long_cleaned <- df_long %>%
    mutate(value = ifelse(event == "deaths", -value, value))
  
  df_temp = data.frame(
    NAME = df$Name,
    Region = df$Region
  )
  
  df_long_cleaned <- df_long_cleaned %>% 
    left_join(df_temp, by="NAME")
  # Add the Region variable to the text aesthetic
  custom_palette <- scale_fill_manual(
    values = c(
      "Births_2010" = "blue",
      "Deaths_2010" = "red",
      "Births_2019" = "lightblue",
      "Deaths_2019" = "pink"
    )
  )
  
  custom_breaks <- seq(-300000, max(df_long_cleaned$value), by = 50000)
  
  
  
  # ===============================================================================================
  # Line Chart
  
  #df_line= db[2:17]
  #df_line = df_line[-c(1,2,3,6,7)]  
  #df_line = df_line[-c(1:5), ]
  #df_line_melt <- melt(df_line, id.vars = "NAME", variable.name = "year")
  #df_line_melt$year = gsub("[A-Za-z]", "", df_line_melt$year)
  #df_line_melt$value = round((df_line_melt$value / 1e6), 2) 
  #
  #
  #df_line_melt <- df_line_melt %>% 
  #  left_join(df_temp, by="NAME")
  #
  
  #===============================================================================================
  #===============================================================================================
  #===============================================================================================
  g_theme = theme_light()
  output$map <- renderPlotly({
    if (input$population_range == "All") {
      filtered_pop_map <- pop_map
    } else {
      region_map <- c("Northeast Region" = 1, "Midwest Region" = 2, "South Region" = 3, "West Region" = 4)
      selected_region <- region_map[input$population_range]
      filtered_pop_map <- pop_map[pop_map$Region == selected_region, ]
    }
    
    p <- ggplot(filtered_pop_map, aes(x = long, y = lat, group = group, text = paste("State: ", region))) +
      geom_polygon(aes(fill = percent_change), color = "black") +
      labs(
        title = "Percent Change in Population(2010-2019)",
        fill = "Population Percent Change"
      ) +
      scale_fill_gradientn(name = "Population\nPercent\nChange",
                           colours=c(yellow,"white", green), 
                           na.value = "grey50",
                           limits = c(-16,16))+ g_theme
    ggplotly(p)
  })
  
  
  output$map_chart <- renderPlotly({
    if (input$population_range == "All") {
      filtered_pop_map <- pop_map
    } else {
      region_map <- c("Northeast Region" = 1, "Midwest Region" = 2, "South Region" = 3, "West Region" = 4)
      selected_region <- region_map[input$population_range]
      filtered_pop_map <- pop_map[pop_map$Region == selected_region, ]
    }
    
    map_chart <- ggplot(filtered_pop_map, aes(x = long, y = lat, group = group, text = paste("State: ", region))) +
      geom_polygon(aes(fill = proportion_perc), color = "black") +
      labs(
        title = "Proportion Change in Population (2010-2019)",
        fill = "Proportion Change"
      ) +
      scale_fill_gradient(
        name = "Proportion\nChange",
        low = "lightcoral",  # Adjust the low and high colors as needed
        high = "darkred",
        na.value = "grey50"
      )+ g_theme
    
    ggplotly(map_chart)
  })
  
  output$bar_chart <- renderPlotly({
    if (input$population_range == "All") {
      filtered_df_plot <- df_plot
    } else {
      region_map <- c("Northeast Region" = 1, "Midwest Region" = 2, "South Region" = 3, "West Region" = 4)
      selected_region <- region_map[input$population_range]
      filtered_df_plot <- df_plot[df_plot$Region == selected_region, ]
    }
    
    bar_chart <- ggplot(filtered_df_plot, aes(x = NAME, y = Population_Millions, fill = Year, text = paste("Region: ", Region))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_manual(values = c("2010" = "green", "2019" = "red")) +
      labs(
        title = "2010 vs. 2019 Population Comparison by State",
        x = "State",
        y = "Population (Millions)"
      ) +
      g_theme +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    ggplotly(bar_chart)
  })
  
  output$birth_chart <- renderPlotly({
    if (input$population_range == "All") {
      filtered_birth_map <- df_long_cleaned
    } else {
      region_map <- c("Northeast Region" = 1, "Midwest Region" = 2, "South Region" = 3, "West Region" = 4)
      selected_region <- region_map[input$population_range]
      filtered_birth_map <- df_long_cleaned[df_long_cleaned$Region == selected_region, ]
    }
    
    plot1 <- ggplot(filtered_birth_map, aes(x = NAME, y = value, fill = paste0(ifelse(value >= 0, "Births_", "Deaths_"), year))) +
      geom_bar(stat = "identity", position = "dodge") +
      custom_palette +
      labs(title = "Comparison of Birth and Death Rates (2010 vs. 2019)",
           x = "Sate Names",
           y = "Count",
           fill = "Year") +
      scale_y_continuous(breaks = custom_breaks) +
      g_theme +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    # Print the plot
    
    ggplotly(q=plot1)
  })
}

shinyApp(ui, server)