library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(forcats)
library(reshape)

ui <- grid_page(
  layout = c(
    "header  area4 area4 ",
    "sidebar area4 area4 ",
    "table   table plotly",
    "table   table plotly"
  ),
  row_sizes = c(
    "105px",
    "1.41fr",
    "0.59fr",
    "1fr"
  ),
  col_sizes = c(
    "210px",
    "0.81fr",
    "1.19fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_body(
      selectInput(
        inputId = "population_range",
        label = "Select Input",
        choices = list(
          "All" = "All",
          "Northeast Region" = "Northeast Region",
          "Midwest Region" = "Midwest Region",
          "South Region" = "South Region",
          "West Region" = "West Region"
        )
      )
    ),
    card_body()
  ),
  grid_card_text(
    area = "header",
    content = "Mid Project Grp. 6",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "table",
    card_body(plotlyOutput(outputId = "map"))
  ),
  grid_card(
    area = "plotly",
    card_body(
      plotlyOutput(
        outputId = "map_chart",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "area4",
    card_body(
      tabsetPanel(
        tabPanel(
          title = "Population",
          plotlyOutput(
            outputId = "bar_chart",
            width = "100%",
            height = "100%"
          )
        ),
        tabPanel(
          title = "Birth/Deaths",
          plotlyOutput(
            outputId = "birth_chart",
            width = "100%",
            height = "100%"
          )
        )
      )
    )
  )
)