library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(bslib)


netflix <- readr::read_csv("netflix_titles.csv", show_col_types = FALSE) %>%
  mutate(
    date_added = suppressWarnings(mdy(date_added)),
    added_year = year(date_added),
    type = if_else(is.na(type) | type == "", "Unknown", type),
    rating = if_else(is.na(rating) | rating == "", "Unknown", rating),
    country = na_if(country, ""),
    listed_in = na_if(listed_in, "")
  ) %>%
  filter(!is.na(added_year))

year_min <- min(netflix$added_year)
year_max <- max(netflix$added_year)

type_levels <- netflix %>% distinct(type) %>% arrange(type) %>% pull(type)
rating_levels <- netflix %>% distinct(rating) %>% arrange(rating) %>% pull(rating)


NETFLIX_RED <- "#E50914"
DARK <- "#111111"
MID  <- "#333333"

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = NETFLIX_RED,
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

plot_theme <- function() {
  theme_minimal(base_size = 16) +   
    theme(
      plot.title = element_text(face = "bold", size = 18, color = DARK),
      plot.subtitle = element_text(size = 14, color = MID),
      plot.caption = element_text(size = 11, color = MID),
      axis.title = element_text(size = 15, color = DARK),
      axis.text = element_text(size = 13, color = MID),
      axis.text.y = element_text(size = 13),
      legend.text = element_text(size = 13),
      legend.position = "right",
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 20, 8, 8)
    )
}

focus_text <- function(year_range, brush) {
  if (is.null(brush)) paste0("Focus: ", year_range[1], "–", year_range[2], " (use brush to zoom)")
  else paste0("Focus: ", floor(brush$xmin), "–", ceiling(brush$xmax), " (from brush)")
}

# UI
ui <- page_sidebar(
  theme = app_theme,
  title = "Netflix Catalog Evolution: Growth, Geography, and Genres",
  fillable = FALSE,
  sidebar = sidebar(
    width = 360,
    h5("Controls"),
    sliderInput("year_range", "Year added (range):",
                min = year_min, max = year_max,
                value = c(max(year_min, year_max - 7), year_max),
                step = 1, sep = ""
    ),
    selectInput("type", "Title type:", choices = c("All", type_levels), selected = "All"),
    selectizeInput("rating", "Rating (optional):",
                   choices = rating_levels, selected = NULL, multiple = TRUE,
                   options = list(placeholder = "All ratings")
    ),
    sliderInput("top_n", "Top N (countries / genres):", min = 5, max = 25, value = 12, step = 1),
    hr(),
    p(strong("Tip:"), "Brush (drag) on the yearly trend to focus other views."),
    p("Country and genre fields are split when multiple values are listed."),
    hr(),
    p(span("Source: Kaggle “Netflix Movies and TV Shows” (shivamb).", style = "color:#555; font-size:12px;"))
  ),
  layout_columns(
    fill = FALSE,
    col_widths = c(12, 12, 12, 12),
    card(card_header("1) Annual additions (brush to focus)"),
         plotOutput("p_trend", height = 300, brush = brushOpts(id = "trend_brush", direction = "x"))
    ),
    card(card_header("2) Top countries"),
         uiOutput("country_plot_ui")
    ),
    card(card_header("3) Top genres"),
         uiOutput("genre_plot_ui")
    ),
    card(card_header("4) Movie vs TV share over time"),
         plotOutput("p_share", height = 280)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  base_data <- reactive({
    d <- netflix %>%
      filter(added_year >= input$year_range[1], added_year <= input$year_range[2])
    if (input$type != "All") d <- d %>% filter(type == input$type)
    if (length(input$rating) > 0) d <- d %>% filter(rating %in% input$rating)
    d
  })
  
  focus_data <- reactive({
    d <- base_data()
    br <- input$trend_brush
    if (is.null(br)) return(d)
    d %>% filter(added_year >= floor(br$xmin), added_year <= ceiling(br$xmax))
  })
  
  output$country_plot_ui <- renderUI({
    h <- max(520, 34 * input$top_n + 260)
    plotOutput("p_country", height = h)
  })
  
  output$genre_plot_ui <- renderUI({
    h <- max(520, 34 * input$top_n + 260)
    plotOutput("p_genre", height = h)
  })
  
  output$p_trend <- renderPlot({
    d <- base_data() %>% count(added_year, type, name = "n")
    ggplot(d, aes(added_year, n, color = type, linetype = type, group = type)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      scale_color_manual(values = c("Movie" = NETFLIX_RED, "TV Show" = DARK, "Unknown" = MID)) +
      scale_linetype_manual(values = c("Movie" = "solid", "TV Show" = "longdash", "Unknown" = "dotted")) +
      labs(
        title = "Titles added by year",
        subtitle = "Additions accelerate after the mid-2010s; brush to focus a window",
        x = "Year added", y = "Number of titles",
        caption = "Computed from titles with non-missing date_added."
      ) +
      plot_theme()
  })
  
  output$p_country <- renderPlot({
    d <- focus_data() %>%
      separate_rows(country, sep = ",\\s*") %>%
      filter(!is.na(country), country != "") %>%
      count(country, name = "n") %>%
      slice_max(n, n = input$top_n, with_ties = FALSE) %>%
      arrange(n)
    
    if (nrow(d) == 0) return(NULL)
    
    ggplot(d, aes(n, reorder(country, n))) +
      geom_col(fill = DARK, alpha = 0.9, width = 0.9) +
      scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 22)) +
      labs(
        title = "Where titles are credited",
        subtitle = focus_text(input$year_range, input$trend_brush),
        x = "Titles (credits; multi-country split)", y = NULL
      ) +
      plot_theme() +
      theme(legend.position = "none")
  })
  
  output$p_genre <- renderPlot({
    d <- focus_data() %>%
      separate_rows(listed_in, sep = ",\\s*") %>%
      filter(!is.na(listed_in), listed_in != "") %>%
      count(listed_in, name = "n") %>%
      slice_max(n, n = input$top_n, with_ties = FALSE) %>%
      arrange(n)
    
    if (nrow(d) == 0) return(NULL)
    
    ggplot(d, aes(n, reorder(listed_in, n))) +
      geom_col(fill = NETFLIX_RED, alpha = 0.9, width = 0.9) +
      scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 22)) +
      labs(
        title = "Dominant genre tags",
        subtitle = focus_text(input$year_range, input$trend_brush),
        x = "Titles (genre tags; multi-genre split)", y = NULL
      ) +
      plot_theme() +
      theme(legend.position = "none")
  })
  
  output$p_share <- renderPlot({
    d <- focus_data() %>%
      count(added_year, type, name = "n") %>%
      group_by(added_year) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()
    
    if (nrow(d) == 0) return(NULL)
    
    ggplot(d, aes(added_year, share, color = type, linetype = type, group = type)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.6) +
      scale_color_manual(values = c("Movie" = NETFLIX_RED, "TV Show" = DARK, "Unknown" = MID)) +
      scale_linetype_manual(values = c("Movie" = "solid", "TV Show" = "longdash", "Unknown" = "dotted")) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(
        title = "Movie vs TV share",
        subtitle = "Share within each year (in the focus window)",
        x = "Year added", y = "Share of titles"
      ) +
      plot_theme()
  })
}

shinyApp(ui, server)