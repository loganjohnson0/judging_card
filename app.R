library(shiny)
library(bslib)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyr)
library(dplyr)
library(readr)
library(ggrepel)
library(tibble)
library(htmltools)
library(lubridate)

load(url("https://raw.githubusercontent.com/loganjohnson0/judging_card/main/national_western_individual.RData"))

data <- data |> 
  mutate(student_school = paste(student_name, school_name, sep = "_"))

students <- data |> 
  distinct(student_name, school_name, .keep_all = TRUE) |> 
  arrange(student_name, school_name) |> 
  mutate(student_label = paste0(student_name, " (", school_name, ")"),
        student_school = paste(student_name, school_name, sep = "_"))

student_choices <- setNames(students$student_school, students$student_label)

max_scores <- tibble::tibble(results_categories = c("Beef Grading", "Beef Judging", "Lamb Judging", "Overall", "Overall Beef", "Pork Judging", "Total Placing", "Total Reas/Quest", "Specifications"),
    max_score = c(300, 300, 150, 1150, 600, 300, 500, 250, 100))

ui <- page_navbar(
    theme = bs_theme(
        preset = "lux"),
    lang = "en",

    title = tags$span(

      "Intercollegiate Meat Judging Results"),
    
    sidebar = sidebar(width = 400, "Hello!",
  
      selectizeInput(inputId = "person", label = "Student Name (School)", 
          choices = NULL, multiple = TRUE, selected = character(0),
          options = list(placeholder = "Select Student", plugins = list("remove_button"), 
          closeAfterSelect = TRUE, maxItems = "1"))),

    nav_spacer(),
    
    nav_panel("Individual Results",
        layout_columns(
      card(
        card_header("Selected Person and University",
            class = "bg-dark")),
        plotOutput("plot"), max_height = 450, col_widths = c(-1, 10, -1))),

    nav_panel("Team Results", "Coming Soon!"),

  )

server <- function(input, output, session) {

  updateSelectizeInput(session, inputId = "person", choices = student_choices, server = TRUE)

  output$plot <- renderPlot({
    req(input$person)

    filtered_data <- data |> 
      filter(student_school == input$person)

    ggplot(filtered_data, aes(
      x = score,
      y = reorder(results_categories, -score),
      color = contest_name)) +
      geom_point() +
      ggrepel::geom_text_repel(aes(label = score), nudge_y = 0.5) +
      ggrepel::geom_label_repel(aes(label = rank, x = 0), nudge_y = 0.4) +
      geom_point(data = max_scores, aes(x = max_score, y = results_categories), color = "green", inherit.aes = FALSE) +
      scale_x_continuous(limits = c(0, 1200), breaks = pretty_breaks(n = 12)) +
      scale_color_colorblind() +
      ggthemes::theme_clean() +
      theme(legend.position = "none") +
      ggtitle(label = stringr::str_replace(input$person, "_", " for "),
      subtitle = paste(lubridate::year(filtered_data$date[1]), filtered_data$contest_name[1]))
  })
}

shinyApp(ui = ui, server = server)