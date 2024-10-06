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

ui <- page_fluid(
  navset_tab(
    nav_panel("Welcome!","Hello! Welcome to a shiny app that will help you explore the history of meat judging based on results from prior Judging Card contests.

You can explore Individual results for any individual, Team results for any team, and directly compare two individuals or teams.

Click on the tabs above to begin exploring!"),
    nav_panel("Individual Results",
      selectizeInput("person", "Student Name (School)", choices = NULL),
      plotOutput("plot")
    ),
    nav_panel("Team Results", "Page B content"),
    nav_panel("Comparison of Individuals", "Page C content")
  )
)

server <- function(input, output, session) {

  updateSelectizeInput(session, "person", choices = student_choices, server = TRUE)

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
      geom_point(data = max_scores, aes(x = max_score, y = results_categories), color = "green", inherit.aes = FALSE) +
      scale_x_continuous(limits = c(0, 1200), breaks = pretty_breaks(n = 12)) +
      scale_color_colorblind() +
      theme_clean() +
      theme(legend.position = "bottom") +
      facet_wrap(~ school_name, ncol = 1)
  })

  }

shiny::shinyApp(ui = ui, server = server)