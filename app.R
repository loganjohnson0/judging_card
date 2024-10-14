library(shiny)
library(bslib)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
# library(tibble)
library(htmltools)
library(lubridate)
library(markdown)

load(url("https://raw.githubusercontent.com/loganjohnson0/judging_card/main/individual.RData"))
load(url("https://raw.githubusercontent.com/loganjohnson0/judging_card/main/team.RData"))

# individual <- readr::read_csv(file = c("~/Desktop/Judging_Card_AMSA/Cleaned_Results/National_Western/2024-09-29_National_Western_Individual_Results.csv",
#                                         "~/Desktop/Judging_Card_AMSA/Cleaned_Results/Southwestern/2024-10-06_Southwestern_Individual_Results.csv"))

# team <- readr::read_csv(file = c("~/Desktop/Judging_Card_AMSA/Cleaned_Results/National_Western/2024-09-29_National_Western_Team_Results.csv",
#                                         "~/Desktop/Judging_Card_AMSA/Cleaned_Results/Southwestern/2024-10-06_Southwestern_Team_Results.csv"))

individual <- individual |> 
  mutate(student_school = paste(student_name, school_name, sep = "_"))

students <- individual |> 
  distinct(student_name, school_name, .keep_all = TRUE) |> 
  arrange(student_name, school_name) |> 
  mutate(student_label = paste0(student_name, " (", school_name, ")"),
        student_school = paste(student_name, school_name, sep = "_"))

student_choices <- setNames(students$student_school, students$student_label)

ui <- page_navbar(
    theme = bs_theme(preset = "lux"),

    id = "nav",

    title = tags$span("Intercollegiate Meat Judging Results"),
    
    sidebar = sidebar(
          width = 350,
          id = "sidebar",


  # Individual
        conditionalPanel(
          condition = "input.nav == 'Individual Results'",
            selectizeInput(inputId = "individual_contest",
              label = "First, Select the Meat Judging Contest",
              choices =  sort(unique(individual$contest_name)), 
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select Contest",
                            closeAfterSelect = TRUE))),

      conditionalPanel(
        condition = "input.nav == 'Individual Results' && input.individual_contest != ''",

            selectizeInput(inputId = "individual_person", 
              label = "Next, Select the Individual (School)", 
              multiple = TRUE,
              choices = NULL)),


  # Team
      conditionalPanel(
        condition = "input.nav == 'Team Results' && input.individual_contest == ''",

            selectizeInput(inputId = "team_contest",
              label = "First, Select the Meat Judging Contest",
              choices = sort(unique(team$contest_name)), 
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select Contest",
                            closeAfterSelect = TRUE))),



      conditionalPanel(
        condition = "input.nav == 'Team Results' && input.team_contest != ''",

            selectizeInput(inputId = "team_year",
              label = "Next, the Year the Contest was Held",
              choices = sort(unique(lubridate::year(team$date))), 
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select Contest",
                            closeAfterSelect = TRUE))),


              
      conditionalPanel(
          condition = "input.nav == 'Team Results' && input.team_contest != '' && input.team_year != ''",
          selectizeInput(inputId = "team_name",
            label = "Finally, Select the University",
            multiple = TRUE, 
            choices = sort(unique(team$school_name)),
            options = list(maxItems = 1, placeholder = "Select University",
                closeAfterSelect = TRUE))
        )
      ),

    nav_spacer(),

    nav_panel("Home",
        layout_columns(
          card(
            card_header("Hello!", class = "bg-dark"),

            # includeMarkdown("welcome_page.md")
          ), col_widths = c(-1, 10, -1), max_height = 600
        )
      ),

    nav_panel("Individual Results",

        layout_columns(
          card(
            card_header("Selected Contest, Individual, and University", class = "bg-dark"),

            plotOutput("individual_plot")
          ), col_widths = c(-1, 10, -1), max_height = 450)
      ),

    nav_panel("Team Results", 
    
        layout_columns(
          card(
            card_header("Selected Contest, Year, and University", class = "bg-dark"),

            plotOutput("team_plot")
          ), col_widths = c(-1, 10, -1), max_height = 450)
      ),

  )

server <- function(input, output, session) {

  observeEvent(input$nav, {
    if (input$nav == "Individual Results") {
      updateSelectizeInput(session, "team_contest", selected = "")
    } else if (input$nav == "Team Results") {
      updateSelectizeInput(session, "individual_contest", selected = "")
    }
  })

  observe({
    sidebar_toggle(
      id = "sidebar",
      open = input$nav != "Home"
    )
  })

  updateSelectizeInput(session, inputId = "individual_person", 
                  choices = student_choices, server = TRUE, selected = "",
                  options = list(maxItems = 1, 
                          closeAfterSelect = TRUE,
                          placeholder = "Individual's Name"))
  
  # individual_max_scores <- tibble::tibble(results_categories = c("Beef Grading", "Beef Judging", "Lamb Judging", 
  #     "Overall", "Overall Beef", "Pork Judging", 
  #     "Total Placing", "Total Reas/Quest", "Specifications"),
  #   max_score = c(300, 300, 150, 1150, 600, 300, 500, 250, 100))
  
  # team_max_scores <- tibble::tibble(results_categories = c("Beef Grading", "Beef Judging", "Lamb Judging", 
  #   "Overall", "Overall Beef", "Pork Judging", 
  #   "Total Placing", "Total Reas/Quest", "Specifications"),
  # max_score = c(300, 300, 150, 1150, 600, 300, 500, 250, 100)*4)

  output$individual_plot <- renderPlot({
    req(input$individual_contest)
    req(input$individual_person)

    filtered_individual <- individual |> 
      dplyr::filter(contest_name == input$individual_contest) |> 
      dplyr::filter(student_school == input$individual_person)

    ggplot2::ggplot(filtered_individual, aes(x = score, y = reorder(results_categories, -score))) +
      ggplot2::geom_point() +
      ggplot2::geom_text(aes(label = score), nudge_y = 0.5) +
      ggplot2::geom_label(aes(label = rank, x = 0), nudge_y = 0.2) +
      # ggplot2::geom_point(data = individual_max_scores, aes(x = max_score, y = results_categories), 
      #                     color = "green") +
      ggplot2::scale_x_continuous(limits = c(0, 1200), breaks = pretty_breaks(n = 12)) +
      ggthemes::theme_clean() +
      ggplot2::xlab("Scores") +
      ggplot2::ylab("Judging Contest Categories") +
      ggtitle(label = stringr::str_replace(input$individual_person, "_", " for "),
              subtitle = paste(lubridate::year(filtered_individual$date), filtered_individual$contest_name))
  })


  output$team_plot <- renderPlot({
    req(input$team_contest)
    req(input$team_year)
    req(input$team_name)

    filtered_team <- team |> 
      dplyr::filter(contest_name == input$team_contest) |> 
      dplyr::filter(lubridate::year(date) == input$team_year) |> 
      dplyr::filter(school_name == input$team_name)

    ggplot2::ggplot(filtered_team, aes(x = score, y = reorder(results_categories, -score))) +
      ggplot2::geom_point() +
      ggplot2::geom_text(aes(label = score), nudge_y = 0.5) +
      ggplot2::geom_label(aes(label = rank, x = 0), nudge_y = 0.2) +
      # ggplot2::geom_point(data = team_max_scores, aes(x = max_score, y = results_categories), 
      #                     color = "green") +
      ggplot2::scale_x_continuous(limits = c(0, 4800), breaks = pretty_breaks(n = 11)) +
      ggthemes::theme_clean() +
      ggplot2::xlab("Scores") +
      ggplot2::ylab("Judging Contest Categories") +
      ggtitle(label = input$team_name,
              subtitle = paste(lubridate::year(filtered_team$date), filtered_team$contest_name))
  })
}

shiny::shinyApp(ui = ui, server = server)
