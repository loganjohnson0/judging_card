library(shiny)
library(bslib)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(htmltools)
library(stringr)
library(nanoparquet)
webr::install("markdown")

  load(url("https://raw.githubusercontent.com/loganjohnson0/judging_card/main/individual.RData"))

  individual <- individual |> 
    dplyr::mutate(student_school = paste(student_name, school_name, sep = "_"))

  students <- individual |> 
    dplyr::distinct(student_name, school_name, .keep_all = TRUE) |> 
    dplyr::arrange(student_name, school_name) |> 
    dplyr::mutate(student_label = paste0(student_name, " (", school_name, ")"),
                student_school = paste(student_name, school_name, sep = "_"))

  student_choices <- setNames(students$student_school, students$student_label)

  team_url <- "https://raw.githubusercontent.com/loganjohnson0/judging_card/main/All_Team.parquet"
  team_path <- "All_Team.parquet"
  download.file(team_url, team_path)
  team <- nanoparquet::read_parquet(team_path)

ui <- bslib::page_navbar(
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
              choices = sort(unique(team$contest_date)), 
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

            shiny::includeMarkdown("welcome_page.md")
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
          ), col_widths = c(-1, 10, -1), max_height = 500)
      ),

  )

server <- function(input, output, session) {

  shiny::observeEvent(input$nav, {
    if (input$nav == "Individual Results") {
      updateSelectizeInput(session, "team_contest", selected = "")
    } else if (input$nav == "Team Results") {
      updateSelectizeInput(session, "individual_contest", selected = "")
    }
  })

  shiny::observe({
    sidebar_toggle(
      id = "sidebar",
      open = input$nav != "Home"
    )
  })

  shiny::updateSelectizeInput(session, inputId = "individual_person", 
                  choices = student_choices, server = TRUE, selected = "",
                  options = list(maxItems = 1, 
                          closeAfterSelect = TRUE,
                          placeholder = "Individual's Name"))
  

  output$individual_plot <- shiny::renderPlot({
    req(input$individual_contest)
    req(input$individual_person)

    filtered_individual <- individual |> 
      dplyr::filter(contest_name == input$individual_contest,
                    student_school == input$individual_person)

    ggplot2::ggplot(filtered_individual, aes(x = score, y = reorder(contest_class, -score))) +
      ggplot2::geom_point() +
      ggplot2::geom_text(aes(label = score), nudge_y = 0.5) +
      ggplot2::geom_label(aes(label = rank, x = 0), nudge_y = 0.2) +
      ggthemes::theme_clean() +
      ggplot2::scale_x_continuous(limits = c(0, 1200), breaks = 12) +
      ggplot2::xlab("Scores") +
      ggplot2::ylab("Judging Contest Categories") +
      ggplot2::ggtitle(label = stringr::str_replace(input$individual_person, "_", " for "),
              subtitle = paste(filtered_individual$contest_date, filtered_individual$contest_name))
  })


  output$team_plot <- shiny::renderPlot({
    req(input$team_contest)
    req(input$team_year)
    req(input$team_name)

    filtered_team <- team |> 
      dplyr::filter(contest_name == input$team_contest,
                    contest_date == input$team_year,
                    school_name == input$team_name)

    ggplot2::ggplot(filtered_team, aes(x = score, y = reorder(contest_class, -score))) +
      ggplot2::geom_point() +
      ggplot2::geom_text(aes(label = score), nudge_y = 0.5) +
      ggplot2::geom_label(aes(label = rank, x = 0), nudge_y = 0.2) +
      ggthemes::theme_clean() +
      ggplot2::scale_x_continuous(limits = c(0, 4800), breaks = 6) +
      ggplot2::xlab("Scores") +
      ggplot2::ylab("Judging Contest Categories") +
      ggplot2::ggtitle(label = input$team_name,
              subtitle = paste(filtered_team$contest_date, filtered_team$contest_name)) +
    ggplot2::facet_wrap(~ alternate, ncol = 1)
  })
}

shiny::shinyApp(ui = ui, server = server)
