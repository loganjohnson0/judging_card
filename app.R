library(shiny)
library(bslib)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(htmltools)
library(stringr)
library(nanoparquet)
webr::install("markdown")

  individual_url <- "https://raw.githubusercontent.com/loganjohnson0/judging_card/main/All_Individual.parquet"
  individual_path <- "All_Team.parquet"
  download.file(individual_url, individual_path)
  individual <- nanoparquet::read_parquet(individual_path)

  individual <- individual |> 
    dplyr::mutate(student_school = paste(student_name, school_name, sep = " at "))

  team_url <- "https://raw.githubusercontent.com/loganjohnson0/judging_card/main/All_Team.parquet"
  team_path <- "All_Team.parquet"
  download.file(team_url, team_path)
  team <- nanoparquet::read_parquet(team_path)

  individual_max <- tibble::tibble(contest_class = c("Beef Grading", "Beef Judging", "Lamb Judging", 
                                                          "Overall", "Overall Beef", "Pork Judging", 
                                                          "Total Placing", "Reasons/Questions", "Specifications"),
                                  max_score = c(300, 300, 150, 
                                                1150, 600, 300, 
                                                500, 250, 100))
  team_max  <-   individual_max  |> 
    dplyr::mutate(max_score = max_score * 4)

ui <- bslib::page_navbar(
    theme = bs_theme(preset = "lux"),

    id = "nav",

    title = tags$span("Intercollegiate Meat Judging Results"),
    
    sidebar = sidebar(
          width = 350,
          id = "sidebar",

  # Individual
        conditionalPanel(
          condition = "input.nav == 'Individual Results' && input.team_contest == ''",
            
            selectizeInput(inputId = "individual_contest",
                          label = "Select the Meat Judging Contest",
                          choices =  sort(unique(individual$contest_name)), 
                          multiple = TRUE,
                          options = list(maxItems = 1, placeholder = "Select Contest",
                                        closeAfterSelect = TRUE)),

            selectizeInput(inputId = "individual_year",
                          label = "Select the Year the Contest was Held",
                          choices = NULL, 
                          multiple = TRUE),

            selectizeInput(inputId = "individual_person", 
                          label = "Select the Person", 
                          multiple = TRUE,
                          choices = NULL)),


  # Team
      conditionalPanel(
        condition = "input.nav == 'Team Results' && input.individual_contest == ''",

            selectizeInput(inputId = "team_contest",
                          label = "Select the Meat Judging Contest",
                          choices = sort(unique(team$contest_name)), 
                          multiple = TRUE,
                          options = list(maxItems = 1, placeholder = "Select Contest",
                                        closeAfterSelect = TRUE)),

            selectizeInput(inputId = "team_year",
                          label = "Select the Year the Contest was Held",
                          choices = NULL, 
                          multiple = TRUE),

            selectizeInput(inputId = "team_name",
                          label = "Select the University",
                          choices = NULL,
                          multiple = TRUE)),


      conditionalPanel(
          condition = "input.nav == 'Team Results' && input.team_contest != '' && input.team_year != '' && input.team_name != ''",
              
            selectizeInput(inputId = "team_alt",
                          label = "(Optional) Select Alternate Teams",
                          choices = NULL,
                          multiple = TRUE))
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
          ), col_widths = c(-1, 10, -1), max_height = 600)
      ),

      nav_panel("Team Results", 
    
      layout_columns(
        card(
          card_header("Selected Contest, Year, and University", class = "bg-dark"),

          plotOutput("team_plot")), col_widths = c(-1, 10, -1), max_height = 600)
          )
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

  filtered_individual <-  shiny::reactive({
    req(input$individual_contest)
    req(input$individual_person)
    req(input$individual_year)

    individual |> 
      dplyr::filter(contest_name == input$individual_contest,
                    student_school == input$individual_person,
                    contest_date == input$individual_year)
  })

  shiny::observeEvent(input$individual_contest, {

      individual_possible_year <- individual |> 
        dplyr::filter(contest_name == input$individual_contest) |> 
        dplyr::pull(contest_date) |> unique() |> sort()
      
    shiny::updateSelectizeInput(session, inputId = "individual_year", 
                              choices = individual_possible_year, 
                              server = TRUE, selected = "",
                              options = list(maxItems = 1, placeholder = "Select Year",
                                            closeAfterSelect = TRUE))
  })

  shiny::observeEvent(input$individual_year, {

    individual_possible_name <- individual |> 
      dplyr::filter(contest_name == input$individual_contest,
                    contest_date == input$individual_year) |> 
      dplyr::arrange(school_name, student_name) |> 
      dplyr::pull(student_school) |> unique()
    
  shiny::updateSelectizeInput(session, inputId = "individual_person", 
                            choices = individual_possible_name, 
                            server = TRUE, selected = "",
                            options = list(maxItems = 1, placeholder = "Select Individual's Name",
                                          closeAfterSelect = TRUE))
})
  

  output$individual_plot <- shiny::renderPlot({
    req(input$individual_contest)
    req(input$individual_person)

    ggplot2::ggplot(filtered_individual(), aes(x = score, y = reorder(contest_class, -score))) +
      ggplot2::geom_point() +
      ggplot2::geom_text(aes(label = score), nudge_y = 0.5) +
      ggplot2::geom_label(aes(label = rank, x = 0), nudge_y = 0.2) +
      ggplot2::geom_point(data = individual_max, aes(x = max_score, y = contest_class), color = "green", inherit.aes = FALSE) +
      ggthemes::theme_clean() +
      ggplot2::scale_x_continuous(limits = c(0, 1200)) +
      ggplot2::xlab("Scores") +
      ggplot2::ylab("Judging Contest Categories") +
      ggplot2::ggtitle(label = input$individual_person,
              subtitle = paste(filtered_individual()$contest_date, filtered_individual()$contest_name))
  })

  filtered_team <- shiny::reactive({
    req(input$team_contest)
    req(input$team_year)
    req(input$team_name)
    req(input$team_alt)

    team |> 
      dplyr::filter(contest_name == input$team_contest,
                    contest_date == input$team_year,
                    school_name == input$team_name,
                    alternate == input$team_alt)
  })

  shiny::observeEvent(input$team_contest, {
    team_possible_year <- team |> 
      dplyr::filter(contest_name == input$team_contest) |> 
      dplyr::pull(contest_date) |> unique() |> sort()
      
    shiny::updateSelectizeInput(session, inputId = "team_year", 
                          choices = team_possible_year, 
                          server = TRUE, selected = "",
                          options = list(maxItems = 1, placeholder = "Select Year",
                                          closeAfterSelect = TRUE))
  })

  shiny::observeEvent(input$team_year, {
    team_possible_school <- team |> 
      dplyr::filter(contest_name == input$team_contest, 
                    contest_date == input$team_year) |> 
      dplyr::pull(school_name) |> unique() |> sort()

    shiny::updateSelectizeInput(session, inputId = "team_name", 
                          choices = team_possible_school, 
                          server = TRUE, selected = "",
                          options = list(maxItems = 1, placeholder = "Select University",
                                          closeAfterSelect = TRUE))

  })

  shiny::observeEvent(input$team_name, {
    team_possible_alt <- team |> 
      dplyr::filter(contest_name == input$team_contest, 
                    contest_date == input$team_year,
                    school_name == input$team_name) |> 
      dplyr::pull(alternate) |> unique() |> sort()

    shiny::updateSelectizeInput(session, inputId = "team_alt", 
                          choices = team_possible_alt, 
                          server = TRUE, selected = "Team 1",
                          options = list(maxItems = 1, placeholder = "Select Alternative Teams",
                                          closeAfterSelect = TRUE))

  })


  output$team_plot <- shiny::renderPlot({
    req(input$team_contest)
    req(input$team_year)
    req(input$team_name)

    ggplot2::ggplot(filtered_team(), aes(x = score, y = reorder(contest_class, -score))) +
      ggplot2::geom_point() +
      ggplot2::geom_text(aes(label = score), nudge_y = 0.5) +
      ggplot2::geom_label(aes(label = rank, x = 0), nudge_y = 0.2) +
      ggplot2::geom_point(data = team_max, aes(x = max_score, y = contest_class), color = "green", inherit.aes = FALSE) +
      ggthemes::theme_clean() +
      ggplot2::scale_x_continuous(limits = c(0, 4800)) +
      ggplot2::xlab("Scores") +
      ggplot2::ylab("Judging Contest Categories") +
      ggplot2::ggtitle(label = input$team_name,
              subtitle = paste(filtered_team()$contest_date, filtered_team()$contest_name))
  })
}

shiny::shinyApp(ui = ui, server = server)