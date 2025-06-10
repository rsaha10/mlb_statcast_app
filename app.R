# Import Packages
library(tidyverse)
library(shiny)
library(ggplot2)
library(ggdensity)


# Load Statcast data for each year
sc17 = read_csv("sc17_filt.csv")
sc18 = read_csv("sc18_filt.csv")
sc19 = read_csv("sc19_filt.csv")
sc21 = read_csv("sc21_filt.csv")
sc22 = read_csv("sc22_filt.csv")
sc_filt = bind_rows(sc17,sc18,sc19,sc21,sc22)

# Count pitches thrown for later filtering
pitch_counts = sc_filt |>
  count(pitcher_name, name = "n_pitches") |>
  filter(n_pitches >= 250)

# Count pitches seen for later filtering
hit_counts = sc_filt |>
  count(batter_name, name = "n_pitches") |>
  filter(n_pitches >= 250)

# Function to create spray chart
spray_chart = function(...){
  ggplot(...) + geom_curve(x=33, xend=223, y =-100, yend =-100, curvature =-0.65) +
    geom_segment(x=128, xend=33, y=-208, yend=-100) +
    geom_segment(x=128, xend=223, y=-208, yend=-100) +
    geom_curve(x=83, xend=173, y=-155, yend=-156,
               curvature=-0.65, linetype="dotted") +
    coord_fixed() +
    scale_x_continuous(NULL, limits = c(25,225)) +
    scale_y_continuous(NULL, limits = c(-225,-25))
}

ui = fluidPage(
  titlePanel("MLB Statcast Pitch Charts"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("role", "View As:", choices = c("Pitcher","Hitter"), selected = "Pitcher", inline = TRUE),
      selectInput(inputId = "team", label = "Select Team:", choices = NULL),
      selectInput(inputId = "player", label = "Select Player:", choices = NULL),
      sliderInput(inputId = "year_range", label = "Select Year Range:",
                  min = 2017, max = 2022, value = c(2017,2022), step = 1, sep = ""),
      checkboxGroupInput("pitch_type_select",
                         label = "Select Pitch Types:",
                         choices = NULL,
                         selected = NULL
      ),
      checkboxGroupInput("batter_hand",
                         label = "Vs:",
                         choices = c("Left" = "L", "Right" = "R"),
                         selected = c("L", "R")),
      uiOutput("player_ui")
    ),
    mainPanel(
      plotOutput("strikezone_plot", height = "600px"),
      plotOutput("spray_chart", height = "600px"),
      tableOutput("summary_stats")
    )
  )
)

server = function(input, output, session) {
  sc_filtered = reactive({
    data = sc_filt |>
      filter(game_year >= input$year_range[1],
             game_year <= input$year_range[2])
    if (input$role == "Pitcher") {
      valid_pitchers = data |>
        count(pitcher_name, name = "n_pitches") |>
        filter(n_pitches >= 250) |>
        pull(pitcher_name)
      data = data |>
        filter(pitcher_name %in% valid_pitchers)
    } else {
      valid_hitters = data |>
        count(batter_name, name = "n_pitches") |>
        filter(n_pitches >= 250) |>
        pull(batter_name)
      data = data |>
        filter(batter_name %in% valid_hitters)
    }

    data
  })
  observe({
    previous_team = isolate(input$team)
    role_col = if (input$role == "Pitcher") "pitcher_team" else "hitter_team"
    team_choices = sort(unique(sc_filtered()[[role_col]]))
    selected_team = if (!is.null(previous_team) && previous_team %in% team_choices) {
      previous_team
    } else {
      team_choices[1]
    }
    updateSelectInput(session, "team", choices = team_choices, selected = selected_team)
  })
  observe({
    req(input$team)
    previous_player = isolate(input$player)
    data_filt = sc_filtered() |>
      filter(if (input$role == "Pitcher") pitcher_team == input$team else hitter_team == input$team)
    player_col = if (input$role == "Pitcher") "pitcher_name" else "batter_name"
    player_choices = sort(unique(data_filt[[player_col]]))
    selected_player = if (!is.null(previous_player) && previous_player %in% player_choices) {
      previous_player
    } else {
      player_choices[1]
    }
    updateSelectInput(session, "player", choices = player_choices, selected = selected_player)
  })
  observeEvent({
    input$player
    input$year_range
    input$batter_hand
  }, {
    req(input$player)
    player_col = if (input$role == "Pitcher") "pitcher_name" else "batter_name"
    available_pitches = sc_filtered() |>
      filter(.data[[player_col]] == input$player,
             if (input$role == "Pitcher") {
               stand %in% input$batter_hand
             } else {
               p_throws %in% input$batter_hand
             }) |>
      pull(pitch_type) |>
      unique() |>
      na.omit()

    updateCheckboxGroupInput(
      session,
      "pitch_type_select",
      choices = available_pitches,
      selected = available_pitches
    )
  })
  output$strikezone_plot = renderPlot({
    player_col = if (input$role == "Pitcher") "pitcher_name" else "batter_name"
    dta = sc_filtered() |>
      filter(
        .data[[player_col]] == input$player,
        pitch_type %in% input$pitch_type_select,
        if (input$role == "Pitcher") {
          stand %in% input$batter_hand
        } else {
          p_throws %in% input$batter_hand
        }
      )
    x_left = -0.83
    x_right = 0.83
    sz_top_avg = 3.4
    sz_bot_avg = 1.57

    ggplot(dta, aes(x=plate_x, y=plate_z)) +
      geom_hdr(
        method = method_kde(h = 0.75),
        probs = c(0.90, 0.70, 0.50, 0.40, 0.30, 0.20),
        show.legend = FALSE,
        aes(fill = after_stat(probs)),
        alpha = 0.75
      ) +
      scale_fill_brewer(
        palette = "RdBu",
        direction = -1
      ) +
      coord_fixed() +
      geom_segment(aes(x = x_left, xend = x_right, y = sz_top_avg, yend = sz_top_avg),
                   color = "black", linetype = "dashed") +
      geom_segment(aes(x = x_left, xend = x_right, y = sz_bot_avg, yend = sz_bot_avg),
                   color = "black", linetype = "dashed") +
      geom_segment(aes(x = x_left, xend = x_left, y = sz_bot_avg, yend = sz_top_avg),
                   color = "black", linetype = "dashed") +
      geom_segment(aes(x = x_right, xend = x_right, y = sz_bot_avg, yend = sz_top_avg),
                   color = "black", linetype = "dashed") +
      scale_x_continuous(trans = "reverse") +
      labs(title = paste("Pitch Locations for", input$player, "(Pitcher's Perspective)"), x="Plate X", y="Plate Z", color="Pitch Type") +
      theme(panel.background = element_blank())
  })
  output$spray_chart = renderPlot({
    req(input$player, input$pitch_type_select)

    player_col = if (input$role == "Pitcher") "pitcher_name" else "batter_name"

    dta = sc_filtered() |>
      filter(.data[[player_col]] == input$player,
             pitch_type %in% input$pitch_type_select,
             type == "X",
             if (input$role == "Pitcher") {
               stand %in% input$batter_hand
             } else {
               p_throws %in% input$batter_hand
             })

    if (nrow(dta) == 0) return(NULL)

    spray_chart(
      dta,
      aes(x = hc_x, y = -hc_y, color = events)
    ) +
      geom_point() +
      theme_minimal() +
      labs(
        title = paste("Spray Chart for", input$player),
        color = "Event"
      )
  })
  output$summary_stats = renderTable({
    player_col = if (input$role == "Pitcher") "pitcher_name" else "batter_name"
    dta = sc_filtered() |>
      filter(.data[[player_col]] == input$player,
             pitch_type %in% input$pitch_type_select,
             if (input$role == "Pitcher") {
               stand %in% input$batter_hand
             } else {
               p_throws %in% input$batter_hand
             })
    tibble(
      Pitches = nrow(dta),
      AB = dta %>%
        filter(
          !is.na(dta$events),
          dta$events != "walk", dta$events != "hit_by_pitch",
          !str_starts(events, "pickoff"), !str_starts(events, "sac_")
        ) %>%
        nrow(),
      H = sum(dta$events %in% c("single", "double", "triple", "home_run"), na.rm = TRUE),
      `1B` = sum(dta$events == "single", na.rm = TRUE),
      `2B` = sum(dta$events == "double", na.rm = TRUE),
      `3B` = sum(dta$events == "triple", na.rm = TRUE),
      HR = sum(dta$events == "home_run", na.rm = TRUE),
      K = sum(dta$events == "strikeout", na.rm = TRUE),
      BB = sum(dta$events == "walk", na.rm = TRUE),
      BA = sprintf("%.3f", H/AB),
      OBP = sprintf("%.3f",(H + BB) / (AB + BB)),
      SLG = sprintf("%.3f",(`1B` + 2 * `2B` + 3 * `3B` + 4 * HR)/AB),
      OPS = sprintf("%.3f",((H + BB) / (AB + BB)) + ((`1B` + 2 * `2B` + 3 * `3B` + 4 * HR)/AB))
    )
  })
}

shinyApp(ui, server)
