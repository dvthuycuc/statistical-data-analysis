library(shiny)
library(dplyr)
library(ggplot2)
source("helper.R")

source("topic_content.R")

ui <- navbarPage(
  "SteamStats Main App",
  
  tabPanel(
    "Statistical Learning App",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .nav.nav-pills > li > a { border-radius: 0; }
          .nav-stacked { border: 1px solid #222; }
          .nav-stacked > li > a { border-bottom: 1px solid #222; }
          .nav-stacked > li:last-child > a { border-bottom: 0; }

          .content-box {
            border: 1px solid #222;
            min-height: 420px;
            padding: 18px;
          }
        "))
      ),
      
      navlistPanel(
        widths = c(3, 9),
        well = FALSE,
        "Topic",
        
        tabPanel(
          "Overview",
          div(class = "content-box", topic_overview_ui())
        ),
        
        tabPanel(
          "Descriptive Statistic",
          div(
            class = "content-box",
            topic_descriptive_statistic_ui(),
            actionButton("open_desc", "Open tutorial", class = "btn-primary")
          )
        ),
        tabPanel(
          "Law of Large Numbers",
          div(
            class = "content-box",
            h3("Law of Large Numbers: Dice Simulation"),
            p("Start rolling a fair die. Watch the running average approach 3.5 as the number of rolls grows."),
            
            fluidRow(
              column(
                4,
                actionButton("start_roll", "Start", class = "btn-success"),
                actionButton("stop_roll", "Stop", class = "btn-warning"),
                actionButton("reset_roll", "Reset", class = "btn-danger"),
                br(), br(),
                strong("Latest roll:"),
                textOutput("latest_roll", inline = TRUE),
                br(),
                strong("N rolls:"),
                textOutput("n_rolls", inline = TRUE),
                br(),
                strong("Running mean:"),
                textOutput("running_mean", inline = TRUE),
                br(), br(),
                plotOutput("dice_plot", height = "200px")
              ),
              column(
                8,
                plotOutput("lln_plot", height = "260px"),
                br(),
                tableOutput("roll_table")
              )
            )
          )
        ),
        tabPanel(
          "Central Limit Theorem",
          div(
            class = "content-box",
            h3("Central Limit Theorem: Galton Board Simulation"),
            p("Each ball hits pegs and randomly goes left/right. The final bin counts approach a bell-shaped distribution as balls increase."),
            
            fluidRow(
              column(
                4,
                sliderInput("gb_rows", "Number of peg rows", min = 5, max = 20, value = 12, step = 1),
                sliderInput("gb_batch", "Balls per second", min = 1, max = 50, value = 10, step = 1),
                actionButton("gb_start", "Start", class = "btn-success"),
                actionButton("gb_stop",  "Stop",  class = "btn-warning"),
                actionButton("gb_reset", "Reset", class = "btn-danger"),
                br(), br(),
                strong("Total balls landed: "),
                textOutput("gb_n", inline = TRUE)
              ),
              column(
                8,
                plotOutput("gb_board", height = "280px"),
                br(),
                plotOutput("gb_hist", height = "240px")
              )
            )
          )
        )
        
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$open_desc, {
    system2("quarto", c("preview", "1_descriptive_statistic.qmd"))
  })
  
  # ---Roll Dice simulation state ---
  rv <- reactiveValues(
    running = FALSE,
    rolls = integer(0)
  )
  
  observeEvent(input$start_roll, { rv$running <- TRUE })
  observeEvent(input$stop_roll,  { rv$running <- FALSE })
  observeEvent(input$reset_roll, {
    rv$running <- FALSE
    rv$rolls <- integer(0)
  })
  
  timer <- reactive({
    reactiveTimer(500)()
    Sys.time()
  })
  
  observeEvent(timer(), {
    if (!isTRUE(rv$running)) return()
    new_roll <- sample(1:6, size = 1)
    rv$rolls <- c(rv$rolls, new_roll)
  }, ignoreInit = TRUE)
  
  df_rolls <- reactive({
    n <- length(rv$rolls)
    if (n == 0) {
      tibble(roll = integer(0), value = integer(0), mean = numeric(0))
    } else {
      tibble(
        roll = seq_len(n),
        value = rv$rolls,
        mean = cumsum(rv$rolls) / seq_len(n)
      )
    }
  })
  
  output$latest_roll <- renderText({
    if (length(rv$rolls) == 0) "-" else as.character(tail(rv$rolls, 1))
  })
  
  output$n_rolls <- renderText({
    as.character(length(rv$rolls))
  })
  
  output$running_mean <- renderText({
    if (length(rv$rolls) == 0) "-" else sprintf("%.3f", mean(rv$rolls))
  })
  
  output$dice_plot <- renderPlot({
    if (length(rv$rolls) == 0) {
      plot.new()
      text(0.5, 0.5, "Press Start", cex = 1.4)
    } else {
      draw_dice(tail(rv$rolls, 1))
    }
  })
  
  output$lln_plot <- renderPlot({
    d <- df_rolls()
    
    if (nrow(d) == 0) {
      ggplot() +
        theme_minimal() +
        labs(title = "Running mean of dice rolls", x = "Roll number", y = "Running mean") +
        annotate("text", x = 1, y = 3.5, label = "Press Start", hjust = 0)
    } else {
      ggplot(d, aes(x = roll, y = mean)) +
        geom_line() +
        geom_hline(yintercept = 3.5, linetype = "dashed") +
        scale_y_continuous(limits = c(1, 6)) +
        theme_minimal() +
        labs(
          title = "Running mean of dice rolls (LLN demo)",
          subtitle = "Dashed line = theoretical mean (3.5)",
          x = "Roll number",
          y = "Running mean"
        )
    }
  })
  
  output$roll_table <- renderTable({
    d <- df_rolls()
    if (nrow(d) == 0) return(d)
    tail(d, 20)
  })
  
  # --- Galton Board (CLT) simulation state (FAST + STABLE) ---
  gb <- reactiveValues(
    running = FALSE,
    balls = tibble::tibble(id = integer(), row = integer(), col = integer()),
    landed_counts = integer(0),   # counts for bins 0..rows (length rows+1)
    next_id = 1L
  )

  # Make stop/start "instant" relative to the simulation loop
  observeEvent(input$gb_start, { gb$running <- TRUE  }, priority = 100)
  observeEvent(input$gb_stop,  { gb$running <- FALSE }, priority = 100)
  
  # --- Simulation loop: ONLY ticks when running ---
  observe({
    # If not running, do nothing and do NOT schedule frequent invalidations
    if (!isTRUE(gb$running)) return()
    
    # Schedule next tick (200 ms)
    invalidateLater(200, session)
    
    rows <- as.integer(input$gb_rows)
    if (rows <= 0) return()
    
    # Optional safety cap to prevent runaway workload
    max_falling <- 5000L
    
    # Spawn control (your current approach is fine; this is just safer)
    spawn_n <- rbinom(1, size = as.integer(input$gb_batch), prob = 0.10)
    
    # If we already have too many falling balls, stop spawning
    if (nrow(gb$balls) >= max_falling) spawn_n <- 0L
    
    if (spawn_n > 0) {
      new <- tibble::tibble(
        id  = gb$next_id + seq_len(spawn_n) - 1L,
        row = 0L,
        col = 0L
      )
      gb$next_id <- gb$next_id + spawn_n
      gb$balls <- dplyr::bind_rows(gb$balls, new)
    }
    
    if (nrow(gb$balls) == 0) return()
    
    # Update positions
    step_lr <- sample(c(-1L, 1L), size = nrow(gb$balls), replace = TRUE)
    
    gb$balls <- gb$balls %>%
      dplyr::mutate(
        row = row + 1L,
        col = col + step_lr
      )
    
    # Landed balls
    landed_now <- gb$balls %>% dplyr::filter(row >= rows)
    
    if (nrow(landed_now) > 0) {
      bins <- (landed_now$col + rows) / 2
      bins <- bins[is.finite(bins) & abs(bins - round(bins)) < 1e-9]
      bins <- as.integer(round(bins))
      bins <- bins[bins >= 0L & bins <= rows]
      
      if (length(bins) > 0) {
        tab <- tabulate(bins + 1L, nbins = rows + 1L)
        if (length(gb$landed_counts) != rows + 1L) gb$landed_counts <- integer(rows + 1L)
        gb$landed_counts <- gb$landed_counts + tab
      }
      
      gb$balls <- gb$balls %>% dplyr::filter(row < rows)
    }
  })
  
  
}

shinyApp(ui, server)
