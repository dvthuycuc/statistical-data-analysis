library(shiny)
library(dplyr)
library(ggplot2)

source("topic_content.R")   # defines topic_overview_ui(), topic_descriptive_statistic_ui(), etc.

# --- helper: draw a dice face with base R plot ---
draw_dice <- function(value) {
  stopifnot(value %in% 1:6)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  
  # dice square
  rect(0.1, 0.1, 0.9, 0.9, col = "white", border = "black", lwd = 3)
  
  # pip positions
  pos <- list(
    TL = c(0.3, 0.7),  TC = c(0.5, 0.7),  TR = c(0.7, 0.7),
    ML = c(0.3, 0.5),  MC = c(0.5, 0.5),  MR = c(0.7, 0.5),
    BL = c(0.3, 0.3),  BC = c(0.5, 0.3),  BR = c(0.7, 0.3)
  )
  
  pips <- switch(
    as.character(value),
    "1" = c("MC"),
    "2" = c("TL", "BR"),
    "3" = c("TL", "MC", "BR"),
    "4" = c("TL", "TR", "BL", "BR"),
    "5" = c("TL", "TR", "MC", "BL", "BR"),
    "6" = c("TL", "ML", "BL", "TR", "MR", "BR")
  )
  
  for (p in pips) points(pos[[p]][1], pos[[p]][2], pch = 16, cex = 5)
}

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
  
  # your existing Quarto launcher
  observeEvent(input$open_desc, {
    system2("quarto", c("preview", "1_descriptive_statistic.qmd"))
  })
  
  # --- LLN simulation state ---
  rv <- reactiveValues(
    running = FALSE,
    rolls = integer(0)
  )
  
  # start/stop/reset
  observeEvent(input$start_roll, { rv$running <- TRUE })
  observeEvent(input$stop_roll,  { rv$running <- FALSE })
  observeEvent(input$reset_roll, {
    rv$running <- FALSE
    rv$rolls <- integer(0)
  })
  
  # timer (ticks repeatedly); speed is controlled by input$roll_speed
  timer <- reactive({
    reactiveTimer(500)()
    Sys.time()
  })
  
  # on each timer tick, if running, append a new roll
  observeEvent(timer(), {
    if (!isTRUE(rv$running)) return()
    new_roll <- sample(1:6, size = 1)
    rv$rolls <- c(rv$rolls, new_roll)
  }, ignoreInit = TRUE)
  
  # derived data
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
  
  # outputs: text
  output$latest_roll <- renderText({
    if (length(rv$rolls) == 0) "-" else as.character(tail(rv$rolls, 1))
  })
  
  output$n_rolls <- renderText({
    as.character(length(rv$rolls))
  })
  
  output$running_mean <- renderText({
    if (length(rv$rolls) == 0) "-" else sprintf("%.3f", mean(rv$rolls))
  })
  
  # outputs: dice face
  output$dice_plot <- renderPlot({
    if (length(rv$rolls) == 0) {
      plot.new()
      text(0.5, 0.5, "Press Start", cex = 1.4)
    } else {
      draw_dice(tail(rv$rolls, 1))
    }
  })
  
  # outputs: LLN plot
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
  
  # CLT
  # --- Galton Board (CLT) simulation state ---
  gb <- reactiveValues(
    running = FALSE,
    balls = tibble::tibble(id = integer(), row = integer(), col = integer()),
    landed = integer(0),
    next_id = 1L
  )
  
  observeEvent(input$gb_start, { gb$running <- TRUE })
  observeEvent(input$gb_stop,  { gb$running <- FALSE })
  observeEvent(input$gb_reset, {
    gb$running <- FALSE
    gb$balls <- tibble::tibble(id = integer(), row = integer(), col = integer())
    gb$landed <- integer(0)
    gb$next_id <- 1L
  })
  
  # timer: 10 fps for smoother animation
  gb_timer <- reactive({
    reactiveTimer(100)()
    Sys.time()
  })
  
  observeEvent(gb_timer(), {
    if (!isTRUE(gb$running)) return()
    
    rows <- input$gb_rows
    
    # spawn new balls gradually (gb_batch per second ~ gb_batch/10 per tick)
    spawn_n <- rbinom(1, size = input$gb_batch, prob = 0.10) # approx batch/second
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
    
    # move all active balls down one row, and randomly left/right
    step_lr <- sample(c(-1L, 1L), size = nrow(gb$balls), replace = TRUE)
    
    gb$balls <- gb$balls %>%
      dplyr::mutate(
        row = row + 1L,
        col = col + step_lr
      )
    
    # balls that reached bottom row are "landed"
    landed_now <- gb$balls %>% dplyr::filter(row >= rows)
    
    if (nrow(landed_now) > 0) {
      # map final col to a bin index from 0..rows
      # If start col=0 and each step +/-1, then (col + rows)/2 is #rights
      bins <- (landed_now$col + rows) / 2
      
      # keep only valid integer bins (should be, but just in case)
      bins <- bins[is.finite(bins) & abs(bins - round(bins)) < 1e-9]
      gb$landed <- c(gb$landed, as.integer(round(bins)))
      
      # remove landed balls from active set
      gb$balls <- gb$balls %>% dplyr::filter(row < rows)
    }
    
  }, ignoreInit = TRUE)
  
  output$gb_n <- renderText({
    as.character(length(gb$landed))
  })
  
  # draw board (simple scatter): pegs + balls
  output$gb_board <- renderPlot({
    rows <- input$gb_rows
    
    # --- pegs (triangle) ---
    pegs <- purrr::map_dfr(1:rows, function(r) {
      tibble::tibble(
        x = seq(-r + 1, r - 1, by = 2),
        y = -r
      )
    })
    
    # --- falling balls ---
    falling <- gb$balls %>%
      dplyr::mutate(x = col, y = -row)
    
    # --- holder/bin geometry (bins = 0..rows) ---
    # bin centers in x are -rows, -rows+2, ..., rows
    bin_centers <- seq(-rows, rows, by = 2)
    bin_edges   <- seq(-rows - 1, rows + 1, by = 2)  # walls between bins
    
    # vertical walls from y = -(rows+1) down to y = -(rows+1) - holder_h
    holder_h <- max(6, ceiling(rows * 0.6))
    y_top <- -(rows + 1)
    y_bot <- y_top - holder_h
    
    walls <- tibble::tibble(
      x = bin_edges,
      xend = bin_edges,
      y = y_top,
      yend = y_bot
    )
    
    floor <- tibble::tibble(
      x = min(bin_edges),
      xend = max(bin_edges),
      y = y_bot,
      yend = y_bot
    )
    
    # --- stacked landed balls in bins ---
    # gb$landed is 0..rows (number of rights)
    if (length(gb$landed) > 0) {
      counts <- tibble::tibble(bin = gb$landed) %>%
        dplyr::count(bin, name = "n") %>%
        dplyr::mutate(
          x = -rows + 2 * bin
        )
      
      # build stacked (x, y) positions: y goes down row-by-row inside holder
      stacked <- counts %>%
        tidyr::uncount(n, .remove = FALSE, .id = "stack_id") %>%
        dplyr::mutate(
          y = y_bot + 0.8 + (stack_id - 1) * 0.9   # spacing
        ) %>%
        # keep only those visible inside the holder
        dplyr::filter(y <= y_top - 0.4)
    } else {
      stacked <- tibble::tibble(x = numeric(0), y = numeric(0))
    }
    
    ggplot2::ggplot() +
      # pegs
      ggplot2::geom_point(data = pegs, ggplot2::aes(x = x, y = y),
                          size = 1.4, alpha = 0.6) +
      
      # holder walls + floor
      ggplot2::geom_segment(data = walls,
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            linewidth = 0.5) +
      ggplot2::geom_segment(data = floor,
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            linewidth = 0.8) +
      
      # stacked landed balls (inside holder)
      ggplot2::geom_point(data = stacked, ggplot2::aes(x = x, y = y),
                          size = 2.2) +
      
      # falling balls
      ggplot2::geom_point(data = falling, ggplot2::aes(x = x, y = y),
                          size = 2.4) +
      
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Galton board + holder (bins)")
  })
  
  
}

shinyApp(ui, server)
