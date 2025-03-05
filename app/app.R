library(shiny)
library(Matrix)

# Function to generate a mine field
generate_mine_field <- function(rows, cols, num_mines) {
  # Create an empty matrix
  field <- Matrix(0, nrow = rows, ncol = cols)
  
  # Randomly place mines
  mine_positions <- sample(rows * cols, num_mines)
  field[mine_positions] <- -1
  
  # Calculate adjacent mine counts
  for (r in 1:rows) {
    for (c in 1:cols) {
      if (field[r, c] != -1) {
        # Check surrounding cells
        surrounding_mines <- 0
        for (dr in -1:1) {
          for (dc in -1:1) {
            nr <- r + dr
            nc <- c + dc
            
            # Check if adjacent cell is within bounds and is a mine
            if (nr > 0 && nr <= rows && nc > 0 && nc <= cols && field[nr, nc] == -1) {
              surrounding_mines <- surrounding_mines + 1
            }
          }
        }
        field[r, c] <- surrounding_mines
      }
    }
  }
  
  return(field)
}

ui <- fluidPage(
  # Dynamic styling based on theme
  uiOutput("dynamic_css"),
  
  titlePanel("MinesweepR"),
  
  sidebarLayout(
    sidebarPanel(

      # Game Configuration
      numericInput("grid_rows", "Grid Rows", 
                   value = 10, min = 5, max = 20),
      numericInput("grid_cols", "Grid Columns", 
                   value = 10, min = 5, max = 20),
      numericInput("num_mines", "Number of Mines", 
                   value = 15, min = 1, max = 100),
      # New Game Button
      actionButton("new_game", "New Game"),
      
      # Theme Toggle
      radioButtons("theme_select", "Theme",
                   choices = c("Light" = "light", "Dark" = "dark"),
                   selected = "light")
    ),
    
    mainPanel(
      # Mode Toggle
      fluidRow(
      radioButtons("game_mode", "Mode", 
                   choices = c("Reveal" = "reveal", "Flag" = "flag"),
                   selected = "reveal"),
      # Flag Counter
      uiOutput("flag_counter")
      ),
      
      # Game grid
      uiOutput("game_grid")
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to manage game state
  game_state <- reactiveValues(
    field = NULL,
    revealed = NULL,
    flagged = NULL,
    game_over = FALSE,
    game_won = FALSE,
    rows = 10,
    cols = 10,
    num_mines = 15
  )
  
  # Dynamic CSS based on theme
  output$dynamic_css <- renderUI({
    theme_colors <- if (input$theme_select == "dark") {
      list(
        bg_main = "#1a1a2e",
        bg_cell = "#16213e",
        bg_revealed = "#0f3460",
        text_color = "#e9e9e9",
        border_color = "#414868",
        sidebar_bg = "#16213e",
        sidebar_text = "#000000"
      )
    } else {
      list(
        bg_main = "#f0f0f0",
        bg_cell = "#cccccc",
        bg_revealed = "#e0e0e0",
        text_color = "#000000",
        border_color = "#999999",
        sidebar_bg = "#f0f0f0",
        sidebar_text = "#000000"
      )
    }
    
    tags$style(HTML(sprintf("
      body {
        background-color: %s;
        color: %s;
      }
      .sidebar {
        background-color: %s;
      }
      .sidebar .radio, .sidebar .control-label, 
      .sidebar label.radio-inline {
        color: %s !important;
      }
      .game-grid {
        display: grid;
        grid-template-columns: repeat(%d, 50px);
        gap: 2px;
      }
      .cell {
        width: 50px;
        height: 50px;
        background-color: %s;
        display: flex;
        justify-content: center;
        align-items: center;
        font-weight: bold;
        border: 1px solid %s;
      }
      .cell-clickable {
        cursor: pointer;
      }
      .cell-revealed {
        background-color: %s;
      }
      .cell-mine {
        background-color: red;
      }
      .flag {
        color: red;
      }
      .flag-counter {
        font-size: 18px;
        margin-bottom: 10px;
        color: %s;
      }
    ", 
    theme_colors$bg_main, 
    theme_colors$text_color, 
    theme_colors$sidebar_bg,
    theme_colors$sidebar_text,
    game_state$cols,
    theme_colors$bg_cell, 
    theme_colors$border_color, 
    theme_colors$bg_revealed,
    theme_colors$text_color)))
  })
  
  # Initialize new game
  initialize_game <- function() {
    rows <- input$grid_rows
    cols <- input$grid_cols
    num_mines <- input$num_mines
    
    # Store current game configuration
    game_state$rows <- rows
    game_state$cols <- input$grid_cols
    game_state$num_mines <- input$num_mines
    
    game_state$field <- generate_mine_field(rows, cols, num_mines)
    game_state$revealed <- Matrix(FALSE, nrow = rows, ncol = cols)
    game_state$flagged <- Matrix(FALSE, nrow = rows, ncol = cols)
    game_state$game_over <- FALSE
    game_state$game_won <- FALSE
  }
  
  # Initial game setup
  observe({
    # Only initialize if no game state exists
    if (is.null(game_state$field)) {
      initialize_game()
    }
  })
  
  # New game button
  observeEvent(input$new_game, {
    initialize_game()
  })
  
  # Render flag counter
  output$flag_counter <- renderUI({
    req(game_state$flagged)
    
    # Count current flags
    flag_count <- sum(game_state$flagged)
    total_mines <- game_state$num_mines
    
    div(
      class = "flag-counter",
      sprintf("Flags: %d / %d", flag_count, total_mines)
    )
  })
  
  # Reveal cell recursively
  reveal_cell <- function(r, c) {
    # Check bounds and if already revealed
    if (r < 1 || r > nrow(game_state$field) || 
        c < 1 || c > ncol(game_state$field) || 
        game_state$revealed[r, c]) {
      return()
    }
    
    # Mark cell as revealed
    game_state$revealed[r, c] <- TRUE
    
    # If mine, end game
    if (game_state$field[r, c] == -1) {
      game_state$game_over <- TRUE
      return()
    }
    
    # If empty cell, reveal surrounding cells
    if (game_state$field[r, c] == 0) {
      for (dr in -1:1) {
        for (dc in -1:1) {
          reveal_cell(r + dr, c + dc)
        }
      }
    }
  }
  
  # Cell click handler
  cell_click <- function(r, c) {
    # Ignore if game is over
    if (game_state$game_over || game_state$game_won) return()
    
    # Check game mode
    if (input$game_mode == "flag") {
      # If not revealed, toggle flag
      if (!game_state$revealed[r, c]) {
        game_state$flagged[r, c] <- !game_state$flagged[r, c]
      }
    } else {
      # Reveal cell if not flagged
      if (!game_state$flagged[r, c]) {
        reveal_cell(r, c)
        
        # Check win condition
        unrevealed_safe_cells <- sum(
          !game_state$revealed & game_state$field != -1
        )
        if (unrevealed_safe_cells == 0) {
          game_state$game_won <- TRUE
        }
      }
    }
  }
  
  # Dynamic cell click handlers
  observe({
    req(game_state$field)
    for (r in 1:nrow(game_state$field)) {
      for (c in 1:ncol(game_state$field)) {
        local({
          local_r <- r
          local_c <- c
          observeEvent(input[[paste0("cell_", local_r, "_", local_c)]], {
            cell_click(local_r, local_c)
          })
        })
      }
    }
  })
  
  # Render game grid
  output$game_grid <- renderUI({
    req(game_state$field)
    
    cells <- lapply(1:nrow(game_state$field), function(r) {
      lapply(1:ncol(game_state$field), function(c) {
        cell_value <- game_state$field[r, c]
        is_revealed <- game_state$revealed[r, c]
        is_flagged <- game_state$flagged[r, c]
        
        cell_class <- "cell"
        cell_content <- ""
        
        if (game_state$game_over && cell_value == -1) {
          cell_class <- paste(cell_class, "cell-mine")
          cell_content <- "💣"
        } else if (is_revealed) {
          cell_class <- paste(cell_class, "cell-revealed")
          if (cell_value > 0) {
            cell_content <- as.character(cell_value)
          }
        } else if (is_flagged) {
          cell_content <- "🚩"
          cell_class <- paste(cell_class, "flag")
        }
        
        # Add clickable class if not revealed and not game over
        if (!game_state$game_over && !game_state$game_won) {
          cell_class <- paste(cell_class, "cell-clickable")
        }
        
        actionLink(
          inputId = paste0("cell_", r, "_", c),
          label = cell_content,
          class = cell_class
        )
      })
    })
    
    # Flatten the list and wrap in a div
    cells_flat <- unlist(cells, recursive = FALSE)
    
    # Game status message
    status_message <- if (game_state$game_over) {
      "Game Over! You didn't sweep good enough :("
    } else if (game_state$game_won) {
      "Congratulations! You swept the mines!"
    } else {
      ""
    }
    
    div(
      div(class = "game-grid", cells_flat),
      div(style = "margin-top: 10px; color: red;", status_message)
    )
  })
}

shinyApp(ui, server)