# ==============================================================================
# Shiny App: ChickWeight Explorer
# A single-file app exploring the datasets::ChickWeight dataset
# ==============================================================================

# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(bslib)
library(plotly)  # For interactive hover tooltips

# Load the data
data("ChickWeight", package = "datasets")

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  theme = bs_theme(preset = "minty", version = 5),
  
  titlePanel("ChickWeight Dataset Explorer"),
  
  sidebarLayout(
    # --------------------------------------------------------------------------
    # SIDEBAR: Global Filters with Tooltips
    # --------------------------------------------------------------------------
    sidebarPanel(
      width = 3,
      
      h4("Filters"),
      
      # Diet filter with tooltip
      div(
        style = "display: flex; align-items: center; gap: 5px;",
        tags$label("Diet", `for` = "diet_filter"),
        bslib::tooltip(
          icon("circle-info"),
          "Diet types 1–4 represent different protein supplements. Select which diets to include in the analysis.",
          placement = "right"
        )
      ),
      checkboxGroupInput(
        "diet_filter",
        label = NULL,
        choices = 1:4,
        selected = 1:4,
        inline = TRUE
      ),
      
      # Time range slider with tooltip
      div(
        style = "display: flex; align-items: center; gap: 5px; margin-top: 15px;",
        tags$label("Time Range (days)", `for` = "time_range"),
        bslib::tooltip(
          icon("circle-info"),
          "Drag to focus on a specific growth interval. All summaries and plots respect this time window.",
          placement = "right"
        )
      ),
      sliderInput(
        "time_range",
        label = NULL,
        min = 0,
        max = 21,
        value = c(0, 21),
        step = 1
      ),
      
      # Chick highlight with tooltip
      div(
        style = "display: flex; align-items: center; gap: 5px; margin-top: 15px;",
        tags$label("Highlight Chicks", `for` = "chick_highlight"),
        bslib::tooltip(
          icon("circle-info"),
          "Type to find a Chick ID. Selected chicks are emphasized in plots and highlighted in the table.",
          placement = "right"
        )
      ),
      selectizeInput(
        "chick_highlight",
        label = NULL,
        choices = unique(ChickWeight$Chick),
        selected = NULL,
        multiple = TRUE,
        options = list(placeholder = "Select chick(s)...")
      ),
      
      # Smoothing option with tooltip
      div(
        style = "margin-top: 15px;",
        checkboxInput(
          "smoothing",
          label = div(
            "Add LOESS smoothing",
            bslib::tooltip(
              icon("circle-info"),
              "Show smoothed average trend by Diet with confidence ribbon. Use with caution for small sample sizes.",
              placement = "right"
            )
          ),
          value = FALSE
        )
      ),
      
      # Scale option with tooltip
      div(
        style = "margin-top: 15px;",
        tags$label("Y-axis Scale"),
        bslib::tooltip(
          icon("circle-info"),
          "Log scale can reveal proportional growth patterns and reduce the influence of outliers.",
          placement = "right"
        ),
        radioButtons(
          "scale",
          label = NULL,
          choices = c("Linear", "Log10"),
          selected = "Linear"
        )
      ),
      
      # Reset button
      actionButton(
        "reset",
        "Reset Filters",
        class = "btn-warning",
        style = "margin-top: 20px; width: 100%;"
      )
    ),
    
    # --------------------------------------------------------------------------
    # MAIN PANEL: Tabset
    # --------------------------------------------------------------------------
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "tabs",
        type = "pills",
        
        # ---- Tab 1: Overview ----
        tabPanel(
          "Overview",
          br(),
          fluidRow(
            column(4, uiOutput("summary_chicks")),
            column(4, uiOutput("summary_weight")),
            column(4, uiOutput("summary_timepoints"))
          ),
          hr(),
          div(
            h4(
              "About the Dataset",
              bslib::popover(
                icon("circle-info"),
                title = "Study Design",
                "This is a repeated-measures experiment tracking chick growth over time under four different diet regimens. Each chick is measured at regular intervals (days 0, 2, 4, ..., 21).",
                placement = "right"
              )
            ),
            p("The ChickWeight dataset contains measurements from an experiment on the effect of diet on early growth of chicks. 
              The body weights of chicks were measured at birth and every second day thereafter until day 21. 
              Four different protein diets were tested on different groups of newly hatched chicks."),
            p(strong("Variables:"), "Weight (grams), Time (days), Chick (ID), Diet (1–4)"),
            p(strong("Sample size:"), "50 chicks total across 4 diet groups"),
            p("Use the sidebar filters to focus on specific diets, time windows, or individual chicks. 
              All visualizations and summaries automatically update based on your selections.")
          )
        ),
        
        # ---- Tab 2: Growth Curves ----
        tabPanel(
          div(
            "Growth Curves",
            bslib::tooltip(
              icon("circle-info"),
              "Each line represents one chick's growth trajectory. Hover over lines to see details. Use the smoothing option to view average trends by diet.",
              placement = "top"
            )
          ),
          br(),
          uiOutput("growth_empty_state"),
          plotlyOutput("growth_plot", height = "600px")
        ),
        
        # ---- Tab 3: Diet Comparison ----
        tabPanel(
          div(
            "Diet Comparison",
            bslib::tooltip(
              icon("circle-info"),
              "Final weight is defined as the maximum weight observed within the selected time window for each chick.",
              placement = "top"
            )
          ),
          br(),
          uiOutput("diet_empty_state"),
          fluidRow(
            column(6, plotlyOutput("diet_boxplot", height = "400px")),
            column(6, plotlyOutput("diet_barplot", height = "400px"))
          )
        ),
        
        # ---- Tab 4: Chick Table ----
        tabPanel(
          div(
            "Chick Table",
            bslib::tooltip(
              icon("circle-info"),
              "Click column headers to sort. Use the search box to filter rows. Select rows to highlight chicks in the Growth Curves tab.",
              placement = "top"
            )
          ),
          br(),
          downloadButton("download_table", "Download CSV", class = "btn-primary"),
          br(), br(),
          DTOutput("chick_table")
        ),
        
        # ---- Tab 5: About / Help ----
        tabPanel(
          "About / Help",
          br(),
          h4("Interpretation Guide"),
          p("This app provides multiple perspectives on chick growth patterns:"),
          tags$ul(
            tags$li(strong("Growth Curves:"), "Show individual trajectories. Look for parallel growth patterns, 
                    divergence over time, or individual outliers."),
            tags$li(strong("Diet Comparison:"), "Summarizes final outcomes. Boxplots reveal distribution shape and outliers; 
                    bar charts show average performance."),
            tags$li(strong("Chick Table:"), "Provides quantitative metrics including growth rate (slope) and total weight gain (Δweight).")
          ),
          
          hr(),
          h4(
            "Caveats",
            bslib::popover(
              icon("triangle-exclamation"),
              title = "Important Considerations",
              HTML("<ul>
                <li>Some chicks have missing measurements at later timepoints</li>
                <li>Sample sizes vary by diet group</li>
                <li>LOESS smoothing can be misleading with small sample sizes</li>
                <li>Log scale is only appropriate for positive weights</li>
                </ul>"),
              placement = "right"
            )
          ),
          tags$ul(
            tags$li(strong("Missing data:"), "Not all chicks were measured at all timepoints. 
                    The 'final weight' is the maximum observed within your selected time window."),
            tags$li(strong("Small samples:"), "Some diet groups have fewer chicks. Be cautious when comparing groups of very different sizes."),
            tags$li(strong("Smoothing caveats:"), "LOESS smoothing assumes smooth, continuous growth. 
                    It may overfit with small samples or hide important individual variation."),
            tags$li(strong("Log scale:"), "Useful for comparing proportional growth rates across different starting weights. 
                    However, it can make small absolute differences appear large.")
          ),
          
          hr(),
          h4("Tips"),
          tags$ul(
            tags$li("Start with all diets selected to get the full picture, then narrow down."),
            tags$li("Use the time slider to focus on specific growth phases (e.g., days 10–21 for late growth)."),
            tags$li("Highlight specific chicks to track individuals across multiple views."),
            tags$li("Row selection in the table automatically highlights those chicks in the growth curves plot.")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # Reactive: Filtered Dataset
  # ----------------------------------------------------------------------------
  filtered_data <- reactive({
    req(input$diet_filter, input$time_range)
    
    ChickWeight %>%
      filter(
        Diet %in% input$diet_filter,
        Time >= input$time_range[1],
        Time <= input$time_range[2]
      )
  })
  
  # ----------------------------------------------------------------------------
  # Reactive: Highlighted Chicks (from sidebar OR table selection)
  # ----------------------------------------------------------------------------
  highlighted_chicks <- reactive({
    # Combine sidebar selection and table row selection
    sidebar_selected <- input$chick_highlight
    table_selected <- if (!is.null(input$chick_table_rows_selected)) {
      summary_table()[input$chick_table_rows_selected, ]$Chick
    } else {
      NULL
    }
    unique(c(sidebar_selected, as.character(table_selected)))
  })
  
  # ----------------------------------------------------------------------------
  # Reactive: Per-Chick Summary Metrics
  # ----------------------------------------------------------------------------
  summary_table <- reactive({
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      group_by(Chick, Diet) %>%
      summarise(
        n_timepoints = n(),
        first_weight = min(weight, na.rm = TRUE),
        last_weight = max(weight, na.rm = TRUE),
        delta_weight = last_weight - first_weight,
        slope = if (n() > 1) {
          coef(lm(weight ~ Time))[2]
        } else {
          NA_real_
        },
        .groups = "drop"
      ) %>%
      mutate(
        slope = round(slope, 2),
        delta_weight = round(delta_weight, 1),
        first_weight = round(first_weight, 1),
        last_weight = round(last_weight, 1)
      )
  })
  
  # ----------------------------------------------------------------------------
  # Observer: Reset Filters
  # ----------------------------------------------------------------------------
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "diet_filter", selected = 1:4)
    updateSliderInput(session, "time_range", value = c(0, 21))
    updateSelectizeInput(session, "chick_highlight", selected = character(0))
    updateCheckboxInput(session, "smoothing", value = FALSE)
    updateRadioButtons(session, "scale", selected = "Linear")
  })
  
  # ----------------------------------------------------------------------------
  # TAB 1: Overview - Summary Cards
  # ----------------------------------------------------------------------------
  output$summary_chicks <- renderUI({
    n_chicks <- filtered_data() %>% pull(Chick) %>% unique() %>% length()
    div(
      class = "card text-center",
      style = "background-color: #e3f2fd; padding: 20px;",
      h3(n_chicks, style = "color: #1976d2; margin: 0;"),
      p("Visible Chicks", style = "margin: 5px 0 0 0;")
    )
  })
  
  output$summary_weight <- renderUI({
    avg_terminal <- filtered_data() %>%
      group_by(Chick) %>%
      summarise(terminal = max(weight, na.rm = TRUE)) %>%
      pull(terminal) %>%
      mean(na.rm = TRUE) %>%
      round(1)
    
    div(
      class = "card text-center",
      style = "background-color: #f3e5f5; padding: 20px;",
      h3(paste0(avg_terminal, " g"), style = "color: #7b1fa2; margin: 0;"),
      p("Avg Terminal Weight", style = "margin: 5px 0 0 0;")
    )
  })
  
  output$summary_timepoints <- renderUI({
    n_points <- filtered_data() %>% pull(Time) %>% unique() %>% length()
    div(
      class = "card text-center",
      style = "background-color: #e8f5e9; padding: 20px;",
      h3(n_points, style = "color: #388e3c; margin: 0;"),
      p("Timepoints in Window", style = "margin: 5px 0 0 0;")
    )
  })
  
  # ----------------------------------------------------------------------------
  # TAB 2: Growth Curves Plot
  # ----------------------------------------------------------------------------
  output$growth_empty_state <- renderUI({
    if (nrow(filtered_data()) == 0) {
      div(
        class = "alert alert-warning",
        icon("triangle-exclamation"),
        " No data available with current filter settings. Try adjusting the filters."
      )
    }
  })
  
  output$growth_plot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    # Prepare data with highlighting
    plot_data <- filtered_data() %>%
      mutate(
        highlight = if (length(highlighted_chicks()) > 0) {
          Chick %in% highlighted_chicks()
        } else {
          FALSE
        },
        alpha_val = ifelse(highlight, 1, 0.4),
        size_val = ifelse(highlight, 1.2, 0.6)
      )
    
    # Add slope to hover text
    slope_data <- summary_table() %>%
      select(Chick, slope)
    
    plot_data <- plot_data %>%
      left_join(slope_data, by = "Chick") %>%
      mutate(
        hover_text = paste0(
          "Chick: ", Chick, "<br>",
          "Diet: ", Diet, "<br>",
          "Time: ", Time, " days<br>",
          "Weight: ", round(weight, 1), " g<br>",
          "Slope: ", ifelse(!is.na(slope), paste0(slope, " g/day"), "N/A")
        )
      )
    
    # Base plot
    p <- ggplot(plot_data, aes(x = Time, y = weight, group = Chick, color = factor(Diet))) +
      geom_line(aes(alpha = alpha_val, size = size_val, text = hover_text)) +
      scale_alpha_identity() +
      scale_size_identity() +
      scale_color_brewer(palette = "Set1", name = "Diet") +
      labs(
        title = "Chick Growth Trajectories",
        x = "Time (days)",
        y = "Weight (grams)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
    
    # Add LOESS smoothing if requested
    if (input$smoothing) {
      p <- p + geom_smooth(
        aes(group = Diet, color = factor(Diet)),
        method = "loess",
        se = TRUE,
        alpha = 0.2,
        linewidth = 1.5,
        inherit.aes = FALSE,
        data = filtered_data() %>% mutate(Diet = factor(Diet))
      )
    }
    
    # Apply scale transformation
    if (input$scale == "Log10") {
      # Filter out non-positive weights for log scale
      plot_data_log <- plot_data %>% filter(weight > 0)
      if (nrow(plot_data_log) < nrow(plot_data)) {
        showNotification("Some weights ≤ 0 were excluded for log scale.", type = "warning")
      }
      p <- p + scale_y_log10()
    }
    
    # Convert to plotly for interactivity
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })
  
  # ----------------------------------------------------------------------------
  # TAB 3: Diet Comparison
  # ----------------------------------------------------------------------------
  output$diet_empty_state <- renderUI({
    if (nrow(filtered_data()) == 0) {
      div(
        class = "alert alert-warning",
        icon("triangle-exclamation"),
        " No data available with current filter settings. Try adjusting the filters."
      )
    }
  })
  
  output$diet_boxplot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    # Get final weight per chick
    final_weights <- filtered_data() %>%
      group_by(Chick, Diet) %>%
      summarise(final_weight = max(weight, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(final_weights, aes(x = factor(Diet), y = final_weight, fill = factor(Diet))) +
      geom_violin(alpha = 0.6) +
      geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA) +
      geom_jitter(width = 0.1, alpha = 0.4, size = 2) +
      scale_fill_brewer(palette = "Set1", name = "Diet") +
      labs(
        title = "Final Weight Distribution by Diet",
        x = "Diet",
        y = "Final Weight (grams)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      layout(hovermode = "closest")
  })
  
  output$diet_barplot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    # Mean delta weight by diet
    diet_summary <- summary_table() %>%
      group_by(Diet) %>%
      summarise(
        mean_delta = mean(delta_weight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_delta))
    
    p <- ggplot(diet_summary, aes(x = reorder(factor(Diet), mean_delta), y = mean_delta, fill = factor(Diet))) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = round(mean_delta, 1)), vjust = -0.5, size = 4) +
      scale_fill_brewer(palette = "Set1", name = "Diet") +
      labs(
        title = "Mean Weight Gain by Diet",
        x = "Diet (ranked)",
        y = "Mean Δ Weight (grams)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      layout(hovermode = "closest")
  })
  
  # ----------------------------------------------------------------------------
  # TAB 4: Chick Table
  # ----------------------------------------------------------------------------
  output$chick_table <- renderDT({
    req(nrow(summary_table()) > 0)
    
    datatable(
      summary_table(),
      options = list(
        pageLength = 15,
        searching = TRUE,
        ordering = TRUE,
        dom = 'frtip'
      ),
      selection = "multiple",
      rownames = FALSE,
      colnames = c(
        "Chick", "Diet", "N Timepoints", "First Weight (g)",
        "Last Weight (g)", "Δ Weight (g)", "Slope (g/day)"
      )
    ) %>%
      formatStyle(
        "Chick",
        target = "row",
        backgroundColor = styleEqual(
          highlighted_chicks(),
          rep("lightyellow", length(highlighted_chicks()))
        )
      )
  })
  
  # Download handler for table
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("chick_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(summary_table(), file, row.names = FALSE)
    }
  )
}

# ==============================================================================
# Run the App
# ==============================================================================
shinyApp(ui = ui, server = server)