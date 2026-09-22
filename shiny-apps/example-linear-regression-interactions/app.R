library(shiny)

interaction_students <- function(center = 5) {
  set.seed(2026)

  students <- data.frame(
    hours = runif(120, min = 1, max = 9),
    method = factor(
      rep(c("Self-study", "Workshop"), each = 60),
      levels = c("Self-study", "Workshop")
    )
  )

  workshop <- as.numeric(students$method == "Workshop")
  students$score <- 65 +
    3 * (students$hours - 5) +
    4 * workshop +
    2 * (students$hours - 5) * workshop +
    rnorm(120, mean = 0, sd = 3)
  students$hours_centered <- students$hours - center
  students
}

fit_interaction_model <- function(center = 5, include_interaction = TRUE) {
  students <- interaction_students(center)
  formula <- if (include_interaction) {
    score ~ hours_centered * method
  } else {
    score ~ hours_centered + method
  }
  lm(formula, data = students)
}

interaction_predictions <- function(model, center, hours) {
  new_data <- expand.grid(
    hours = hours,
    method = c("Self-study", "Workshop")
  )
  new_data$method <- factor(
    new_data$method,
    levels = c("Self-study", "Workshop")
  )
  new_data$hours_centered <- new_data$hours - center
  new_data$predicted_score <- as.numeric(predict(model, newdata = new_data))
  new_data
}

interaction_slopes <- function(model) {
  coefficients <- coef(model)
  reference_slope <- unname(coefficients[["hours_centered"]])
  interaction_name <- "hours_centered:methodWorkshop"
  slope_difference <- if (interaction_name %in% names(coefficients)) {
    unname(coefficients[[interaction_name]])
  } else {
    0
  }

  c(
    "Self-study" = reference_slope,
    "Workshop" = reference_slope + slope_difference
  )
}

interaction_state <- function(
  center = 5,
  include_interaction = TRUE,
  prediction_hour = 8,
  line_hours = seq(1, 9, length.out = 81)
) {
  students <- interaction_students(center)
  model <- fit_interaction_model(center, include_interaction)

  list(
    students = students,
    model = model,
    coefficients = coef(model),
    slopes = interaction_slopes(model),
    selected_predictions = interaction_predictions(model, center, prediction_hour),
    comparison_predictions = interaction_predictions(model, center, c(2, 5, 8)),
    line_predictions = interaction_predictions(model, center, line_hours)
  )
}

app_default_state <- function() {
  state <- interaction_state()
  list(
    coefficients = state$coefficients,
    slopes = state$slopes,
    comparison_predictions = state$comparison_predictions,
    line_predictions = state$line_predictions
  )
}

format_coefficient_name <- function(name) {
  labels <- c(
    "(Intercept)" = "Self-study prediction at the centering value",
    "hours_centered" = "Self-study hourly slope",
    "methodWorkshop" = "Workshop difference at the centering value",
    "hours_centered:methodWorkshop" = "Workshop slope difference"
  )
  unname(labels[[name]] %||% name)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f6f7f9; color: #20252b; }
      .app-shell { max-width: 1180px; margin: 0 auto; padding: 24px 16px 48px; }
      .app-header { margin-bottom: 22px; }
      .app-header h1 { margin: 0 0 6px; font-family: Georgia, serif; font-size: 2rem; }
      .app-header p { max-width: 760px; color: #52606d; }
      .control-panel { background: #fff; border: 1px solid #d9dee5; padding: 20px; }
      .output-panel { background: #fff; border: 1px solid #d9dee5; padding: 20px; margin-bottom: 18px; }
      .output-panel h2 { margin: 0 0 16px; font-size: 1.2rem; }
      .formula-note { border-left: 4px solid #466f99; background: #eef4f8; padding: 10px 12px; margin-bottom: 14px; }
      .slope-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px; }
      .slope-value { border-top: 3px solid #466f99; background: #f7f9fb; padding: 12px; }
      .slope-value.workshop { border-color: #b85c20; }
      .slope-value strong { display: block; font-size: 1.45rem; }
      .prediction-table { width: 100%; border-collapse: collapse; }
      .prediction-table th, .prediction-table td { border-bottom: 1px solid #e2e6ea; padding: 9px; text-align: left; }
      .limitation { color: #5b6570; font-size: .92rem; margin-top: 12px; }
      @media (max-width: 767px) {
        .app-header h1 { font-size: 1.55rem; }
        .control-panel { margin-bottom: 18px; }
        .slope-grid { grid-template-columns: 1fr; }
      }
    "))
  ),
  div(
    class = "app-shell",
    div(
      class = "app-header",
      h1("Interactions in Linear Regression"),
      p("Compare a model that allows study-hour slopes to differ by study method with one that requires parallel slopes.")
    ),
    fluidRow(
      column(
        width = 4,
        div(
          class = "control-panel",
          radioButtons(
            "model_type",
            "Model specification",
            choices = c(
              "Different slopes (interaction)" = "interaction",
              "Parallel slopes (no interaction)" = "parallel"
            ),
            selected = "interaction"
          ),
          sliderInput(
            "center",
            "Center study hours at",
            min = 1,
            max = 9,
            value = 5,
            step = 0.5
          ),
          sliderInput(
            "prediction_hour",
            "Compare methods at this many hours",
            min = 1,
            max = 9,
            value = 8,
            step = 0.5
          ),
          checkboxInput("show_points", "Show the synthetic student data", value = FALSE),
          p(
            class = "limitation",
            "The data are synthetic. Fitted associations do not show that either study method causes higher scores."
          )
        )
      ),
      column(
        width = 8,
        div(
          class = "output-panel",
          h2("Fitted scores"),
          plotOutput("interaction_plot", height = "390px")
        ),
        fluidRow(
          column(
            width = 6,
            div(
              class = "output-panel",
              h2("Slope interpretation"),
              uiOutput("formula_note"),
              uiOutput("slope_summary")
            )
          ),
          column(
            width = 6,
            div(
              class = "output-panel",
              h2("Selected-hour comparison"),
              tableOutput("prediction_table")
            )
          )
        ),
        div(
          class = "output-panel",
          h2("Coefficient estimates"),
          tableOutput("coefficient_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  state <- reactive({
    interaction_state(
      center = input$center,
      include_interaction = identical(input$model_type, "interaction"),
      prediction_hour = input$prediction_hour
    )
  })

  output$interaction_plot <- renderPlot({
    current <- state()
    line_data <- current$line_predictions
    self_study <- line_data[line_data$method == "Self-study", ]
    workshop <- line_data[line_data$method == "Workshop", ]

    plot(
      predicted_score ~ hours,
      data = self_study,
      type = "l",
      col = "steelblue4",
      lwd = 3,
      ylim = range(c(line_data$predicted_score, current$students$score)),
      xlab = "Study hours",
      ylab = "Score"
    )
    lines(
      predicted_score ~ hours,
      data = workshop,
      col = "darkorange3",
      lwd = 3,
      lty = 2
    )
    if (isTRUE(input$show_points)) {
      point_colors <- ifelse(current$students$method == "Self-study", "#466f9966", "#b85c2066")
      points(current$students$hours, current$students$score, pch = 16, col = point_colors)
      lines(predicted_score ~ hours, data = self_study, col = "steelblue4", lwd = 3)
      lines(predicted_score ~ hours, data = workshop, col = "darkorange3", lwd = 3, lty = 2)
    }
    legend(
      "topleft",
      legend = c("Self-study", "Workshop"),
      col = c("steelblue4", "darkorange3"),
      lwd = 3,
      lty = c(1, 2),
      bty = "n"
    )
  })

  output$formula_note <- renderUI({
    formula_text <- if (identical(input$model_type, "interaction")) {
      "score ~ hours_centered * method"
    } else {
      "score ~ hours_centered + method"
    }
    div(
      class = "formula-note",
      strong(if (identical(input$model_type, "interaction")) "Different slopes" else "Parallel slopes"),
      tags$br(),
      tags$code(formula_text)
    )
  })

  output$slope_summary <- renderUI({
    slopes <- state()$slopes
    div(
      class = "slope-grid",
      div(class = "slope-value", span("Self-study"), strong(sprintf("%.2f", slopes[["Self-study"]])), span("points per hour")),
      div(class = "slope-value workshop", span("Workshop"), strong(sprintf("%.2f", slopes[["Workshop"]])), span("points per hour"))
    )
  })

  output$prediction_table <- renderTable({
    predictions <- state()$selected_predictions
    data.frame(
      Method = predictions$method,
      Hours = predictions$hours,
      `Predicted score` = round(predictions$predicted_score, 1),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$coefficient_table <- renderTable({
    estimates <- coef(state()$model)
    data.frame(
      Term = vapply(names(estimates), format_coefficient_name, character(1)),
      Estimate = round(unname(estimates), 2),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

if (!identical(tolower(Sys.getenv("SHINY_APP_VALIDATE")), "true")) {
  shinyApp(ui, server)
}
