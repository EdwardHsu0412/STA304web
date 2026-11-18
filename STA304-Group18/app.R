library(shiny)
library(ggplot2)
source("analysis.R", local = TRUE)

responses <- load_responses("data/responses.csv")
palettes <- list(
  teal = c(Domestic = "#14776B", International = "#B4512D"),
  blue = c(Domestic = "#285AA0", International = "#8C620F"),
  gray = c(Domestic = "#263531", International = "#6C7773")
)
colour_ids <- c(Domestic = "domestic_colour", International = "international_colour")

metric <- function(label, value) {
  tags$div(class = "metric", tags$dt(label), tags$dd(value))
}

data_table <- function(data, title) {
  tags$div(
    class = "table-scroll", role = "region", "aria-label" = title, tabindex = "0",
    tags$table(
      class = "study-table",
      tags$caption(class = "sr-only", title),
      tags$thead(tags$tr(lapply(names(data), function(name) tags$th(scope = "col", name)))),
      tags$tbody(lapply(seq_len(nrow(data)), function(i) {
        tags$tr(
          tags$th(scope = "row", data[[1]][i]),
          lapply(data[-1], function(column) tags$td(column[i]))
        )
      }))
    )
  )
}

study_theme <- function() {
  theme_minimal(base_family = "sans", base_size = 13) +
    theme(
      axis.title = element_text(color = "#243A33", face = "bold", size = 11),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(color = "#41564F"),
      panel.grid.major = element_line(color = "#E4EBE5", linewidth = 0.45),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(color = "#243A33", size = 10, face = "bold"),
      legend.text = element_text(color = "#41564F", size = 10),
      plot.margin = margin(8, 15, 6, 7)
    )
}

ui <- navbarPage(
  title = tagList(
    tags$a(class = "skip-link", href = "#study-content", "Skip to the study"),
    span(
      class = "site-brand",
      span(class = "course-mark", "STA304"),
      span("Social media & student GPA")
    )
  ),
  windowTitle = "Social media and student GPA | STA304",
  lang = "en",
  id = "page",
  header = tagList(
    tags$head(
      tags$meta(name = "description", content = paste(
        "Explore social media use, study habits, and self-reported cGPA in a Fall 2026 STA304 survey."
      )),
      tags$link(rel = "stylesheet", href = "study.css", type = "text/css"),
      tags$script(src = "study.js", defer = NA)
    ),
    tags$aside(
      class = "control-panel", "aria-label" = "Study controls",
      conditionalPanel(
        "input.page === 'explore'",
        h2("Chart controls"),
        selectInput(
          "chart", "Chart",
          choices = c(
            "GPA and reported time" = "time",
            "GPA satisfaction" = "satisfaction",
            "Distribution of reported time" = "distribution"
          ),
          selectize = FALSE, width = "100%"
        ),
        conditionalPanel(
          "input.chart === 'time' || input.chart === 'distribution'",
          selectInput(
            "measure", "Time measure",
            choices = setNames(names(time_labels), time_labels),
            selectize = FALSE, width = "100%"
          )
        )
      ),
      conditionalPanel("input.page === 'regression'", h2("Analysis controls")),
      selectInput(
        "status", "Student status",
        choices = c("All students" = "all", status_levels),
        selectize = FALSE, width = "100%"
      ),
      div(
        class = "filter-scope", "aria-live" = "polite",
        textOutput("sample_scope", container = tags$span)
      ),
      tags$details(
        class = "display-options",
        tags$summary("Plot appearance"),
        selectInput(
          "palette", "Plot colours",
          choices = c(
            "Teal and rust" = "teal", "Blue and gold" = "blue",
            "Grayscale" = "gray", "Custom colours" = "custom"
          ),
          selectize = FALSE, width = "100%"
        ),
        conditionalPanel(
          "input.palette === 'custom'",
          div(
            class = "custom-colours",
            lapply(status_levels, function(group) {
              id <- colour_ids[[group]]
              div(
                tags$label(`for` = id, group),
                tags$input(id = id, type = "color", value = palettes$teal[[group]],
                           `data-group-colour` = "true")
              )
            })
          )
        ),
        conditionalPanel(
          "input.page === 'explore' && input.chart === 'time'",
          sliderInput(
            "gpa_range", "Visible cGPA range",
            min = 0, max = 4, value = c(2, 4), step = 0.25, width = "100%"
          ),
          checkboxInput("trend", "Show straight-line fit", TRUE)
        ),
        conditionalPanel(
          "input.page === 'explore' && input.chart === 'satisfaction'",
          radioButtons(
            "bar_measure", "Show bars as",
            choices = c("Percentage within each group" = "percent", "Response count" = "count"),
            selected = "percent", width = "100%"
          )
        ),
        conditionalPanel(
          "input.page === 'explore' && input.chart === 'distribution'",
          sliderInput(
            "bins", "Number of bins",
            min = 4, max = 20, value = 10, step = 1, width = "100%"
          )
        )
      ),
      div(class = "course-context", "Group 18 \u00b7 UTM \u00b7 Fall 2026")
    )
  ),
  tabPanel(
    "Explore", value = "explore",
    div(
      class = "study-page explore-page",
      tags$section(
        class = "card chart-card", "aria-label" = "Selected chart",
        conditionalPanel(
          "input.chart === 'time'",
          tags$figure(
            tags$figcaption(textOutput("scatter_title", container = tags$h1)),
            plotOutput("scatter", height = "410px")
          ),
          div(
            class = "insight",
            h2("What the scatterplot shows"),
            uiOutput("correlation_text")
          )
        ),
        conditionalPanel(
          "input.chart === 'satisfaction'",
          tags$figure(
            tags$figcaption(h1("Satisfaction with cGPA")),
            plotOutput("satisfaction", height = "410px")
          ),
          div(
            class = "insight",
            h2("What the satisfaction ratings show"),
            uiOutput("satisfaction_text")
          )
        ),
        conditionalPanel(
          "input.chart === 'distribution'",
          tags$figure(
            tags$figcaption(textOutput("distribution_title", container = tags$h1)),
            plotOutput("distribution", height = "410px")
          ),
          div(
            class = "insight",
            h2("What the distribution shows"),
            textOutput("distribution_text", container = tags$p)
          )
        )
      ),
      uiOutput("sample_metrics"),
      tags$details(
        class = "card summary-card",
        tags$summary("Summary statistics"),
        uiOutput("descriptive_table")
      )
    )
  ),
  tabPanel(
    "Analysis", value = "regression",
    div(
      class = "study-page analysis-page",
      h1("Survey results"),
      tags$section(
        class = "card model-card",
        h2("Daily social media use and cGPA"),
        uiOutput("total_association")
      ),
      tags$section(
        class = "card model-card",
        h2("GPA after accounting for daily habits"),
        p(
          class = "section-intro",
          "The model estimates how cGPA varies with study time, three kinds of social media use,",
          "time off social media before bed, and student status when both groups are selected."
        ),
        uiOutput("model_summary"),
        h3("Coefficient estimates"),
        uiOutput("coefficient_table"),
        p(
          class = "table-note",
          "Time estimates are in cGPA points per extra daily hour or per extra minute before bed.",
          "Each estimate holds the other model variables fixed."
        )
      ),
      tags$section(
        class = "card model-card",
        h2("GPA by student status"),
        p(class = "section-intro", "These group summaries show the observed grades before accounting for daily habits."),
        uiOutput("status_table")
      ),
      tags$section(
        class = "card model-card",
        h2("Model diagnostics"),
        div(
          class = "diagnostic-grid",
          tags$figure(
            tags$figcaption("Residuals vs. fitted grades"),
            plotOutput("linearity", height = "230px")
          ),
          tags$figure(
            tags$figcaption("Residual spread"),
            plotOutput("variance", height = "230px")
          ),
          tags$figure(
            tags$figcaption("Normal Q\u2013Q plot"),
            plotOutput("normality", height = "230px")
          )
        ),
        h3("Interpretation of model assumptions"),
        uiOutput("assumption_text")
      )
    )
  ),
  tabPanel(
    "About", value = "about",
    div(
      class = "study-page about-page",
      tags$section(
        class = "card about-card",
        h1("About the survey"),
        p(paste(
          "Group 18 studied students enrolled in STA304H5 at the University of Toronto",
          "Mississauga in Fall 2026. Volunteers answered a Google Forms questionnaire shared",
          "on the course discussion board. The group also exchanged survey responses with peer groups."
        )),
        h2("Research questions"),
        tags$ol(
          tags$li("Is total daily social media use associated with self-reported cumulative GPA (cGPA)?"),
          tags$li("Do domestic and international respondents differ in cGPA after accounting for study time and social media habits?")
        ),
        h2("How the data were prepared"),
        p(paste(
          "The exploratory analysis kept 64 of 72 responses (42 domestic and 22 international).",
          "It excluded seven responses after reviewing implausible daily time reports and one with a reported cGPA of 0.0.",
          "The website uses those same 64 responses and omits student IDs."
        )),
        p(paste(
          "Total daily social media use adds hours spent on short-form video, text-based platforms,",
          "and calls and messaging. The model uses those three measures individually."
        )),
        h2("What the sample can tell us"),
        p(paste(
          "Participation was voluntary. Respondents may differ from other students in the course.",
          "Self-reported grades and hours can reflect rounding or recall error.",
          "The findings describe associations in this sample and leave cause and effect open."
        ))
      )
    )
  )
)
ui <- htmltools::tagQuery(ui)$find(".tab-content")$
  addAttrs(id = "study-content", role = "main", tabindex = "-1")$allTags()

server <- function(input, output, session) {
  selected <- reactive({
    req(input$status)
    if (input$status == "all") {
      responses
    } else {
      responses[responses$status == input$status, , drop = FALSE]
    }
  })
  colours <- reactive({
    req(input$palette)
    if (input$palette != "custom") {
      return(palettes[[input$palette]])
    }
    vapply(status_levels, function(group) {
      colour <- input[[colour_ids[[group]]]]
      if (length(colour) == 1L && grepl("^#[[:xdigit:]]{6}$", colour)) {
        colour
      } else {
        palettes$teal[[group]]
      }
    }, character(1))
  })
  association <- reactive({
    req(input$measure)
    correlation_result(selected(), input$measure)
  })
  fit <- reactive(fit_regression(selected()))

  output$sample_scope <- renderText(
    sprintf("%d of %d usable responses", nrow(selected()), nrow(responses))
  )
  output$sample_metrics <- renderUI({
    data <- selected()
    tags$dl(
      class = "metric-strip",
      metric("Responses shown", nrow(data)),
      metric("Median social media use", paste0(dec(median(data$total_hours), 1), " h/day")),
      metric("Median cGPA", dec(median(data$gpa)))
    )
  })

  output$scatter_title <- renderText({
    req(input$measure)
    paste(time_labels[[input$measure]], "& cGPA")
  })
  output$scatter <- renderPlot({
    req(input$measure, input$gpa_range)
    measure <- input$measure
    limits <- input$gpa_range
    validate(need(diff(limits) > 0, "Choose a cGPA range with two different endpoints."))
    visible_count <- sum(selected()$gpa >= limits[1] & selected()$gpa <= limits[2])
    validate(need(visible_count > 0, "Widen the cGPA range to include responses."))
    plot <- ggplot(selected(), aes(x = .data[[measure]], y = gpa))
    if (isTRUE(input$trend) && !is.null(association())) {
      plot <- plot + geom_smooth(
        method = "lm", formula = y ~ x, se = FALSE, color = "#263D35",
        linewidth = 0.85, linetype = "longdash"
      )
    }
    plot +
      geom_point(aes(color = status, shape = status), size = 3.2, alpha = 0.82) +
      scale_color_manual(values = colours()) +
      scale_shape_manual(values = c(Domestic = 16, International = 17)) +
      scale_y_continuous(breaks = seq(0, 4, by = 0.5)) +
      coord_cartesian(ylim = limits) +
      labs(
        x = paste0(time_labels[[measure]], " (", time_units[[measure]], ")"),
        y = "Self-reported cGPA (0\u20134)", color = "Student status", shape = "Student status"
      ) +
      study_theme()
  }, alt = "Scatterplot of self-reported GPA versus the selected time measure. An interpretation and correlation appear below.")

  correlation_reading <- function(result, explain_range = FALSE) {
    validate(need(!is.null(result), "The correlation needs at least four responses with varying grades and times."))
    r <- unname(result$estimate)
    if (abs(r) < 0.1) {
      pattern <- "Reported time and cGPA show almost no straight-line relationship in this selection."
    } else {
      strength <- if (abs(r) < 0.3) "weak" else if (abs(r) < 0.5) "moderate" else "strong"
      direction <- if (r < 0) "lower" else "higher"
      pattern <- sprintf(
        "The responses suggest a %s straight-line pattern. Higher reported time tends to go with %s grades.",
        strength, direction
      )
    }
    uncertainty <- if (result$conf.int[1] <= 0 && result$conf.int[2] >= 0) {
      "The data leave open both a positive and a negative relationship."
    } else {
      "The uncertainty range lies on one side of zero."
    }
    tagList(
      p(pattern, uncertainty,
        if (explain_range) "The correlation uses all responses in the selected group.",
        class = "interpretation"),
      div(
        class = "result-line",
        span(paste("Pearson r", dec(r, 3))),
        span(paste("95% CI", interval_label(result$conf.int[1], result$conf.int[2]))),
        span(paste("p-value", p_label(result$p.value)))
      )
    )
  }
  output$correlation_text <- renderUI(correlation_reading(association(), explain_range = TRUE))
  output$total_association <- renderUI(
    correlation_reading(correlation_result(selected(), "total_hours"))
  )

  output$distribution_title <- renderText({
    req(input$measure)
    paste("Distribution of", tolower(time_labels[[input$measure]]))
  })
  output$distribution <- renderPlot({
    req(input$measure, input$bins)
    data <- selected()
    measure <- input$measure
    ggplot(data, aes(x = .data[[measure]], fill = status)) +
      geom_histogram(bins = input$bins, boundary = 0, color = "#FFFFFF", linewidth = 0.35) +
      geom_vline(xintercept = median(data[[measure]]), linetype = "longdash",
                 color = "#263D35", linewidth = 0.85) +
      scale_fill_manual(values = colours()) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
      labs(
        x = paste0(time_labels[[measure]], " (", time_units[[measure]], ")"),
        y = "Responses", fill = "Student status"
      ) +
      study_theme() +
      theme(panel.grid.major.x = element_blank())
  }, alt = "Histogram of the selected time measure, with a dashed vertical line at its median.")
  output$distribution_text <- renderText({
    req(input$measure)
    values <- selected()[[input$measure]]
    unit <- time_units[[input$measure]]
    middle <- quantile(values, c(0.25, 0.75), type = 6)
    sprintf(
      "The middle half of reported times fell between %s and %s %s. The dashed line marks the median of %s %s.",
      dec(middle[1], 1), dec(middle[2], 1), unit, dec(median(values), 1), unit
    )
  })

  satisfaction_counts <- reactive(satisfaction_summary(selected()))
  output$satisfaction <- renderPlot({
    req(input$bar_measure)
    counts <- satisfaction_counts()
    percent <- input$bar_measure == "percent"
    values <- counts[[input$bar_measure]]
    counts$bar_value <- values
    counts$bar_label <- if (percent) paste0(dec(values, 1), "%") else as.character(values)
    dodge <- position_dodge(width = 0.78)
    ggplot(counts, aes(x = satisfaction, y = bar_value, group = status)) +
      geom_col(aes(fill = status), width = 0.68, position = dodge) +
      geom_text(
        aes(label = bar_label), position = dodge, vjust = -0.6,
        size = 3.3, fontface = "bold", color = "#293E36"
      ) +
      scale_fill_manual(values = colours()) +
      scale_y_continuous(
        labels = if (percent) function(x) paste0(x, "%") else waiver(),
        expand = expansion(mult = c(0, 0.14))
      ) +
      labs(
        x = "Satisfaction rating", y = if (percent) "Share within group" else "Responses",
        fill = "Student status"
      ) +
      study_theme() +
      theme(panel.grid.major.x = element_blank())
  }, alt = "Grouped bar chart of low, moderate, and high GPA satisfaction. Group results appear below.")

  output$satisfaction_text <- renderUI({
    counts <- satisfaction_counts()
    groups <- split(counts, counts$status, drop = TRUE)
    lines <- lapply(groups, function(group) {
      top <- group$count == max(group$count)
      level <- paste(as.character(group$satisfaction[top]), collapse = " and ")
      share <- sprintf("%d of %d (%s%%)", max(group$count), sum(group$count), dec(max(group$percent), 1))
      sprintf("For %s respondents, %s was the most common rating (%s).", group$status[1], tolower(level), share)
    })
    p(paste(unlist(lines), collapse = " "), "Percentages use the total within each student-status group.",
      class = "interpretation")
  })

  output$descriptive_table <- renderUI({
    data <- selected()
    labels <- c(gpa = "cGPA (0\u20134)", satisfaction = "Satisfaction (1\u201310)",
                setNames(paste0(time_labels, " (", time_units, ")"), names(time_labels)))
    measures <- data[names(labels)]
    data_table(
      data.frame(
        Measure = unname(labels),
        Mean = dec(vapply(measures, mean, numeric(1))),
        Median = dec(vapply(measures, median, numeric(1))),
        SD = dec(vapply(measures, sd, numeric(1))),
        IQR = dec(vapply(measures, IQR, numeric(1), type = 6)),
        N = nrow(data),
        row.names = NULL
      ),
      "Summary statistics for selected respondents"
    )
  })

  output$status_table <- renderUI({
    group_gpa <- split(selected()$gpa, selected()$status, drop = TRUE)
    data_table(
      data.frame(
        "Student status" = names(group_gpa),
        N = lengths(group_gpa),
        "Mean cGPA" = dec(vapply(group_gpa, mean, numeric(1))),
        "Median cGPA" = dec(vapply(group_gpa, median, numeric(1))),
        SD = dec(vapply(group_gpa, sd, numeric(1))),
        IQR = dec(vapply(group_gpa, IQR, numeric(1), type = 6)),
        row.names = NULL, check.names = FALSE
      ),
      "Observed cGPA by student status"
    )
  })

  output$model_summary <- renderUI({
    model <- fit()
    validate(need(!is.null(model), "This selection has too little variation to fit the regression."))
    coefs <- coef(model)
    explained <- summary(model)$r.squared
    overall_p <- regression_test(model)
    if ("statusInternational" %in% names(coefs)) {
      gap <- unname(coefs["statusInternational"])
      status_ci <- confint(model, "statusInternational")[1, ]
      direction <- if (gap < 0) "lower" else "higher"
      comparison <- paste(
        sprintf(
          "At the same reported values for the five time measures, the model estimates an average cGPA %s points %s for international respondents than domestic respondents.",
          dec(abs(gap)), direction
        ),
        sprintf("The 95%% interval of %s GPA points is consistent with a gap in either direction.",
                interval_label(status_ci[1], status_ci[2]))
      )
    } else {
      comparison <- sprintf(
        "The model estimates time associations within %s respondents. Choose All students to compare the groups.",
        tolower(input$status)
      )
    }
    model_reading <- if (overall_p < 0.05) {
      "The overall test suggests that at least one model variable is related to cGPA in this selection."
    } else {
      "The overall test offers little evidence of a relationship between these measures and cGPA."
    }
    tagList(
      tags$dl(
        class = "model-metrics",
        metric("Responses", nobs(model)),
        metric("GPA variation explained", paste0(dec(100 * explained, 1), "%")),
        metric("Overall model p-value", p_label(overall_p))
      ),
      p(class = "interpretation model-reading", model_reading, comparison)
    )
  })

  output$coefficient_table <- renderUI({
    model <- fit()
    validate(need(!is.null(model), "This selection has too little variation to fit the regression."))
    estimate <- coef(summary(model))
    terms <- setdiff(rownames(estimate), "(Intercept)")
    limits <- confint(model)[terms, , drop = FALSE]
    labels <- c(time_labels, statusInternational = "International vs. domestic")
    data_table(
      data.frame(
        Predictor = unname(labels[terms]),
        Coefficient = dec(estimate[terms, "Estimate"], 4),
        "95% confidence interval" = interval_label(limits[, 1], limits[, 2], 4),
        "p-value" = p_label(estimate[terms, "Pr(>|t|)"]),
        check.names = FALSE
      ),
      "Regression coefficients for selected respondents"
    )
  })

  diagnostics <- reactive({
    model <- fit()
    validate(need(!is.null(model), "This selection has too little variation for model diagnostics."))
    standardized <- rstandard(model)
    data.frame(
      fitted = fitted(model),
      residual = residuals(model),
      standardized = standardized,
      spread = sqrt(abs(standardized))
    )
  })
  diagnostic_colour <- reactive({
    req(input$status)
    if (input$status == "all") colours()[["Domestic"]] else colours()[[input$status]]
  })
  output$linearity <- renderPlot({
    ggplot(diagnostics(), aes(x = fitted, y = residual)) +
      geom_hline(yintercept = 0, colour = "#7B8D83", linetype = "dashed") +
      geom_point(color = diagnostic_colour(), size = 2.1, alpha = 0.76) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#263D35", linewidth = 0.65) +
      labs(x = "Fitted cGPA", y = "Residual") +
      study_theme()
  }, alt = "Residuals plotted against fitted cGPA, with a horizontal reference line at zero.")
  output$variance <- renderPlot({
    ggplot(diagnostics(), aes(x = fitted, y = spread)) +
      geom_point(color = diagnostic_colour(), size = 2.1, alpha = 0.76) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#263D35", linewidth = 0.65) +
      labs(x = "Fitted cGPA", y = "Residual size (scaled)") +
      study_theme()
  }, alt = "Square roots of absolute standardized residuals plotted against fitted cGPA.")
  output$normality <- renderPlot({
    ggplot(diagnostics(), aes(sample = standardized)) +
      stat_qq_line(color = "#263D35", linewidth = 0.65) +
      stat_qq(color = diagnostic_colour(), size = 2.1, alpha = 0.76) +
      labs(x = "Expected normal quantiles", y = "Observed quantiles") +
      study_theme()
  }, alt = "Normal quantile plot of standardized residuals with a straight reference line.")

  output$assumption_text <- renderUI({
    validate(need(!is.null(fit()), "This selection has too little variation for model diagnostics."))
    if (input$status == "all") {
      notes <- c(
        "The curve bends around zero, so the straight-line model may miss some structure.",
        "Residuals spread out more near a fitted cGPA of 3.0, suggesting uneven variance.",
        "The middle of the Q–Q plot follows the reference line, with modest departures near the ends."
      )
    } else {
      notes <- c(
        "A roughly flat curve near zero supports a straight-line fit within this group.",
        "An even spread across fitted grades supports constant variance within this group.",
        "Points near the reference line support approximately normal residuals within this group."
      )
    }
    div(
      class = "assumption-grid",
      div(h4("Linearity"), p(notes[1])),
      div(h4("Constant variance"), p(notes[2])),
      div(h4("Residual normality"), p(notes[3]))
    )
  })
}

shinyApp(ui = ui, server = server)
