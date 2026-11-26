library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# =====================================================================
# 1. LOAD AND PREPARE THE DATA
# =====================================================================
# The CSV sits next to app.R (relative path, so it also works on Posit).
df <- read.csv("AI_Attendance_Survey_Clean.csv")
names(df) <- sub("^(\ufeff|\u00ef\\.\\.|X\\.U\\.FEFF\\.)", "", names(df))
df$studentID <- NULL

# Program codes come from the codebook
df$program_f <- factor(
  df$program, levels = 1:4,
  labels = c("Statistics", "Computer Science", "Mathematics", "Other")
)
# Anyone reporting more than 0 paid-work hours counts as having paid work
df$paid_f <- factor(
  df$paid_work_hrs > 0, levels = c(FALSE, TRUE),
  labels = c("No paid work", "Has paid work")
)

# Readable names: label = column name in the data
var_labels <- c(
  "Weekly coursework hours"                      = "coursework_hrs",
  "Number of courses"                            = "n_courses",
  "Weekly paid-work hours"                       = "paid_work_hrs",
  "Lectures skipped (%)"                         = "lecture_skip_pct",
  "Tutorials skipped (%)"                        = "tutorial_skip_pct",
  "Skipped lectures partly for AI (%)"           = "lecture_skip_ai_pct",
  "Skipped tutorials partly for AI (%)"          = "tutorial_skip_ai_pct",
  "Agreement that AI can replace attendance (1-7)" = "ai_replace_agree"
)
outcome_choices <- var_labels[var_labels %in% c(
  "lecture_skip_pct", "tutorial_skip_pct", "lecture_skip_ai_pct",
  "tutorial_skip_ai_pct", "ai_replace_agree")]
group_choices <- c("Paid work status" = "paid_f", "Program of study" = "program_f")

# Look up the readable name for any column
all_labels <- c(setNames(names(var_labels), var_labels),
                paid_f = "Paid work status", program_f = "Program of study")
label_of <- function(v) unname(all_labels[v])

# Sensible slider range for a variable
axis_range <- function(v) {
  r <- range(df[[v]], na.rm = TRUE)
  c(floor(r[1]), ceiling(r[2]))
}

# Colour palettes (the first one is colour-blind friendly)
palettes <- list(
  "Colour-blind friendly (Okabe-Ito)" = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2"),
  "Viridis"                           = c("#440154", "#31688E", "#35B779", "#FDE725"),
  "Bright"                            = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
  "Soft pastels"                      = c("#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462"),
  "Greyscale"                         = c("#252525", "#737373", "#BDBDBD", "#969696")
)
get_cols <- function(name, n) rep_len(palettes[[name]], n)

# Small helpers for writing results in plain language
fmt_p <- function(p) if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
strength_word <- function(r) {
  a <- abs(r)
  if (a < 0.2) "very weak" else if (a < 0.4) "weak" else if (a < 0.6) "moderate" else "strong"
}

# =====================================================================
# 2. USER INTERFACE
# =====================================================================
ui <- page_navbar(
  title = "Skipping Class & AI: STA304 Survey Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # ---- Sidebar: filters that apply to every tab ----
  sidebar = sidebar(
    title = "Filter the students",
    width = 300,
    checkboxGroupInput("f_program", "Program of study",
                       choices = levels(df$program_f),
                       selected = levels(df$program_f)),
    radioButtons("f_paid", "Paid work",
                 choices = c("Everyone" = "all",
                             "Has paid work" = "yes",
                             "No paid work" = "no")),
    sliderInput("f_hours", "Weekly coursework hours",
                min = axis_range("coursework_hrs")[1],
                max = axis_range("coursework_hrs")[2],
                value = axis_range("coursework_hrs"), step = 1),
    hr(),
    selectInput("palette", "Plot colours", choices = names(palettes)),
    strong(textOutput("n_selected"))
  ),

  # ---- Tab 1: About ----
  nav_panel(
    "About",
    card(
      card_header("About this survey"),
      p("This app explores a survey of ", strong(nrow(df)), " students in STA304H5 at the
        University of Toronto Mississauga (Fall 2026). We asked how heavy their weekly
        workload is, how many of their lectures and tutorials they skip, how much of that
        skipping was partly to use AI tools instead, and how much they agree that AI can
        replace attending class."),
      p("Use the filters on the left to look at a subgroup (for example, only students with
        paid work). Every plot, test and table then updates to match."),
      h5("Where to look"),
      tags$ul(
        tags$li(strong("Compare groups: "), "compare skipping and AI use between groups
                (e.g. paid work vs. none, or by program)."),
        tags$li(strong("Explore relationships: "), "see how any two numeric answers move
                together, with a fitted line."),
        tags$li(strong("Data table: "), "browse, sort and search the filtered responses.")
      )
    ),
    card(
      card_header("Things to keep in mind"),
      tags$ul(
        tags$li(strong("Not a random sample. "), "Students chose to fill in an anonymous online
                questionnaire, so the results describe the students who responded and may not
                represent everyone in the course."),
        tags$li(strong("Missing answers are expected. "), "The two 'skipped partly for AI'
                questions only apply to students who skipped something, so those plots use
                fewer students (44 for lectures). This is not a data error."),
        tags$li(strong("Small groups. "), "Only 3 students are in the 'Other' program, so any
                comparison involving them is very uncertain."),
        tags$li(strong("Association is not causation. "), "Finding two things go together does
                not show that one causes the other."),
        tags$li(strong("Approximate p-values. "), "Skipping percentages are right-skewed (most
                students skip little, a few skip a lot), so the tests are only approximate.")
      )
    )
  ),

  # ---- Tab 2: Compare groups ----
  nav_panel(
    "Compare groups",
    card(
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("grp_y", "What to measure", choices = outcome_choices,
                    selected = "lecture_skip_pct"),
        selectInput("grp_by", "Compare by", choices = group_choices),
        radioButtons("grp_type", "Plot type", choices = c("Box plot", "Violin plot"),
                     inline = TRUE)
      ),
      layout_columns(
        col_widths = c(8, 4),
        sliderInput("grp_ylim", "Vertical axis range",
                    min = 0, max = 100, value = c(0, 100), step = 1),
        checkboxInput("grp_points", "Show each student as a dot", TRUE)
      )
    ),
    card(plotOutput("grp_plot", height = 430)),
    card(card_header("What does this show?"), uiOutput("grp_text"))
  ),

  # ---- Tab 3: Explore relationships ----
  nav_panel(
    "Explore relationships",
    card(
      layout_columns(
        col_widths = c(6, 6),
        selectInput("sc_x", "Horizontal axis (x)", choices = var_labels,
                    selected = "lecture_skip_ai_pct"),
        selectInput("sc_y", "Vertical axis (y)", choices = var_labels,
                    selected = "ai_replace_agree")
      ),
      layout_columns(
        col_widths = c(6, 6),
        sliderInput("sc_xlim", "Horizontal axis range", min = 0, max = 100,
                    value = axis_range("lecture_skip_ai_pct"), step = 1),
        sliderInput("sc_ylim", "Vertical axis range",
                    min = axis_range("ai_replace_agree")[1],
                    max = axis_range("ai_replace_agree")[2],
                    value = axis_range("ai_replace_agree"), step = 1)
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        checkboxInput("sc_line", "Show fitted line", TRUE),
        checkboxInput("sc_se", "Show uncertainty band", TRUE),
        checkboxInput("sc_jitter", "Spread overlapping dots", TRUE)
      )
    ),
    card(plotOutput("sc_plot", height = 430)),
    card(card_header("What does this show?"), uiOutput("sc_text"))
  ),

  # ---- Tab 4: Data table ----
  nav_panel(
    "Data table",
    card(card_header("Filtered survey responses (click a column to sort, or use the search boxes)"),
         DTOutput("tbl"))
  )
)

# =====================================================================
# 3. SERVER
# =====================================================================
server <- function(input, output, session) {

  # ---- The filtered data: every output below uses this one reactive ----
  filtered <- reactive({
    d <- df
    d <- d[d$program_f %in% input$f_program, , drop = FALSE]
    if (identical(input$f_paid, "yes")) d <- d[d$paid_f == "Has paid work", , drop = FALSE]
    if (identical(input$f_paid, "no"))  d <- d[d$paid_f == "No paid work", , drop = FALSE]
    d <- d[d$coursework_hrs >= input$f_hours[1] &
             d$coursework_hrs <= input$f_hours[2], , drop = FALSE]
    d
  })

  output$n_selected <- renderText({
    paste0(nrow(filtered()), " of ", nrow(df), " students selected")
  })

  # ---- Keep the axis sliders sensible when the variable changes ----
  observeEvent(input$grp_y, {
    r <- axis_range(input$grp_y)
    updateSliderInput(session, "grp_ylim", min = r[1], max = r[2], value = r)
  })
  observeEvent(input$sc_x, {
    r <- axis_range(input$sc_x)
    updateSliderInput(session, "sc_xlim", min = r[1], max = r[2], value = r)
  })
  observeEvent(input$sc_y, {
    r <- axis_range(input$sc_y)
    updateSliderInput(session, "sc_ylim", min = r[1], max = r[2], value = r)
  })

  # -------------------------------------------------------------------
  # TAB: COMPARE GROUPS
  # -------------------------------------------------------------------
  grp_data <- reactive({
    y <- input$grp_y
    g <- input$grp_by
    d <- filtered()
    d <- d[!is.na(d[[y]]) & !is.na(d[[g]]), , drop = FALSE]
    validate(need(nrow(d) >= 2,
                  "Too few students match the current filters. Try widening them in the sidebar."))
    data.frame(val = d[[y]], grp = droplevels(d[[g]]))
  })

  output$grp_plot <- renderPlot({
    d <- grp_data()
    y <- input$grp_y
    n_by <- table(d$grp)
    # Put each group's sample size in its label
    d$grp <- factor(d$grp, levels = names(n_by),
                    labels = paste0(names(n_by), "\n(n = ", as.integer(n_by), ")"))
    cols <- get_cols(input$palette, nlevels(d$grp))

    p <- ggplot(d, aes(x = grp, y = val, fill = grp))
    if (input$grp_type == "Violin plot") {
      p <- p + geom_violin(alpha = 0.7, trim = TRUE, colour = "grey30")
    } else {
      p <- p + geom_boxplot(alpha = 0.7, colour = "grey30",
                            outlier.shape = if (input$grp_points) NA else 19)
    }
    if (input$grp_points) {
      jh <- if (y == "ai_replace_agree") 0.1 else 0
      p <- p + geom_jitter(width = 0.12, height = jh, alpha = 0.55, size = 2,
                           colour = "grey20")
    }
    p +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4,
                   fill = "white", colour = "black") +
      scale_fill_manual(values = cols, guide = "none") +
      coord_cartesian(ylim = input$grp_ylim) +
      labs(title = paste0(label_of(y), " by ", tolower(label_of(input$grp_by))),
           x = label_of(input$grp_by),
           y = label_of(y),
           caption = "Diamond = average; line inside box = median; box = middle half of students") +
      theme_minimal(base_size = 14)
  })

  output$grp_text <- renderUI({
    d <- grp_data()
    y_lab <- label_of(input$grp_y)
    n_by <- table(d$grp)
    keep <- names(n_by)[n_by >= 2]
    d2 <- d[d$grp %in% keep, , drop = FALSE]
    d2$grp <- droplevels(d2$grp)
    validate(need(nlevels(d2$grp) >= 2,
                  "Choose filters that leave at least two groups with 2 or more students each to run a comparison."))

    means <- tapply(d2$val, d2$grp, mean)
    mean_txt <- paste0(names(means), ": ", sprintf("%.1f", means), collapse = "  |  ")

    if (nlevels(d2$grp) == 2) {
      res <- tryCatch(t.test(val ~ grp, data = d2), error = function(e) NULL)
      validate(need(!is.null(res), "Not enough variation in these answers to run a test."))
      pval <- res$p.value
      test_name <- "Welch two-sample t-test (compares the two averages)"
      diff <- abs(means[1] - means[2])
      higher <- names(means)[which.max(means)]
      plain <- if (pval < 0.05) {
        sprintf("The average is about %.1f points higher for \"%s\". A gap this large would be unlikely if the two groups were really the same, so the difference is statistically significant.",
                diff, higher)
      } else {
        sprintf("The averages differ by about %.1f points, but a gap this size could easily happen by chance among this many students. We cannot conclude the groups truly differ.",
                diff)
      }
    } else {
      fit <- tryCatch(summary(aov(val ~ grp, data = d2)), error = function(e) NULL)
      validate(need(!is.null(fit), "Not enough variation in these answers to run a test."))
      pval <- fit[[1]][["Pr(>F)"]][1]
      test_name <- "One-way ANOVA (compares all the averages at once)"
      plain <- if (!is.na(pval) && pval < 0.05) {
        "At least one group's average differs from the others by more than chance would explain, so the difference is statistically significant."
      } else {
        "The averages differ a little, but not by more than chance could explain, so we cannot conclude the groups truly differ."
      }
    }

    tagList(
      p(strong("Average of "), y_lab, strong(": "), mean_txt),
      p(strong("Test: "), test_name, " (", fmt_p(pval), ")."),
      p(strong("In plain language: "), plain),
      p(em("A p-value below 0.05 is the usual cut-off for calling a difference 'statistically
           significant'. This is a comparison of averages, not proof of cause and effect."))
    )
  })

  # -------------------------------------------------------------------
  # TAB: EXPLORE RELATIONSHIPS
  # -------------------------------------------------------------------
  sc_data <- reactive({
    x <- input$sc_x
    y <- input$sc_y
    validate(need(x != y, "Please pick two different variables."))
    d <- filtered()
    d <- d[complete.cases(d[, c(x, y)]), , drop = FALSE]
    validate(need(nrow(d) >= 5,
                  "Fewer than 5 students have answers for both variables with the current filters."))
    data.frame(x = d[[x]], y = d[[y]])
  })

  output$sc_plot <- renderPlot({
    d <- sc_data()
    cols <- get_cols(input$palette, 2)
    pos <- if (input$sc_jitter) {
      position_jitter(width = 0.02 * diff(range(d$x)),
                      height = 0.02 * diff(range(d$y)), seed = 1)
    } else {
      position_identity()
    }
    p <- ggplot(d, aes(x = x, y = y)) +
      geom_point(position = pos, colour = cols[1], size = 3, alpha = 0.75)
    if (input$sc_line) {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, se = input$sc_se,
                           colour = cols[2], fill = cols[2], linewidth = 1.2)
    }
    p +
      coord_cartesian(xlim = input$sc_xlim, ylim = input$sc_ylim) +
      labs(title = paste0(label_of(input$sc_y), " vs. ", label_of(input$sc_x)),
           x = label_of(input$sc_x), y = label_of(input$sc_y),
           caption = paste0("Each dot is one student (n = ", nrow(d), ")")) +
      theme_minimal(base_size = 14)
  })

  output$sc_text <- renderUI({
    d <- sc_data()
    validate(need(sd(d$x) > 0 && sd(d$y) > 0,
                  "One of these variables has the same value for every selected student, so no relationship can be fitted."))
    x_lab <- label_of(input$sc_x)
    y_lab <- label_of(input$sc_y)
    fit <- lm(y ~ x, data = d)
    b <- unname(coef(fit)[2])
    pval <- summary(fit)$coefficients[2, 4]
    r <- cor(d$x, d$y)
    dir <- if (b > 0) "higher" else "lower"

    plain <- sprintf(
      "Among these %d students, each 1-unit increase in \"%s\" goes with an average of %.2f units %s in \"%s\". The relationship is %s (r = %.2f), and the line explains about %.0f%% of the variation between students.",
      nrow(d), x_lab, abs(b), dir, y_lab, strength_word(r), r, 100 * r^2)
    sig <- if (pval < 0.05) {
      "This is statistically significant: a pattern this strong would be unlikely if the two were unrelated."
    } else {
      "This is not statistically significant: a pattern like this could easily appear by chance, so we cannot conclude the two are related."
    }

    tagList(
      p(strong("Fitted line (simple linear regression): "),
        sprintf("slope = %.3f per unit, ", b), fmt_p(pval), "."),
      p(strong("In plain language: "), plain),
      p(sig),
      p(em("The line shows an association only. It does not show that one thing causes the other."))
    )
  })

  # -------------------------------------------------------------------
  # TAB: DATA TABLE
  # -------------------------------------------------------------------
  output$tbl <- renderDT({
    d <- filtered()
    out <- d[, c("program_f", "paid_f", unname(var_labels)), drop = FALSE]
    names(out) <- c("Program of study", "Paid work status", names(var_labels))
    datatable(out, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui, server)
