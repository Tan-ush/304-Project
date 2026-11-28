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
  fillable = FALSE,  # let pages scroll instead of squeezing plots to fit the screen

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
        tags$li(strong("Research questions: "), "our main findings, with the analysis behind each."),
        tags$li(strong("Summary statistics: "), "typical answers and who responded."),
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

  # ---- Tab 2: Research questions ----
  nav_panel(
    "Research questions",
    p("Our three main questions, answered for the students currently selected in the sidebar.
      With no filters applied, these are our main results. We call a result statistically
      significant when p < 0.05. Filtered results use fewer students and are exploratory."),
    card(card_header("Summary of answers"), uiOutput("rq_summary")),
    navset_card_tab(
      nav_panel("RQ1: Workload and skipping",
                p(strong("Do students with heavier weekly coursework skip more of their classes?")),
                plotOutput("rq1_plot", height = 380), uiOutput("rq1_text")),
      nav_panel("RQ2: Lectures vs. tutorials",
                p(strong("Among students who skipped both, does the share of skipping that was partly
                  for AI differ between lectures and tutorials?")),
                plotOutput("rq2_plot", height = 380), uiOutput("rq2_text")),
      nav_panel("RQ3: Workload and AI-related skipping",
                p(strong("Is workload still linked to AI-related skipping after accounting for other
                  factors?")),
                uiOutput("rq3_text"), tableOutput("rq3_table"),
                p("Residual check: dots scattered evenly around the dashed line, with no curve or
                  funnel shape, suggest the model is reasonable."),
                plotOutput("rq3_resid", height = 320),
                p("With all students selected, we see no clear curve, but the spread is uneven,
                  partly because percentages are limited to 0-100. Treat the model's p-values and
                  intervals as approximate."))
    )
  ),

  # ---- Summary statistics ----
  nav_panel(
    "Summary statistics",
    p("A snapshot of the students currently selected in the sidebar. The mean is the average
      and the median is the middle answer. SD and IQR describe how spread out answers are (IQR
      is the width of the middle half). When the mean is well above the median, a few students
      with large values are pulling the average up."),
    card(card_header("Numeric answers"), div(class = "table-responsive", tableOutput("sum_num"))),
    card(card_header("Program of study and paid work"), tableOutput("sum_freq")),
    card(card_header("Paid work within each program (row percentages)"), tableOutput("sum_cross"))
  ),

  # ---- Tab 3: Compare groups ----
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

  # ---- Tab 4: Explore relationships ----
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

  # ---- Tab 5: Data table ----
  nav_panel(
    "Data table",
    card(card_header("Filtered survey responses (click a column to sort, or use the search boxes)"),
         DTOutput("tbl", fill = FALSE))
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
  # TAB: RESEARCH QUESTIONS
  # -------------------------------------------------------------------
  is_sig <- function(p) !is.na(p) && p < 0.05

  # RQ1: average of lecture and tutorial skipping (lectures only if no tutorial answer)
  rq1 <- reactive({
    d <- filtered()
    d$skip <- rowMeans(d[, c("lecture_skip_pct", "tutorial_skip_pct")], na.rm = TRUE)
    d <- d[!is.na(d$skip), , drop = FALSE]
    validate(need(nrow(d) >= 5 && sd(d$coursework_hrs) > 0 && sd(d$skip) > 0,
                  "RQ1: not enough students or variation with the current filters."))
    fit <- lm(skip ~ coursework_hrs, data = d)
    list(d = d, fit = fit, b = unname(coef(fit)[2]), ci = confint(fit)[2, ],
         p = summary(fit)$coefficients[2, 4], r = cor(d$coursework_hrs, d$skip))
  })

  # RQ2: paired comparison, only students who skipped both formats
  rq2 <- reactive({
    d <- filtered()
    d <- d[which(d$lecture_skip_pct > 0 & d$tutorial_skip_pct > 0 &
                   !is.na(d$lecture_skip_ai_pct) & !is.na(d$tutorial_skip_ai_pct)), , drop = FALSE]
    diffs <- d$lecture_skip_ai_pct - d$tutorial_skip_ai_pct
    validate(need(nrow(d) >= 5 && sd(diffs) > 0,
                  "RQ2: not enough students who skipped both formats with the current filters."))
    list(d = d, t = t.test(d$lecture_skip_ai_pct, d$tutorial_skip_ai_pct, paired = TRUE))
  })

  # RQ3: workload always stays in; pick the other predictors by adjusted R-squared
  rq3 <- reactive({
    d <- filtered()
    d$ai <- rowMeans(d[, c("lecture_skip_ai_pct", "tutorial_skip_ai_pct")], na.rm = TRUE)
    d <- d[which((d$lecture_skip_pct > 0 | d$tutorial_skip_pct > 0) & !is.na(d$ai)), , drop = FALSE]
    d$program_f <- droplevels(d$program_f)
    validate(need(nrow(d) >= 15 && sd(d$coursework_hrs) > 0 && sd(d$ai) > 0,
                  "RQ3: at least 15 eligible students with varying answers are needed for this model."))
    others <- c("n_courses", "paid_work_hrs", "program_f", "ai_replace_agree")
    others <- others[vapply(others, function(v) length(unique(d[[v]])) > 1, logical(1))]
    sets <- list(character(0))
    for (v in others) sets <- c(sets, lapply(sets, c, v))
    fits <- lapply(sets, function(s) lm(reformulate(c("coursework_hrs", s), "ai"), data = d))
    adj <- vapply(fits, function(m) summary(m)$adj.r.squared, numeric(1))
    best <- which.max(adj)
    fit <- fits[[best]]
    list(d = d, fit = fit, adj = adj[best], coefs = summary(fit)$coefficients,
         ci = confint(fit), others = sets[[best]])
  })

  rq1_sentence <- function(x) sprintf(
    "Each extra weekly coursework hour goes with %.2f percentage points %s skipping (95%% CI %.2f to %.2f, %s, r = %.2f, n = %d). %s",
    abs(x$b), if (x$b > 0) "more" else "less", x$ci[1], x$ci[2], fmt_p(x$p), x$r, nrow(x$d),
    if (!is_sig(x$p)) "We found no clear evidence that workload is linked to how much students skip."
    else if (x$b > 0) "Students with heavier workloads tended to skip more."
    else "Students with heavier workloads tended to skip less.")

  rq2_sentence <- function(x) {
    p <- x$t$p.value
    sprintf(
      "On average, %.1f%% of skipped lectures and %.1f%% of skipped tutorials were skipped partly for AI. The lecture minus tutorial difference is %.1f points (95%% CI %.1f to %.1f, %s, n = %d). %s",
      mean(x$d$lecture_skip_ai_pct), mean(x$d$tutorial_skip_ai_pct), unname(x$t$estimate),
      x$t$conf.int[1], x$t$conf.int[2], fmt_p(p), nrow(x$d),
      if (is_sig(p)) "The two formats differ by more than chance would easily explain."
      else "We found no clear evidence of a difference between lectures and tutorials.")
  }

  rq3_sentence <- function(x) {
    b <- x$coefs["coursework_hrs", 1]
    p <- x$coefs["coursework_hrs", 4]
    sprintf(
      "Holding the other factors in the model fixed, each extra coursework hour goes with %.2f points %s AI-related skipping (95%% CI %.2f to %.2f, %s, n = %d). %s",
      abs(b), if (b > 0) "more" else "less", x$ci["coursework_hrs", 1], x$ci["coursework_hrs", 2],
      fmt_p(p), nrow(x$d),
      if (is_sig(p)) "Workload remains a significant predictor."
      else "We found no clear evidence that workload predicts AI-related skipping.")
  }

  # Each answer is computed separately, so one failing does not hide the others
  output$rq_summary <- renderUI({
    answer <- function(expr) tryCatch(expr, error = function(e) {
      msg <- conditionMessage(e)
      if (nzchar(msg)) msg else "Not available with the current filters."
    })
    tags$ul(
      tags$li(strong("RQ1: "), answer(rq1_sentence(rq1()))),
      tags$li(strong("RQ2: "), answer(rq2_sentence(rq2()))),
      tags$li(strong("RQ3: "), answer(rq3_sentence(rq3()))),
      if (nrow(filtered()) < nrow(df))
        tags$li(em("Filters are on, so these are subgroup results."))
    )
  })

  output$rq1_plot <- renderPlot({
    d <- rq1()$d
    cols <- get_cols(input$palette, 2)
    ggplot(d, aes(coursework_hrs, skip)) +
      geom_point(colour = cols[1], size = 3, alpha = 0.75) +
      geom_smooth(method = "lm", formula = y ~ x, colour = cols[2], fill = cols[2]) +
      labs(title = "Classes skipped vs. weekly coursework hours",
           x = "Weekly coursework hours", y = "Average classes skipped (%)") +
      theme_minimal(base_size = 14)
  })
  output$rq1_text <- renderUI(p(rq1_sentence(rq1())))

  output$rq2_plot <- renderPlot({
    d <- rq2()$d
    long <- data.frame(
      format = factor(rep(c("Lectures", "Tutorials"), each = nrow(d))),
      diff = c(d$lecture_skip_ai_pct, d$tutorial_skip_ai_pct),
      id = rep(seq_len(nrow(d)), 2))
    ggplot(long, aes(format, diff)) +
      geom_line(aes(group = id), colour = "grey60", alpha = 0.4) +
      geom_boxplot(aes(fill = format), width = 0.3, alpha = 0.7, outlier.shape = NA) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
      scale_fill_manual(values = get_cols(input$palette, 2), guide = "none") +
      labs(title = "Share of skipped classes that were partly for AI",
           x = "Class format", y = "Skipped partly for AI (%)",
           caption = "Each grey line is one student; diamond = average") +
      theme_minimal(base_size = 14)
  })
  output$rq2_text <- renderUI(p(rq2_sentence(rq2())))

  output$rq3_text <- renderUI({
    x <- rq3()
    chosen <- c("Weekly coursework hours", vapply(x$others, label_of, character(1)))
    tagList(
      p(rq3_sentence(x)),
      p("The outcome is the average of each student's lecture and tutorial AI-skipping
        percentages (or whichever one they have). We compared models with different sets of
        predictors, always keeping workload, and chose the highest adjusted R-squared."),
      p(strong("Chosen predictors: "), paste(chosen, collapse = ", "),
        sprintf(" (adjusted R-squared = %.2f).", x$adj)),
      if ("ai_replace_agree" %in% x$others) p(sprintf(
        "In this model, each 1-point rise in agreement that AI can replace attendance goes with a %.1f point change in AI-related skipping (%s).",
        x$coefs["ai_replace_agree", 1], fmt_p(x$coefs["ai_replace_agree", 4]))),
      p(em("Because the predictors were chosen using this same data, p-values and intervals
           are likely optimistic. These are associations, not proof of cause."))
    )
  })

  output$rq3_table <- renderTable({
    x <- rq3()
    terms <- rownames(x$coefs)
    nice <- terms
    nice[terms == "(Intercept)"] <- "Baseline (intercept)"
    for (v in names(all_labels)) nice[terms == v] <- label_of(v)
    prog <- startsWith(terms, "program_f")
    nice[prog] <- paste0("Program: ", sub("program_f", "", terms[prog]),
                         " (vs. ", levels(x$d$program_f)[1], ")")
    data.frame(Term = nice,
               Estimate = sprintf("%.2f", x$coefs[, 1]),
               `95% CI` = sprintf("%.2f to %.2f", x$ci[, 1], x$ci[, 2]),
               `p-value` = vapply(x$coefs[, 4], fmt_p, character(1)),
               check.names = FALSE)
  })

  output$rq3_resid <- renderPlot({
    fit <- rq3()$fit
    ggplot(data.frame(f = fitted(fit), r = resid(fit)), aes(f, r)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(colour = get_cols(input$palette, 1), size = 3) +
      labs(title = "Model errors vs. predicted values",
           x = "Predicted AI-related skipping (%)", y = "Actual minus predicted (points)") +
      theme_minimal(base_size = 14)
  })

  # -------------------------------------------------------------------
  # TAB: SUMMARY STATISTICS
  # -------------------------------------------------------------------
  some_students <- function() {
    d <- filtered()
    validate(need(nrow(d) > 0, "No students match the current filters."))
    d
  }
  # Count with percentage, e.g. "32 (54.2%)"
  pct <- function(n, total) sprintf("%d (%.1f%%)", n, 100 * n / total)

  output$sum_num <- renderTable({
    d <- some_students()
    rows <- lapply(unname(var_labels), function(v) {
      x <- d[[v]][!is.na(d[[v]])]
      num <- function(f) if (length(x)) sprintf("%.2f", f(x)) else "-"
      data.frame(Variable = label_of(v), Median = num(median), Mean = num(mean),
                 SD = if (length(x) > 1) num(sd) else "-", IQR = num(function(z) IQR(z, type = 6)),  # same quartile method as our EDA
                 Min = num(min), Max = num(max), n = length(x),
                 Missing = nrow(d) - length(x))
    })
    do.call(rbind, rows)
  })

  output$sum_freq <- renderTable({
    d <- some_students()
    one <- function(f, name) {
      n <- as.integer(table(f))
      data.frame(Variable = name, Group = levels(f), Students = pct(n, nrow(d)))
    }
    rbind(one(d$program_f, "Program of study"), one(d$paid_f, "Paid work status"))
  })

  output$sum_cross <- renderTable({
    d <- some_students()
    tab <- table(droplevels(d$program_f), d$paid_f)
    total <- as.integer(rowSums(tab))
    data.frame(`Program of study` = rownames(tab),
               `No paid work` = pct(as.integer(tab[, 1]), total),
               `Has paid work` = pct(as.integer(tab[, 2]), total),
               Total = total, check.names = FALSE)
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
           caption = if (input$grp_type == "Violin plot") "Diamond = average; wider shape = more students"
                     else "Diamond = average; line inside box = median; box = middle half of students") +
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
