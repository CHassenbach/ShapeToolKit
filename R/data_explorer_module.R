# Suppress R CMD CHECK notes for ggplot2 aes() column-name variables
utils::globalVariables(c("Var1", "Var2", "Correlation", "grp", "val", "err"))

#' Data Explorer Module UI
#'
#' Interactive data analysis tab with independent, self-contained panels for
#' distribution plots, scatter + custom regression, correlation analysis,
#' group comparison tests, and disparity analysis (PC morphospace via dispRity).
#' Each panel has its own configuration controls and its own run button.
#'
#' @param id Module id
#' @importFrom dispRity dispRity summary.dispRity test.dispRity
#' @export
data_explorer_ui <- function(id) {
  ns <- NS(id)

  # Helper: consistent inner 2-col layout (controls | output) inside a box
  .panel <- function(controls, output_widget) {
    fluidRow(
      column(width = 4, controls),
      column(width = 8, output_widget)
    )
  }

  tagList(

    # ── 1. Distribution Plot ─────────────────────────────────────────────────
    fluidRow(
      column(
        width = 12,
        box(
          title = "Distribution Plot", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = FALSE,
          .panel(
            controls = tagList(
              uiOutput(ns("dp_y_ui")),
              uiOutput(ns("dp_group_ui")),
              uiOutput(ns("dp_groupvals_ui")),
              selectInput(ns("dp_type"), "Plot type",
                choices = c("Boxplot" = "boxplot", "Violin" = "violin",
                            "Histogram / Density" = "histogram"),
                selected = "boxplot"),
              # Boxplot options
              conditionalPanel(
                condition = paste0("input['", ns("dp_type"), "'] == 'boxplot'"),
                checkboxInput(ns("bp_notch"),   "Notched",          value = FALSE),
                checkboxInput(ns("bp_jitter"),  "Overlay jitter",   value = TRUE),
                checkboxInput(ns("bp_outliers"),"Show outliers",    value = TRUE)
              ),
              # Violin options
              conditionalPanel(
                condition = paste0("input['", ns("dp_type"), "'] == 'violin'"),
                checkboxInput(ns("vio_box"),    "Overlay boxplot",  value = TRUE),
                checkboxInput(ns("vio_jitter"), "Overlay jitter",   value = FALSE)
              ),
              # Histogram options
              conditionalPanel(
                condition = paste0("input['", ns("dp_type"), "'] == 'histogram'"),
                numericInput(ns("hist_bins"), "Bins", value = 30, min = 2, step = 1),
                checkboxInput(ns("hist_density"), "Overlay density", value = TRUE)
              ),
              hr(),
              .appearance_controls(ns, prefix = "dp"),
              actionButton(ns("dp_run"), "Plot", class = "btn-success btn-block")
            ),
            output_widget = shinycssloaders::withSpinner(
              plotOutput(ns("dp_plot"), height = 430)
            )
          )
        )
      )
    ),

    # ── 2. Scatter + Regression ──────────────────────────────────────────────
    fluidRow(
      column(
        width = 12,
        box(
          title = "Scatter Plot & Regression", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          .panel(
            controls = tagList(
              uiOutput(ns("sc_x_ui")),
              uiOutput(ns("sc_y_ui")),
              uiOutput(ns("sc_group_ui")),
              uiOutput(ns("sc_groupvals_ui")),
              hr(),
              selectInput(ns("sc_model_type"), "Model type",
                choices = c("Linear regression (lm)" = "lm",
                            "GLM" = "glm"),
                selected = "lm"),
              conditionalPanel(
                condition = paste0("input['", ns("sc_model_type"), "'] == 'glm'"),
                selectInput(ns("sc_glm_family"), "GLM family",
                  choices = c("Gaussian (identity)"  = "gaussian",
                              "Binomial (logit)"     = "binomial",
                              "Poisson (log)"        = "poisson",
                              "Gamma (inverse)"      = "Gamma",
                              "Quasi-binomial"       = "quasibinomial",
                              "Quasi-Poisson"        = "quasipoisson"),
                  selected = "binomial"),
                helpText("Binomial/quasi-binomial: Y must be 0/1 or a factor.")
              ),
              conditionalPanel(
                condition = paste0("input['", ns("sc_model_type"), "'] == 'lm'"),
                selectInput(ns("sc_method"), "Scatter smoother",
                  choices = c("Linear (lm)" = "lm", "LOESS" = "loess", "GAM" = "gam"),
                  selected = "lm"),
                checkboxInput(ns("sc_se"), "Confidence band", value = TRUE)
              ),
              hr(),
              tags$strong("Formula"),
              helpText("Edit freely. Examples: y ~ x   |   y ~ x + z   |   group ~ PC1 + PC2"),
              uiOutput(ns("sc_formula_ui")),
              checkboxInput(ns("sc_label"), "Label points (row names)", value = FALSE),
              hr(),
              .appearance_controls(ns, prefix = "sc"),
              actionButton(ns("sc_run"), "Plot + Fit", class = "btn-success btn-block")
            ),
            output_widget = tagList(
              shinycssloaders::withSpinner(plotOutput(ns("sc_plot"), height = 340)),
              verbatimTextOutput(ns("sc_regression_txt"))
            )
          )
        )
      )
    ),

    # ── 3. Correlation Analysis ──────────────────────────────────────────────
    fluidRow(
      column(
        width = 12,
        box(
          title = "Correlation Analysis", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          .panel(
            controls = tagList(
              uiOutput(ns("cor_cols_ui")),
              selectInput(ns("cor_method"), "Method",
                choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                selected = "pearson"),
              uiOutput(ns("cor_group_ui")),
              uiOutput(ns("cor_groupvals_ui")),
              helpText("If a group is selected, correlations are shown per group."),
              hr(),
              checkboxInput(ns("cor_pvals"), "Show p-values", value = TRUE),
              actionButton(ns("cor_run"), "Calculate", class = "btn-success btn-block"),
              br(),
              downloadButton(ns("cor_dl_csv"), "Download CSV", class = "btn-default btn-sm btn-block")
            ),
            output_widget = tagList(
              plotOutput(ns("cor_heatmap"), height = 360),
              br(),
              DT::dataTableOutput(ns("cor_table"))
            )
          )
        )
      )
    ),

    # ── 4. Group Comparison Tests ────────────────────────────────────────────
    fluidRow(
      column(
        width = 12,
        box(
          title = "Group Comparison Tests", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          .panel(
            controls = tagList(
              uiOutput(ns("gt_y_ui")),
              uiOutput(ns("gt_group_ui")),
              uiOutput(ns("gt_groupvals_ui")),
              hr(),
              selectInput(ns("gt_parametric"), "Parametric test",
                choices = c("t-test (2 groups)" = "ttest",
                            "One-way ANOVA"      = "anova"),
                selected = "anova"),
              selectInput(ns("gt_nonparam"), "Non-parametric test",
                choices = c("Wilcoxon / Mann-Whitney" = "wilcox",
                            "Kruskal-Wallis"          = "kruskal"),
                selected = "kruskal"),
              hr(),
              checkboxInput(ns("gt_posthoc"), "Post-hoc test", value = TRUE),
              conditionalPanel(
                condition = paste0("input['", ns("gt_posthoc"), "'] == true"),
                selectInput(ns("gt_posthoc_type"), "Post-hoc method",
                  choices = c("Tukey HSD (parametric)" = "tukey",
                              "Pairwise Wilcoxon"      = "wilcox_pw"),
                  selected = "tukey"),
                selectInput(ns("gt_padj"), "P-value adjustment",
                  choices = c("BH" = "BH", "Bonferroni" = "bonferroni",
                              "Holm" = "holm", "None" = "none"),
                  selected = "BH")
              ),
              checkboxInput(ns("gt_normality"), "Shapiro-Wilk per group", value = TRUE),
              hr(),
              actionButton(ns("gt_run"), "Calculate", class = "btn-success btn-block"),
              br(),
              downloadButton(ns("gt_dl_csv"), "Download CSV", class = "btn-default btn-sm btn-block")
            ),
            output_widget = verbatimTextOutput(ns("gt_results"), placeholder = TRUE)
          )
        )
      )
    ),

    # ── 5. Disparity Analysis ────────────────────────────────────────────────
    fluidRow(
      column(
        width = 12,
        box(
          title = "Disparity Analysis (PC Morphospace)", status = "primary",
          solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          .panel(
            controls = tagList(
              uiOutput(ns("disp_cols_ui")),
              uiOutput(ns("disp_group_ui")),
              uiOutput(ns("disp_groupvals_ui")),
              hr(),
              checkboxInput(ns("disp_sov"),  "Sum of Variances (SOV)",        value = TRUE),
              checkboxInput(ns("disp_sor"),  "Sum of Ranges (SOR)",           value = TRUE),
              checkboxInput(ns("disp_mpd"),  "Mean Pairwise Distance (MPD)",  value = TRUE),
              checkboxInput(ns("disp_test"), "Pairwise significance test",    value = TRUE),
              conditionalPanel(
                condition = paste0("input['", ns("disp_test"), "'] == true"),
                numericInput(ns("disp_perms"), "Permutations", value = 999,
                             min = 99, max = 9999, step = 100),
                selectInput(ns("disp_padj"), "P-value adjustment",
                  choices = c("BH" = "BH", "Bonferroni" = "bonferroni",
                              "Holm" = "holm", "None" = "none"),
                  selected = "BH")
              ),
              hr(),
              actionButton(ns("disp_run"), "Calculate", class = "btn-success btn-block"),
              br(),
              downloadButton(ns("disp_dl_csv"), "Download CSV", class = "btn-default btn-sm btn-block")
            ),
            output_widget = tagList(
              DT::dataTableOutput(ns("disp_table")),
              br(),
              verbatimTextOutput(ns("disp_test_txt"), placeholder = TRUE)
            )
          )
        )
      )
    )
  )
}

# ── Shared appearance controls ─────────────────────────────────────────────────
# Returns a tagList of inputs; prefix ensures unique input ids per panel
.appearance_controls <- function(ns, prefix) {
  tagList(
    tags$details(
      tags$summary(tags$strong("Appearance"), style = "cursor:pointer; margin-bottom:4px;"),
      numericInput(ns(paste0(prefix, "_pt_size")), "Point size",  value = 2,   min = 0.2, step = 0.2),
      numericInput(ns(paste0(prefix, "_alpha")),   "Transparency",value = 0.6, min = 0, max = 1, step = 0.05),
      selectInput(ns(paste0(prefix, "_palette")), "Color palette",
        choices = c("Default" = "default", "Viridis" = "viridis",
                    "Set1" = "Set1", "Dark2" = "Dark2"),
        selected = "default"),
      textInput(ns(paste0(prefix, "_title")), "Plot title", value = ""),
      selectInput(ns(paste0(prefix, "_theme")), "Theme",
        choices = c("Minimal" = "minimal", "Classic" = "classic",
                    "BW" = "bw", "Light" = "light"),
        selected = "minimal"),
      downloadButton(ns(paste0(prefix, "_dl_png")), "PNG",
                     class = "btn-default btn-xs"),
      downloadButton(ns(paste0(prefix, "_dl_pdf")), "PDF",
                     class = "btn-default btn-xs")
    ),
    br()
  )
}


#' Data Explorer Module Server
#'
#' @param id Module id
#' @param data_reactive A reactive returning a data.frame (from Data Import)
#' @export
data_explorer_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Shared helpers ────────────────────────────────────────────────────────

    .num_cols <- reactive({
      df <- data_reactive(); req(df)
      names(df)[vapply(df, is.numeric, logical(1))]
    })

    .all_cols <- reactive({
      df <- data_reactive(); req(df)
      names(df)
    })

    .get_palette <- function(prefix, n) {
      pal <- input[[paste0(prefix, "_palette")]]
      switch(pal %||% "default",
        viridis = viridisLite::viridis(n),
        Set1    = RColorBrewer::brewer.pal(min(n, 9), "Set1"),
        Dark2   = RColorBrewer::brewer.pal(min(n, 8), "Dark2"),
        scales::hue_pal()(n)
      )
    }

    .apply_theme <- function(p, prefix) {
      th <- input[[paste0(prefix, "_theme")]] %||% "minimal"
      theme_fn <- switch(th,
        classic = ggplot2::theme_classic,
        bw      = ggplot2::theme_bw,
        light   = ggplot2::theme_light,
        ggplot2::theme_minimal
      )
      title_str <- input[[paste0(prefix, "_title")]]
      p + theme_fn() +
        ggplot2::labs(title = if (nzchar(title_str %||% "")) title_str else NULL)
    }

    # Filter data to a given group column + group value subset
    .subset_df <- function(df, gcol, gvals) {
      if (!is.null(gcol) && nzchar(gcol) && gcol %in% names(df)) {
        if (!is.null(gvals) && length(gvals) > 0)
          df <- df[as.character(df[[gcol]]) %in% as.character(gvals), , drop = FALSE]
        df[[gcol]] <- factor(df[[gcol]])
      }
      df
    }

    # Make a download handler that saves the current plot
    .plot_dl <- function(plot_rv, prefix, ext) {
      downloadHandler(
        filename = function() paste0("data_explorer_", prefix, "_", Sys.Date(), ".", ext),
        content  = function(file) {
          p <- plot_rv()
          if (is.null(p)) { writeLines("No plot generated.", file); return() }
          w <- tryCatch(as.numeric(input[[paste0(prefix, "_export_w")]]), error = function(e) 8)
          h <- tryCatch(as.numeric(input[[paste0(prefix, "_export_h")]]), error = function(e) 6)
          if (is.na(w)) w <- 8; if (is.na(h)) h <- 6
          ggplot2::ggsave(file, plot = p, device = ext, width = w, height = h)
        }
      )
    }

    # ── 1. Distribution Plot ─────────────────────────────────────────────────

    output$dp_y_ui <- renderUI({
      selectInput(ns("dp_y"), "Y column (values)", choices = .num_cols())
    })
    output$dp_group_ui <- renderUI({
      selectInput(ns("dp_group"), "Group column (optional)",
                  choices = c("(none)" = "", .all_cols()))
    })
    output$dp_groupvals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$dp_group
      if (is.null(gcol) || !nzchar(gcol) || !gcol %in% names(df)) return(NULL)
      vals <- unique(as.character(df[[gcol]]))
      selectizeInput(ns("dp_groupvals"), "Include groups",
                     choices = vals, selected = vals, multiple = TRUE)
    })

    dp_plot_rv <- reactiveVal(NULL)

    observeEvent(input$dp_run, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      req(df, nrow(df) > 0)

      ycol  <- input$dp_y;    req(ycol, ycol %in% names(df))
      gcol  <- input$dp_group
      gvals <- input$dp_groupvals
      df    <- .subset_df(df, gcol, gvals)
      has_g <- !is.null(gcol) && nzchar(gcol) && gcol %in% names(df)
      n_g   <- if (has_g) nlevels(df[[gcol]]) else 1L
      cols  <- .get_palette("dp", n_g)
      alpha <- input$dp_alpha %||% 0.6
      pt_sz <- input$dp_pt_size %||% 2

      p <- tryCatch({
        switch(input$dp_type,

          boxplot = {
            ae <- if (has_g)
              ggplot2::aes(x = .data[[gcol]], y = .data[[ycol]], fill = .data[[gcol]])
            else
              ggplot2::aes(x = "", y = .data[[ycol]])
            p <- ggplot2::ggplot(df, ae) +
              ggplot2::geom_boxplot(
                notch = isTRUE(input$bp_notch),
                outlier.shape = if (isTRUE(input$bp_outliers)) 19 else NA,
                alpha = alpha, width = 0.55
              )
            if (isTRUE(input$bp_jitter))
              p <- p + ggplot2::geom_jitter(width = 0.15, size = pt_sz,
                                            alpha = alpha * 0.7, show.legend = FALSE)
            if (has_g) p <- p + ggplot2::scale_fill_manual(values = cols)
            p + ggplot2::labs(x = if (has_g) gcol else "", y = ycol, fill = gcol)
          },

          violin = {
            ae <- if (has_g)
              ggplot2::aes(x = .data[[gcol]], y = .data[[ycol]], fill = .data[[gcol]])
            else
              ggplot2::aes(x = "", y = .data[[ycol]])
            p <- ggplot2::ggplot(df, ae) +
              ggplot2::geom_violin(alpha = alpha)
            if (isTRUE(input$vio_box))
              p <- p + ggplot2::geom_boxplot(width = 0.1, fill = "white",
                                             outlier.shape = NA)
            if (isTRUE(input$vio_jitter))
              p <- p + ggplot2::geom_jitter(width = 0.08, size = pt_sz, alpha = 0.5)
            if (has_g) p <- p + ggplot2::scale_fill_manual(values = cols)
            p + ggplot2::labs(x = if (has_g) gcol else "", y = ycol, fill = gcol)
          },

          histogram = {
            ae <- if (has_g)
              ggplot2::aes(x = .data[[ycol]], fill = .data[[gcol]])
            else
              ggplot2::aes(x = .data[[ycol]])
            p <- ggplot2::ggplot(df, ae) +
              ggplot2::geom_histogram(bins = input$hist_bins %||% 30,
                                      alpha = alpha, position = "identity")
            if (isTRUE(input$hist_density))
              p <- p + ggplot2::geom_density(
                mapping = ggplot2::aes(x = .data[[ycol]], y = ggplot2::after_stat(count)),
                data = df, color = "black", fill = NA, linewidth = 0.8,
                inherit.aes = FALSE
              )
            if (has_g) p <- p + ggplot2::scale_fill_manual(values = cols)
            p + ggplot2::labs(x = ycol, y = "Count", fill = gcol)
          }
        )
      }, error = function(e) {
        showNotification(paste("Plot error:", conditionMessage(e)), type = "error"); NULL
      })

      if (!is.null(p)) {
        p <- .apply_theme(p, "dp")
        dp_plot_rv(p)
      }
    })

    output$dp_plot <- renderPlot({ p <- dp_plot_rv(); req(p); print(p) })

    output$dp_dl_png <- .plot_dl(dp_plot_rv, "dp", "png")
    output$dp_dl_pdf <- .plot_dl(dp_plot_rv, "dp", "pdf")


    # ── 2. Scatter + Regression ──────────────────────────────────────────────

    output$sc_x_ui <- renderUI({
      selectInput(ns("sc_x"), "X column", choices = .all_cols())
    })
    output$sc_y_ui <- renderUI({
      # Y can be any column for GLM (e.g. factor for binomial)
      all  <- .all_cols()
      nc   <- .num_cols()
      selectInput(ns("sc_y"), "Y / response column", choices = all,
                  selected = if (length(nc) >= 2) nc[2] else nc[1])
    })
    output$sc_group_ui <- renderUI({
      selectInput(ns("sc_group"), "Color by (optional)",
                  choices = c("(none)" = "", .all_cols()))
    })
    output$sc_groupvals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$sc_group
      if (is.null(gcol) || !nzchar(gcol) || !gcol %in% names(df)) return(NULL)
      vals <- unique(as.character(df[[gcol]]))
      selectizeInput(ns("sc_groupvals"), "Include groups",
                     choices = vals, selected = vals, multiple = TRUE)
    })

    # Pre-fill formula box whenever x/y change
    output$sc_formula_ui <- renderUI({
      x <- input$sc_x %||% "x"
      y <- input$sc_y %||% "y"
      textInput(ns("sc_formula"), label = NULL,
                value = paste0(y, " ~ ", x))
    })

    sc_plot_rv  <- reactiveVal(NULL)
    sc_lm_rv    <- reactiveVal(NULL)

    observeEvent(input$sc_run, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      req(df, nrow(df) > 0)

      xcol  <- input$sc_x;   req(xcol, xcol %in% names(df))
      ycol  <- input$sc_y;   req(ycol, ycol %in% names(df))
      gcol  <- input$sc_group
      gvals <- input$sc_groupvals
      df    <- .subset_df(df, gcol, gvals)
      has_g <- !is.null(gcol) && nzchar(gcol) && gcol %in% names(df)
      n_g   <- if (has_g) nlevels(df[[gcol]]) else 1L
      cols  <- .get_palette("sc", n_g)
      alpha <- input$sc_alpha %||% 0.7
      pt_sz <- input$sc_pt_size %||% 2

      model_type   <- input$sc_model_type %||% "lm"
      formula_str  <- trimws(input$sc_formula %||% paste0(ycol, " ~ ", xcol))
      frm          <- tryCatch(as.formula(formula_str),
                               error = function(e) { showNotification(paste("Bad formula:", conditionMessage(e)), type = "error"); NULL })
      req(frm)

      # Detect whether Y is categorical (factor or character)
      y_vals     <- df[[ycol]]
      y_is_categ <- is.factor(y_vals) || is.character(y_vals) ||
                    (model_type == "glm" && input$sc_glm_family %in% c("binomial", "quasibinomial"))

      # ── Fit model ────────────────────────────────────────────────────────
      fit <- tryCatch({
        if (model_type == "lm") {
          lm(frm, data = df)
        } else {
          fam <- switch(input$sc_glm_family %||% "gaussian",
            binomial     = binomial(link = "logit"),
            poisson      = poisson(link = "log"),
            Gamma        = Gamma(link = "inverse"),
            quasibinomial  = quasibinomial(link = "logit"),
            quasipoisson   = quasipoisson(link = "log"),
            gaussian(link = "identity")
          )
          # For binomial: coerce Y to factor then numeric 0/1
          if (inherits(fam, "family") && fam$family %in% c("binomial", "quasibinomial")) {
            df[[ycol]] <- as.integer(as.factor(df[[ycol]])) - 1L
          }
          glm(frm, data = df, family = fam)
        }
      }, error = function(e) {
        showNotification(paste("Model error:", conditionMessage(e)), type = "error"); NULL
      })
      req(fit)

      # ── Plot ─────────────────────────────────────────────────────────────
      p <- tryCatch({
        if (y_is_categ && model_type == "glm") {
          # Coefficient plot: makes sense for classification GLMs
          sm    <- summary(fit)
          cdf   <- as.data.frame(sm$coefficients)
          names(cdf) <- c("Estimate", "SE", "Stat", "p")
          cdf$Term <- rownames(cdf)
          cdf   <- cdf[cdf$Term != "(Intercept)", , drop = FALSE]
          cdf$lower <- cdf$Estimate - 1.96 * cdf$SE
          cdf$upper <- cdf$Estimate + 1.96 * cdf$SE
          cdf$sig   <- ifelse(cdf$p < 0.05, "p<0.05", "n.s.")
          ggplot2::ggplot(cdf,
            ggplot2::aes(x = stats::reorder(.data[["Term"]], .data[["Estimate"]]),
                         y = .data[["Estimate"]],
                         ymin = .data[["lower"]], ymax = .data[["upper"]],
                         color = .data[["sig"]])) +
            ggplot2::geom_pointrange(size = 0.8) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
            ggplot2::scale_color_manual(values = c("p<0.05" = "#d62728", "n.s." = "#7f7f7f")) +
            ggplot2::coord_flip() +
            ggplot2::labs(x = NULL, y = "Coefficient (± 1.96 SE)",
                          title = paste("GLM coefficients:", ycol, "~", xcol),
                          color = NULL)
        } else {
          # Scatter + smoother
          ae <- if (has_g)
            ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]], color = .data[[gcol]])
          else
            ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])
          p <- ggplot2::ggplot(df, ae) +
            ggplot2::geom_point(size = pt_sz, alpha = alpha)
          if (model_type == "lm") {
            p <- p + ggplot2::geom_smooth(
              method  = input$sc_method %||% "lm",
              se      = isTRUE(input$sc_se),
              formula = y ~ x
            )
          } else {
            # GLM smoother: use method=glm with method.args
            fam_str <- input$sc_glm_family %||% "gaussian"
            p <- p + ggplot2::geom_smooth(
              method      = "glm",
              se          = TRUE,
              formula     = y ~ x,
              method.args = list(family = fam_str)
            )
          }
          if (isTRUE(input$sc_label))
            p <- p + ggplot2::geom_text(ggplot2::aes(label = rownames(df)),
                                        size = 2.5, vjust = -0.5)
          if (has_g) p <- p + ggplot2::scale_color_manual(values = cols)
          p + ggplot2::labs(x = xcol, y = ycol, color = gcol)
        }
      }, error = function(e) {
        showNotification(paste("Plot error:", conditionMessage(e)), type = "error"); NULL
      })

      if (!is.null(p)) { p <- .apply_theme(p, "sc"); sc_plot_rv(p) }

      # ── Model summary text ───────────────────────────────────────────────
      reg_txt <- tryCatch({
        sm  <- summary(fit)
        coef_tbl <- capture.output(printCoefmat(sm$coefficients, digits = 4,
                                                 signif.stars = TRUE))
        if (model_type == "lm") {
          paste0(
            "Model    : lm\n",
            "Formula  : ", formula_str, "\n",
            "R²       : ", round(sm$r.squared,     4), "\n",
            "Adj. R²  : ", round(sm$adj.r.squared, 4), "\n",
            "F-stat   : ", round(sm$fstatistic[1], 3),
            "  (df ", sm$fstatistic[2], ", ", sm$fstatistic[3], ")\n",
            "p-value  : ", format.pval(
              pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3],
                 lower.tail = FALSE), digits = 4), "\n\n",
            "Coefficients:\n", paste(coef_tbl, collapse = "\n"), "\n"
          )
        } else {
          fam_used <- fit$family$family
          lnk_used <- fit$family$link
          null_dev <- sm$null.deviance
          res_dev  <- sm$deviance
          pseudo_r2 <- round(1 - res_dev / null_dev, 4)
          aic_val  <- round(AIC(fit), 2)
          paste0(
            "Model    : glm\n",
            "Family   : ", fam_used, " (link: ", lnk_used, ")\n",
            "Formula  : ", formula_str, "\n",
            "Null dev.: ", round(null_dev, 2), "  (df ", sm$df.null, ")\n",
            "Resid.dev: ", round(res_dev,  2), "  (df ", sm$df.residual, ")\n",
            "McFadden R²: ", pseudo_r2, "\n",
            "AIC      : ", aic_val, "\n\n",
            "Coefficients:\n", paste(coef_tbl, collapse = "\n"), "\n"
          )
        }
      }, error = function(e) paste("Summary error:", conditionMessage(e)))

      sc_lm_rv(reg_txt)
    })

    output$sc_plot          <- renderPlot({ p <- sc_plot_rv(); req(p); print(p) })
    output$sc_regression_txt <- renderText({ t <- sc_lm_rv(); req(t); t })
    output$sc_dl_png <- .plot_dl(sc_plot_rv, "sc", "png")
    output$sc_dl_pdf <- .plot_dl(sc_plot_rv, "sc", "pdf")


    # ── 3. Correlation Analysis ──────────────────────────────────────────────

    output$cor_cols_ui <- renderUI({
      nc <- .num_cols()
      selectizeInput(ns("cor_cols"), "Columns to correlate",
                     choices = nc, selected = nc, multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    })
    output$cor_group_ui <- renderUI({
      selectInput(ns("cor_group"), "Stratify by group (optional)",
                  choices = c("(none)" = "", .all_cols()))
    })
    output$cor_groupvals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$cor_group
      if (is.null(gcol) || !nzchar(gcol) || !gcol %in% names(df)) return(NULL)
      vals <- unique(as.character(df[[gcol]]))
      selectizeInput(ns("cor_groupvals"), "Include groups",
                     choices = vals, selected = vals, multiple = TRUE)
    })

    cor_mat_rv    <- reactiveVal(NULL)
    cor_pmat_rv   <- reactiveVal(NULL)
    cor_display_rv <- reactiveVal(NULL)  # data.frame shown in DT

    .calc_cor_pmat <- function(m) {
      n   <- ncol(m)
      nms <- colnames(m)
      pmat <- matrix(NA_real_, n, n, dimnames = list(nms, nms))
      for (i in seq_len(n)) for (j in seq_len(n)) {
        if (i != j) {
          ct <- tryCatch(
            cor.test(m[, i], m[, j], method = input$cor_method %||% "pearson"),
            error = function(e) list(p.value = NA_real_)
          )
          pmat[i, j] <- ct$p.value
        } else {
          pmat[i, j] <- NA_real_
        }
      }
      pmat
    }

    observeEvent(input$cor_run, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      req(df, nrow(df) > 0)

      cols  <- input$cor_cols;  req(length(cols) >= 2)
      gcol  <- input$cor_group
      gvals <- input$cor_groupvals
      df    <- .subset_df(df, gcol, gvals)
      meth  <- input$cor_method %||% "pearson"

      mat   <- as.matrix(df[, cols, drop = FALSE])
      valid <- apply(mat, 2, function(x) sum(!is.na(x))) >= 3
      mat   <- mat[, valid, drop = FALSE]
      if (ncol(mat) < 2) {
        showNotification("Need at least 2 columns with sufficient data.", type = "warning")
        return()
      }

      cm <- tryCatch(
        cor(mat, use = "pairwise.complete.obs", method = meth),
        error = function(e) {
          showNotification(paste("Correlation error:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      req(cm)
      cor_mat_rv(cm)

      pm <- if (isTRUE(input$cor_pvals)) .calc_cor_pmat(mat) else NULL
      cor_pmat_rv(pm)

      # Build display data.frame with r and (optionally) p
      cm_r    <- round(cm, 3)
      if (!is.null(pm)) {
        pm_r  <- round(pm, 4)
        combined <- matrix(
          paste0(cm_r, "\n(p=", ifelse(is.na(pm_r), "", pm_r), ")"),
          nrow = nrow(cm_r),
          dimnames = dimnames(cm_r)
        )
        cor_display_rv(as.data.frame(combined))
      } else {
        cor_display_rv(as.data.frame(cm_r))
      }
    })

    output$cor_heatmap <- renderPlot({
      cm <- cor_mat_rv(); req(cm)
      cm_long <- as.data.frame(as.table(cm))
      names(cm_long) <- c("Var1", "Var2", "Correlation")
      ggplot2::ggplot(cm_long,
        ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]], fill = .data[["Correlation"]])) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                      midpoint = 0, limits = c(-1, 1)) +
        ggplot2::geom_text(ggplot2::aes(label = round(.data[["Correlation"]], 2)), size = 3.5) +
        ggplot2::coord_fixed() +
        ggplot2::labs(title = paste(input$cor_method %||% "Pearson", "Correlation"),
                      x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })

    output$cor_table <- DT::renderDataTable({
      d <- cor_display_rv(); req(d)
      DT::datatable(d, options = list(scrollX = TRUE, pageLength = 20))
    })

    output$cor_dl_csv <- downloadHandler(
      filename = function() paste0("correlation_", Sys.Date(), ".csv"),
      content  = function(file) {
        cm <- cor_mat_rv()
        if (is.null(cm)) { writeLines("No correlation computed.", file); return() }
        utils::write.csv(as.data.frame(round(cm, 4)), file)
      }
    )


    # ── 4. Group Comparison Tests ────────────────────────────────────────────

    output$gt_y_ui <- renderUI({
      selectInput(ns("gt_y"), "Variable (Y)", choices = .num_cols())
    })
    output$gt_group_ui <- renderUI({
      selectInput(ns("gt_group"), "Group column",
                  choices = c("(required)" = "", .all_cols()))
    })
    output$gt_groupvals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$gt_group
      if (is.null(gcol) || !nzchar(gcol) || !gcol %in% names(df)) return(NULL)
      vals <- unique(as.character(df[[gcol]]))
      selectizeInput(ns("gt_groupvals"), "Include groups",
                     choices = vals, selected = vals, multiple = TRUE)
    })

    gt_results_rv <- reactiveVal(NULL)

    observeEvent(input$gt_run, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      req(df, nrow(df) > 0)

      ycol  <- input$gt_y;   req(ycol %in% names(df))
      gcol  <- input$gt_group; req(nzchar(gcol %||% ""), gcol %in% names(df))
      gvals <- input$gt_groupvals
      df    <- .subset_df(df, gcol, gvals)
      n_g   <- nlevels(df[[gcol]])
      req(n_g >= 2)

      form  <- as.formula(paste(ycol, "~", gcol))
      lines <- paste0("Variable: ", ycol, "  |  Group: ", gcol,
                      "  |  N groups: ", n_g, "\n",
                      strrep("─", 60), "\n")

      # Normality
      if (isTRUE(input$gt_normality)) {
        lines <- paste0(lines, "\n── Shapiro-Wilk per group ──\n")
        for (g in levels(df[[gcol]])) {
          x <- df[[ycol]][df[[gcol]] == g]
          x <- x[!is.na(x)]
          if (length(x) < 3) {
            lines <- paste0(lines, sprintf("  %s: too few obs\n", g)); next
          }
          if (length(x) > 5000) x <- sample(x, 5000)
          sw <- shapiro.test(x)
          lines <- paste0(lines, sprintf("  %-15s W=%.4f  p=%.4f %s\n",
            g, sw$statistic, sw$p.value, if (sw$p.value < 0.05) "(*)" else ""))
        }
      }

      # Parametric
      lines <- paste0(lines, "\n── Parametric ──\n")
      param_res <- tryCatch({
        if (n_g == 2 && input$gt_parametric == "ttest") {
          tt <- t.test(form, data = df)
          sprintf("  t-test: t=%.3f, df=%.1f, p=%.4f, 95%%CI [%.3f, %.3f]\n",
            tt$statistic, tt$parameter, tt$p.value,
            tt$conf.int[1], tt$conf.int[2])
        } else {
          av  <- aov(form, data = df)
          sm  <- summary(av)[[1]]
          sprintf("  ANOVA: F=%.3f, df=(%d,%d), p=%.4f\n",
            sm[["F value"]][1], sm[["Df"]][1], sm[["Df"]][2], sm[["Pr(>F)"]][1])
        }
      }, error = function(e) paste("  Error:", conditionMessage(e), "\n"))
      lines <- paste0(lines, param_res)

      # Non-parametric
      lines <- paste0(lines, "\n── Non-parametric ──\n")
      np_res <- tryCatch({
        if (n_g == 2 && input$gt_nonparam == "wilcox") {
          wt <- wilcox.test(form, data = df, conf.int = TRUE)
          sprintf("  Wilcoxon: W=%.1f, p=%.4f\n", wt$statistic, wt$p.value)
        } else {
          kw <- kruskal.test(form, data = df)
          sprintf("  Kruskal-Wallis: chi2=%.3f, df=%d, p=%.4f\n",
            kw$statistic, kw$parameter, kw$p.value)
        }
      }, error = function(e) paste("  Error:", conditionMessage(e), "\n"))
      lines <- paste0(lines, np_res)

      # Post-hoc
      if (isTRUE(input$gt_posthoc) && n_g >= 2) {
        adj <- input$gt_padj %||% "BH"
        lines <- paste0(lines, "\n── Post-hoc (", adj, " adjustment) ──\n")
        ph_res <- tryCatch({
          ph_lines <- ""
          if (input$gt_posthoc_type == "tukey") {
            if (n_g == 2) {
              ph_lines <- "  (only 2 groups; see parametric test above)\n"
            } else {
              tk <- TukeyHSD(aov(form, data = df))[[gcol]]
              tk_df <- as.data.frame(tk)
              tk_df$padj <- p.adjust(tk_df[["p adj"]], method = adj)
              for (i in seq_len(nrow(tk_df))) {
                ph_lines <- paste0(ph_lines,
                  sprintf("  %-30s  diff=%7.3f  p_adj=%.4f %s\n",
                    rownames(tk_df)[i], tk_df$diff[i], tk_df$padj[i],
                    if (tk_df$padj[i] < 0.05) "*" else ""))
              }
            }
          } else {
            pw <- pairwise.wilcox.test(df[[ycol]], df[[gcol]], p.adjust.method = adj)
            pm <- pw$p.value
            for (r in rownames(pm)) {
              for (cc in colnames(pm)) {
                pv <- pm[r, cc]
                if (!is.na(pv))
                  ph_lines <- paste0(ph_lines,
                    sprintf("  %-15s vs %-15s  p_adj=%.4f %s\n",
                      r, cc, pv, if (pv < 0.05) "*" else ""))
              }
            }
          }
          ph_lines
        }, error = function(e) paste("  Post-hoc error:", conditionMessage(e), "\n"))
        lines <- paste0(lines, ph_res)
      }

      gt_results_rv(lines)
    })

    output$gt_results <- renderText({ t <- gt_results_rv(); req(t); t })

    output$gt_dl_csv <- downloadHandler(
      filename = function() paste0("group_tests_", Sys.Date(), ".csv"),
      content  = function(file) {
        t <- gt_results_rv()
        if (is.null(t)) { writeLines("No results computed.", file); return() }
        writeLines(t, file)
      }
    )


    # ── 5. Disparity Analysis ────────────────────────────────────────────────

    output$disp_cols_ui <- renderUI({
      nc <- .num_cols()
      selectizeInput(ns("disp_cols"), "PC / numeric columns",
                     choices = nc, selected = head(nc, 4), multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    })
    output$disp_group_ui <- renderUI({
      selectInput(ns("disp_group"), "Group column",
                  choices = c("(required)" = "", .all_cols()))
    })
    output$disp_groupvals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$disp_group
      if (is.null(gcol) || !nzchar(gcol) || !gcol %in% names(df)) return(NULL)
      vals <- unique(as.character(df[[gcol]]))
      selectizeInput(ns("disp_groupvals"), "Include groups",
                     choices = vals, selected = vals, multiple = TRUE)
    })

    disp_table_rv  <- reactiveVal(NULL)
    disp_test_rv   <- reactiveVal(NULL)

    observeEvent(input$disp_run, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      req(df, nrow(df) > 0)

      dcols <- input$disp_cols; req(length(dcols) >= 2)
      gcol  <- input$disp_group; req(nzchar(gcol %||% ""), gcol %in% names(df))
      gvals <- input$disp_groupvals
      df    <- .subset_df(df, gcol, gvals)

      mat  <- as.matrix(df[, dcols, drop = FALSE])
      grps <- levels(df[[gcol]])
      req(length(grps) >= 2)

      grp_idx <- lapply(stats::setNames(grps, grps), function(g) which(df[[gcol]] == g))

      # Local base-R metric functions (avoids dispRity internal API issues)
      .sov <- function(m, ...) sum(diag(var(m)))
      .sor <- function(m, ...) sum(apply(m, 2, function(x) diff(range(x, na.rm = TRUE))))
      .mpd <- function(m, ...) { d <- as.matrix(dist(m)); mean(d[upper.tri(d)]) }

      # Compute summary table per group in base R
      grp_rows <- lapply(grps, function(g) {
        sub <- mat[grp_idx[[g]], , drop = FALSE]
        if (nrow(sub) < 2) return(NULL)
        row <- data.frame(Group = g, N = nrow(sub), stringsAsFactors = FALSE)
        if (isTRUE(input$disp_sov)) row$SOV <- round(.sov(sub), 5)
        if (isTRUE(input$disp_sor)) row$SOR <- round(.sor(sub), 5)
        if (isTRUE(input$disp_mpd)) row$MPD <- round(.mpd(sub), 5)
        row
      })
      disp_table_rv(do.call(rbind, Filter(Negate(is.null), grp_rows)))

      # Pairwise permutation test via dispRity (using custom.subsets + local metric)
      if (isTRUE(input$disp_test)) {
        test_txt <- tryCatch({
          # primary metric for test: first checked box
          primary_fn <- if (isTRUE(input$disp_sov)) .sov
                        else if (isTRUE(input$disp_sor)) .sor
                        else .mpd
          do_obj <- dispRity::custom.subsets(mat, group = grp_idx)
          do_obj <- dispRity::dispRity(do_obj, metric = primary_fn)
          res    <- dispRity::test.dispRity(
            do_obj,
            test       = wilcox.test,
            comparison = "pairwise",
            correction = input$disp_padj %||% "BH"
          )
          paste(capture.output(print(res)), collapse = "\n")
        }, error = function(e) paste("Test error:", conditionMessage(e)))
        disp_test_rv(test_txt)
      }
    })

    output$disp_table <- DT::renderDataTable({
      d <- disp_table_rv(); req(d)
      DT::datatable(d, rownames = FALSE, options = list(scrollX = TRUE))
    })

    output$disp_test_txt <- renderText({
      t <- disp_test_rv()
      if (is.null(t)) return("Enable 'Pairwise significance test' and click Calculate.")
      t
    })

    output$disp_dl_csv <- downloadHandler(
      filename = function() paste0("disparity_", Sys.Date(), ".csv"),
      content  = function(file) {
        d <- disp_table_rv()
        if (is.null(d)) { writeLines("No results computed.", file); return() }
        utils::write.csv(d, file, row.names = FALSE)
      }
    )

    invisible(NULL)
  })
}

# Null-coalescing operator (local to this file)
if (!exists("%||%", envir = parent.env(environment()), inherits = FALSE)) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !identical(a, "")) a else b
}
