# Suppress R CMD CHECK notes for ggplot2 aes() column-name variables
utils::globalVariables(c("Var1", "Var2", "Correlation", "grp", "val", "err"))

#' Data Explorer Module UI
#'
#' Full-featured data analysis tab: boxplots, scatter+regression, violin,
#' histogram/density, correlation heatmap, bar charts, descriptive statistics,
#' normality tests, group comparisons, post-hoc tests, linear regression,
#' correlation matrix, and disparity analysis (PC morphospace via dispRity).
#'
#' @param id Module id
#' @importFrom dispRity dispRity summary.dispRity test.dispRity
#' @export
data_explorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # ── LEFT: control boxes ─────────────────────────────────────────────────
      column(
        width = 4,

        # 1. Data Mapping
        box(
          title = "Data Mapping", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = FALSE,
          uiOutput(ns("x_col_ui")),
          uiOutput(ns("y_col_ui")),
          uiOutput(ns("group_col_ui")),
          uiOutput(ns("group_vals_ui")),
          hr(),
          helpText("Optional row filter: subset data before analysis."),
          uiOutput(ns("filter_col_ui")),
          uiOutput(ns("filter_vals_ui"))
        ),

        # 2. Plot Type
        box(
          title = "Plot Type", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = FALSE,
          selectInput(ns("plot_type"), "Plot type",
            choices = c(
              "Boxplot"             = "boxplot",
              "Scatter + Regression"= "scatter",
              "Violin"              = "violin",
              "Histogram / Density" = "histogram",
              "Correlation Heatmap" = "heatmap",
              "Bar Chart"           = "bar"
            ),
            selected = "boxplot"
          ),
          # Per-type conditional options
          uiOutput(ns("plot_type_opts_ui"))
        ),

        # 3. Statistics
        box(
          title = "Statistics", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = FALSE,
          checkboxInput(ns("stat_descriptive"),  "Descriptive stats",           value = TRUE),
          checkboxInput(ns("stat_normality"),    "Normality (Shapiro-Wilk)",    value = TRUE),
          checkboxInput(ns("stat_group_test"),   "Group comparison (ANOVA / KW)",value = TRUE),
          checkboxInput(ns("stat_posthoc"),      "Post-hoc test",               value = TRUE),
          conditionalPanel(
            condition = paste0("input['", ns("stat_posthoc"), "'] == true"),
            selectInput(ns("posthoc_adjust"), "P-value adjustment",
              choices = c("BH", "Bonferroni" = "bonferroni", "Holm" = "holm", "None" = "none"),
              selected = "BH"
            )
          ),
          checkboxInput(ns("stat_regression"),  "Linear regression (lm)",       value = TRUE),
          checkboxInput(ns("stat_correlation"), "Correlation matrix",            value = TRUE),
          conditionalPanel(
            condition = paste0("input['", ns("stat_correlation"), "'] == true"),
            selectInput(ns("cor_method"), "Correlation method",
              choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
              selected = "pearson"
            )
          ),
          checkboxInput(ns("stat_disparity"),   "Disparity analysis (dispRity)", value = FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("stat_disparity"), "'] == true"),
            uiOutput(ns("disparity_cols_ui")),
            numericInput(ns("disparity_perms"), "Permutations", value = 999, min = 99, max = 9999, step = 100)
          )
        ),

        # 4. Appearance
        box(
          title = "Appearance", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          textInput(ns("plot_title"), "Plot title", value = ""),
          numericInput(ns("point_size"), "Point size", value = 2, min = 0.2, step = 0.2),
          numericInput(ns("alpha"),      "Transparency (alpha)", value = 0.6, min = 0, max = 1, step = 0.05),
          selectInput(ns("color_palette"), "Color palette",
            choices = c("Default ggplot2" = "default", "Viridis" = "viridis",
                        "RColorBrewer Set1" = "Set1", "RColorBrewer Dark2" = "Dark2",
                        "Custom (manual)" = "manual"),
            selected = "default"
          ),
          conditionalPanel(
            condition = paste0("input['", ns("color_palette"), "'] == 'manual'"),
            textInput(ns("manual_colors"), "Colors (comma-separated)", placeholder = "#E41A1C, #377EB8, #4DAF4A")
          ),
          numericInput(ns("axis_title_size"), "Axis title size",  value = 14, min = 6, step = 1),
          numericInput(ns("axis_text_size"),  "Axis text size",   value = 11, min = 6, step = 1),
          selectInput(ns("ggtheme"), "Theme",
            choices = c("Minimal" = "minimal", "Classic" = "classic",
                        "BW" = "bw", "Light" = "light", "Gray" = "gray"),
            selected = "minimal"
          )
        ),

        # 5. Export
        box(
          title = "Export", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          numericInput(ns("export_width"),  "Plot width (in)",  value = 8, min = 2, step = 0.5),
          numericInput(ns("export_height"), "Plot height (in)", value = 6, min = 2, step = 0.5),
          downloadButton(ns("dl_png"),  "Download PNG",  class = "btn-default btn-sm"),
          downloadButton(ns("dl_pdf"),  "Download PDF",  class = "btn-default btn-sm"),
          br(), br(),
          downloadButton(ns("dl_csv"),  "Download Statistics CSV", class = "btn-default btn-sm")
        ),

        div(style = "margin: 10px 0;",
            actionButton(ns("run"), "Run Analysis", class = "btn-success btn-lg", width = "100%")
        )
      ),

      # ── RIGHT: output tabBox ────────────────────────────────────────────────
      column(
        width = 8,
        tabBox(
          width = 12, id = ns("result_tabs"),
          tabPanel("Plot",
            shinycssloaders::withSpinner(plotOutput(ns("main_plot"), height = 500))
          ),
          tabPanel("Descriptive Stats",
            DT::dataTableOutput(ns("tbl_descriptive"))
          ),
          tabPanel("Normality",
            verbatimTextOutput(ns("txt_normality"))
          ),
          tabPanel("Group Tests",
            verbatimTextOutput(ns("txt_group_test"))
          ),
          tabPanel("Post-hoc",
            verbatimTextOutput(ns("txt_posthoc"))
          ),
          tabPanel("Regression",
            verbatimTextOutput(ns("txt_regression"))
          ),
          tabPanel("Correlation",
            fluidRow(
              column(6, DT::dataTableOutput(ns("tbl_correlation"))),
              column(6, plotOutput(ns("cor_heatmap"), height = 350))
            )
          ),
          tabPanel("Disparity",
            DT::dataTableOutput(ns("tbl_disparity")),
            br(),
            verbatimTextOutput(ns("txt_disparity_test"))
          )
        )
      )
    )
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

    # ── Dynamic column selectors ─────────────────────────────────────────────

    num_cols <- reactive({
      df <- data_reactive(); req(df)
      names(df)[vapply(df, is.numeric, logical(1))]
    })

    all_cols <- reactive({
      df <- data_reactive(); req(df)
      names(df)
    })

    output$x_col_ui <- renderUI({
      selectInput(ns("x_col"), "X column", choices = all_cols(), selected = all_cols()[1])
    })

    output$y_col_ui <- renderUI({
      nc <- num_cols()
      selectInput(ns("y_col"), "Y column (numeric)", choices = nc,
                  selected = if (length(nc) >= 2) nc[2] else nc[1])
    })

    output$group_col_ui <- renderUI({
      selectInput(ns("group_col"), "Group column (optional)", choices = c("(none)" = "", all_cols()))
    })

    output$group_vals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectizeInput(ns("group_vals"), "Group values", choices = vals, selected = vals, multiple = TRUE)
    })

    output$filter_col_ui <- renderUI({
      selectInput(ns("filter_col"), "Filter column (optional)", choices = c("(none)" = "", all_cols()))
    })

    output$filter_vals_ui <- renderUI({
      df <- data_reactive(); req(df)
      fcol <- input$filter_col
      if (is.null(fcol) || fcol == "" || !fcol %in% names(df)) return(NULL)
      vals <- unique(df[[fcol]])
      selectizeInput(ns("filter_vals"), "Keep values", choices = vals, selected = vals, multiple = TRUE)
    })

    output$disparity_cols_ui <- renderUI({
      nc <- num_cols()
      selectizeInput(ns("disparity_cols"), "PC / numeric columns for disparity",
                     choices = nc, selected = head(nc, 4), multiple = TRUE)
    })

    # ── Per-type plot options ─────────────────────────────────────────────────

    output$plot_type_opts_ui <- renderUI({
      switch(input$plot_type,
        boxplot = tagList(
          checkboxInput(ns("bp_notch"),   "Notched boxplot",    value = FALSE),
          checkboxInput(ns("bp_jitter"),  "Overlay jitter points", value = TRUE),
          checkboxInput(ns("bp_outliers"),"Show outlier points", value = TRUE)
        ),
        scatter = tagList(
          selectInput(ns("reg_method"), "Regression method",
            choices = c("Linear (lm)" = "lm", "LOESS (loess)" = "loess",
                        "GAM (gam)" = "gam"), selected = "lm"),
          checkboxInput(ns("reg_se"), "Show confidence band", value = TRUE),
          checkboxInput(ns("scatter_label"), "Label points (row names)", value = FALSE)
        ),
        violin = tagList(
          checkboxInput(ns("vio_box"),    "Overlay boxplot",    value = TRUE),
          checkboxInput(ns("vio_jitter"), "Overlay jitter",     value = FALSE),
          numericInput(ns("vio_scale"),  "Scale adjust", value = 1, min = 0.1, step = 0.1)
        ),
        histogram = tagList(
          numericInput(ns("hist_bins"), "Number of bins", value = 30, min = 2, step = 1),
          checkboxInput(ns("hist_density"), "Overlay density curve", value = TRUE),
          checkboxInput(ns("hist_rug"),     "Show rug",              value = FALSE)
        ),
        heatmap = tagList(
          helpText("Correlation heatmap uses all selected numeric columns."),
          uiOutput(ns("heatmap_cols_ui"))
        ),
        bar = tagList(
          selectInput(ns("bar_stat"), "Bar represents",
            choices = c("Mean" = "mean", "Median" = "median", "Count" = "count"),
            selected = "mean"),
          checkboxInput(ns("bar_error"), "Show error bars (±SD)", value = TRUE),
          checkboxInput(ns("bar_coord_flip"), "Flip coordinates",   value = FALSE)
        )
      )
    })

    output$heatmap_cols_ui <- renderUI({
      nc <- num_cols()
      selectizeInput(ns("heatmap_cols"), "Columns for heatmap",
                     choices = nc, selected = nc, multiple = TRUE)
    })

    # ── Helpers ───────────────────────────────────────────────────────────────

    .apply_theme <- function(p) {
      theme_fn <- switch(input$ggtheme,
        minimal = ggplot2::theme_minimal,
        classic = ggplot2::theme_classic,
        bw      = ggplot2::theme_bw,
        light   = ggplot2::theme_light,
        gray    = ggplot2::theme_gray,
        ggplot2::theme_minimal
      )
      p + theme_fn(base_size = input$axis_text_size) +
        ggplot2::theme(
          axis.title = ggplot2::element_text(size = input$axis_title_size),
          plot.title = ggplot2::element_text(size = input$axis_title_size + 2, face = "bold")
        )
    }

    .get_colors <- function(n) {
      switch(input$color_palette,
        viridis  = viridisLite::viridis(n),
        Set1     = RColorBrewer::brewer.pal(min(n, 9), "Set1"),
        Dark2    = RColorBrewer::brewer.pal(min(n, 8), "Dark2"),
        manual   = {
          cols <- trimws(strsplit(input$manual_colors, ",")[[1]])
          rep(cols, length.out = n)
        },
        scales::hue_pal()(n)
      )
    }

    .get_data <- reactive({
      df <- data_reactive(); req(df)
      # Apply row filter
      fcol <- input$filter_col
      if (!is.null(fcol) && nzchar(fcol) && fcol %in% names(df)) {
        fvals <- input$filter_vals
        if (!is.null(fvals) && length(fvals) > 0)
          df <- df[df[[fcol]] %in% fvals, , drop = FALSE]
      }
      # Apply group filter
      gcol <- input$group_col
      if (!is.null(gcol) && nzchar(gcol) && gcol %in% names(df)) {
        gvals <- input$group_vals
        if (!is.null(gvals) && length(gvals) > 0)
          df <- df[df[[gcol]] %in% gvals, , drop = FALSE]
      }
      df
    })

    # ── Reactive results store ────────────────────────────────────────────────

    r_plot        <- reactiveVal(NULL)
    r_descriptive <- reactiveVal(NULL)
    r_normality   <- reactiveVal(NULL)
    r_group_test  <- reactiveVal(NULL)
    r_posthoc     <- reactiveVal(NULL)
    r_regression  <- reactiveVal(NULL)
    r_correlation <- reactiveVal(NULL)
    r_disparity   <- reactiveVal(NULL)

    # ── Run ───────────────────────────────────────────────────────────────────

    observeEvent(input$run, {
      df <- tryCatch(.get_data(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        showNotification("No data available. Please import data first.", type = "warning")
        return()
      }

      x_col   <- input$x_col
      y_col   <- input$y_col
      gcol    <- input$group_col
      has_grp <- !is.null(gcol) && nzchar(gcol) && gcol %in% names(df)

      # Make group a factor for plotting if present
      if (has_grp) df[[gcol]] <- factor(df[[gcol]])
      grp_levels <- if (has_grp) levels(df[[gcol]]) else character(0)
      n_groups   <- length(grp_levels)
      colors     <- if (n_groups > 0) .get_colors(n_groups) else .get_colors(1)
      title_str  <- if (nzchar(input$plot_title)) input$plot_title else NULL

      # ── Plot ──────────────────────────────────────────────────────────────

      plt <- tryCatch({
        switch(input$plot_type,

          boxplot = {
            aes_base <- if (has_grp)
              ggplot2::aes(x = .data[[gcol]], y = .data[[y_col]], fill = .data[[gcol]])
            else
              ggplot2::aes(x = "", y = .data[[y_col]])
            p <- ggplot2::ggplot(df, aes_base) +
              ggplot2::geom_boxplot(
                notch    = isTRUE(input$bp_notch),
                outlier.shape = if (isTRUE(input$bp_outliers)) 19 else NA,
                alpha    = input$alpha,
                width    = 0.5
              )
            if (isTRUE(input$bp_jitter))
              p <- p + ggplot2::geom_jitter(width = 0.15, size = input$point_size,
                                            alpha = input$alpha * 0.7, show.legend = FALSE)
            if (has_grp && n_groups > 0)
              p <- p + ggplot2::scale_fill_manual(values = colors)
            p <- p + ggplot2::labs(title = title_str, x = if (has_grp) gcol else "",
                                   y = y_col, fill = gcol)
            .apply_theme(p)
          },

          scatter = {
            aes_base <- if (has_grp)
              ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[gcol]])
            else
              ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])
            p <- ggplot2::ggplot(df, aes_base) +
              ggplot2::geom_point(size = input$point_size, alpha = input$alpha) +
              ggplot2::geom_smooth(method = input$reg_method, se = isTRUE(input$reg_se))
            if (isTRUE(input$scatter_label))
              p <- p + ggplot2::geom_text(ggplot2::aes(label = rownames(df)),
                                          size = 2.5, vjust = -0.5)
            if (has_grp && n_groups > 0)
              p <- p + ggplot2::scale_color_manual(values = colors)
            p <- p + ggplot2::labs(title = title_str, x = x_col, y = y_col, color = gcol)
            .apply_theme(p)
          },

          violin = {
            aes_base <- if (has_grp)
              ggplot2::aes(x = .data[[gcol]], y = .data[[y_col]], fill = .data[[gcol]])
            else
              ggplot2::aes(x = "", y = .data[[y_col]])
            p <- ggplot2::ggplot(df, aes_base) +
              ggplot2::geom_violin(scale = "area", adjust = input$vio_scale, alpha = input$alpha)
            if (isTRUE(input$vio_box))
              p <- p + ggplot2::geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA)
            if (isTRUE(input$vio_jitter))
              p <- p + ggplot2::geom_jitter(width = 0.08, size = input$point_size, alpha = 0.5)
            if (has_grp && n_groups > 0)
              p <- p + ggplot2::scale_fill_manual(values = colors)
            p <- p + ggplot2::labs(title = title_str, x = if (has_grp) gcol else "",
                                   y = y_col, fill = gcol)
            .apply_theme(p)
          },

          histogram = {
            aes_base <- if (has_grp)
              ggplot2::aes(x = .data[[y_col]], fill = .data[[gcol]])
            else
              ggplot2::aes(x = .data[[y_col]])
            p <- ggplot2::ggplot(df, aes_base) +
              ggplot2::geom_histogram(bins = input$hist_bins, alpha = input$alpha,
                                      position = "identity")
            if (isTRUE(input$hist_density))
              p <- p + ggplot2::geom_density(
                ggplot2::aes(y = ggplot2::after_stat(count) * (max(df[[y_col]], na.rm=TRUE) -
                               min(df[[y_col]], na.rm=TRUE)) / input$hist_bins),
                color = "black", fill = NA, linewidth = 0.8)
            if (isTRUE(input$hist_rug))
              p <- p + ggplot2::geom_rug(alpha = 0.4)
            if (has_grp && n_groups > 0)
              p <- p + ggplot2::scale_fill_manual(values = colors)
            p <- p + ggplot2::labs(title = title_str, x = y_col, y = "Count", fill = gcol)
            .apply_theme(p)
          },

          heatmap = {
            hcols <- input$heatmap_cols
            req(length(hcols) >= 2)
            cm   <- cor(df[, hcols, drop = FALSE], use = "pairwise.complete.obs",
                        method = input$cor_method)
            cm_long <- as.data.frame(as.table(cm))
            names(cm_long) <- c("Var1", "Var2", "Correlation")
            p <- ggplot2::ggplot(cm_long, ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]], fill = .data[["Correlation"]])) +
              ggplot2::geom_tile(color = "white") +
              ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                            midpoint = 0, limits = c(-1, 1)) +
              ggplot2::geom_text(ggplot2::aes(label = round(.data[["Correlation"]], 2)), size = 3) +
              ggplot2::coord_fixed() +
              ggplot2::labs(title = title_str %||% "Correlation Heatmap", x = NULL, y = NULL)
            .apply_theme(p) +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          },

          bar = {
            req(has_grp || nzchar(x_col))
            x_var <- if (has_grp) gcol else x_col
            stat_fn <- switch(input$bar_stat,
              mean   = function(x) mean(x, na.rm = TRUE),
              median = function(x) median(x, na.rm = TRUE),
              count  = function(x) length(x)
            )
            sd_fn <- function(x) if (length(x) > 1) sd(x, na.rm = TRUE) else 0
            bar_df <- do.call(rbind, lapply(split(df, df[[x_var]]), function(sub) {
              data.frame(
                grp  = sub[[x_var]][1],
                val  = stat_fn(sub[[y_col]]),
                err  = sd_fn(sub[[y_col]]),
                stringsAsFactors = FALSE
              )
            }))
            p <- ggplot2::ggplot(bar_df, ggplot2::aes(x = .data[["grp"]], y = .data[["val"]], fill = .data[["grp"]])) +
              ggplot2::geom_col(alpha = input$alpha)
            if (isTRUE(input$bar_error) && input$bar_stat != "count")
              p <- p + ggplot2::geom_errorbar(
                ggplot2::aes(ymin = .data[["val"]] - .data[["err"]], ymax = .data[["val"]] + .data[["err"]]), width = 0.2)
            if (isTRUE(input$bar_coord_flip)) p <- p + ggplot2::coord_flip()
            p <- p + ggplot2::scale_fill_manual(values = .get_colors(nrow(bar_df))) +
              ggplot2::labs(title = title_str, x = x_var, y = input$bar_stat, fill = x_var)
            .apply_theme(p)
          }
        )
      }, error = function(e) {
        showNotification(paste("Plot error:", conditionMessage(e)), type = "error")
        NULL
      })
      r_plot(plt)

      # ── Descriptive stats ─────────────────────────────────────────────────

      if (isTRUE(input$stat_descriptive)) {
        desc <- tryCatch({
          nc <- names(df)[vapply(df, is.numeric, logical(1))]
          if (has_grp) {
            do.call(rbind, lapply(levels(df[[gcol]]), function(g) {
              sub <- df[df[[gcol]] == g, nc, drop = FALSE]
              data.frame(
                Group    = g,
                Column   = nc,
                N        = vapply(sub, function(x) sum(!is.na(x)), integer(1)),
                Mean     = round(vapply(sub, mean,   numeric(1), na.rm = TRUE), 4),
                SD       = round(vapply(sub, sd,     numeric(1), na.rm = TRUE), 4),
                Median   = round(vapply(sub, median, numeric(1), na.rm = TRUE), 4),
                IQR      = round(vapply(sub, IQR,    numeric(1), na.rm = TRUE), 4),
                Min      = round(vapply(sub, min,    numeric(1), na.rm = TRUE), 4),
                Max      = round(vapply(sub, max,    numeric(1), na.rm = TRUE), 4),
                row.names = NULL, stringsAsFactors = FALSE
              )
            }))
          } else {
            data.frame(
              Column   = nc,
              N        = vapply(df[, nc, drop=FALSE], function(x) sum(!is.na(x)), integer(1)),
              Mean     = round(vapply(df[, nc, drop=FALSE], mean,   numeric(1), na.rm = TRUE), 4),
              SD       = round(vapply(df[, nc, drop=FALSE], sd,     numeric(1), na.rm = TRUE), 4),
              Median   = round(vapply(df[, nc, drop=FALSE], median, numeric(1), na.rm = TRUE), 4),
              IQR      = round(vapply(df[, nc, drop=FALSE], IQR,    numeric(1), na.rm = TRUE), 4),
              Min      = round(vapply(df[, nc, drop=FALSE], min,    numeric(1), na.rm = TRUE), 4),
              Max      = round(vapply(df[, nc, drop=FALSE], max,    numeric(1), na.rm = TRUE), 4),
              row.names = NULL, stringsAsFactors = FALSE
            )
          }
        }, error = function(e) {
          showNotification(paste("Descriptive stats error:", conditionMessage(e)), type = "warning")
          NULL
        })
        r_descriptive(desc)
      }

      # ── Normality ─────────────────────────────────────────────────────────

      if (isTRUE(input$stat_normality)) {
        norm_txt <- tryCatch({
          nc <- names(df)[vapply(df, is.numeric, logical(1))]
          lines <- "=== Shapiro-Wilk Normality Tests ===\n"
          groups_to_test <- if (has_grp) levels(df[[gcol]]) else list(NULL)
          for (g in groups_to_test) {
            sub <- if (is.null(g)) df else df[df[[gcol]] == g, , drop = FALSE]
            header <- if (is.null(g)) "" else paste0("\nGroup: ", g, "\n")
            lines <- paste0(lines, header)
            for (col in nc) {
              x <- sub[[col]][!is.na(sub[[col]])]
              if (length(x) < 3) {
                lines <- paste0(lines, sprintf("  %-20s : too few observations\n", col))
                next
              }
              if (length(x) > 5000) x <- sample(x, 5000)
              sw <- shapiro.test(x)
              lines <- paste0(lines, sprintf("  %-20s : W = %.4f, p = %.4f %s\n",
                col, sw$statistic, sw$p.value,
                ifelse(sw$p.value < 0.05, "(non-normal *)", "")))
            }
          }
          lines
        }, error = function(e) paste("Normality error:", conditionMessage(e)))
        r_normality(norm_txt)
      }

      # ── Group tests ───────────────────────────────────────────────────────

      if (isTRUE(input$stat_group_test) && has_grp && n_groups >= 2) {
        gt_txt <- tryCatch({
          nc <- names(df)[vapply(df, is.numeric, logical(1))]
          lines <- if (n_groups == 2)
            "=== Two-Group Tests (t-test & Wilcoxon) ===\n"
          else
            "=== Multi-Group Tests (one-way ANOVA & Kruskal-Wallis) ===\n"
          for (col in nc) {
            lines <- paste0(lines, "\nVariable: ", col, "\n")
            form <- as.formula(paste(col, "~", gcol))
            if (n_groups == 2) {
              tt  <- t.test(form, data = df)
              wt  <- wilcox.test(form, data = df)
              lines <- paste0(lines,
                sprintf("  t-test       : t = %.3f, df = %.1f, p = %.4f\n",
                        tt$statistic, tt$parameter, tt$p.value),
                sprintf("  Wilcoxon     : W = %.1f, p = %.4f\n",
                        wt$statistic, wt$p.value))
            } else {
              av  <- summary(aov(form, data = df))
              kw  <- kruskal.test(form, data = df)
              f_p <- av[[1]][["Pr(>F)"]][1]
              f_v <- av[[1]][["F value"]][1]
              lines <- paste0(lines,
                sprintf("  ANOVA        : F = %.3f, p = %.4f\n", f_v, f_p),
                sprintf("  Kruskal-Wallis: chi2 = %.3f, df = %d, p = %.4f\n",
                        kw$statistic, kw$parameter, kw$p.value))
            }
          }
          lines
        }, error = function(e) paste("Group test error:", conditionMessage(e)))
        r_group_test(gt_txt)
      }

      # ── Post-hoc ──────────────────────────────────────────────────────────

      if (isTRUE(input$stat_posthoc) && has_grp && n_groups >= 2) {
        ph_txt <- tryCatch({
          nc <- names(df)[vapply(df, is.numeric, logical(1))]
          adj <- input$posthoc_adjust
          lines <- paste0("=== Post-hoc Tests (p-adjust: ", adj, ") ===\n")
          for (col in nc) {
            lines <- paste0(lines, "\nVariable: ", col, "\n")
            form <- as.formula(paste(col, "~", gcol))
            if (n_groups == 2) {
              lines <- paste0(lines, "  (only 2 groups — see Group Tests tab)\n")
            } else {
              # Tukey HSD
              tk  <- TukeyHSD(aov(form, data = df))[[gcol]]
              tk_df <- as.data.frame(tk)
              tk_df$padj <- p.adjust(tk_df[["p adj"]], method = adj)
              lines <- paste0(lines, "  Tukey HSD:\n")
              for (i in seq_len(nrow(tk_df))) {
                lines <- paste0(lines, sprintf("    %-30s diff=%.3f, p_adj=%.4f %s\n",
                  rownames(tk_df)[i], tk_df$diff[i], tk_df$padj[i],
                  ifelse(tk_df$padj[i] < 0.05, "*", "")))
              }
              # Dunn (non-parametric) via base pairwise.wilcox.test
              pw  <- pairwise.wilcox.test(df[[col]], df[[gcol]], p.adjust.method = adj)
              p_mat <- pw$p.value
              lines <- paste0(lines, "  Pairwise Wilcoxon:\n")
              for (r in rownames(p_mat)) {
                for (cc in colnames(p_mat)) {
                  pv <- p_mat[r, cc]
                  if (!is.na(pv))
                    lines <- paste0(lines, sprintf("    %-15s vs %-15s p_adj=%.4f %s\n",
                      r, cc, pv, ifelse(pv < 0.05, "*", "")))
                }
              }
            }
          }
          lines
        }, error = function(e) paste("Post-hoc error:", conditionMessage(e)))
        r_posthoc(ph_txt)
      }

      # ── Regression ────────────────────────────────────────────────────────

      if (isTRUE(input$stat_regression) && x_col %in% names(df) && y_col %in% names(df)) {
        reg_txt <- tryCatch({
          x_num <- is.numeric(df[[x_col]])
          if (!x_num) {
            "Regression requires a numeric X column."
          } else {
            form <- as.formula(paste(y_col, "~", x_col,
                                     if (has_grp) paste("+", gcol) else ""))
            fit  <- lm(form, data = df)
            sm   <- summary(fit)
            coef_df <- as.data.frame(sm$coefficients)
            names(coef_df) <- c("Estimate", "Std.Error", "t.value", "p.value")
            coef_lines <- paste(capture.output(print(round(coef_df, 4))), collapse = "\n")
            paste0(
              "=== Linear Regression ===\n",
              "Formula : ", deparse(form), "\n",
              "R²       : ", round(sm$r.squared, 4), "\n",
              "Adj. R²  : ", round(sm$adj.r.squared, 4), "\n",
              "F-stat   : ", round(sm$fstatistic[1], 3),
              "  (df1=", sm$fstatistic[2], ", df2=", sm$fstatistic[3], ")\n",
              "p-value  : ", format.pval(pf(sm$fstatistic[1], sm$fstatistic[2],
                                             sm$fstatistic[3], lower.tail = FALSE), digits = 4), "\n\n",
              "Coefficients:\n", coef_lines, "\n"
            )
          }
        }, error = function(e) paste("Regression error:", conditionMessage(e)))
        r_regression(reg_txt)
      }

      # ── Correlation ───────────────────────────────────────────────────────

      if (isTRUE(input$stat_correlation)) {
        cor_res <- tryCatch({
          nc <- names(df)[vapply(df, is.numeric, logical(1))]
          if (length(nc) < 2) return(NULL)
          cm <- cor(df[, nc, drop = FALSE], use = "pairwise.complete.obs",
                    method = input$cor_method)
          round(cm, 3)
        }, error = function(e) {
          showNotification(paste("Correlation error:", conditionMessage(e)), type = "warning")
          NULL
        })
        r_correlation(cor_res)
      }

      # ── Disparity (dispRity) ──────────────────────────────────────────────

      if (isTRUE(input$stat_disparity) && has_grp) {
        disp_res <- tryCatch({
          dcols <- input$disparity_cols
          req(length(dcols) >= 2)
          mat <- as.matrix(df[, dcols, drop = FALSE])
          grp_list <- lapply(levels(df[[gcol]]), function(g) {
            which(df[[gcol]] == g)
          })
          names(grp_list) <- levels(df[[gcol]])

          # Build dispRity object and compute metrics
          disp_obj <- dispRity::dispRity(
            data      = mat,
            metric    = c(dispRity::sum.variances, dispRity::sum.ranges,
                          dispRity::mean.pairwise.distance),
            subsets   = grp_list
          )
          disp_summary <- summary(disp_obj)

          # Permutation test between groups
          disp_test <- tryCatch({
            dispRity::test.dispRity(disp_obj,
              test          = wilcox.test,
              comparison    = "pairwise",
              correction    = input$posthoc_adjust,
              rarefaction   = FALSE
            )
          }, error = function(e) NULL)

          list(summary = disp_summary, test = disp_test)
        }, error = function(e) {
          showNotification(paste("Disparity error:", conditionMessage(e)), type = "warning")
          NULL
        })
        r_disparity(disp_res)
      }

      showNotification("Analysis complete.", type = "message")
    })

    # ── Outputs ───────────────────────────────────────────────────────────────

    output$main_plot <- renderPlot({
      p <- r_plot(); req(p); print(p)
    })

    output$tbl_descriptive <- DT::renderDataTable({
      d <- r_descriptive(); req(d)
      DT::datatable(d, options = list(scrollX = TRUE, pageLength = 15),
                    rownames = FALSE) |>
        DT::formatRound(which(vapply(d, is.numeric, logical(1))), digits = 4)
    })

    output$txt_normality <- renderText({
      t <- r_normality(); req(t); t
    })

    output$txt_group_test <- renderText({
      t <- r_group_test(); req(t); t
    })

    output$txt_posthoc <- renderText({
      t <- r_posthoc(); req(t); t
    })

    output$txt_regression <- renderText({
      t <- r_regression(); req(t); t
    })

    output$tbl_correlation <- DT::renderDataTable({
      cm <- r_correlation(); req(cm)
      df_cm <- as.data.frame(cm)
      DT::datatable(df_cm, options = list(scrollX = TRUE, pageLength = 20)) |>
        DT::formatRound(colnames(df_cm), digits = 3)
    })

    output$cor_heatmap <- renderPlot({
      cm <- r_correlation(); req(cm)
      cm_long <- as.data.frame(as.table(cm))
      names(cm_long) <- c("Var1", "Var2", "Correlation")
      p <- ggplot2::ggplot(cm_long, ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]], fill = .data[["Correlation"]])) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                      midpoint = 0, limits = c(-1, 1)) +
        ggplot2::geom_text(ggplot2::aes(label = round(.data[["Correlation"]], 2)), size = 3) +
        ggplot2::coord_fixed() +
        ggplot2::labs(title = paste(input$cor_method, "Correlation"), x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      print(p)
    })

    output$tbl_disparity <- DT::renderDataTable({
      res <- r_disparity(); req(res)
      sm  <- res$summary; req(sm)
      DT::datatable(as.data.frame(sm), options = list(scrollX = TRUE), rownames = FALSE)
    })

    output$txt_disparity_test <- renderText({
      res <- r_disparity(); req(res)
      tst <- res$test
      if (is.null(tst)) return("Pairwise disparity test not available.")
      paste(capture.output(print(tst)), collapse = "\n")
    })

    # ── Downloads ─────────────────────────────────────────────────────────────

    .render_plot_dl <- function(ext) {
      function(file) {
        p <- r_plot()
        if (is.null(p)) {
          write("No plot generated yet.", file); return()
        }
        ggplot2::ggsave(file, plot = p, device = ext,
                        width  = input$export_width,
                        height = input$export_height)
      }
    }

    output$dl_png <- downloadHandler(
      filename = function() paste0("data_explorer_", Sys.Date(), ".png"),
      content  = .render_plot_dl("png")
    )

    output$dl_pdf <- downloadHandler(
      filename = function() paste0("data_explorer_", Sys.Date(), ".pdf"),
      content  = .render_plot_dl("pdf")
    )

    output$dl_csv <- downloadHandler(
      filename = function() paste0("data_explorer_stats_", Sys.Date(), ".csv"),
      content  = function(file) {
        desc <- r_descriptive()
        if (!is.null(desc)) {
          utils::write.csv(desc, file, row.names = FALSE)
        } else {
          write("No descriptive statistics computed yet.", file)
        }
      }
    )

    invisible(list(plot = r_plot, descriptive = r_descriptive))
  })
}

# Null-coalescing operator (local to this file if not already defined)
`%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
