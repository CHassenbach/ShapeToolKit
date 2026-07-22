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
              helpText("The numeric variable whose distribution you want to visualise."),
              uiOutput(ns("dp_group_ui")),
              helpText("Optional: split the distribution by a categorical column (e.g. species, stage)."),
              uiOutput(ns("dp_groupvals_ui")),
              helpText("Deselect levels to exclude them from the plot."),
              selectInput(ns("dp_type"), "Plot type",
                choices = c("Boxplot" = "boxplot", "Violin" = "violin",
                            "Histogram / Density" = "histogram"),
                selected = "boxplot"),
              helpText("Boxplot: shows median, IQR (box), and whiskers (±1.5×IQR).\nViolin: like a boxplot but shows the full density shape.\nHistogram: shows how many specimens fall in each value range."),
              # Multi-column note for boxplot/violin
              conditionalPanel(
                condition = paste0("input['", ns("dp_type"), "'] != 'histogram'"),
                helpText("Select one or more numeric columns. When multiple are chosen they appear\nside-by-side in one plot with your group variable as fill colour.")
              ),
              # Boxplot options
              conditionalPanel(
                condition = paste0("input['", ns("dp_type"), "'] == 'boxplot'"),
                checkboxInput(ns("bp_notch"),   "Notched",          value = FALSE),
                helpText("Notches show a 95% CI around the median. Non-overlapping notches\nsuggest medians differ significantly."),
                checkboxInput(ns("bp_jitter"),  "Overlay jitter",   value = TRUE),
                helpText("Adds individual data points with random horizontal scatter so\nyou can see the actual sample size and distribution."),
                checkboxInput(ns("bp_outliers"),"Show outliers",    value = TRUE),
                helpText("Outliers are points > 1.5×IQR beyond the box hinges. Hiding them\ncan reduce visual clutter without losing the box shape.")
              ),
              # Violin options
              conditionalPanel(
                condition = paste0("input['", ns("dp_type"), "'] == 'violin'"),
                checkboxInput(ns("vio_box"),    "Overlay boxplot",  value = TRUE),
                helpText("Draws a thin boxplot inside the violin to show median and IQR\nalongside the density shape."),
                checkboxInput(ns("vio_jitter"), "Overlay jitter",   value = FALSE),
                helpText("Adds raw data points. Useful for small samples to show every specimen.")
              ),
              # Histogram options
              conditionalPanel(
                condition = paste0("input['", ns("dp_type"), "'] == 'histogram'"),
                numericInput(ns("hist_bins"), "Bins", value = 30, min = 2, step = 1),
                helpText("Number of vertical bars. More bins → finer detail but noisier.\nFewer bins → smoother overview. Try 15\u201350 as a starting range."),
                checkboxInput(ns("hist_density"), "Overlay density curve", value = TRUE),
                helpText("Draws a smooth Kernel Density Estimate (KDE) on top of the histogram.\nThe curve shows the continuous shape of the distribution."),
                checkboxInput(ns("hist_normalize"), "Normalize to density", value = TRUE),
                helpText("Normalise Y to probability density so groups with very different\nsample sizes can be visually compared. Disable to show raw counts.")
              ),
              hr(),
              .appearance_controls(ns, prefix = "dp"),
              actionButton(ns("dp_run"), "Plot", class = "btn-success btn-block")
            ),
            output_widget = tagList(
              shinycssloaders::withSpinner(
                plotOutput(ns("dp_plot"), height = 430)
              ),
              tags$div(style = "margin-top: 6px;",
                downloadButton(ns("dp_dl_png"), "PNG", class = "btn-default btn-sm"),
                downloadButton(ns("dp_dl_pdf"), "PDF", class = "btn-default btn-sm"),
                downloadButton(ns("dp_dl_rds"), "RDS", class = "btn-default btn-sm")
              )
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
              helpText("Predictor variable plotted on the horizontal axis."),
              uiOutput(ns("sc_y_ui")),
              helpText("Response variable plotted on the vertical axis."),
              uiOutput(ns("sc_group_ui")),
              helpText("Optional: colour points by a categorical variable. Each group\ngets its own regression line if selected."),
              uiOutput(ns("sc_groupvals_ui")),
              hr(),
              selectInput(ns("sc_method"), "Regression line",
                choices = c("Linear (lm)" = "lm", "LOESS" = "loess", "GAM" = "gam"),
                selected = "lm"),
              helpText("lm: straight line; best for testing a linear relationship.\nLOESS: locally weighted smooth curve; reveals non-linear trends\nwithout assuming a specific shape.\nGAM: Generalised Additive Model; flexible smooth like LOESS but\nstatistically principled."),
              checkboxInput(ns("sc_se"), "Confidence band", value = TRUE),
              helpText("Shaded band around the regression line shows the 95% CI of the\nfitted mean. Wider bands = more uncertainty (small N or high scatter)."),
              hr(),
              tags$strong("Formula (for lm summary)"),
              helpText("Used for the linear model summary below the plot. Edit to add\ncovariates, interactions, or polynomial terms. Examples:\ny ~ x  → simple regression\ny ~ x + z  → multiple regression (z held constant)\ny ~ x * z  → x + z + their interaction"),
              uiOutput(ns("sc_formula_ui")),
              checkboxInput(ns("sc_label"), "Label points (row names)", value = FALSE),
              helpText("Annotates each point with its row name/index. Useful for\nidentifying individual specimens."),
              hr(),
              .appearance_controls(ns, prefix = "sc"),
              actionButton(ns("sc_run"), "Plot + Fit", class = "btn-success btn-block")
            ),
            output_widget = tagList(
              shinycssloaders::withSpinner(plotOutput(ns("sc_plot"), height = 340)),
              verbatimTextOutput(ns("sc_regression_txt")),
              tags$div(style = "margin-top: 6px;",
                downloadButton(ns("sc_dl_png"), "PNG", class = "btn-default btn-sm"),
                downloadButton(ns("sc_dl_pdf"), "PDF", class = "btn-default btn-sm"),
                downloadButton(ns("sc_dl_rds"), "RDS", class = "btn-default btn-sm")
              )
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
              helpText("Select the numeric columns to include in the correlation matrix.\nOnly selected columns appear in the heatmap and table."),
              selectInput(ns("cor_method"), "Method",
                choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                selected = "pearson"),
              helpText("Pearson (r): measures linear association; assumes normally\ndistributed data. Best for continuous numeric variables.\nSpearman (ρ): rank-based; robust to outliers and non-normal\ndistributions. Preferred for ordinal data or skewed morphometrics."),
              uiOutput(ns("cor_group_ui")),
              helpText("Optional: subset rows to a specific group before computing correlations.\nUseful for testing whether relationships differ between groups."),
              uiOutput(ns("cor_groupvals_ui")),
              hr(),
              checkboxInput(ns("cor_pvals"), "Show p-values", value = TRUE),
              helpText("Displays the p-value for each pairwise correlation (H₀: r = 0).\np < 0.05 indicates the correlation is unlikely to be zero by chance.\nCaution: many simultaneous tests inflate false-positive risk."),
              actionButton(ns("cor_run"), "Calculate", class = "btn-success btn-block"),
              br(),
              downloadButton(ns("cor_dl_csv"), "Download CSV", class = "btn-default btn-sm btn-block"),
              downloadButton(ns("cor_dl_rds"), "Download Heatmap RDS", class = "btn-default btn-sm btn-block")
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
              # ── Mode toggle ──────────────────────────────────────────────
              checkboxInput(ns("gt_multivariate"),
                            tags$strong("Test across multiple PCs (PERMANOVA)"),
                            value = FALSE),
              helpText("Unchecked: univariate tests on a single variable (t-test / ANOVA / KW).\nChecked: multivariate PERMANOVA + PERMDISP across several PC axes."),
              hr(),
              # ── Always-visible: group column + filter ────────────────────
              uiOutput(ns("gt_group_ui")),
              helpText("The categorical variable that defines the groups to compare."),
              uiOutput(ns("gt_groupvals_ui")),
              helpText("Include only these levels in the test. Deselect to exclude a group."),
              hr(),
              # ── Univariate controls (hidden in multivariate mode) ─────────
              conditionalPanel(
                condition = paste0("!input['", ns("gt_multivariate"), "']"),
                uiOutput(ns("gt_y_ui")),
                helpText("The continuous numeric variable to test between groups (e.g. a PC score)."),
                hr(),
                selectInput(ns("gt_parametric"), "Parametric test",
                  choices = c("t-test (2 groups)" = "ttest",
                              "One-way ANOVA"      = "anova"),
                  selected = "anova"),
                helpText("t-test: compares means of exactly 2 groups; assumes normality.\nANOVA: compares means across 3+ groups simultaneously; assumes\nnormality and equal variances."),
                selectInput(ns("gt_nonparam"), "Non-parametric test",
                  choices = c("Wilcoxon / Mann-Whitney" = "wilcox",
                              "Kruskal-Wallis"          = "kruskal"),
                  selected = "kruskal"),
                helpText("Wilcoxon / Mann-Whitney: compares 2 groups on ranks; does not\nassume normality. Use when Shapiro-Wilk is significant.\nKruskal-Wallis: rank-based ANOVA equivalent for 3+ groups."),
                hr(),
                checkboxInput(ns("gt_posthoc"), "Post-hoc test", value = TRUE),
                helpText("Run after a significant ANOVA/KW to identify which specific group\npairs differ. Applies multiple-comparison correction."),
                conditionalPanel(
                  condition = paste0("input['", ns("gt_posthoc"), "'] == true"),
                  selectInput(ns("gt_posthoc_type"), "Post-hoc method",
                    choices = c("Tukey HSD (parametric)" = "tukey",
                                "Pairwise Wilcoxon"      = "wilcox_pw"),
                    selected = "tukey"),
                  helpText("Tukey HSD: controls family-wise error for all pairwise ANOVA\ncomparisons; use when ANOVA is appropriate.\nPairwise Wilcoxon: non-parametric pairwise comparisons; use when\ndata violate normality."),
                  selectInput(ns("gt_padj"), "P-value adjustment",
                    choices = c("BH" = "BH", "Bonferroni" = "bonferroni",
                                "Holm" = "holm", "None" = "none"),
                    selected = "BH"),
                  helpText("BH (Benjamini-Hochberg): controls the False Discovery Rate (FDR);\nless conservative, recommended for exploratory analyses.\nBonferroni: most conservative (controls family-wise error rate).\nHolm: step-down Bonferroni; slightly less conservative than Bonferroni.")
                ),
                checkboxInput(ns("gt_normality"), "Shapiro-Wilk per group", value = TRUE),
                helpText("Tests whether each group\'s values are normally distributed.\nW close to 1 = normal; p < 0.05 suggests non-normality → prefer\nthe non-parametric test results."),
                checkboxInput(ns("gt_qqplot"), "Show QQ plots (per group + residuals)", value = FALSE),
                helpText("Displays normal QQ plots for each group and ANOVA residuals.\nUse the selector to navigate. Points on the reference line → normal data.")
              ),
              # ── Multivariate / PERMANOVA controls ────────────────────────
              conditionalPanel(
                condition = paste0("input['", ns("gt_multivariate"), "']"),
                uiOutput(ns("gt_permanova_cols_ui")),
                helpText("Select the PC / numeric columns as the multivariate response.\nAll selected axes are analysed simultaneously."),
                uiOutput(ns("gt_permanova_groups_ui")),
                helpText("One or more categorical columns that define the grouping.\nMultiple columns produce an interaction term by default\n(e.g. Genus * Caste)."),
                uiOutput(ns("gt_permanova_rhs_ui")),
                uiOutput(ns("gt_permanova_strata_ui")),
                helpText("Optional: restrict permutations within strata (blocks).\nSet to a species/genus column to test Caste + Bioregion effects\nwhile holding the block structure constant \u2014 the PERMANOVA\nequivalent of  mat ~ Caste + Bioregion + (1|Species).\nTip: also add the strata column to your formula to partition\nout its fixed-effect variance at the same time."),
                numericInput(ns("gt_permanova_perms"), "Permutations", value = 999,
                             min = 99, max = 9999, step = 100),
                helpText("Number of permutations. 999 is standard; use 4999 for publication."),
                selectInput(ns("gt_permanova_dist"), "Distance metric",
                  choices = c("Euclidean" = "euclidean", "Manhattan" = "manhattan"),
                  selected = "euclidean"),
                helpText("Euclidean: appropriate for PC scores and centred morphometric data.\nManhattan: city-block distance; more robust to outliers in high dimensions."),
                selectInput(ns("gt_permanova_by"), "Term testing (adonis2 'by')",
                  choices = c(
                    "Marginal / Type III  (by = 'margin')" = "margin",
                    "Sequential / Type I  (by = 'terms')" = "terms"
                  ),
                  selected = "margin"),
                helpText("'margin' (recommended for multi-factor): each term is tested after\nall other terms are already in the model \u2014 order-independent.\n'terms': sequential (Type I) \u2014 each term added in formula order;\nR\u00b2 and p depend on the order of predictors."),
                checkboxInput(ns("gt_permanova_pairwise"), "Pairwise PERMANOVA", value = TRUE),
                helpText("Run adonis2 on every pair of levels separately (single grouping variable only).\nWith multiple grouping variables the omnibus table already partitions\nvariance per term \u2014 pairwise is skipped automatically."),
                conditionalPanel(
                  condition = paste0("input['", ns("gt_permanova_pairwise"), "'] == true"),
                  selectInput(ns("gt_permanova_padj"), "Pairwise p-value adjustment",
                    choices = c("BH" = "BH", "Bonferroni" = "bonferroni",
                                "Holm" = "holm", "None" = "none"),
                    selected = "BH"),
                  helpText("Correction applied across all pairwise p-values.\nBH: recommended for exploratory work (FDR control).\nBonferroni: most conservative (family-wise error control).")
                ),
                hr(),
                checkboxInput(ns("gt_permanova_parallel"), "Parallel processing", value = FALSE),
                helpText("Distribute permutations across multiple CPU cores to speed up\ncomputation. Requires the 'parallel' package (included with R)."),
                conditionalPanel(
                  condition = paste0("input['", ns("gt_permanova_parallel"), "'] == true"),
                  uiOutput(ns("gt_permanova_cores_ui")),
                  helpText("Leave 1-2 cores free for the OS. More cores = faster permutation\nloop but higher memory usage.")
                )
              ),
              hr(),
              actionButton(ns("gt_run"), "Calculate", class = "btn-success btn-block"),
              br(),
              downloadButton(ns("gt_dl_csv"), "Download Stats CSV", class = "btn-default btn-sm btn-block"),
              downloadButton(ns("gt_dl_qq_rds"), "Download QQ Plot RDS", class = "btn-default btn-sm btn-block")
            ),
            output_widget = tagList(
              verbatimTextOutput(ns("gt_results"), placeholder = TRUE),
              conditionalPanel(
                condition = paste0("!input['", ns("gt_multivariate"), "'] && input['", ns("gt_qqplot"), "'] == true"),
                tags$hr(),
                tags$strong("QQ Plots"),
                uiOutput(ns("gt_qq_nav_ui")),
                plotOutput(ns("gt_qqplot_out"), height = 350)
              ),
              conditionalPanel(
                condition = paste0("input['", ns("gt_multivariate"), "']"),
                tags$hr(),
                verbatimTextOutput(ns("gt_permanova_txt"), placeholder = TRUE),
                tags$div(style = "margin-top: 6px;",
                  downloadButton(ns("gt_dl_permanova_txt"), "Save results (.txt)",
                                 class = "btn-default btn-sm"),
                  downloadButton(ns("gt_dl_permanova_rds"), "Save results (.rds)",
                                 class = "btn-default btn-sm")
                )
              )
            )
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
              helpText("Select the PC axes (or any numeric columns) that define your morphospace.\nAll selected columns are used together to compute the disparity metrics."),
              uiOutput(ns("disp_group_ui")),
              helpText("The categorical variable that defines the groups to compare.\nDisparity is calculated separately for each group."),
              uiOutput(ns("disp_groupvals_ui")),
              helpText("Include only these groups in the analysis."),
              hr(),
              checkboxInput(ns("disp_sov"),  "Sum of Variances (SOV)",        value = TRUE),
              helpText("SOV = sum(diag(var(X))). Measures the total spread of specimens\nin morphospace. Larger SOV = greater morphological diversity.\nSensitive to the number of axes included \u2014 always compare groups\nusing the same set of PC columns."),
              checkboxInput(ns("disp_sor"),  "Sum of Ranges (SOR)",           value = TRUE),
              helpText("SOR = sum of (max \u2212 min) per PC axis. Measures the total extent\nof occupied morphospace. More sensitive to extreme specimens\n(outliers) than SOV."),
              checkboxInput(ns("disp_mpd"),  "Mean Pairwise Distance (MPD)",  value = TRUE),
              helpText("MPD = mean Euclidean distance between all specimen pairs in a group.\nCaptures average morphological diversity regardless of axis scaling.\nComparable across groups with very different N."),
              hr(),
              checkboxInput(ns("disp_rarefy"), "Rarefy (correct for unequal N)", value = FALSE),
              helpText("Groups with more specimens tend to have higher SOV and SOR simply\nbecause they sample more of the morphospace. Rarefaction corrects\nfor this by repeatedly subsampling each group down to the size of\nthe smallest group and averaging the result."),
              conditionalPanel(
                condition = paste0("input['", ns("disp_rarefy"), "'] == true"),
                numericInput(ns("disp_rare_reps"), "Rarefaction replicates", value = 200,
                             min = 50, max = 2000, step = 50),
                helpText("Number of random subsamples per group. More replicates give a\nmore stable mean estimate. 200 is a good default; use 1000+ for\npublication-quality results.")
              ),
              checkboxInput(ns("disp_test"), "Pairwise significance test",    value = TRUE),
              helpText("Permutation test: shuffles group labels N times to build a null\ndistribution of metric differences. The p-value is the proportion\nof permutations where |null difference| ≥ |observed difference|."),
              conditionalPanel(
                condition = paste0("input['", ns("disp_test"), "'] == true"),
                numericInput(ns("disp_perms"), "Permutations", value = 999,
                             min = 99, max = 9999, step = 100),
                helpText("Number of random label shuffles. 999 gives a minimum possible\np-value of 0.001. Use 4999 for finer resolution."),
                selectInput(ns("disp_padj"), "P-value adjustment",
                  choices = c("BH" = "BH", "Bonferroni" = "bonferroni",
                              "Holm" = "holm", "None" = "none"),
                  selected = "BH"),
                helpText("Corrects for multiple pairwise comparisons.\nBH: recommended for exploratory work (controls FDR).\nBonferroni: most conservative (controls family-wise error rate).")
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
      helpText("Diameter of individual data points when jitter or scatter is shown."),
      numericInput(ns(paste0(prefix, "_alpha")),   "Transparency",value = 0.6, min = 0, max = 1, step = 0.05),
      helpText("0 = fully transparent, 1 = fully opaque. Lower values help reveal\noverlapping points."),
      selectInput(ns(paste0(prefix, "_palette")), "Color palette",
        choices = c("Default" = "default", "Viridis" = "viridis",
                    "Set1" = "Set1", "Dark2" = "Dark2"),
        selected = "default"),
      helpText("Default: ggplot2 hues (evenly spaced on colour wheel).\nViridis: perceptually uniform, colour-blind safe, prints well in greyscale.\nSet1 / Dark2: RColorBrewer palettes designed for categorical data."),
      textInput(ns(paste0(prefix, "_title")), "Plot title", value = ""),
      helpText("Optional title displayed above the plot."),
      selectInput(ns(paste0(prefix, "_theme")), "Theme",
        choices = c("Minimal" = "minimal", "Classic" = "classic",
                    "BW" = "bw", "Light" = "light"),
        selected = "minimal"),
      helpText("Controls the background and grid lines.\nMinimal / Light: subtle grids, clean look for screen viewing.\nClassic: white background with axis lines only, publication-ready.\nBW: black-and-white friendly."),
      downloadButton(ns(paste0(prefix, "_dl_png")), "PNG",
                     class = "btn-default btn-xs"),
      downloadButton(ns(paste0(prefix, "_dl_pdf")), "PDF",
                     class = "btn-default btn-xs"),
      downloadButton(ns(paste0(prefix, "_dl_rds")), "RDS",
                     class = "btn-default btn-xs"),
      helpText("PNG/PDF: raster/vector image files for direct use in reports.\nRDS: saves the ggplot object so you can reload and customise it\nlater in R with readRDS() and ggsave().")
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
          if (is.null(p)) { writeLines("No plot generated yet.", file); return() }
          if (ext == "rds") {
            saveRDS(p, file)
          } else {
            w_raw <- input[[paste0(prefix, "_export_w")]]
            h_raw <- input[[paste0(prefix, "_export_h")]]
            w <- if (length(w_raw) == 0L || is.na(w_raw[1])) 8 else as.numeric(w_raw[1])
            h <- if (length(h_raw) == 0L || is.na(h_raw[1])) 6 else as.numeric(h_raw[1])
            ggplot2::ggsave(file, plot = p, device = ext, width = w, height = h)
          }
        }
      )
    }

    # ── 1. Distribution Plot ─────────────────────────────────────────────────

    output$dp_y_ui <- renderUI({
      nc <- .num_cols()
      plt <- input$dp_type %||% "boxplot"
      if (plt == "histogram") {
        selectInput(ns("dp_y"), "Y column (values)", choices = nc)
      } else {
        selectizeInput(ns("dp_y"), "Y column(s)", choices = nc,
                       selected = head(nc, 1), multiple = TRUE,
                       options = list(plugins = list("remove_button"),
                                      placeholder = "Select one or more columns"))
      }
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

      ycols <- input$dp_y
      req(length(ycols) > 0)
      # For histogram use only the first selected column
      ycol_hist <- ycols[1]
      gcol  <- input$dp_group
      gvals <- input$dp_groupvals
      df    <- .subset_df(df, gcol, gvals)
      has_g <- !is.null(gcol) && nzchar(gcol) && gcol %in% names(df)
      multi <- length(ycols) > 1 && input$dp_type != "histogram"
      alpha <- input$dp_alpha %||% 0.6
      pt_sz <- input$dp_pt_size %||% 2

      # For multi-PC: pivot to long format
      # Value col = ".value", PC name col = ".PC"
      if (multi) {
        valid_ycols <- ycols[ycols %in% names(df)]
        keep_cols <- unique(c(valid_ycols, if (has_g) gcol else NULL))
        df_long <- tidyr::pivot_longer(
          df[, keep_cols, drop = FALSE],
          cols = valid_ycols,
          names_to  = ".PC",
          values_to = ".value"
        )
        df_long$.PC <- factor(df_long$.PC, levels = valid_ycols)
        n_g  <- if (has_g) nlevels(factor(df_long[[gcol]])) else 1L
        cols <- .get_palette("dp", n_g)
      } else {
        ycol <- ycols[1]
        req(ycol %in% names(df))
        n_g  <- if (has_g) nlevels(df[[gcol]]) else 1L
        cols <- .get_palette("dp", n_g)
      }

      p <- tryCatch({
        switch(input$dp_type,

          boxplot = {
            if (multi) {
              # Single grouped plot: x=PC, fill=group, dodged within each PC
              if (has_g) {
                ae <- ggplot2::aes(x = .data[['.PC']], y = .data[['.value']],
                                   fill = .data[[gcol]])
                p <- ggplot2::ggplot(df_long, ae) +
                  ggplot2::geom_boxplot(
                    notch = isTRUE(input$bp_notch),
                    outlier.shape = if (isTRUE(input$bp_outliers)) 19 else NA,
                    alpha = alpha, position = ggplot2::position_dodge(0.8), width = 0.7
                  )
                if (isTRUE(input$bp_jitter))
                  p <- p + ggplot2::geom_jitter(
                    position = ggplot2::position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
                    size = pt_sz, alpha = alpha * 0.6, show.legend = FALSE
                  )
                p <- p + ggplot2::scale_fill_manual(values = cols)
                p + ggplot2::labs(x = "Variable", y = "Value", fill = gcol)
              } else {
                # No group: just show distribution per PC
                ae <- ggplot2::aes(x = .data[['.PC']], y = .data[['.value']])
                p <- ggplot2::ggplot(df_long, ae) +
                  ggplot2::geom_boxplot(
                    notch = isTRUE(input$bp_notch),
                    outlier.shape = if (isTRUE(input$bp_outliers)) 19 else NA,
                    alpha = alpha, fill = cols[1]
                  )
                if (isTRUE(input$bp_jitter))
                  p <- p + ggplot2::geom_jitter(width = 0.15, size = pt_sz,
                                                alpha = alpha * 0.7, show.legend = FALSE)
                p + ggplot2::labs(x = "Variable", y = "Value")
              }
            } else {
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
            }
          },

          violin = {
            if (multi) {
              if (has_g) {
                ae <- ggplot2::aes(x = .data[['.PC']], y = .data[['.value']],
                                   fill = .data[[gcol]])
                p <- ggplot2::ggplot(df_long, ae) +
                  ggplot2::geom_violin(alpha = alpha,
                                       position = ggplot2::position_dodge(0.8))
                if (isTRUE(input$vio_box))
                  p <- p + ggplot2::geom_boxplot(
                    width = 0.08, fill = "white", outlier.shape = NA,
                    position = ggplot2::position_dodge(0.8))
                if (isTRUE(input$vio_jitter))
                  p <- p + ggplot2::geom_jitter(
                    position = ggplot2::position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
                    size = pt_sz, alpha = 0.5, show.legend = FALSE)
                p <- p + ggplot2::scale_fill_manual(values = cols)
                p + ggplot2::labs(x = "Variable", y = "Value", fill = gcol)
              } else {
                ae <- ggplot2::aes(x = .data[['.PC']], y = .data[['.value']])
                p <- ggplot2::ggplot(df_long, ae) +
                  ggplot2::geom_violin(alpha = alpha, fill = cols[1])
                if (isTRUE(input$vio_box))
                  p <- p + ggplot2::geom_boxplot(width = 0.08, fill = "white", outlier.shape = NA)
                if (isTRUE(input$vio_jitter))
                  p <- p + ggplot2::geom_jitter(width = 0.08, size = pt_sz, alpha = 0.5)
                p + ggplot2::labs(x = "Variable", y = "Value")
              }
            } else {
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
            }
          },

          histogram = {
            ycol <- ycol_hist
            normalize <- isTRUE(input$hist_normalize)
            # Build aes explicitly — after_stat() must live inside aes(), not pre-stored
            if (has_g && normalize)
              ae <- ggplot2::aes(x = .data[[ycol]], y = ggplot2::after_stat(density),
                                 fill = .data[[gcol]])
            else if (has_g)
              ae <- ggplot2::aes(x = .data[[ycol]], y = ggplot2::after_stat(count),
                                 fill = .data[[gcol]])
            else if (normalize)
              ae <- ggplot2::aes(x = .data[[ycol]], y = ggplot2::after_stat(density))
            else
              ae <- ggplot2::aes(x = .data[[ycol]], y = ggplot2::after_stat(count))
            p <- ggplot2::ggplot(df, ae) +
              ggplot2::geom_histogram(bins = input$hist_bins %||% 30,
                                      alpha = alpha, position = "identity")
            if (isTRUE(input$hist_density)) {
              if (has_g) {
                p <- p + ggplot2::geom_density(
                  mapping = ggplot2::aes(x = .data[[ycol]], color = .data[[gcol]]),
                  data = df, fill = NA, linewidth = 0.9, inherit.aes = FALSE
                ) + ggplot2::scale_color_manual(values = cols)
              } else {
                p <- p + ggplot2::geom_density(
                  mapping = ggplot2::aes(x = .data[[ycol]]),
                  data = df, color = "black", fill = NA, linewidth = 0.9,
                  inherit.aes = FALSE
                )
              }
            }
            if (has_g) p <- p + ggplot2::scale_fill_manual(values = cols)
            p + ggplot2::labs(
              x = ycol,
              y = if (normalize) "Density" else "Count",
              fill = gcol, color = gcol,
              caption = if (normalize && has_g)
                "Normalized to density: groups with different N are directly comparable."
              else if (!normalize && has_g)
                "Raw counts: bars reflect actual specimen numbers per group."
              else NULL
            )
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
    output$dp_dl_rds <- .plot_dl(dp_plot_rv, "dp", "rds")


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

      formula_str  <- trimws(input$sc_formula %||% paste0(ycol, " ~ ", xcol))
      frm          <- tryCatch(as.formula(formula_str),
                               error = function(e) { showNotification(paste("Bad formula:", conditionMessage(e)), type = "error"); NULL })
      req(frm)

      # ── Plot ─────────────────────────────────────────────────────────────
      p <- tryCatch({
        ae <- if (has_g)
          ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]], color = .data[[gcol]])
        else
          ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])
        p <- ggplot2::ggplot(df, ae) +
          ggplot2::geom_point(size = pt_sz, alpha = alpha) +
          ggplot2::geom_smooth(
            method  = input$sc_method %||% "lm",
            se      = isTRUE(input$sc_se),
            formula = y ~ x
          )
        if (isTRUE(input$sc_label))
          p <- p + ggplot2::geom_text(ggplot2::aes(label = rownames(df)),
                                      size = 2.5, vjust = -0.5)
        if (has_g) p <- p + ggplot2::scale_color_manual(values = cols)
        p + ggplot2::labs(x = xcol, y = ycol, color = gcol)
      }, error = function(e) {
        showNotification(paste("Plot error:", conditionMessage(e)), type = "error"); NULL
      })

      if (!is.null(p)) { p <- .apply_theme(p, "sc"); sc_plot_rv(p) }

      # ── lm summary ───────────────────────────────────────────────────────
      reg_txt <- tryCatch({
        fit <- lm(frm, data = df)
        sm  <- summary(fit)
        coef_tbl <- capture.output(printCoefmat(sm$coefficients, digits = 4, signif.stars = TRUE))
        r2  <- round(sm$r.squared, 4);  ar2 <- round(sm$adj.r.squared, 4)
        fst <- round(sm$fstatistic[1], 3)
        fpv <- format.pval(pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3],
                              lower.tail = FALSE), digits = 4)
        paste0("Formula: ", formula_str, "\n",
               "R\u00b2: ", r2, "   Adj.R\u00b2: ", ar2, "\n",
               "F: ", fst, " (df ", sm$fstatistic[2], ", ", sm$fstatistic[3], ")  p=", fpv, "\n\n",
               "Coefficients:\n", paste(coef_tbl, collapse = "\n"), "\n\n",
               "R\u00b2 = ", r2 * 100, "% of variance explained.\n",
               "Estimates = change in Y per unit increase in each predictor.\n")
      }, error = function(e) paste("lm error:", conditionMessage(e)))

      sc_lm_rv(reg_txt)
    })

    output$sc_plot           <- renderPlot({ p <- sc_plot_rv(); req(p); print(p) })
    output$sc_regression_txt <- renderText({ t <- sc_lm_rv(); req(t); t })
    output$sc_dl_png <- .plot_dl(sc_plot_rv, "sc", "png")
    output$sc_dl_pdf <- .plot_dl(sc_plot_rv, "sc", "pdf")
    output$sc_dl_rds <- .plot_dl(sc_plot_rv, "sc", "rds")


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
    cor_heatmap_rv <- reactiveVal(NULL)  # ggplot object for download

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
      p <- ggplot2::ggplot(cm_long,
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
      cor_heatmap_rv(p)
      print(p)
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

    output$cor_dl_rds <- downloadHandler(
      filename = function() paste0("correlation_heatmap_", Sys.Date(), ".rds"),
      content  = function(file) {
        p <- cor_heatmap_rv()
        if (is.null(p)) { writeLines("No heatmap generated yet.", file); return() }
        saveRDS(p, file)
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

    gt_results_rv  <- reactiveVal(NULL)
    gt_aov_rv      <- reactiveVal(NULL)
    gt_qq_rv       <- reactiveVal(NULL)  # ggplot object for QQ plot download
    gt_qq_data_rv  <- reactiveVal(NULL)  # per-group data for QQ navigation
    gt_permanova_rv <- reactiveVal(NULL) # PERMANOVA results text

    observeEvent(input$gt_run, {
      df_raw <- tryCatch(data_reactive(), error = function(e) NULL)
      req(df_raw, nrow(df_raw) > 0)

      if (isTRUE(input$gt_multivariate)) {

        gcol_f <- input$gt_group
        df <- if (!is.null(gcol_f) && nzchar(gcol_f) && gcol_f %in% names(df_raw))
          .subset_df(df_raw, gcol_f, input$gt_groupvals)
        else df_raw

        gt_results_rv(NULL)
        gt_permanova_rv("Running PERMANOVA\u2026 please wait.")

        pcols <- input$gt_permanova_cols
        if (is.null(pcols) || length(pcols) < 2) {
          gt_permanova_rv("Please select at least 2 PC columns for PERMANOVA.")
          return()
        }
        valid <- pcols[pcols %in% names(df)]
        if (length(valid) < 2) {
          gt_permanova_rv(paste0("Response columns not found: ",
                                 paste(setdiff(pcols, names(df)), collapse = ", ")))
          return()
        }

        grp_cols <- input$gt_permanova_groups
        if (is.null(grp_cols) || length(grp_cols) == 0) {
          gt_permanova_rv("Please select at least one grouping variable.")
          return()
        }
        missing_grp <- setdiff(grp_cols, names(df))
        if (length(missing_grp) > 0) {
          gt_permanova_rv(paste0("Grouping columns not found: ",
                                 paste(missing_grp, collapse = ", ")))
          return()
        }

        formula_type <- input$gt_permanova_formula_type %||% "interaction"
        rhs <- switch(formula_type,
          additive    = paste(grp_cols, collapse = " + "),
          interaction = paste(grp_cols, collapse = " * "),
          custom      = trimws(input$gt_permanova_rhs %||%
                               paste(grp_cols, collapse = " + "))
        )
        frm <- tryCatch(
          as.formula(paste("mat_p ~", rhs)),
          error = function(e) { gt_permanova_rv(paste("Formula error:", conditionMessage(e))); NULL }
        )
        if (is.null(frm)) return()

        mat_p  <- as.matrix(df[, valid, drop = FALSE])
        grp_df <- df[, grp_cols, drop = FALSE]
        for (col in grp_cols) grp_df[[col]] <- factor(grp_df[[col]])

        complete_rows <- complete.cases(mat_p) & complete.cases(grp_df)
        if (sum(complete_rows) < nrow(mat_p)) {
          showNotification(paste0(nrow(mat_p) - sum(complete_rows),
            " rows with NA removed before PERMANOVA."), type = "warning", duration = 6)
          mat_p  <- mat_p[complete_rows, , drop = FALSE]
          grp_df <- grp_df[complete_rows, , drop = FALSE]
        }
        for (col in grp_cols) grp_df[[col]] <- droplevels(grp_df[[col]])

        multi_factor <- length(grp_cols) > 1
        grp_f_single <- if (!multi_factor) grp_df[[grp_cols[1]]] else
          interaction(grp_df, drop = TRUE)
        grp_lv  <- levels(grp_f_single)
        n_p     <- as.integer(input$gt_permanova_perms %||% 999)
        d_mth   <- input$gt_permanova_dist %||% "euclidean"
        by_arg  <- input$gt_permanova_by   %||% "margin"
        do_pw   <- isTRUE(input$gt_permanova_pairwise) && !multi_factor
        padj_m  <- input$gt_permanova_padj %||% "BH"
        n_cores <- if (isTRUE(input$gt_permanova_parallel))
          as.integer(input$gt_permanova_cores %||% 1L) else NULL

        # Strata-restricted permutations (random-effects analog)
        strata_col <- input$gt_permanova_strata %||% ""
        use_strata <- nzchar(strata_col) && strata_col %in% names(df)
        strata_vec <- if (use_strata) factor(df[[strata_col]])[complete_rows] else NULL
        perm_arg   <- if (!is.null(strata_vec))
          permute::how(blocks = strata_vec, nperm = n_p)
        else n_p

        message("[PERMANOVA] Formula: mat_p ~ ", rhs,
                "  N=", nrow(mat_p), "  dist=", d_mth, "  perms=", n_p,
                if (use_strata) paste0("  strata=", strata_col) else "")

        perm_txt <- withProgress(message = "Running PERMANOVA\u2026", value = 0, {

          # Omnibus
          setProgress(0.05, detail = "Omnibus adonis2\u2026")
          message("[PERMANOVA] adonis2 omnibus\u2026")
          ad <- tryCatch(
            { set.seed(42L)
              vegan::adonis2(frm, data = grp_df, permutations = perm_arg,
                             method = d_mth, by = by_arg, parallel = n_cores) },
            error = function(e) { message("[PERMANOVA] adonis2 error: ", conditionMessage(e)); stop(e) }
          )
          ad_lines <- capture.output(print(ad))
          message("[PERMANOVA] adonis2 omnibus done.")

          # Distance matrix
          setProgress(0.35, detail = "Distance matrix\u2026")
          dmat <- tryCatch(
            vegan::vegdist(mat_p, method = d_mth),
            error = function(e) { message("[PERMANOVA] vegdist error: ", conditionMessage(e)); stop(e) }
          )

          # Pairwise (single factor only)
          pw_block <- if (multi_factor && isTRUE(input$gt_permanova_pairwise))
            paste0("\n\u2139 Pairwise PERMANOVA skipped: with multiple grouping variables ",
                   "the omnibus table already partitions variance per term.\n")
          else ""

          if (do_pw && length(grp_lv) >= 2) {
            pairs    <- combn(grp_lv, 2, simplify = FALSE)
            n_pairs  <- length(pairs)
            raw_p    <- numeric(n_pairs)
            raw_r2   <- numeric(n_pairs)
            pair_lbl <- character(n_pairs)
            pw_col   <- grp_cols[1]
            for (i in seq_along(pairs)) {
              pr      <- pairs[[i]]
              idx     <- which(grp_f_single %in% pr)
              sub_mat <- mat_p[idx, , drop = FALSE]
              sub_gdf <- grp_df[idx, , drop = FALSE]
              sub_gdf[[pw_col]] <- droplevels(sub_gdf[[pw_col]])
              setProgress(0.35 + 0.40 * (i / n_pairs),
                          detail = paste0("Pairwise ", pr[1], " vs ", pr[2], "\u2026"))
              message("[PERMANOVA] pairwise ", pr[1], " vs ", pr[2])
              res_i <- tryCatch(
                { set.seed(42L + i)
                  sub_perm <- if (!is.null(strata_vec))
                    permute::how(blocks = strata_vec[idx], nperm = n_p)
                  else n_p
                  vegan::adonis2(as.formula(paste("sub_mat ~", pw_col)),
                                 data = sub_gdf, permutations = sub_perm,
                                 method = d_mth, parallel = n_cores) },
                error = function(e) {
                  message("[PERMANOVA] pairwise error ", pr[1], " vs ", pr[2],
                          ": ", conditionMessage(e)); NULL
                }
              )
              raw_p[i]    <- if (!is.null(res_i)) res_i[1L, "Pr(>F)"] else NA_real_
              raw_r2[i]   <- if (!is.null(res_i)) res_i[1L, "R2"]     else NA_real_
              pair_lbl[i] <- paste0(pr[1], " vs ", pr[2])
            }
            adj_p    <- p.adjust(raw_p, method = padj_m)
            pw_lines <- vapply(seq_along(pairs), function(i)
              sprintf("  %-35s  R\u00b2=%6.4f  p=%6.4f  p.adj=%6.4f %s",
                pair_lbl[i], raw_r2[i], raw_p[i], adj_p[i],
                if (!is.na(adj_p[i]) && adj_p[i] < 0.05) "*" else ""),
              character(1))
            pw_block <- paste0(
              "\n=== Pairwise PERMANOVA (", padj_m, " adjustment) ===\n",
              strrep("\u2500", 60), "\n",
              paste(pw_lines, collapse = "\n"), "\n\n",
              strrep("\u2500", 60), "\n",
              "\u2139 Each pair tested independently; R\u00b2 = variance explained by group.\n",
              "  p.adj corrected for ", n_pairs, " comparisons (", padj_m, ").\n",
              "  * = significant at p.adj < 0.05.\n"
            )
          }

          # PERMDISP
          setProgress(0.80, detail = "PERMDISP\u2026")
          message("[PERMANOVA] betadisper\u2026")
          bd <- tryCatch(
            vegan::betadisper(dmat, grp_f_single),
            error = function(e) { message("[PERMANOVA] betadisper error: ", conditionMessage(e)); stop(e) }
          )
          bd_p <- tryCatch(
            vegan::permutest(bd,
              permutations = if (!is.null(strata_vec))
                permute::how(blocks = strata_vec, nperm = n_p) else n_p,
              parallel = n_cores),
            error = function(e) { message("[PERMANOVA] permutest error: ", conditionMessage(e)); stop(e) }
          )
          bd_lines <- capture.output(print(bd_p))
          message("[PERMANOVA] PERMDISP done.")

          disp_label <- if (multi_factor)
            paste0("interaction(", paste(grp_cols, collapse = ", "), ")")
          else grp_cols[1]
          setProgress(1, detail = "Done.")
          paste0(
            "=== PERMANOVA (vegan::adonis2) \u2014 Omnibus ===\n",
            "Formula: mat_p ~ ", rhs, "\n",
            "Response: ", paste(valid, collapse = ", "), "\n",
            "Distance: ", d_mth, "  |  by = '", by_arg, "'  |  N=", nrow(mat_p),
            "  |  Permutations: ", n_p,
            if (use_strata) paste0("  |  Strata: ", strata_col) else "",
            if (!is.null(n_cores)) paste0("  |  Cores: ", n_cores) else "", "\n",
            strrep("\u2500", 60), "\n",
            paste(ad_lines, collapse = "\n"), "\n\n",
            strrep("\u2500", 60), "\n",
            "\u2139 R\u00b2 per term = proportion of total variation explained by that predictor.\n",
            "  p < 0.05 \u2192 significant effect on position in morphospace.\n",
            "  Caution: PERMANOVA is sensitive to unequal dispersions (check PERMDISP).\n",
            pw_block,
            "\n=== PERMDISP (", disp_label, ") ===\n",
            strrep("\u2500", 60), "\n",
            paste(bd_lines, collapse = "\n"), "\n\n",
            "\u2139 Non-significant PERMDISP \u2192 dispersions homogeneous (PERMANOVA assumption met).\n",
            "  Significant PERMDISP \u2192 groups differ in spread; interpret PERMANOVA with caution.\n"
          )
        })
        if (inherits(perm_txt, "error")) {
          gt_permanova_rv(paste("PERMANOVA error:", conditionMessage(perm_txt)))
        } else {
          gt_permanova_rv(perm_txt)
        }

      } else {

        gcol  <- input$gt_group
        req(nzchar(gcol %||% ""), gcol %in% names(df_raw))
        df    <- .subset_df(df_raw, gcol, input$gt_groupvals)
        n_g   <- nlevels(df[[gcol]])
        req(n_g >= 2)

        gt_permanova_rv(NULL)  # clear multivariate output
        ycol <- input$gt_y; req(ycol %in% names(df))
        form <- as.formula(paste(ycol, "~", gcol))
        lines <- paste0("Variable: ", ycol, "  |  Group: ", gcol,
                        "  |  N groups: ", n_g, "\n",
                        strrep("\u2500", 60), "\n")

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

        # Always fit aov for QQ plot
        aov_fit <- tryCatch(aov(form, data = df), error = function(e) NULL)
        gt_aov_rv(aov_fit)
        gt_qq_data_rv(list(
          groups   = levels(df[[gcol]]),
          grp_vals = lapply(
            setNames(levels(df[[gcol]]), levels(df[[gcol]])),
            function(g) df[[ycol]][df[[gcol]] == g]
          ),
          resids   = if (!is.null(aov_fit)) residuals(aov_fit) else NULL
        ))

        gt_results_rv(lines)
      }
    })

    output$gt_results <- renderText({ t <- gt_results_rv(); req(t); t })

    output$gt_permanova_cols_ui <- renderUI({
      nc <- .num_cols()
      selectizeInput(ns("gt_permanova_cols"), "PC / numeric columns for PERMANOVA",
                     choices = nc, selected = head(nc, 4), multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    })

    output$gt_permanova_groups_ui <- renderUI({
      all_cols <- .all_cols()
      selectizeInput(ns("gt_permanova_groups"), "Grouping variable(s)",
                     choices = all_cols, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"),
                                    placeholder = "Select one or more columns"))
    })

    output$gt_permanova_rhs_ui <- renderUI({
      gcols <- input$gt_permanova_groups
      if (is.null(gcols) || length(gcols) == 0) return(NULL)
      default_rhs <- paste(gcols, collapse = " * ")
      tagList(
        selectInput(ns("gt_permanova_formula_type"), "Formula structure",
          choices = c("Main effects (+)"   = "additive",
                      "Full interaction (*)" = "interaction",
                      "Custom"              = "custom"),
          selected = if (length(gcols) > 1) "interaction" else "additive"),
        helpText("Main effects: each variable tested independently (additive).\nFull interaction: tests main effects + all interaction terms.\nCustom: type any valid R formula right-hand side."),
        conditionalPanel(
          condition = paste0("input['", ns("gt_permanova_formula_type"), "'] == 'custom'"),
          textInput(ns("gt_permanova_rhs"), "Formula RHS", value = default_rhs),
          helpText("e.g.  Genus * Caste   or   Genus + Caste   or   Genus:Caste")
        )
      )
    })

    output$gt_permanova_strata_ui <- renderUI({
      selectInput(ns("gt_permanova_strata"),
                  "Strata / blocks — (1|Species) analog (optional)",
                  choices = c("(none)" = "", .all_cols()))
    })

    output$gt_permanova_cores_ui <- renderUI({
      n_avail <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
      numericInput(ns("gt_permanova_cores"), "CPU cores",
                   value = n_avail, min = 1L, max = parallel::detectCores(), step = 1L)
    })

    output$gt_permanova_txt <- renderText({
      t <- gt_permanova_rv()
      if (is.null(t)) return("Enable PERMANOVA and click Calculate.")
      t
    })

    output$gt_dl_permanova_txt <- downloadHandler(
      filename = function() paste0("permanova_", Sys.Date(), ".txt"),
      content  = function(file) {
        t <- gt_permanova_rv()
        if (is.null(t)) { writeLines("No PERMANOVA results yet.", file); return() }
        writeLines(t, file)
      }
    )

    output$gt_dl_permanova_rds <- downloadHandler(
      filename = function() paste0("permanova_", Sys.Date(), ".rds"),
      content  = function(file) {
        t <- gt_permanova_rv()
        if (is.null(t)) { writeLines("No PERMANOVA results yet.", file); return() }
        saveRDS(list(text = t, generated = Sys.time()), file)
      }
    )

    output$gt_qq_nav_ui <- renderUI({
      qd <- gt_qq_data_rv(); req(qd)
      choices <- c(
        setNames(as.list(seq_along(qd$groups)), paste0("Group: ", qd$groups)),
        list("ANOVA Residuals" = length(qd$groups) + 1L)
      )
      selectInput(ns("gt_qq_idx"), "Select QQ plot", choices = choices, selected = 1)
    })

    output$gt_qqplot_out <- renderPlot({
      req(isTRUE(input$gt_qqplot))
      qd  <- gt_qq_data_rv(); req(qd)
      idx <- as.integer(input$gt_qq_idx %||% 1L)
      n_g <- length(qd$groups)

      .qq_plot <- function(x_vals, title_str, y_label) {
        x_vals <- x_vals[!is.na(x_vals)]
        req(length(x_vals) >= 3)
        n      <- length(x_vals)
        probs  <- c(0.25, 0.75)
        q_s    <- quantile(x_vals, probs)
        q_n    <- qnorm(probs)
        slope  <- diff(q_s) / diff(q_n)
        interc <- q_s[1] - slope * q_n[1]
        df_qq  <- data.frame(theoretical = qnorm(ppoints(n)), sample = sort(x_vals))
        ggplot2::ggplot(df_qq,
            ggplot2::aes(x = .data[["theoretical"]], y = .data[["sample"]])) +
          ggplot2::geom_point(size = 1.5, alpha = 0.7, color = "#2166AC") +
          ggplot2::geom_abline(intercept = interc, slope = slope,
                               color = "#D62728", linewidth = 0.8) +
          ggplot2::labs(
            x       = "Theoretical quantiles (standard normal)",
            y       = y_label,
            title   = title_str,
            caption = paste0(
              "Red line: reference through Q25/Q75.\n",
              "Points on the line \u2192 normally distributed data.\n",
              "Systematic curvature \u2192 non-normality."
            )
          ) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, color = "grey50"))
      }

      p <- if (idx <= n_g) {
        g_name <- qd$groups[idx]
        vals   <- qd$grp_vals[[g_name]]
        .qq_plot(vals,
                 paste0("QQ Plot: Group \u2018", g_name, "\u2019  (N=", sum(!is.na(vals)), ")"),
                 paste0("Sample quantiles (", g_name, ")"))
      } else {
        req(!is.null(qd$resids))
        .qq_plot(qd$resids,
                 "QQ Plot of ANOVA Residuals",
                 "Sample quantiles (ANOVA residuals)")
      }
      gt_qq_rv(p)
      print(p)
    })

    output$gt_dl_qq_rds <- downloadHandler(
      filename = function() paste0("anova_qq_plot_", Sys.Date(), ".rds"),
      content  = function(file) {
        p <- gt_qq_rv()
        if (is.null(p)) { writeLines("No QQ plot generated yet.", file); return() }
        saveRDS(p, file)
      }
    )

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

    disp_table_rv     <- reactiveVal(NULL)
    disp_test_rv      <- reactiveVal(NULL)

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
      min_n   <- min(lengths(grp_idx))
      do_rare <- isTRUE(input$disp_rarefy)
      n_reps  <- as.integer(input$disp_rare_reps %||% 200)

      # Helper: compute all requested metrics on a matrix (used for both observed & rarefied)
      .compute_metrics <- function(sub) {
        vals <- c()
        if (isTRUE(input$disp_sov)) vals <- c(vals, SOV = .sov(sub))
        if (isTRUE(input$disp_sor)) vals <- c(vals, SOR = .sor(sub))
        if (isTRUE(input$disp_mpd)) vals <- c(vals, MPD = .mpd(sub))
        vals
      }

      grp_rows <- lapply(grps, function(g) {
        idx <- grp_idx[[g]]
        sub <- mat[idx, , drop = FALSE]
        if (nrow(sub) < 2) return(NULL)
        row <- data.frame(Group = g, N = nrow(sub), stringsAsFactors = FALSE)
        obs  <- .compute_metrics(sub)
        for (nm in names(obs)) row[[nm]] <- round(obs[[nm]], 5)
        if (do_rare && nrow(sub) > min_n) {
          # Rarefaction: subsample to min_n, repeat n_reps times, average
          rare_vals <- replicate(n_reps, {
            s <- sub[sample(nrow(sub), min_n, replace = FALSE), , drop = FALSE]
            .compute_metrics(s)
          })
          # rare_vals is matrix [n_metrics x n_reps]
          if (!is.matrix(rare_vals)) rare_vals <- matrix(rare_vals, nrow = 1)
          rare_means <- rowMeans(rare_vals, na.rm = TRUE)
          for (nm in names(rare_means)) {
            col_nm <- paste0(nm, "_rarefied")
            row[[col_nm]] <- round(rare_means[[nm]], 5)
          }
        } else if (do_rare) {
          # This is the smallest group — rarefied == observed
          for (nm in names(obs)) row[[paste0(nm, "_rarefied")]] <- round(obs[[nm]], 5)
        }
        row
      })
      disp_table_rv(do.call(rbind, Filter(Negate(is.null), grp_rows)))

      # Append metric explanations to display
      rare_note <- if (do_rare)
        paste0("\n  _rarefied columns: each group was subsampled to N=", min_n,
               " (smallest group) over ", n_reps, " replicates; values are means.\n",
               "  Use rarefied metrics when comparing groups of very different sizes.\n")
      else
        "  Tip: Enable 'Rarefy' if groups have very different sample sizes.\n"

      expl <- paste0(
        strrep("\u2500", 60), "\n",
        "\u2139 Metric definitions\n",
        strrep("\u2500", 60), "\n",
        "  SOV (Sum of Variances): sum of per-axis variances = sum(diag(var(X))).\n",
        "      Measures total spread of points in morphospace.\n",
        "      Sensitive to number of axes; use same axes across groups.\n",
        "  SOR (Sum of Ranges)   : sum of per-axis ranges = sum(max-min per PC).\n",
        "      Measures the total extent of occupied morphospace.\n",
        "      More sensitive to outliers than SOV.\n",
        "  MPD (Mean Pairwise Distance): average Euclidean distance between all\n",
        "      specimen pairs within a group.\n",
        "      Intuitively captures average morphological diversity.\n",
        rare_note
      )
      disp_test_rv(expl)

      # Pairwise permutation test — base-R implementation (more robust than dispRity test)
      if (isTRUE(input$disp_test)) {
        test_txt <- tryCatch({
          n_perms   <- as.integer(input$disp_perms %||% 999)
          adj_meth  <- input$disp_padj %||% "BH"
          # Choose primary metric
          primary_fn <- if (isTRUE(input$disp_sov)) .sov
                        else if (isTRUE(input$disp_sor)) .sor
                        else .mpd
          metric_nm  <- if (isTRUE(input$disp_sov)) "SOV"
                        else if (isTRUE(input$disp_sor)) "SOR" else "MPD"

          # Observed metric per group
          obs_vals <- vapply(grps, function(g) primary_fn(mat[grp_idx[[g]], , drop=FALSE]),
                             numeric(1))
          names(obs_vals) <- grps

          # All pairwise combinations
          pairs <- combn(grps, 2, simplify = FALSE)
          obs_diffs <- vapply(pairs, function(pr) obs_vals[pr[1]] - obs_vals[pr[2]], numeric(1))

          # Permutation null distribution
          group_vec <- rep(grps, lengths(grp_idx))
          null_diffs <- matrix(NA_real_, nrow = n_perms, ncol = length(pairs))
          set.seed(42L)
          for (b in seq_len(n_perms)) {
            perm <- sample(group_vec)
            null_vals <- vapply(grps, function(g) {
              idx <- which(perm == g)
              if (length(idx) < 2) return(NA_real_)
              primary_fn(mat[idx, , drop = FALSE])
            }, numeric(1))
            null_diffs[b, ] <- vapply(pairs, function(pr) null_vals[pr[1]] - null_vals[pr[2]],
                                      numeric(1))
          }

          # Compute two-sided p-values
          raw_p <- vapply(seq_along(pairs), function(i) {
            mean(abs(null_diffs[, i]) >= abs(obs_diffs[i]), na.rm = TRUE)
          }, numeric(1))
          adj_p <- p.adjust(raw_p, method = adj_meth)

          # Format output
          lines <- paste0(
            "=== Pairwise Permutation Test (", metric_nm, ") ===",
            "  n.perm=", n_perms, ", adj=", adj_meth, "\n",
            strrep("\u2500", 60), "\n"
          )
          for (i in seq_along(pairs)) {
            pr <- pairs[[i]]
            lines <- paste0(lines,
              sprintf("  %-15s vs %-15s  obs.diff=%7.4f  p=%6.4f  p.adj=%6.4f %s\n",
                pr[1], pr[2], obs_diffs[i], raw_p[i], adj_p[i],
                if (adj_p[i] < 0.05) "*" else ""))
          }
          lines <- paste0(lines, "\n",
            strrep("\u2500", 60), "\n",
            "\u2139 Interpretation\n",
            strrep("\u2500", 60), "\n",
            "  obs.diff: Observed difference in ", metric_nm, " between groups.\n",
            "  p       : Proportion of ", n_perms, " permutations where |null diff| \u2265 |obs diff|.\n",
            "  p.adj   : p-value corrected for ", length(pairs), " comparisons (", adj_meth, " method).\n",
            "  *       : Significant at p.adj < 0.05.\n"
          )
          lines
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
