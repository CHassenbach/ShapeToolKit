.validate_3d_colour_columns <- function(data, options) {
  if (is.null(options)) return(character())
  if (!is.list(options)) {
    stop("colour_groups must be a list with a columns vector.", call. = FALSE)
  }
  columns <- options$columns
  if (is.null(columns)) return(character())
  if (!is.character(columns) || anyNA(columns) || any(!nzchar(columns))) {
    stop("colour_groups columns must be non-empty column names.", call. = FALSE)
  }
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop("Colour grouping columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (any(vapply(data[columns], function(x) !is.atomic(x) || !is.null(dim(x)), logical(1)))) {
    stop("Colour grouping columns must be scalar, atomic columns.", call. = FALSE)
  }
  unique(columns)
}

.add_3d_colour_groups <- function(p, data, x_col, y_col, z_col, group_col,
                                 params, columns, gradient, verbose) {
  gradient_active <- !is.null(gradient$col) && gradient$col %in% names(data)
  base_label <- if (gradient_active) {
    paste0("Gradient: ", gradient$legend_title %||% gradient$col)
  } else if (!is.null(group_col)) group_col else "Original colours"
  if (!gradient_active) columns <- setdiff(columns, group_col)
  if (!length(columns)) return(p)

  modes <- lapply(columns, function(column) {
    grouped <- data
    values <- as.character(data[[column]])
    missing <- is.na(values) | !nzchar(trimws(values))
    missing_label <- "(Missing)"
    while (missing_label %in% values[!missing]) missing_label <- paste0(missing_label, "*")
    values[missing] <- missing_label
    grouped[[column]] <- values
    groups <- unique(values)
    mode_params <- params
    mode_params$group_vals <- groups
    mode_params$features$colour_groups <- NULL
    mode_params$features$specimen_viewer <- list(show = FALSE)
    colours <- if (length(groups) > 1L) scales::hue_pal()(length(groups)) else "#1f77b4"
    mode_params$styling$point$color <- colours
    if (!is.null(mode_params$features$hulls_3d)) {
      mode_params$features$hulls_3d$groups <- groups
      mode_params$features$hulls_3d$colors <- colours
    }
    mode <- .build_3d_plot(grouped, x_col, y_col, z_col, column, mode_params, verbose)
    traces <- plotly::plotly_build(mode)$x$data
    for (i in seq_along(groups)) {
      traces[[i]]$customdata <- I(which(values == groups[i]))
    }
    for (i in seq_along(traces)) {
      traces[[i]]$meta <- list(shape_toolkit_grouping = "group")
    }
    list(label = column, traces = traces)
  })

  htmlwidgets::onRender(p, "
function(el, x, options) {
  if (el._colourGroupControl) el._colourGroupControl.remove();
  el.style.position = 'relative';
  var control = document.createElement('label');
  control.style.cssText = 'position:absolute;top:4px;left:12px;z-index:20;' +
    'background:white;color:#222;padding:4px;font:14px sans-serif;';
  control.appendChild(document.createTextNode('Colour by: '));
  var select = document.createElement('select');
  select.setAttribute('aria-label', 'Colour by');
  control.appendChild(select);
  var status = document.createElement('span');
  status.setAttribute('role', 'status');
  control.appendChild(status);
  el.appendChild(control);
  el._colourGroupControl = control;
  var modes = [{label: options.base_label, traces: el.data.filter(function(trace) {
    return trace.meta && trace.meta.shape_toolkit_grouping === 'group';
  })}].concat(options.modes);
  modes.forEach(function(mode, i) {
    var option = document.createElement('option');
    option.value = String(i);
    option.textContent = mode.label;
    select.appendChild(option);
  });
  var current = 0;
  var originalTitle = el.layout.legend && el.layout.legend.title;
  function clone(value) { return JSON.parse(JSON.stringify(value)); }
  select.addEventListener('change', function() {
    var next = Number(select.value);
    var fixed = el.data.filter(function(trace) {
      return trace.meta && trace.meta.shape_toolkit_grouping === 'fixed';
    });
    var overlays = el.data.filter(function(trace) {
      return !trace.meta || !trace.meta.shape_toolkit_grouping;
    });
    modes[current].traces = el.data.filter(function(trace) {
      return trace.meta && trace.meta.shape_toolkit_grouping === 'group';
    });
    var layout = clone(el.layout);
    layout.legend = layout.legend || {};
    layout.legend.title = next === 0 ? originalTitle : {text: modes[next].label};
    if (el._fullLayout && el._fullLayout.scene) {
      layout.scene = layout.scene || {};
      layout.scene.camera = clone(el._fullLayout.scene.camera);
    }
    select.disabled = true;
    status.textContent = ' Updating...';
    Promise.resolve().then(function() {
      return Plotly.react(el, fixed.concat(clone(modes[next].traces), overlays),
        layout, el._context);
    }).then(function() {
      current = next;
      status.textContent = '';
    }).catch(function(error) {
      select.value = String(current);
      status.textContent = ' Unable to change colouring: ' + error.message;
      console.error('Colour grouping failed', error);
    }).finally(function() { select.disabled = false; });
  });
}", data = list(base_label = base_label, modes = modes))
}
