# Browser-side specimen details, shared by Shiny and standalone HTML exports.
.specimen_viewer_data <- function(data, options, axes) {
  defaults <- list(shape_col = "shape", id_col = "", genus_col = "",
                   species_col = "", latitude_col = "", longitude_col = "",
                   accession_col = "", metadata_cols = character())
  options <- utils::modifyList(defaults, options)
  fields <- setdiff(names(defaults), "metadata_cols")
  for (field in fields) {
    col <- options[[field]]
    if (!is.character(col) || length(col) != 1L || is.na(col)) {
      stop("Specimen viewer ", field, " must be a single column name.", call. = FALSE)
    }
  }
  if (!is.character(options$metadata_cols) || anyNA(options$metadata_cols)) {
    stop("Specimen viewer metadata_cols must be column names.", call. = FALSE)
  }
  columns <- unique(c(unlist(options[fields], use.names = FALSE), options$metadata_cols))
  columns <- columns[nzchar(columns)]
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop("Specimen viewer columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nzchar(options$shape_col) && !is.list(data[[options$shape_col]])) {
    stop("Specimen viewer shape_col must name a list-column of mapped shapes.", call. = FALSE)
  }
  scalar_columns <- unique(c(unlist(options[setdiff(fields, "shape_col")], use.names = FALSE),
                             options$metadata_cols))
  scalar_columns <- scalar_columns[nzchar(scalar_columns)]
  if (any(vapply(data[scalar_columns], function(x) !is.atomic(x) || !is.null(dim(x)), logical(1)))) {
    stop("Specimen viewer metadata must use scalar, atomic columns.", call. = FALSE)
  }
  text <- function(col, i) {
    if (!nzchar(col) || is.na(data[[col]][i])) return("")
    trimws(as.character(data[[col]][i]))
  }
  lapply(seq_len(nrow(data)), function(i) {
    shape <- if (nzchar(options$shape_col)) data[[options$shape_col]][[i]] else NULL
    coords <- if (is.matrix(shape)) shape else if (is.list(shape) &&
      is.list(shape$coo) && length(shape$coo)) shape$coo[[1]] else NULL
    shape_status <- if (is.null(shape)) "No mapped shape for this specimen." else "Invalid mapped shape."
    outline <- NULL
    if (is.matrix(coords) && is.numeric(coords) && ncol(coords) == 2L &&
        nrow(coords) >= 3L && all(is.finite(coords)) &&
        any(apply(coords, 2, function(x) diff(range(x))) > 0)) {
      outline <- list(x = unname(coords[, 1]), y = unname(coords[, 2]))
      shape_status <- ""
    }
    lat_text <- text(options$latitude_col, i)
    lon_text <- text(options$longitude_col, i)
    lat <- suppressWarnings(as.numeric(lat_text))
    lon <- suppressWarnings(as.numeric(lon_text))
    valid_location <- length(lat) == 1L && length(lon) == 1L &&
      is.finite(lat) && is.finite(lon) && abs(lat) <= 90 && abs(lon) <= 180
    list(
      id = if (nzchar(options$id_col)) text(options$id_col, i) else paste("Specimen", i),
      species = paste(Filter(nzchar, c(text(options$genus_col, i), text(options$species_col, i))),
                      collapse = " "),
      accession = text(options$accession_col, i),
      shape = outline, shape_status = shape_status,
      latitude = if (valid_location) lat else NULL,
      longitude = if (valid_location) lon else NULL,
      location_status = if (valid_location) "" else if (!nzchar(lat_text) || !nzchar(lon_text)) {
        "No location coordinates for this specimen."
      } else "Invalid location: use decimal latitude [-90, 90] and longitude [-180, 180].",
      coordinates = lapply(axes, function(col) list(label = col, value = text(col, i))),
      metadata = lapply(options$metadata_cols, function(col) list(label = col, value = text(col, i)))
    )
  })
}

.add_specimen_viewer <- function(p, records) {
  map_file <- system.file("extdata", "equal-earth-world-path.txt", package = "ShapeToolKit")
  if (!nzchar(map_file)) {
    candidates <- file.path(c(".", "..", file.path("..", "..")),
                            "inst", "extdata", "equal-earth-world-path.txt")
    existing <- candidates[file.exists(candidates)]
    if (length(existing)) map_file <- existing[1]
  }
  if (!nzchar(map_file)) {
    stop("Bundled Equal Earth world map not found. Reinstall ShapeToolKit.", call. = FALSE)
  }
  map_paths <- readLines(map_file, warn = FALSE)
  if (length(map_paths) != 2L || any(!nzchar(map_paths))) {
    stop("Bundled Equal Earth world map is invalid.", call. = FALSE)
  }
  p$x$specimen_map <- as.list(map_paths)
  htmlwidgets::onRender(p, "
function(el, x, records) {
  if (el._specimenClick) el.removeListener('plotly_click', el._specimenClick);
  if (el._specimenPanel) el._specimenPanel.remove();
  el.style.position = 'relative';
  var panel = document.createElement('section');
  panel.setAttribute('aria-label', 'Specimen details');
  panel.setAttribute('aria-live', 'polite');
  panel.style.cssText = 'position:absolute;left:0;top:35px;bottom:0;width:40%;max-width:320px;' +
    'overflow:auto;background:white;color:#222;border-right:1px solid #ccc;padding:12px;' +
    'box-sizing:border-box;font:14px sans-serif;z-index:10;';
  el.appendChild(panel);
  el._specimenPanel = panel;
  function add(tag, value, parent) {
    var node = document.createElement(tag);
    if (value !== null) node.textContent = value;
    (parent || panel).appendChild(node);
    return node;
  }
  function equalEarth(lon, lat) {
    var theta = Math.asin(Math.sqrt(3) / 2 * Math.sin(lat * Math.PI / 180));
    var t2 = theta * theta, t6 = t2 * t2 * t2;
    var denominator = 1.340264 - 3 * 0.081106 * t2 +
      t6 * (7 * 0.000893 + 9 * 0.003796 * t2);
    var px = 2 * Math.sqrt(3) * lon * Math.PI / 180 * Math.cos(theta) / (3 * denominator);
    var py = theta * (1.340264 - 0.081106 * t2 + t6 * (0.000893 + 0.003796 * t2));
    return [250 + 85 * px, 135 - 85 * py];
  }
  add('h3', 'Specimen details');
  add('p', 'Click a specimen point to see its mapped shape, species and location.');
  el._specimenClick = function(event) {
    var point = event.points && event.points[0];
    // Only specimen marker traces carry a row key; hulls and grids do not.
    if (!point || point.data.type !== 'scatter3d' || point.data.mode !== 'markers') return;
    var key = point.customdata;
    if (Array.isArray(key)) key = key[0];
    if (!Number.isInteger(key) || key < 1 || key > records.length) return;
    var record = records[key - 1];
    panel.replaceChildren();
    add('h3', record.id || 'Specimen');
    add('p', record.species || 'No species information.');
    add('p', 'Accession ID: ' + (record.accession || 'Not available'));
    (record.coordinates || []).concat(record.metadata || []).forEach(function(item) {
      add('p', item.label + ': ' + (item.value || 'Not available'));
    });
    add('h4', 'Mapped shape');
    if (record.shape) {
      var xs = record.shape.x, ys = record.shape.y;
      var xmin = xs.reduce(function(a, b) { return Math.min(a, b); }, Infinity);
      var xmax = xs.reduce(function(a, b) { return Math.max(a, b); }, -Infinity);
      var ymin = ys.reduce(function(a, b) { return Math.min(a, b); }, Infinity);
      var ymax = ys.reduce(function(a, b) { return Math.max(a, b); }, -Infinity);
      var scale = 180 / Math.max(xmax - xmin, ymax - ymin);
      var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
      svg.setAttribute('viewBox', '0 0 200 200');
      svg.setAttribute('role', 'img');
      svg.setAttribute('aria-label', 'Mapped specimen outline');
      svg.style.cssText = 'width:100%;height:200px;';
      var polygon = document.createElementNS(svg.namespaceURI, 'polygon');
      polygon.setAttribute('points', xs.map(function(v, i) {
        return (100 + (v - (xmin + xmax) / 2) * scale) + ',' +
          (100 - (ys[i] - (ymin + ymax) / 2) * scale);
      }).join(' '));
      polygon.setAttribute('fill', '#ddd');
      polygon.setAttribute('stroke', '#222');
      svg.appendChild(polygon);
      panel.appendChild(svg);
    } else {
      add('p', record.shape_status);
    }
    add('h4', 'Location');
    if (record.latitude === null || record.longitude === null) {
      add('p', record.location_status);
      return;
    }
    var lat = record.latitude, lon = record.longitude;
    add('p', 'Latitude: ' + lat + ', Longitude: ' + lon);
    var map = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    map.setAttribute('viewBox', '0 0 500 270');
    map.setAttribute('role', 'img');
    map.setAttribute('aria-label', 'Equal Earth world map: latitude ' + lat + ', longitude ' + lon);
    map.style.cssText = 'width:100%;height:auto;background:white;';
    x.specimen_map.forEach(function(d, i) {
      var path = document.createElementNS(map.namespaceURI, 'path');
      path.setAttribute('d', d);
      path.setAttribute('fill', i === 0 ? '#e5f3fa' : '#d1d8ce');
      path.setAttribute('fill-rule', 'evenodd');
      path.setAttribute('stroke', '#7b898c');
      path.setAttribute('stroke-width', i === 0 ? '0.8' : '0.35');
      map.appendChild(path);
    });
    var position = equalEarth(lon, lat);
    var marker = document.createElementNS(map.namespaceURI, 'circle');
    marker.setAttribute('cx', position[0]);
    marker.setAttribute('cy', position[1]);
    marker.setAttribute('r', '4');
    marker.setAttribute('fill', '#d62728');
    marker.setAttribute('stroke', 'white');
    marker.setAttribute('stroke-width', '1');
    map.appendChild(marker);
    panel.appendChild(map);
    add('p', 'Equal Earth projection. World map: Natural Earth (public domain). Works offline.');
  };
  el.on('plotly_click', el._specimenClick);
  Plotly.relayout(el, {'margin.l': Math.min(320, el.clientWidth * 0.4) + 20});
}
", data = records)
}
