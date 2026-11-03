# Browser-side specimen details, shared by Shiny and standalone HTML exports.
.specimen_viewer_data <- function(data, options, axes) {
  defaults <- list(shape_col = "shape", id_col = "", genus_col = "",
                   species_col = "", latitude_col = "", longitude_col = "",
                   antweb_col = "", metadata_cols = character())
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
  if (!nzchar(options$shape_col) || !is.list(data[[options$shape_col]])) {
    stop("Specimen viewer shape_col must name a list-column of mapped shapes.", call. = FALSE)
  }
  scalar_columns <- unique(c(unlist(options[setdiff(fields, "shape_col")], use.names = FALSE),
                             options$metadata_cols))
  scalar_columns <- scalar_columns[nzchar(scalar_columns)]
  if (any(vapply(data[scalar_columns], function(x) !is.atomic(x) || !is.null(dim(x)), logical(1)))) {
    stop("Specimen viewer metadata must use scalar, atomic columns.", call. = FALSE)
  }
  if (xor(nzchar(options$latitude_col), nzchar(options$longitude_col))) {
    stop("Select both latitude and longitude columns, or neither.", call. = FALSE)
  }
  text <- function(col, i) {
    if (!nzchar(col) || is.na(data[[col]][i])) return("")
    trimws(as.character(data[[col]][i]))
  }
  lapply(seq_len(nrow(data)), function(i) {
    shape <- data[[options$shape_col]][[i]]
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
      antweb = text(options$antweb_col, i),
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
  htmlwidgets::onRender(p, "
function(el, x, records) {
  if (el._specimenClick) el.removeListener('plotly_click', el._specimenClick);
  if (el._specimenPanel) el._specimenPanel.remove();
  el.style.position = 'relative';
  var panel = document.createElement('section');
  panel.setAttribute('aria-label', 'Specimen details');
  panel.setAttribute('aria-live', 'polite');
  panel.style.cssText = 'position:absolute;right:0;top:35px;bottom:0;width:40%;max-width:320px;' +
    'overflow:auto;background:white;color:#222;border-left:1px solid #ccc;padding:12px;' +
    'box-sizing:border-box;font:14px sans-serif;z-index:10;';
  el.appendChild(panel);
  el._specimenPanel = panel;
  function add(tag, value, parent) {
    var node = document.createElement(tag);
    if (value !== null) node.textContent = value;
    (parent || panel).appendChild(node);
    return node;
  }
  function link(label, url) {
    var a = add('a', label);
    a.href = url;
    a.target = '_blank';
    a.rel = 'noopener noreferrer';
    a.style.display = 'block';
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
    if (record.antweb) {
      link('AntWeb: ' + record.antweb,
        'https://www.antweb.org/specimen.do?name=' + encodeURIComponent(record.antweb));
    }
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
    link('View on OpenStreetMap', 'https://www.openstreetmap.org/?mlat=' +
      lat + '&mlon=' + lon + '#map=12/' + lat + '/' + lon);
    if (Math.abs(lat) > 85) {
      add('p', 'This polar location is outside the embedded web map coverage.');
      return;
    }
    var bbox = [Math.max(-180, lon - 0.05), Math.max(-85, lat - 0.05),
      Math.min(180, lon + 0.05), Math.min(85, lat + 0.05)].join(',');
    var frame = add('iframe', null);
    frame.title = 'Specimen location on OpenStreetMap';
    frame.style.cssText = 'width:100%;height:220px;border:0;';
    frame.referrerPolicy = 'no-referrer';
    frame.src = 'https://www.openstreetmap.org/export/embed.html?bbox=' +
      encodeURIComponent(bbox) + '&layer=mapnik&marker=' + encodeURIComponent(lat + ',' + lon);
    add('p', 'Map tiles require internet access. Map data: OpenStreetMap contributors.');
  };
  el.on('plotly_click', el._specimenClick);
  Plotly.relayout(el, {'margin.r': Math.min(320, el.clientWidth * 0.4) + 20});
}
", data = records)
}
