.save_interactive_html <- function(plot, file, selfcontained = TRUE) {
  if (selfcontained &&
      (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available())) {
    stop("Single-file HTML export requires Pandoc. Install Pandoc or uncheck ",
         "'Single-file HTML' to export HTML with a companion files folder.",
         call. = FALSE)
  }
  output_dir <- normalizePath(dirname(file), mustWork = TRUE)
  staging_dir <- tempfile("shapetoolkit-export-", tmpdir = output_dir)
  if (!dir.create(staging_dir)) stop("Unable to create export staging folder.", call. = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)
  staged_html <- file.path(staging_dir, basename(file))
  libdir <- paste0(tools::file_path_sans_ext(basename(file)), "_files")
  htmlwidgets::saveWidget(plot, file = staged_html, selfcontained = selfcontained,
                          libdir = file.path(staging_dir, libdir))

  input <- base::file(staged_html, "rb")
  on.exit(close(input), add = TRUE, after = FALSE)
  header <- rawToChar(readBin(input, "raw", n = 4096L))
  seek(input, where = max(0, file.info(staged_html)$size - 128), origin = "start")
  ending <- rawToChar(readBin(input, "raw", n = 128L))
  if (!grepl("^\\s*<!DOCTYPE html>", header, ignore.case = TRUE) ||
      !grepl("<meta\\s+charset=[\"']utf-8[\"']", header, ignore.case = TRUE) ||
      !grepl("</html>\\s*$", ending, ignore.case = TRUE)) {
    stop("HTML conversion did not produce a complete UTF-8 HTML document. ",
         "The previous export was not replaced. Try exporting with 'Single-file HTML' unchecked.",
         call. = FALSE)
  }

  # A UTF-8 BOM takes precedence over conflicting server charset headers.
  encoded_html <- tempfile("utf8-", tmpdir = staging_dir, fileext = ".html")
  output <- base::file(encoded_html, "wb")
  tryCatch({
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), output)
    seek(input, where = 0, origin = "start")
    repeat {
      chunk <- readBin(input, "raw", n = 1024L * 1024L)
      if (!length(chunk)) break
      writeBin(chunk, output)
    }
  }, finally = close(output))

  if (!selfcontained) {
    staged_lib <- file.path(staging_dir, libdir)
    target_lib <- file.path(output_dir, libdir)
    if (dir.exists(staged_lib)) {
      if (!dir.exists(target_lib) && !dir.create(target_lib)) {
        stop("Unable to create HTML dependency folder: ", target_lib, call. = FALSE)
      }
      assets <- list.files(staged_lib, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      if (!all(file.copy(assets, target_lib, recursive = TRUE, overwrite = TRUE))) {
        stop("Unable to copy all HTML dependencies. The previous HTML was not replaced.",
             call. = FALSE)
      }
    }
  }
  if (!file.copy(encoded_html, file, overwrite = TRUE)) {
    stop("Unable to save HTML export: ", file, call. = FALSE)
  }
  invisible(file)
}
