# Morphospace functions -------
#
# This file contains code adapted from the Momocs package
# (https://github.com/MomX/Momocs)
# Original authors: Vincent Bonhomme, Sandrine Picq, Cedric Gaucherel, Julien Claude
# Momocs is licensed under GPL-2 | GPL-3
# Reference: Bonhomme et al. (2014) J. Stat. Softw. 56(13). doi:10.18637/jss.v056.i13

# Helper function for window dimensions
.wdw <- function() {
  wdw <- par("usr")
  x <- wdw[2] - wdw[1]
  y <- wdw[4] - wdw[3]
  return(c(x, y))
}

# ggplot helper functions
.x.range.gg <- function(gg){
  ggplot2::ggplot_build(gg)$panel$ranges[[1]]$x.range
}

.y.range.gg <- function(gg){
  ggplot2::ggplot_build(gg)$panel$ranges[[1]]$y.range
}

.wdw.gg <- function(gg){
  c(diff(gg$layout$panel_scales_x[[1]]$range$range),
    diff(gg$layout$panel_scales_y[[1]]$range$range))
}

# Confidence ellipse function
conf_ell <- function(x, y, conf = 0.95, nb.pts = 60) {
  if (is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  centroid <- apply(cbind(x, y), 2, mean)
  theta.i <- seq(0, 2 * pi, length = nb.pts + 1)[-c(nb.pts + 1)]
  z <- cbind(cos(theta.i), sin(theta.i))
  rad <- qnorm((1 - conf)/2, mean = 0, sd = 1, lower.tail = FALSE)
  vcvxy <- var(cbind(x, y))
  r <- cor(x, y)
  M1 <- matrix(c(1, 1, -1, 1), nrow = 2, ncol = 2)
  M2 <- matrix(c(var(x), var(y)), nrow = 2, ncol = 2)
  M3 <- matrix(c(1 + r, 1 - r), nrow = 2, ncol = 2, byrow = TRUE)
  ellpar <- M1 * sqrt(M2 * M3/2)
  ell <- t(centroid + rad * ellpar %*% t(z))
  colnames(ell) <- c("x", "y")
  # stupid approximation
  ell.al <- coo_align(ell)
  ell.ids <- c(which.min(ell.al[, 1]), which.max(ell.al[, 1]),
               which.min(ell.al[, 2]), which.max(ell.al[, 2]))
  seg <- ell[ell.ids, ]
  return(list(ell = ell, seg = seg))
}

# Domestic helper function for matrix multiplication
.mprod <- function(m, s) {
  res <- m
  for (i in 1:ncol(m)) {
    res[, i] <- m[, i] * s[i]
  }
  return(res)
}

# Helper function to split coefficients
coeff_split <- function(coe, cph = 4) {
  nb.h <- length(coe) / cph
  res <- list()
  if (cph == 2) {
    res$an <- coe[1:nb.h]
    res$bn <- coe[(nb.h + 1):(2 * nb.h)]
  }
  if (cph == 4) {
    res$an <- coe[1:nb.h]
    res$bn <- coe[(nb.h + 1):(2 * nb.h)]
    res$cn <- coe[(2 * nb.h + 1):(3 * nb.h)]
    res$dn <- coe[(3 * nb.h + 1):(4 * nb.h)]
  }
  return(res)
}

# Calculates shapes from PC plane: efourier
#' @export
PCA2shp_efourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
  if (ncol(pos) != ncol(rot))
    stop("'rot' and 'pos' must have the same ncol")
  if (length(mshape) != nrow(rot))
    stop("'mshape' and ncol(rot) lengths differ")
  nb.h <- length(mshape)/4
  n <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
    coe <- mshape + apply(ax.contrib, 1, sum)
    xf <- coeff_split(coe, cph = 4)
    coo <- efourier_i(xf, nb.h = nb.h, nb.pts = pts.shp)
    # reconstructed shapes are translated on their centroid
    dx <- pos[i, 1] - coo_centpos(coo)[1]
    dy <- pos[i, 2] - coo_centpos(coo)[2]
    coo <- coo_trans(coo, dx, dy)
    res[[i]] <- coo
  }
  return(res)
}

# Calculates shapes from PC plane: rfourier
#' @export
PCA2shp_rfourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
  if (ncol(pos) != ncol(rot))
    stop("'rot' and 'pos' must have the same ncol")
  if (length(mshape) != nrow(rot))
    stop("'mshape' and ncol(rot) lengths differ")
  nb.h <- length(mshape)/2
  n <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
    coe <- mshape + apply(ax.contrib, 1, sum)
    xf <- coeff_split(coe, cph = 2)
    coo <- rfourier_i(xf, nb.h = nb.h, nb.pts = pts.shp)
    # reconstructed shapes are translated on their centroid
    dx <- pos[i, 1] - coo_centpos(coo)[1]
    dy <- pos[i, 2] - coo_centpos(coo)[2]
    coo <- coo_trans(coo, dx, dy)
    res[[i]] <- coo
  }
  return(res)
}

# Calculates shapes from PC plane: tfourier
#' @export
PCA2shp_tfourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
  if (ncol(pos) != ncol(rot))
    stop("'rot' and 'pos' must have the same ncol")
  if (length(mshape) != nrow(rot))
    stop("'mshape' and ncol(rot) lengths differ")
  nb.h <- length(mshape)/2
  n <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
    coe <- mshape + apply(ax.contrib, 1, sum)
    xf <- coeff_split(coe, cph = 2)
    coo <- tfourier_i(xf, nb.h = nb.h, nb.pts = pts.shp)
    # reconstructed shapes are translated on their centroid
    dx <- pos[i, 1] - coo_centpos(coo)[1]
    dy <- pos[i, 2] - coo_centpos(coo)[2]
    coo <- coo_trans(coo, dx, dy)
    res[[i]] <- coo
  }
  return(res)
}

# Calculates shapes from PC plane: sfourier
#' @export
PCA2shp_sfourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
  if (ncol(pos) != ncol(rot))
    stop("'rot' and 'pos' must have the same ncol")
  if (length(mshape) != nrow(rot))
    stop("'mshape' and ncol(rot) lengths differ")
  nb.h <- length(mshape)/2
  n <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
    coe <- mshape + apply(ax.contrib, 1, sum)
    xf <- coeff_split(coe, cph = 2)
    coo <- sfourier_i(xf, nb.h = nb.h, nb.pts = pts.shp)
    # reconstructed shapes are translated on their centroid
    dx <- pos[i, 1] - coo_centpos(coo)[1]
    dy <- pos[i, 2] - coo_centpos(coo)[2]
    coo <- coo_trans(coo, dx, dy)
    res[[i]] <- coo
  }
  return(res)
}

# Calculates shapes from PC plane: dfourier
#' @export
PCA2shp_dfourier <- function(pos, rot, mshape, amp.shp = 1, pts.shp = 60) {
  if (ncol(pos) != ncol(rot))
    stop("'rot' and 'pos' must have the same ncol")
  if (length(mshape) != nrow(rot))
    stop("'mshape' and ncol(rot) lengths differ")
  nb.h <- length(mshape)/2
  n <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
    coe <- mshape + apply(ax.contrib, 1, sum)
    xf <- coeff_split(coe, cph=2)
    coo <- dfourier_i(xf, nb.h = nb.h, nb.pts = pts.shp)
    # reconstructed shapes are translated on their centroid
    dx <- pos[i, 1] - coo_centpos(coo)[1]
    dy <- pos[i, 2] - coo_centpos(coo)[2]
    coo <- coo_trans(coo, dx, dy)
    res[[i]] <- coo
  }
  return(res)
}

# Calculates shapes from PC plane: polynomials
#' @export
PCA2shp_polynomials <- function(pos, rot, mshape, amp.shp = 1,
                                pts.shp = 120, ortho = TRUE,
                                baseline1, baseline2) {
  if (ncol(pos) != ncol(rot))
    stop("'rot' and 'pos' must have the same ncol")
  if (length(mshape) != nrow(rot))
    stop("'mshape' and ncol(rot) lengths differ")
  degree <- length(mshape)
  n <- nrow(pos)
  # we prepare the array
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
    coe <- mshape + apply(ax.contrib, 1, sum)
    # depending on whether we work with opoly or npoly
    if (ortho) {
      coo <- opoly_i(list(coeff = coe, ortho = TRUE,
                         baseline1 = baseline1, baseline2 = baseline2),
                    nb.pts = pts.shp)
    } else {
      coo <- npoly_i(list(coeff = coe),
                    nb.pts = pts.shp,
                    baseline1 = baseline1,
                    baseline2 = baseline2)
    }
    # pol$coeff <- rep(NA, degree)
    # reconstructed shapes are translated on their centroid
    dx <- pos[i, 1] - coo_centpos(coo)[1]
    dy <- pos[i, 2] - coo_centpos(coo)[2]
    coo <- coo_trans(coo, dx, dy)
    res[[i]] <- coo
  }
  return(res)
}

# Calculates shapes from PC plane: (aligned) landmarks
#' @export
PCA2shp_procrustes <- function(pos, rot, mshape, amp.shp = 1) {
  if (ncol(pos) != ncol(rot))
    stop("'rot' and 'pos' must have the same ncol")
  if (length(mshape) != nrow(rot))
    stop("'mshape' and ncol(rot) lengths differ")
  k <- 2
  nb.ldk <- length(mshape)/k
  n <- nrow(pos)
  res <- list()
  for (i in 1:n) {
    ax.contrib <- .mprod(rot, pos[i, ]) * amp.shp
    coe <- mshape + apply(ax.contrib, 1, sum)
    coo <- matrix(coe, ncol = k, byrow = FALSE)
    res[[i]] <- coo
  }
  return(res)
}
