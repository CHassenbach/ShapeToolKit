# PCA on Coe objects
#
# This file contains code adapted from the Momocs package
# (https://github.com/MomX/Momocs)
# Original authors: Vincent Bonhomme, Sandrine Picq, Cedric Gaucherel, Julien Claude
# Momocs is licensed under GPL-2 | GPL-3
# Reference: Bonhomme et al. (2014) J. Stat. Softw. 56(13). doi:10.18637/jss.v056.i13

# 1. PCA calculation and builder --------------------------

fac_dispatcher <- function(x, fac) {
  n <- NULL
  if (!is.null(x$x)) {
    n <- nrow(as.matrix(x$x))
  } else if (!is.null(x$coe)) {
    n <- nrow(as.matrix(x$coe))
  }

  if (inherits(fac, "formula")) {
    if (is.null(x$fac) || ncol(x$fac) == 0) {
      stop("formula provided but x$fac is empty")
    }
    cols <- attr(terms(fac), "term.labels")
    if (any(!cols %in% colnames(x$fac))) {
      stop("formula provided must match with $fac column names")
    }
    selected <- x$fac[, cols, drop = FALSE]
    if (ncol(selected) == 1) {
      return(selected[[1]])
    }
    return(factor(apply(selected, 1, paste, collapse = "_")))
  }

  if (length(fac) == 1 && is.character(fac)) {
    if (is.null(x$fac) || ncol(x$fac) == 0) {
      stop("x$fac is empty")
    }
    if (!fac %in% colnames(x$fac)) {
      stop("fac must match a column in x$fac")
    }
    return(x$fac[[fac]])
  }

  if (length(fac) == 1 && is.numeric(fac)) {
    if (is.null(x$fac) || ncol(x$fac) == 0) {
      stop("x$fac is empty")
    }
    if (fac < 1 || fac > ncol(x$fac)) {
      stop("fac index out of bounds for x$fac")
    }
    return(x$fac[[fac]])
  }

  if (!is.null(n) && length(fac) == n) {
    return(fac)
  }

  stop("fac must be a formula, x$fac column name/index, or a vector of appropriate length")
}

#' Principal component analysis on Coe objects
#'
#' Performs a PCA on \code{Coe} objects, using \link{prcomp}.
#'
#' By default, methods on \code{Coe} object do not scale the input data but center them.
#' There is also a generic method (eg for traditional morphometrics) that centers and scales data.
#' @aliases PCA
#' @rdname PCA
#' @param x a \code{Coe} object or an appropriate object (eg \link{prcomp}) for \code{as_PCA}
#' @param fac any factor or data.frame to be passed to \code{as_PCA} and for use with \link{plot.PCA}
#' @param scale. logical whether to scale the input data
#' @param center logical whether to center the input data
#' @return a 'PCA' object on which to apply \link{plot.PCA}, among others. This list has several
#' components, most of them inherited from the \code{prcomp} object:
#' \enumerate{
#'   \item \code{sdev} the standard deviations of the principal components
#'    (i.e., the square roots of the eigenvalues of the
#'    covariance/correlation matrix, though the calculation
#'    is actually done with the singular values of the data matrix)
#'    \item \code{eig} the cumulated proportion of variance along the PC axes
#'   \item \code{rotation} the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors).
#'   The function princomp returns this in the element loadings.
#'   \item \code{center}, scale the centering and scaling used
#'   \item \code{x} PCA scores (the value of the rotated data (the centred (and scaled if requested)
#'   data multiplied by the rotation matrix))
#'   \item other components are inherited from the \code{Coe} object passed to \code{PCA},
#'   eg \code{fac}, \code{mshape}, \code{method}, \code{baseline1} and \code{baseline2}, etc. They
#'   are documented in the corresponding \code{*Coe} file.
#' }
#' @family multivariate
#' @examples
#' head(iris)
#' iris.p <- prcomp(iris[, 1:4])
#' iris.p <- as_PCA(iris.p, iris[, 5])
#' class(iris.p)
#' iris.p$x[1:3, 1:2]
#'
#' # Coe-object examples (run only when Momocs example data are available)
#' if (exists("bot", inherits = TRUE)) {
#'   bot.f <- efourier(bot, 12)
#'   bot.p <- PCA(bot.f)
#'   bot.p
#'   summary(bot.p$eig[1:5])
#' }
#'
#' if (exists("olea", inherits = TRUE)) {
#'   op <- npoly(olea, 5)
#'   op.p <- PCA(op)
#'   op.p
#'   summary(op.p$eig[1:5])
#' }
#'
#' if (exists("wings", inherits = TRUE)) {
#'   wp <- fgProcrustes(wings, tol = 1e-4)
#'   wpp <- PCA(wp)
#'   wpp
#'   summary(wpp$eig[1:5])
#' }
#' @export
PCA <- function(x, scale., center, fac) {
  UseMethod("PCA")
}

#' @rdname PCA
#' @export
PCA.OutCoe <- function(x, scale. = FALSE, center = TRUE, fac) {
  OutCoe <- x
  PCA <- prcomp(OutCoe$coe, scale. = scale., center = center)
  eig <- (PCA$sdev^2)
  PCA$eig <- eig/sum(eig)
  PCA$fac <- OutCoe$fac
  PCA$mshape <- apply(OutCoe$coe, 2, mean)
  PCA$method <- OutCoe$method
  if (!is.null(OutCoe$baseline1)){
    PCA$baseline1 <- OutCoe$baseline1
    PCA$baseline2 <- OutCoe$baseline2}
  PCA$cuts   <- OutCoe$cuts
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.OpnCoe <- function(x, scale. = FALSE, center = TRUE, fac) {
  OpnCoe <- x
  PCA <- prcomp(OpnCoe$coe, scale. = scale., center = center)
  eig <- (PCA$sdev^2)
  PCA$eig <- eig/sum(eig)
  PCA$fac <- OpnCoe$fac
  PCA$mshape <- apply(OpnCoe$coe, 2, mean)
  PCA$method <- OpnCoe$method
  PCA$mod <- OpnCoe$mod  #the only diff so far
  PCA$baseline1 <- OpnCoe$baseline1
  PCA$baseline2 <- OpnCoe$baseline2
  PCA$cuts   <- OpnCoe$cuts
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.LdkCoe <- function(x, scale. = FALSE, center = TRUE, fac) {
  LdkCoe <- x
  # LdkCoe$coe <- a2m(l2a(Coe$coo))
  PCA <- prcomp(LdkCoe$coe, scale. = scale., center = center)
  eig <- (PCA$sdev^2)
  PCA$eig <- eig/sum(eig)
  PCA$fac <- LdkCoe$fac
  PCA$mshape <- apply(LdkCoe$coe, 2, mean)
  PCA$method <- "procrustes"
  PCA$cuts   <- LdkCoe$cuts
  PCA$links <- LdkCoe$links
  # PCA$mod <- OpnCoe$mod #the only diff so far PCA$baseline1
  # <- OpnCoe$baseline1 PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.TraCoe <- function(x, scale. = TRUE, center = TRUE, fac) {
  TraCoe <- x
  # LdkCoe$coe <- a2m(l2a(Coe$coo))
  PCA <- prcomp(TraCoe$coe, scale. = scale., center = center)
  eig <- (PCA$sdev^2)
  PCA$eig <- eig/sum(eig)
  PCA$fac <- TraCoe$fac
  PCA$mshape <- NULL
  PCA$method <- NULL

  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

#' @rdname PCA
#' @export
PCA.default <- function(x, scale. = TRUE, center = TRUE, fac=data.frame()) {
  PCA <- prcomp(x, scale. = scale., center = center)
  eig <- (PCA$sdev^2)
  PCA$eig <- eig/sum(eig)
  if (!is.null(fac)) fac <- tibble::as_tibble()(fac)
  PCA$fac <- fac
  PCA$method <- NULL
  # PCA$baseline2 <- OpnCoe$baseline2
  class(PCA) <- c("PCA", class(PCA))
  return(PCA)
}

# 2. PCA Bridges ------------------------------------------
#' @rdname PCA
#' @export
as_PCA <- function(x, fac){
  UseMethod("as_PCA")
}

#' @export
as_PCA.default <- function(x, fac){
  if (class(x)[1] != "PCA"){
    class(x) <- c("PCA", class(x))
    if (!missing(fac)) x$fac <- tibble::as_tibble(fac)
    return(x)}}

#' @export
print.PCA <- function(x, ...){
  cat("A PCA object\n")
  cat(rep("-", 20), "\n", sep = "")
  cat(" -", ifelse(is.null(nrow(x$x)), 1, nrow(x$x)), "shapes \n")
  # Method printer
  if (length(x$method)>1) {
    cat(" - $method: [", paste0(x$method, collapse=" + "), "analyses ]\n")
  } else {
    cat(" - $method: [", x$method, "analysis ]\n")}
  # we print the fac
  .print_fac(x$fac)
  cat(" - All components: ",  paste(names(x), collapse=", "), ".\n", sep="")
}

# 3. PCA methods ------------------------------------------
#' Get paired individual on a Coe, PCA or LDA objects
#'
#' If you have paired individuals, i.e. before and after a treatment or for repeated measures,
#' and if you have coded coded it into \code{$fac}, this methods allows you to retrieve the corresponding PC/LD scores,
#' or coefficients for \code{Coe} objects.
#' @param x any \code{Coe}, \link{PCA} of \code{LDA} object.
#' @param fac factor or column name or id corresponding to the pairing factor.
#' @param range numeric the range of coefficients for \code{Coe}, or PC (LD) axes on which to return scores.
#' @return a list with components \code{x1} all coefficients/scores corresponding to the
#' first level of the \code{fac} provided; \code{x2} same thing for the second level;
#' \code{fac} the corresponding \code{fac}.
#' @examples
#' x <- list(
#'   x = matrix(rnorm(16), ncol = 2),
#'   fac = data.frame(
#'     session = factor(rep(c("session1", "session2"), each = 4)),
#'     type = factor(rep(c("A", "B"), times = 4))
#'   )
#' )
#' class(x) <- "PCA"
#' pairs <- get_pairs(x, fac = "session", range = 1:2)
#' names(pairs)
#' dim(pairs$session1)
#' dim(pairs$session2)
#'
#'
#' @export
get_pairs <- function(x, fac, range){UseMethod("get_pairs")}
#' @export
get_pairs.Coe <- function(x, fac, range){
  # we check and prepare
  fac <- fac_dispatcher(x, fac)
  if (nlevels(fac) != 2) stop("more than two levels for the 'fac' provided")
  tab <- table(fac)
  if (length(unique(tab))!=1) stop("some mismatches between pairs")
  # we get paired individuals
  if (missing(range)) range <- 1:ncol(x$coe)
  fl  <- levels(fac)
  x1   <- x$coe[fac==fl[1], range]
  x2   <- x$coe[fac==fl[2], range]
  res <- list(x1, x2, fac=x$fac[fac==fl[1],])
  names(res)[1:2] <- fl
  return(res)
}
#' @export
get_pairs.PCA <- function(x, fac, range){
  # we check and prepare
  fac <- fac_dispatcher(x, fac)
  if (nlevels(fac) != 2) stop("more than two levels for the 'fac' provided")
  tab <- table(fac)
  if (length(unique(tab))!=1) stop("some mismatches between pairs")
  # we get paired individuals
  if (missing(range)) range <- 1:ncol(x$x)
  fl  <- levels(fac)
  x1   <- x$x[fac==fl[1], range]
  x2   <- x$x[fac==fl[2], range]
  res <- list(x1, x2, fac=x$fac[fac==fl[1],])
  names(res)[1:2] <- fl
  return(res)
}
#' @export
get_pairs.LDA <- get_pairs.PCA


# 4. redo PCA ---------


#' "Redo" a PCA on a new Coe
#'
#' Basically reapply rotation to a new Coe object.
#' @param PCA a \link{PCA} object
#' @param Coe a \code{Coe} object
#' @note Quite experimental. Dimensions of the matrices and methods must match.
#' @examples
#' t <- seq(0, 2 * pi, length.out = 120)
#' mk <- function(sx, sy) cbind((1 + sx) * cos(t), (1 + sy) * sin(t))
#' b <- Out(list(mk(0.00, 0.00), mk(0.05, -0.03), mk(-0.04, 0.02)))
#' w <- Out(list(mk(0.10, 0.06), mk(0.08, 0.04), mk(0.12, 0.08)))
#'
#' bf <- efourier(b, 8)
#' bp <- PCA(bf)
#'
#' wf <- efourier(w, 8)
#'
#' # and we use the "beer" PCA on the whisky coefficients
#' wp <- rePCA(bp, wf)
#' dim(wp$x)
#'
#'@export
rePCA <- function(PCA, Coe){
  UseMethod("rePCA")
}


#'@export
rePCA.default <- function(PCA, Coe){
  stop("method only defined for PCA objetcs")
}


#'@export
rePCA.PCA <- function(PCA, Coe){
  if (Coe$method != PCA$method)
    warning("methods differ between Coe and PCA")
  scores <- PCA$x
  rot <- PCA$rotation
  coe <- Coe$coe
  if (any(colnames(coe) != rownames(rot)))    warning("matrices coefficients must match")
  # we prepare a new PCA object
  PCA2 <- PCA
  PCA2$x <- matrix(NA, nrow(coe), ncol(rot), dimnames = list(rownames(coe), colnames(rot)))
  PCA2$fac <- Coe$fac
  # we recenter
  coe <- apply(coe, 2, function(x) x - mean(x))
  # learn matrix calculus bitch
  for (PC in 1:ncol(rot)){
    for (ind in 1:nrow(coe)){
      PCA2$x[ind, PC] <- sum(coe[ind, ] * rot[, PC])
    }
  }
  return(PCA2)
}


#' Calculates convex hull area/volume of PCA scores
#'
#' May be useful to compare shape diversity. Expressed in PCA units that should
#' only be compared within the same PCA.
#'
#' @param x a PCA object
#' @param fac (optionnal) column name or ID from the $fac slot.
#' @param xax the first PC axis to use (1 by default)
#' @param yax the second PC axis (2 by default)
#' @param zax the third PC axis (3 by default only for volume)
#'
#' @return
#' If fac is not provided global area/volume is returned; otherwise a named
#' list for every level of fac
#'
#' @details get_chull_area is calculated using \link{coo_chull} followed by \link{coo_area};
#'  get_chull_volume is calculated using geometry::convexhulln
#'
#' @examples
#' bp <- list(x = matrix(rnorm(90), ncol = 3))
#' class(bp) <- "PCA"
#' get_chull_area(bp)
#' get_chull_volume(bp)
#' @export
get_chull_area <- function (x, fac, xax = 1, yax = 2) {
  if (!is_PCA(x)) stop("'x' must be a PCA object")
  # no fac provided
  if (missing(fac)){
    xy <- x$x[, c(xax, yax)]
    return(coo_area(coo_chull(xy)))
  }
  # else... if a fac is provided
  fac <- fac_dispatcher(x, fac)
  x <- x$x[, c(xax, yax)]
  # we prepare the list
  res <- list()
  # we loop over to extract subset of coordinates
  for (i in seq(along = levels(fac))) {
    xy.i <- x[fac == levels(fac)[i], ]
    # boring but prevents the numeric/2rows matrices
    if (is.matrix(xy.i)) {
      if (nrow(xy.i) > 2) {
        res[[i]] <- coo_chull(xy.i)
      }  else {
        res[[i]] <- NULL
      }
    } else {
      res[[i]] <- NULL
    }
  }
  # we calculate the area and return the results
  names(res) <- levels(fac)
  res <- lapply(res, coo_area)
  return(res)
}

#' @rdname get_chull_area
#' @export
get_chull_volume <- function (x, fac, xax = 1, yax = 2, zax = 3) {
  if (!is_PCA(x)) stop("'x' must be a PCA object")

  # no fac provided
  if (missing(fac)){
    xy <- x$x[, c(xax, yax, zax)]
    res <- geometry::convhulln(xy, options="FA")$vol
    return(res)
  }
  # else...fac provided
  fac <- fac_dispatcher(x, fac)
  # we prepare the list
  x <- x$x[, c(xax, yax, zax)]
  res <- list()
  # we loop over to extract subset of coordinates
  for (i in seq(along = levels(fac))) {
    xy.i <- x[fac == levels(fac)[i], ]
    # boring but prevents the numeric/2rows matrices
    if (is.matrix(xy.i)) {
      if (nrow(xy.i) >= 4) {
        res[[i]] <- geometry::convhulln(xy.i, options="FA")$vol
      }  else {
        res[[i]] <- NULL
      }
    } else {
      res[[i]] <- NULL
    }
  }
  # we calculate the area and return the results
  names(res) <- levels(fac)
  return(res)
}

# 5. Flip PCA axes -------------
#' Flips PCA axes
#'
#' Simply multiply by -1, corresponding scores and rotation vectors for PCA objects.
#' PC orientation being arbitrary, this may help to have a better display.
#' @param x a PCA object
#' @param axs numeric which PC(s) to flip
#' @examples
#' bp <- list(
#'   x = matrix(c(1, 2, 3, 4), ncol = 2),
#'   rotation = diag(2)
#' )
#' class(bp) <- "PCA"
#' flip_PCaxes(bp, 1)$x
#' flip_PCaxes(bp, 1)$rotation
#' @export
flip_PCaxes <- function(x, axs){
  UseMethod("flip_PCaxes")
}

#' @export
flip_PCaxes.default <- function(x, axs){
  message("only defined on PCA objects")
}

#' @export
flip_PCaxes.PCA <- function(x, axs){
  x$x[, axs] <- x$x[, axs] * -1
  x$rotation[, axs] <- x$rotation[, axs] * -1
  x
}

#### end PCA