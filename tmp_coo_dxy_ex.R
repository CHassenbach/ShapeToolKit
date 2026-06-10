### Name: coo_dxy
### Title: Calculate abscissa and ordinate on a shape
### Aliases: coo_dxy

### ** Examples

t <- seq(0, 2 * pi, length.out = 120)
xy <- cbind(cos(t), sin(t))
coo_dxy(coo_sample(xy, 12))



