### Name: coo_convexity
### Title: Calculates the convexity of a shape
### Aliases: coo_convexity

### ** Examples

t <- seq(0, 2 * pi, length.out = 120)
xy <- cbind(cos(t), sin(t)) + matrix(rnorm(240, sd = 0.03), ncol = 2)
coo_convexity(xy)



