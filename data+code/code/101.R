x = 10
name = "Camera Front"

mode(x)
length(x)

mode(name)
length(name)

# random integer number generator
passgen <- function(n, m) {
  pass <- sample(n:m, 1)
  return (pass)
}
pass = passgen(1, 10)

old = 5
if (old == pass) pass = passgen(1, 10) else print(paste("New password is ", pass))

x <- rnorm(10)
y <- rnorm(10)
plot(x,y)
plot(x, y, xlab="Ten random values", ylab="Ten other values",
     xlim=c(-2, 2), ylim=c(-2, 2), pch=22, col="red",
     bg="yellow", bty="l", tcl=0.4,
     main="How to customize a plot with R", las=1, cex=1.5)

opar <- par()
par(bg="lightyellow", col.axis="blue", mar=c(4, 4, 2.5, 0.25))
plot(x, y, xlab="Ten random values", ylab="Ten other values",
     xlim=c(-2, 2), ylim=c(-2, 2), pch=22, col="red", bg="yellow",
     bty="l", tcl=-.25, las=1, cex=1.5)
title("How to customize a plot with R (bis)", font.main=3, adj=1)
par(opar)