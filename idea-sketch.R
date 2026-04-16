setwd("~/Research/Seasonal fragmentation")


#### 1. Schematic of flow thresholds (wet/dry and passable/impassable)

# Define x (river location)
x <- seq(0, 2*pi, length = 1001)

# Define y (wet/dry threshold of relative flow)
y <- sin(12*x + pi/2) + 2*(sin(10*x)) + 3*sin(2*x) + x
y <- rev(y)
y <- pmax(0, y)
y <- 0.85 * y / max(y)

# Define z (passable/impassable threshold of relative flow)
barriers <- data.frame(location = c(1.3, 2.9, 4.2, 5.6),
                       flow.threshold = c(0.5, 0.9, 1.0, 0.7))
barrier.width = 0.07
# add 
z <- rep(0, length(x))
for(i in 1:nrow(barriers)) {
  z[abs(x - barriers$location[i]) <= barrier.width/2] <- barriers$flow.threshold[i]
}



png(filename = "flow-threshold-schematic.png",
    width = 6, height = 4, units = "in", res = 250)
plot(x, y, type = "n",
     ylim = c(0, 1),
     xlab = "River length",
     ylab = "Relative flow") # bquote("Flow threshold for movement ("*Q[mvmt]*")"))
# Shade area below the line 
polygon(c(x, rev(x)), 
        c(rep(0, length(x)), rev(y)), 
        col = "lightpink", border = NA) 
# Shade area above the line 
polygon(c(x, rev(x)), 
        c(y, rep(100, length(x))), 
        col = "lightblue", border = NA) 
# Shade barriers
polygon(c(x, rev(x)), 
        c(y, rev(pmax(y,z))),
        col = "indianred1", border = NA)
# Redraw the original line to be on top 
lines(x, y, col = "black", lwd = 1)
## Add barriers
# lines(x, pmax(y,z), col = "black", lwd = 1.5, lty = 3)
# # (Barriers separately, as vertical bars:)
# for(i in 1:nrow(barriers)) {
#   lines(rep(barriers$location[i], 2), 
#         c(0, barriers$flow.threshold[i]), 
#         col = "grey40", lwd = 4)
# }
# example of high flow
abline(h = 0.82, col = "darkblue", lty = 2, lwd = 3)
text(x = 0.3, y = 0.84, 
     adj = c(0, 0),
     labels = "high flow", col = "darkblue")
# example of low flow
abline(h = 0.2, col = "darkred", lty = 2, lwd = 3)
text(x = 1.8, y = 0.22, 
     adj = c(0, 0),
     labels = "low flow", col = "darkred")
# Legend
legend("topright", fill = c("lightblue", "lightpink", "indianred1"),
       legend = c("Wet (passable)", "Dry (impassable)",
                  "Barrier"),
       inset = c(-0.01, -0.15), xpd = TRUE,
       cex = 0.8)
dev.off()



#### 2. Habitat size vs. flow ####
# Habitat size is a measure of in-stream habitat size. For now, assume proportional to resources, but in reality, in-stream (autochthonous) prey availability may lag habitat availability and be supplemented by terrestrial fluxes.

rm(list = ls())
x <- seq(0, 1, length = 1001)
y <- 2*(sin(x*pi/2) - x*pi/2 * cos(x*pi/2))# sin(x*pi/2)
  # Derive a saturating function or fit to data.
  # e.g., x^(2/3) # 1 - exp(-5*x)

# png(filename = "flow-habitat-schematic.png",
#     width = 6, height = 4, units = "in", res = 250)
plot(x, y,
     type = "l", lwd = 2,
     xlab = "Discharge", 
     ylab = "Habitat size")
# dev.off()

# # With benches:
# rm(list = ls())
# x <- seq(0, 1, length = 1001)
# y <- x^(1/2) # x^(2/3) # 1 - exp(-5*x)
# y.w.benches <- y + 0.5 * sin(1.8*2*pi*x)/(1.8*2*pi)
# 
# png(filename = "flow-habitat-schematic.png",
#     width = 6, height = 4, units = "in", res = 250)
# plot(y, x,
#      type = "l",
#      ylab = "Absolute flow", 
#      xlab = "Habitat size")
# lines(y.w.benches, x, lty = 2)
# legend("topleft",
#        lty = 1:2, 
#        legend = c("Uniformly shaped river", "River with benches"))
# dev.off()
