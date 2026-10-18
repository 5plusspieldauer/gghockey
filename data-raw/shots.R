# Simulated shots for examples and tests. Run from the package root:
#   Rscript data-raw/shots.R
# Everything here is invented: shot locations are drawn around the net, and xg
# is a toy logistic model of distance and angle, not a fitted one.

set.seed(2026)
n <- 1500

goal_x <- 60 - 4   # IIHF: goal line 4 m from the end boards
goal_y <- 15

# Distance from the net (most shots close in, a long tail from the points) and
# angle off the centre line, wider for shots taken in tight. Draw extra
# candidates and keep the first n that land inside the zone, in front of the
# goal line and off the boards (rejection rather than clamping, so no shots
# pile up on the blue line).
m <- 3 * n
dist <- rgamma(m, shape = 2.6, rate = 0.25)
angle <- rnorm(m, 0, ifelse(dist < 6, 0.75, 0.5))
x <- goal_x - dist * cos(angle)
y <- goal_y + dist * sin(angle)
keep <- which(x > 38 & x < goal_x - 0.2 & y > 0.5 & y < 29.5)[seq_len(n)]
stopifnot(!anyNA(keep))
x <- x[keep]
y <- y[keep]

dist <- sqrt((goal_x - x)^2 + (goal_y - y)^2)
angle <- abs(atan2(y - goal_y, goal_x - x))
xg <- plogis(-0.3 - 0.17 * dist - 0.9 * angle)

shots <- data.frame(
  shot_id = seq_len(n),
  game_id = sample(1:10, n, replace = TRUE),
  period = sample(1:3, n, replace = TRUE),
  team = sample(c("Lakeside", "Northfield"), n, replace = TRUE, prob = c(0.55, 0.45)),
  x = round(x, 2),
  y = round(y, 2),
  xg = round(xg, 4),
  goal = rbinom(n, 1, xg)
)
shots <- shots[order(shots$game_id, shots$period, shots$shot_id), ]
shots$shot_id <- seq_len(n)
rownames(shots) <- NULL

stopifnot(
  nrow(shots) == n,
  all(shots$x >= 37.5 & shots$x <= 60),
  all(shots$y >= 0 & shots$y <= 30),
  all(shots$xg > 0 & shots$xg < 1)
)

save(shots, file = "data/shots.rda", compress = "xz")
