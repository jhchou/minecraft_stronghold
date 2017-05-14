library(tidyverse)
library(plotly)
library(corpcor) # for pseudo-inverse

#
# Example data from: http://www.purplefrog.com/~thoth/MinecraftStronghold/stronghold.html
# - https://github.com/mutantbob/minecraft-stronghold-triangulator
#
df <- tribble( # data frame of origin points and Minecraft 'heading'
  ~x, ~z, ~heading,
  # -413, 12, 88.88,
  # -427, 249, 108.687,
  # -546, 292, 116.48,
  # -923, 274, 150.38

  0, 1000, -146,
  500, 0, 18.98,
  500, 500, 71.78,
  400, 700, 142
  
  # 500, -500, 96,
  # 300, -600, -34.4,
  # 300, -300, -165.2
)

df <- df %>%
  mutate(
    radians = pi * heading / 180.0,
    unit_x = -sin(radians),
    unit_z = cos(radians)
  )

# Determine the 'best' point for the intersection of the lines,
# by minimizing the perpendicular distances of the point to the lines
#
# From:
# - "Least-Squares Intersection of Lines, by Johannes Traa - UIUC 2013"
# - http://cal.cs.illinois.edu/~johannes/research/
# - http://cal.cs.illinois.edu/~johannes/research/LS_line_intersect.pdf
#
# Also:
# - https://math.stackexchange.com/questions/61719/finding-the-intersection-point-of-many-lines-in-3d-point-closest-to-all-lines

k <- nrow(df) # number of lines
dimension <- 2
a <- df[, 1:2] %>% as.matrix() %>% t() # *columns* of origin points
n <- df[, 5:6] %>% as.matrix() %>% t() # *columns* of the points' unit direction vectors
R = matrix(data = 0, nrow = dimension, ncol = dimension) # initialize an empty matrix
q = vector(mode = 'numeric', length = dimension) # initialize an empty vector
# Generating a system of linear equations, with Rp = q, where p will be the 'best' point
for (i in 1:k) {
  R <- R + (diag(dimension) - n[, i] %*% t(n[, i]))
  q <- q + (diag(dimension) - n[, i] %*% t(n[, i])) %*% a[, i]
}
# So p_hat = pseudoinverse(R) x q
p_hat <- pseudoinverse(R) %*% q # column vector of the least squares fit best point

# Turn solution into proper data frame to plot optimal point
df_p <- t(p_hat) %>% data.frame # need to convert it to a row vector for data frame
names(df_p) <- c('x', 'z') # and match the column names to plot


# Generate line segment end points for plotting
df <- df %>% # generate line segment end points that are 1.3 * the distance from the origin point to the optimal point
  mutate(
    # p_x = p_hat[1],
    # p_z = p_hat[2],
    # distance = sqrt((p_x - x) ^2 + (p_z - z) ^2),
    distance = 1000, # or, just set each line segment length to a constant
    x2 = x - 1.3 * distance * sin(radians),
    z2 = z + 1.3 * distance * cos(radians)
  )

# Plot the origin points + rays and the optimal intersection point
# - ggplotly would allow hovering over the intersection point to see the coordinates
# - but, arrowhead not supported by ggplotly
g <- df %>% ggplot(aes(x, z)) +
  geom_segment(aes(xend = x2, yend = z2), alpha = 0.3, arrow = arrow(length = unit(0.25, "cm"))) +
  geom_point(color = 'blue') +
  scale_y_reverse() + # because of the inverted 'z' axis of positive to North and negative to South
  geom_point(data = df_p, color = 'red') +
  theme_bw()
# g
ggplotly(g)






