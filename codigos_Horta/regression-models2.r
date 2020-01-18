library(plotly)
library(reshape2)

beta0 = -5
beta1 = 5
beta2 = -1

n = 50

set.seed(4)
epsilon = rnorm(n, sd = .7)
X = runif(n, min = 0, max = 5)

Y = beta0 + beta1*X + beta2*X^2 + epsilon

f = function(x) beta0+beta1*x+beta2*x^2

plot(f, xlim = c(0,5), ylim = range(Y))
points(X, Y, pch = 21, col=rgb(0,.4,.8,.6), bg=rgb(0,.4,.8,.3), cex = .8)


# 3D plot
data =
  data.frame(
    y = Y,
    x1 = X,
    x2 = X^2
    )

foo_lm =
  lm(
    y ~ x1 + x2,
    data = data
    )

axis_x1 =
  seq(
    from=min(data$x1),
    to=max(data$x1),
    length=10
    )

axis_x2 =
  seq(
    from=min(data$x2),
    to=max(data$x2),
    length=10
    )

foo_lm_surface =
  expand.grid(
    x1 = axis_x1,
    x2 = axis_x2,
    KEEP.OUT.ATTRS = F)

foo_lm_surface$y =
  predict.lm(
    foo_lm,
    newdata = foo_lm_surface
    )

foo_lm_surface =
  acast(
    foo_lm_surface,
    x2 ~ x1,
    value.var = "y"
    )

plot_ly(
  data,
  x = ~x1,
  y = ~x2,
  z = ~y
  ) %>%
  add_markers(size=1) %>%
  layout(
    scene = list(
      xaxis = list(title = 'x1'),
      yaxis = list(title = 'x2'),
      zaxis = list(title = 'y')
      )
    ) %>%
  add_trace(
    z = foo_lm_surface,
    x = axis_x1,
    y = axis_x2,
    type = "surface",
    opacity = 0.4
    )
