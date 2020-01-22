# Hastie, Tibshirani and Friedman's
# example in section 2.3.1
library(plotly)
library(reshape2)
library(mda)
data("ESL.mixture")

data =
  data.frame(
    y = ESL.mixture$y,
    x1 = ESL.mixture$x[,1],
    x2 = ESL.mixture$x[,2],
    g = as.factor(ESL.mixture$y)
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

foo_rule = matrix(0.5, 10,10)

plot_ly(
  data,
  x = ~x1,
  y = ~x2,
  z = ~y,
  color = ~g,
  colors = c(rgb(0,.2,1,.5), rgb(1,.2,0,.5))
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
    ) %>%
  add_trace(
    z = foo_rule,
    x = axis_x1,
    y = axis_x2,
    type = "surface",
    colors='black',
    opacity = 0.4
    )
