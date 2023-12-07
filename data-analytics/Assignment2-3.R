library(plotly)

model <- function(gpa, iq, level) {
  50 +
    gpa * 20 +
    iq * 0.07 +
    level * 35 +
    gpa * iq * 0.01 +
    gpa * level * -10
}
x <- seq(1, 5, length = 10)
y <- seq(1, 200, length = 20)
college <- t(outer(x, y, model, level = 1))
high_school <- t(outer(x, y, model, level = 0))

plot_ly(x = x, y = y) |>
  add_surface(
    z = ~college,
    colorscale = list(c(0, 1), c("rgb(107,184,214)", "rgb(0,90,124)")),
    colorbar = list(title = "College")) |>
  add_surface(
    z = ~high_school,
    colorscale = list(c(0, 1), c("rgb(255,112,184)", "rgb(128,0,64)")),
    colorbar = list(title = "High school")) |>
  layout(scene = list(
    xaxis = list(title = "GPA"),
    yaxis = list(title = "IQ"),
    zaxis = list(title = "Salary")))

#b
model(gpa = 4, iq = 110, level = 1)

