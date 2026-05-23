## corr matrix visualisation

library(ggplot2)
library(GGally)
df <- read.csv("extras/synthetic-data-2024-03-10.csv")

cor(df) |> round(2)

bins <- 5

p2 <- ggpairs(
  df,
  upper = list(continuous = wrap("cor", stars = FALSE, digits = 2, title = "r", size = 5)),
  diag = list(continuous = wrap("barDiag",
    bins = bins,
    fill = "steelblue", colour = "grey"
  )),
  # lower = list(continuous = wrap("points", shape = 21, fill = "blue", alpha = 0.1))
  lower = list(continuous = wrap("smooth", shape = 21, fill = "steelblue", colour = "steelblue", alpha = 0.1, se = FALSE))
) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "grey"))

p2




# library(ggplot2)
library(psych)

p2 <- pairs.panels(df,
  lm = TRUE,
  ellipses = FALSE,
  rug = FALSE,
  cex.cor = 0.75,
  # hist.col="#999999",
  pch = 21, col = "#0000ff",
  jiggle = TRUE, factor = 1
  # main = "Scatter Plot Matrix for df"
)


# ggsave("scatterPlotMatrix.png", p2, width = 7, height = 7, units = "in")


h <- hist(df$V1)
str(df)
df[] <- lapply(df, as.numeric)
