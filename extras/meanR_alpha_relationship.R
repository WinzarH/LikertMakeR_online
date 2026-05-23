## Mean corr for Alpha

library(LikertMakeR)
library(dplyr)

library(ggplot2)
library(geomtextpath)

## alpha formula

# alpha <- (k * meanr) / (1 + (k - 1) * meanr)

# meanr <- alpha / (k - alpha * (k - 1))


calculate_alpha <- function(k, meanr) {
  (k * meanr) / (1 + (k - 1) * meanr)
}


items <- c(2, 4, 8, 16)
means <- seq(from = 0.00, to = 1.00, by = 0.025)





r2alpha <- NULL



for (mean in means) {
  for (item in items) {
    alpha <- calculate_alpha(item, mean)
    newline <- c(mean, item, alpha)
    r2alpha <- rbind(r2alpha, newline)
  }
}


r2alpha <- as.data.frame(r2alpha)

names(r2alpha) <- c("mean", "items", "alpha")

r2alpha$items <- as.factor(r2alpha$items)



r2alpha$items <- recode(r2alpha$items,
  "2" = "k = 2",
  "4" = "k = 4",
  "8" = "k = 8",
  "16" = "k = 16"
)



str(r2alpha)
head(r2alpha)
tail(r2alpha)


# alpha <- (k * meanr) / (1 + (k - 1) * meanr)

alphaformula <- expression(paste(
  alpha, " = ",
  frac(
    {
      italic(k) %.% bar(italic(r))
    },
    {
      1 + (italic(k) - 1) %.% bar(italic(r))
    }
  )
))

# meanr <- alpha / (k - alpha * (k - 1))

rformula <- expression(paste(
  bar(italic(r)), " = ",
  frac(
    alpha,
    {
      italic(k) - alpha * (italic(k) - 1)
    }
  )
))


ggp <- ggplot(
  r2alpha,
  aes(x = mean, y = alpha, colour = items, label = items)
) +
  geom_line(linewidth = 1.0) +
  geom_textpath(
    size = 4, vjust = -0.1,
    colour = "black", text_only = TRUE
  ) +
  labs( # title = alpha2r,
    x = "Mean Correlation", y = "Cronbach's Alpha"
  ) +
  annotate(
    geom = "text", x = 0.75, y = 0.30, label = rformula,
    color = "black", size = 6
  ) +
  # theme_classic() +
  theme_linedraw() +
  theme(legend.position = "none")

ggp
