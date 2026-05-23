### Function to generate the pairs plot
##
generatePairsPlot <- function(data) {
  ##
  ## Upper triangle for correlation coefficients
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = "complete.obs")
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
    text(0.5, 0.5, txt, cex = 1.0 + abs(r))
  }
  ## Diagonal for histograms
  panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, breaks = "scott", plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y / max(y)
    rect(
      xleft = breaks[-nB], ybottom = 0, xright = breaks[-1], ytop = y,
      col = "#4682B4"
    )
  }
  ## Lower triangle for scatterplots with regression lines
  panel.lm <- function(x, y, col = "#4682B410", bg = NA,
                       pch = 19, cex = 1, col.smooth = "#884936", ...) {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    abline(stats::lm(y ~ x), col = col.smooth, ...)
  }
  ## plotting the visualisation
  pairs(data,
    upper.panel = panel.cor,
    diag.panel = panel.hist,
    lower.panel = panel.lm,
    gap = 0.25
  )
}

data <- read.csv("extras/synthetic-data-2024-03-10.csv")

generatePairsPlot(data)

data <- as.numeric(data)
str(data)

data$v1 |>
  as.numeric() |>
  hist()

hist(data$v1)

?hist()
