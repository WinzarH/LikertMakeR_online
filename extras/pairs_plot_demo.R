## pairsplot demo


### Generate some toy data
##

n <- 512
set.seed(42)
x1 <- sample(c(1:5), size = n, replace = TRUE)
x2 <- sample(c(1:7), size = n, replace = TRUE)
x3 <- sample(c(-4:4), size = n, replace = TRUE)

data <- data.frame(x1, x2, x3)


### Function to generate the pairs plot
##
generatePairsPlot <- function(data) {
  ##
  ## Upper triangle for correlation coefficients
  panel.cor <- function(x, y, prefix = "", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = "complete.obs")
    txt <- format(c(r, 0.123456789), digits = 2)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
    text(0.5, 0.5, txt, cex = 1.0 + abs(r))
  }
  ## Diagonal for histograms
  panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr))
    par(usr = c(usr[1:2], 0, 1.5))
    # h <- barplot(table(x), plot = FALSE)
    # print(h)
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    # breaks <- (min(h$breaks) - 1):(max(h$breaks))
    # axis(side=1,(min(h$breaks) - 1):(max(h$breaks)))
    nB <- length(breaks)
    y <- h$counts
    y <- y / max(y)
    rect(
      xleft = breaks[-nB], ybottom = 0, xright = breaks[-1], ytop = y,
      col = "#4682B411"
    )
  }
  ## Lower triangle for scatterplots with regression lines
  panel.lm <- function(x, y) {
    points(x, y, pch = 19, col = "#4682B410")
    abline(stats::lm(y ~ x))
  }
  ## plotting the visualisation
  pairs(data,
    upper.panel = panel.cor,
    diag.panel = panel.hist,
    lower.panel = panel.lm,
    gap = 0.25
  )
}



generatePairsPlot(data)


n <- 128
set.seed(42)
x1 <- sample(c(1:5), size = n, replace = TRUE)
x2 <- sample(c(1:7), size = n, replace = TRUE)
x3 <- sample(c(1:9), size = n, replace = TRUE)
hist(x1, breaks = 0:5)
hist(x2, breaks = 0:7)
hist(x3, breaks = 0:9)
## break options: Sturges, Scott, FD


brmin <- sapply(data, min) - 1
brmax <- sapply(data, max)
br <- cbind(brmin, brmax)
br

library(ggplot2)
library(GGally)

ggpairs(data,
  upper = list(continuous = wrap("cor", size = 5)),
  diag = list(continuous = wrap("barDiag", fill = "#4682B4")),
  lower = list(continuous = wrap("smooth", colour = "#4682B411", alpha = 0.05))
) +
  theme_classic()


library(LikertMakeR)
myMat <- makeCorrAlpha(3, 0.85)

n <- 128
dfMeans <- c(3.0, 4.0, 0.5)
dfSds <- c(1.0, 1.75, 1.5)
lowerbound <- c(1, 1, -3)
upperbound <- c(5, 7, 3)

data <- makeItems(
  n = n, means = dfMeans, sds = dfSds,
  lowerbound = lowerbound, upperbound = upperbound,
  cormatrix = myMat
)


# Function to return points and geom_smooth
# allow for the method to be changed
my_fn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "#4682B411") +
    geom_smooth(method = method, se = F, ...)
  p
}

ggpairs(data,
  upper = list(continuous = wrap("cor", size = 5, stars = FALSE)),
  # upper = list(continuous = cor_fun),
  diag = list(continuous = wrap("barDiag", fill = "#4682B4")),
  # lower = list(continuous = wrap("smooth", colour = "#4682B411"))
  lower = list(continuous = my_fn)
) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "lightgrey", size = 1))

library(factoextra)
library(FactoMineR)

res.pca <- PCA(data, graph = TRUE)
res.pca$eig
# show scree plot of PCA
fviz_screeplot(res.pca, addlabels = TRUE)


fviz_pca_var(res.pca, axes = c(1, 2), repel = TRUE, col.var = "blue", col.circle = "#0033ff22", title = "Principal Components Analysis of Generated Variables")

cor(data) |> round(2)


n <- 64
dfMeans <- c(3.0, 4.0, 0.5, 3.5)
dfSds <- c(1.0, 1.75, 1.5, 1.25)
lowerbound <- c(1, 1, -3, 1)
upperbound <- c(5, 7, 3, 5)

myMat <- makeCorrAlpha(4, 0.85, variance = 0.75)

data <- makeItems(
  n = n, means = dfMeans, sds = dfSds,
  lowerbound = lowerbound, upperbound = upperbound,
  cormatrix = myMat
)


library(psych)
pairs.panels(data) # see the graphics window
pairs.panels(data,
  lm = TRUE,
  cex.cor = 0.65,
  ellipses = FALSE,
  density = FALSE,
  smoother = TRUE
)
