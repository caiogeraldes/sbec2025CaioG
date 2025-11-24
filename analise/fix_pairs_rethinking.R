# Fixing errors in rethinking
setMethod("pairs", "ulam", function(x, n = 200, alpha = 0.7, cex = 0.7, pch = 16, adj = 1, pars, ...) { # nolint
  if (missing(pars))
    posterior <- extract.samples(x)
  else
    posterior <- extract.samples(x, pars = pars)
  if (!missing(pars)) {
    # select out named parameters
    p <- list()
    for (k in pars) p[[k]] <- posterior[[k]]
    posterior <- p
  }
  panel.dens <- function(x, ...) { # nolint
    usr <- par("usr"); on.exit(par(usr)) # nolint
    par(usr = c(usr[1:2], 0, 1.5))
    h <- density(x, adj = adj)
    y <- h$y
    y <- y / max(y)
    abline(v = 0, col = "gray", lwd = 0.5)
    lines(h$x, y)
  }
  panel.2d <- function(x, y, ...) { # nolint
    i <- sample(1:length(x), size = n) # nolint
    abline(v = 0, col = "gray", lwd = 0.5)
    abline(h = 0, col = "gray", lwd = 0.5)
    dcols <- densCols(x[i], y[i])
    dcols <- sapply(dcols, function(k) col.alpha(k, alpha))
    points(x[i], y[i], col = dcols, ...)
  }
  panel.cor <- function(x, y, ...) { # nolint
    k <- cor(x, y)
    cx <- sum(range(x)) / 2
    cy <- sum(range(y)) / 2
    text(cx, cy, round(k, 2), cex = 2 * exp(abs(k)) / exp(1))
  }
  pairs(posterior, cex = cex, pch = pch, upper.panel = panel.2d, lower.panel = panel.cor, diag.panel = panel.dens, ...) # nolint
})
