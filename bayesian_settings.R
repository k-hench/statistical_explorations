library(tidyverse)
library(prismatic)
library(patchwork)
library(glue)
library(tidygraph)
library(ggraph)
library(EnvStats)
library(brms)
library(ggtext)

clr0 <- "#E9E5D9" # "#A9A9A9"
clr0d <- clr_darken(clr0, .1)
clr1 <- "#54436D" # "#083F82"
clr2 <- "#B35136" #"#FF8029"
clr1l <- clr_lighten(clr1, .3)
clrd <- clr1l

fll0 <- clr_alpha(clr0)
fll1 <- clr_alpha(clr1)
fll2 <- clr_alpha(clr2)

clr_grd1 <- c(clr0d, clr2, clr1, "black")
clr_grd2 <- c(clr1, clr0, clr2)
clr_grd3 <- c(clr1, clr_lighten(clr1,.2), clr_darken(clr0, .35), clr_lighten(clr2,.2), clr2)
clr_grd4 <- c(clr0d, clr2, clr_darken(clr2, .6))
clr_grd5 <- c(clr0d, clr1, "black")

clr_bayes <- c(prior = clr0d, likelihood = clr1, posterior = clr2)

fnt_sel <- "Josefin Sans"
theme_set(theme_minimal(base_family = fnt_sel) +
            theme(plot.title = element_text(hjust = .5)))

mth <- function(str){
  str_c("<span style = 'font-family:Helvetica;'>", str, "</span>")
}

buffer_range <- function(x, buffer = .1){range(x) + c(-buffer, buffer) * diff(range(x))}
