library(tidyverse)
library(prismatic)
library(patchwork)
library(glue)
library(tidygraph)
library(ggraph)
library(EnvStats)
library(brms)
library(ggtext)
library(GGally)
library(ggdag)

clr0 <- "#E9E5D9" # "#A9A9A9"
clr0d <- clr_darken(clr0, .1)
clr1 <- "#54436D" # "#083F82"
clr2 <- "#B35136" #"#FF8029"
clr1l <- clr_lighten(clr1, .3)
clrd <- clr1l
clr3 <- "#81ACA4"

clr_dag <- rgb(.8,.8,.8,.7)

fll0 <- clr_alpha(clr0)
fll1 <- clr_alpha(clr1)
fll2 <- clr_alpha(clr2)
fll3 <- clr_alpha(clr3)

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


plot_dag <- function(dag, clr_in = clr1){
  dag %>% 
    ggplot(aes(x = x,  y = y, xend = xend, yend = yend)) +
    geom_dag_edges_link(aes(edge_color = stage, edge_linetype = stage)) +
    geom_dag_node(aes(color = stage, fill = after_scale(clr_lighten(color,.7))),
                  shape = 21, size = 12) +
    geom_dag_text(aes(label = name), color = "black", size = 4)+
    scale_color_manual(values = c(predictor = "black",
                                  confounds = clr_dag,
                                  response = clr_in),
                       guide = "none") +
    scale_edge_linetype_manual(values = c(confounds = 3,
                                          predictor = 1), guide = "none") +
    scale_edge_color_manual(values = c(confounds = "gray70",
                                       predictor = "black"), guide = "none") +
    theme_dag()
}
