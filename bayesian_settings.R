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
clr_dark <- rgb(.2,.2,.2,.5)
clr_theme <- rgb(0,0,0,.1)
fll_theme <- clr_alpha(clr_theme, .03)

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
    geom_dag_text(aes(label = name), color = "black", size = 4, parse = TRUE,
                  family = fnt_sel, fontface = "bold")+
    scale_color_manual(values = c(predictor = "black",
                                  confounds = clr_dag,
                                  response = clr_in),
                       guide = "none") +
    scale_edge_linetype_manual(values = c(confounds = 3,
                                          predictor = 1,
                                          response = 1), guide = "none") +
    scale_edge_color_manual(values = c(confounds = "gray70",
                                       predictor = "black",
                                       response = "black"), guide = "none") +
    theme_dag()
}

plot_coeftab <- function(ct, prob = .95){
  ct@coefs %>%
  as_tibble() %>%
  mutate(param = row.names(ct@coefs)) %>% 
  separate(param, into = c("parameter", "type"),
           sep = "\\.") %>%
  mutate(type = if_else(is.na(type), "est", "se")) %>% 
  pivot_longer(cols = starts_with("model"), 
               names_to = "model") %>% 
  pivot_wider(values_from = value, names_from = type) %>% 
  mutate(model = str_remove(model, "model_")) %>% 
  filter(grepl("beta", parameter))  %>% 
  ggplot(aes(y = model, color = model)) +
  geom_vline(xintercept = 0, lty = 3, color = rgb(0,0,0,.6)) +
  geom_segment(aes(yend = model,
                   x = est - qnorm(1 - (1 - prob)/2) * se,
                   xend = est + qnorm(1 - (1 - prob)/2) * se)) +
  geom_point(aes(x = est, fill = after_scale(clr_lighten(color))),
             shape = 21, size = 3) +
  facet_wrap(parameter ~ .#, scales = "free_x"
             ) +
  scale_color_manual(values = c(clr3, clr1, clr2),
                     guide = "none") +
  labs(y = NULL, x = "estimate") +
  theme(panel.background = element_rect(color = clr_theme, size = .5,
                                        fill = "transparent"),
        strip.background = element_rect(color = clr_theme, size = 0,
                                        fill = fll_theme))
  }

as_tibble_rn <- function(x, nm = "param"){ as_tibble(x) %>% mutate(nm = row.names(x)) %>% set_names(nm = c(names(x), nm))}


knit_precis <- function(prec){
  prec %>% 
    as_tibble_rn() %>% 
    mutate(across(where(is.double), round, digits = 2)) %>%
    dplyr::select(param, everything()) %>% 
    knitr::kable()
}
