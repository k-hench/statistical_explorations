library(tidyverse)
library(prismatic)
library(patchwork)
library(glue)
library(tidygraph)
library(ggraph)
library(EnvStats)

clr0 <- "#E9E5D9" # "#A9A9A9"
clr0d <- clr_darken(clr0, .1)
clr1 <- "#54436D" # "#083F82"
clr2 <- "#B35136" #"#FF8029"
clr1l <- clr_lighten(clr1, .3)
clrd <- clr1l

fll0 <- clr_alpha(clr0)
fll1 <- clr_alpha(clr1)
fll2 <- clr_alpha(clr2)

fnt_sel <- "Josefin Sans"
theme_set(theme_minimal(base_family = fnt_sel))