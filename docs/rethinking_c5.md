---
output:
  html_document:
  theme: yeti
pdf_document: default
editor_options: 
  chunk_output_type: console
---

# Rethinking: Chapter 5

**Spurious waffles**



by [Richard McElreath](https://xcelab.net/rm/statistical-rethinking/), building on the Summaries by [Solomon Kurz](https://bookdown.org/content/4857/) and [Jake Thompson](https://sr2-solutions.wjakethompson.com/linear-models.html).


```r
library(sf)
library(rethinking)
library(ggfx)

data(WaffleDivorce)

WaffleDivorce <- WaffleDivorce %>% as_tibble()

usa <- read_sf("~/work/geo_store/USA/usa_states_albers_revised.gpkg") %>% 
  left_join(WaffleDivorce, by = c(name = "Location" ))

p_waffle <- usa %>% 
  ggplot(aes(fill = WaffleHouses / Population)) +
  scale_fill_gradientn(colours = c(clr0d, clr2) %>%
                         clr_lighten(.3))

p_divorce <- usa %>% 
  ggplot(aes(fill = Divorce))+
  scale_fill_gradientn(colours = c(clr0d, clr1) %>%
                         clr_lighten(.3))

p_age <- usa %>% 
  ggplot(aes(fill = MedianAgeMarriage))+
  scale_fill_gradientn(colours = c(clr_lighten(clr0d, .3), clr3))
```


```r
p_waffle +
  p_divorce +
  p_age +
  plot_layout(guides = "collect") & 
  with_shadow(geom_sf(aes(color = after_scale(clr_darken(fill)))),
              x_offset = 0, y_offset = 0, sigma = 3) & 
  guides(fill = guide_colorbar(title.position = "top",
                               barheight = unit(5,"pt"))) &
  theme(legend.position = "bottom")
```

<img src="rethinking_c5_files/figure-html/unnamed-chunk-2-1.svg" width="672" style="display: block; margin: auto;" />

Age Model: first model (divorce rate depends on age at marriage)

$$
\begin{array}{cccr} 
D_i & {\sim} & Normal(\mu, \sigma) & \textrm{[likelihood]}\\
\mu_i & = & \alpha + \beta_{A} A_{i} & \textrm{[linear model]}\\
\alpha & \sim & Normal(0, 0.2) & \textrm{[$\alpha$ prior]}\\
\beta_{A} & \sim & Normal(0, 0.5) & \textrm{[$\beta$ prior]}\\
\sigma & \sim & Exponential(1) & \textrm{[$\sigma$ prior]}
\end{array}
$$


```r
data_waffle <- WaffleDivorce %>% 
  mutate(across(.cols = c(Divorce, Marriage, MedianAgeMarriage),
                .fns = standardize,
                .names = "{str_to_lower(.col)}_std"),
         waffle_pop = WaffleHouses / Population) %>% 
  rename(median_age_std = "medianagemarriage_std") 

sd(data_waffle$MedianAgeMarriage)
```

```
#> [1] 1.24363
```

```r
model_age <- quap(
  flist = alist(
    divorce_std ~ dnorm( mu, sigma ) ,
    mu <- alpha + beta_A * median_age_std ,
    alpha ~ dnorm( 0, 0.2 ),
    beta_A ~ dnorm( 0, 0.5 ),
    sigma ~ dexp( 1 )
  ),
  data = data_waffle
)

set.seed(10)
age_priors <- extract.prior(model_age) %>% 
  as_tibble()

prior_prediction_range <- c(-2, 2)
age_prior_predictions <- link(model_age,
                        post = age_priors,
                        data = list(median_age_std = prior_prediction_range)) %>% 
  as_tibble() %>% 
  set_names(nm = as.character(prior_prediction_range)) %>% 
  mutate(.draw = row_number())

age_prior_predictions %>% 
  filter(.draw < 51) %>% 
  ggplot() +
  geom_segment(aes(x = -2, xend = 2, y = `-2`, yend = `2`, group = .draw),
               color = clr2, alpha = .2) +
  labs(x = "median age of marriage (std)", 
       y = "divorce rate (std)")
```

<img src="rethinking_c5_files/figure-html/unnamed-chunk-3-1.svg" width="672" style="display: block; margin: auto;" />


```r
age_seq <- seq(min(data_waffle$median_age_std),
               max(data_waffle$median_age_std),
               length.out = 101)
model_age_posterior_prediction_samples <- link(model_age, data = data.frame(median_age_std = age_seq)) %>% 
  as_tibble() %>% 
  set_names(nm = age_seq) %>% 
  pivot_longer(cols = everything(), names_to = "median_age_std", values_to = "divorce_std") %>% 
  mutate(median_age_std = as.numeric(median_age_std),
         MedianAgeMarriage = median_age_std * sd(data_waffle$MedianAgeMarriage) +
           mean(data_waffle$MedianAgeMarriage),
         Divorce = divorce_std * sd(data_waffle$Divorce) +
           mean(data_waffle$Divorce)) 

model_age_posterior_prediction_pi <- model_age_posterior_prediction_samples %>% 
  group_by(median_age_std, MedianAgeMarriage) %>% 
  summarise(mean = mean(Divorce),
            PI_lower = PI(Divorce)[1],
            PI_upper = PI(Divorce)[2]) %>% 
  ungroup()

model_age_posterior_prediction_simulation <- sim(model_age,
                                                 data = data.frame(median_age_std = age_seq), 
                                                 n = 5e3) %>% 
  as_tibble() %>% 
  set_names(nm = age_seq) %>% 
  pivot_longer(cols = everything(), names_to = "median_age_std", values_to = "divorce_std") %>% 
  mutate(median_age_std = as.numeric(median_age_std),
         MedianAgeMarriage = median_age_std * sd(data_waffle$MedianAgeMarriage) +
           mean(data_waffle$MedianAgeMarriage),
         Divorce = divorce_std * sd(data_waffle$Divorce) +
           mean(data_waffle$Divorce))
  
model_age_posterior_prediction_simulation_pi <- model_age_posterior_prediction_simulation %>% 
  group_by(median_age_std, MedianAgeMarriage) %>% 
  summarise(mean = mean(Divorce),
            PI_lower = PI(Divorce)[1],
            PI_upper = PI(Divorce)[2]) %>% 
  ungroup()

p_age <- ggplot(mapping = aes(x = MedianAgeMarriage)) +
  geom_ribbon(data = model_age_posterior_prediction_simulation_pi,
              aes(ymin = PI_lower, ymax = PI_upper), fill = clr0d, alpha = .35)  +
  geom_smooth(data = model_age_posterior_prediction_pi, stat = "identity",
              aes(y = mean, ymin = PI_lower, ymax = PI_upper),
              color = clr3, fill = fll3, size = .4) +
  geom_point(data = data_waffle, aes(y = Divorce), color = rgb(0,0,0,.5), size = .6) +
  labs(y = " divorce")
```

Marriage Model: alternative model (divorce rate depends on marriage rate)

$$
\begin{array}{cccr} 
D_i & {\sim} & Normal(\mu, \sigma) & \textrm{[likelihood]}\\
\mu_i & = & \alpha + \beta_{M} M_{i} & \textrm{[linear model]}\\
\alpha & \sim & Normal(0, 0.2) & \textrm{[$\alpha$ prior]}\\
\beta_{M} & \sim & Normal(0, 0.5) & \textrm{[$\beta$ prior]}\\
\sigma & \sim & Exponential(1) & \textrm{[$\sigma$ prior]}
\end{array}
$$


```r
model_mariage <- quap(
  flist = alist(
    divorce_std ~ dnorm( mu, sigma ) ,
    mu <- alpha + beta_M * marriage_std ,
    alpha ~ dnorm( 0, 0.2 ),
    beta_M ~ dnorm( 0, 0.5 ),
    sigma ~ dexp( 1 )
  ),
  data = data_waffle
)
```


```r
marriage_seq <- seq(min(data_waffle$marriage_std),
               max(data_waffle$marriage_std),
               length.out = 101)
model_marriage_posterior_prediction_samples <- link(model_mariage,
                                               data = data.frame(marriage_std = marriage_seq)) %>% 
  as_tibble() %>% 
  set_names(nm = marriage_seq) %>% 
  pivot_longer(cols = everything(), names_to = "marriage_std", values_to = "divorce_std") %>% 
  mutate(marriage_std = as.numeric(marriage_std),
         Marriage = marriage_std * sd(data_waffle$Marriage) +
           mean(data_waffle$Marriage),
         Divorce = divorce_std * sd(data_waffle$Divorce) +
           mean(data_waffle$Divorce)) 

model_marriage_posterior_prediction_pi <- model_marriage_posterior_prediction_samples %>% 
  group_by(marriage_std, Marriage) %>% 
  summarise(mean = mean(Divorce),
            PI_lower = PI(Divorce)[1],
            PI_upper = PI(Divorce)[2]) %>% 
  ungroup()

model_marriage_posterior_prediction_simulation <- sim(model_mariage,
                                                 data = data.frame(marriage_std = marriage_seq), 
                                                 n = 5e3) %>% 
  as_tibble() %>% 
  set_names(nm = marriage_seq) %>% 
  pivot_longer(cols = everything(), names_to = "marriage_std", values_to = "divorce_std") %>% 
  mutate(marriage_std = as.numeric(marriage_std),
         Marriage = marriage_std * sd(data_waffle$Marriage) +
           mean(data_waffle$Marriage),
         Divorce = divorce_std * sd(data_waffle$Divorce) +
           mean(data_waffle$Divorce))
  
model_marriage_posterior_prediction_simulation_pi <- model_marriage_posterior_prediction_simulation %>% 
  group_by(marriage_std, Marriage) %>% 
  summarise(mean = mean(Divorce),
            PI_lower = PI(Divorce)[1],
            PI_upper = PI(Divorce)[2]) %>% 
  ungroup()

p_marriage <- ggplot(mapping = aes(x = Marriage)) +
  geom_ribbon(data = model_marriage_posterior_prediction_simulation_pi,
              aes(ymin = PI_lower, ymax = PI_upper), fill = clr0d, alpha = .35)  +
  geom_smooth(data = model_marriage_posterior_prediction_pi, stat = "identity",
              aes(y = mean, ymin = PI_lower, ymax = PI_upper),
              color = clr1, fill = fll1, size = .2) +
  geom_point(data = data_waffle, aes(y = Divorce), color = rgb(0,0,0,.5), size = .6) +
  labs(y = " divorce")
```

Waffle Model:


```r
model_waffle <- quap(
  flist = alist(
    divorce_std ~ dnorm( mu, sigma ) ,
    mu <- alpha + beta_W * waffle_pop ,
    alpha ~ dnorm( 0, 0.2 ),
    beta_W ~ dnorm( 0, 0.5 ),
    sigma ~ dexp( 1 )
  ),
  data = data_waffle
)

waffle_seq <- seq(min(data_waffle$waffle_pop),
               max(data_waffle$waffle_pop),
               length.out = 101)
model_waffle_posterior_prediction_samples <- link(model_waffle,
                                               data = data.frame(waffle_pop = waffle_seq)) %>% 
  as_tibble() %>% 
  set_names(nm = waffle_seq) %>% 
  pivot_longer(cols = everything(), names_to = "waffle_pop", values_to = "divorce_std") %>% 
  mutate(waffle_pop = as.numeric(waffle_pop),
         Divorce = divorce_std * sd(data_waffle$Divorce) +
           mean(data_waffle$Divorce)) 

model_waffle_posterior_prediction_pi <- model_waffle_posterior_prediction_samples %>% 
  group_by(waffle_pop) %>% 
  summarise(mean = mean(Divorce),
            PI_lower = PI(Divorce)[1],
            PI_upper = PI(Divorce)[2]) %>% 
  ungroup()

p_waffle <- ggplot(mapping = aes(x = waffle_pop)) +
  geom_smooth(data = model_waffle_posterior_prediction_pi, stat = "identity",
              aes(y = mean, ymin = PI_lower, ymax = PI_upper),
              color = clr2, fill = fll2, size = .2) +
  geom_point(data = data_waffle, aes(y = Divorce), color = rgb(0,0,0,.5), size = .6) +
  labs(y = " divorce")
```


```r
p_waffle + 
p_marriage + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  p_age + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) &
  lims(y = c(4, 15))
```

<img src="rethinking_c5_files/figure-html/unnamed-chunk-8-1.svg" width="672" style="display: block; margin: auto;" />

## Directed Acyclic Graphs


```r
dag1 <- dagify(
  D ~ A + M,
  M ~ A,
  exposure = "A",
  outcome = "M") %>% 
  tidy_dagitty(.dagitty = .,layout = tibble(x = c(0,1,.5), y = c(1,1, .4))) %>%
  mutate(stage = if_else(name == "D", "response",
                         if_else(name %in% c("A", "M"),
                                 "predictor", "confounds")))

dag2 <- dagify(
  D ~ A,
  M ~ A,
  exposure = "A",
  outcome = "M") %>% 
  tidy_dagitty(.dagitty = .,layout = tibble(x = c(0,.5,1), y = c(1, .4, 1))) %>%
  mutate(stage = if_else(name == "D", "response",
                         if_else(name %in% c("A", "M"),
                                 "predictor", "confounds")))

plot_dag(dag1, clr_in = clr3) + 
  plot_dag(dag2, clr_in = clr3) &
  scale_y_continuous(limits = c(.35, 1.05)) &
  coord_equal()
```

<img src="rethinking_c5_files/figure-html/unnamed-chunk-9-1.svg" width="672" style="display: block; margin: auto;" />

DAG notation:

- $Y \perp \!\!\! \perp X | Z$: *"$Y$ is independent of $X$ conditional on $Z$"*
- $D \not\!\perp\!\!\!\perp A$: "*$D$ is associated with $A$"*

Check pair wise correlations with `cor()`:


```r
data_waffle %>% 
  dplyr::select(divorce_std,marriage_std, median_age_std) %>% 
  cor() %>% 
  as.data.frame(row.names = row.names(.)) %>% 
  round(digits = 2) %>% 
  knitr::kable()
```



|               | divorce_std| marriage_std| median_age_std|
|:--------------|-----------:|------------:|--------------:|
|divorce_std    |        1.00|         0.37|          -0.60|
|marriage_std   |        0.37|         1.00|          -0.72|
|median_age_std |       -0.60|        -0.72|           1.00|


```r
library(dagitty)

dagitty('dag{ D <- A -> M -> D}') %>% 
  impliedConditionalIndependencies()

dagitty('dag{ D <- A -> M }') %>% 
  impliedConditionalIndependencies()
```

```
#> D _||_ M | A
```

## Multiple Regression notion

$$
\begin{array}{cccr} 
D_i & {\sim} & Normal(\mu, \sigma) & \textrm{[likelihood]}\\
\mu_i & = & \alpha + \beta_{M} M_{i} + \beta_{A} A_{i} & \textrm{[linear model]}\\
\alpha & \sim & Normal(0, 0.2) & \textrm{[$\alpha$ prior]}\\
\beta_{M} & \sim & Normal(0, 0.5) & \textrm{[$\beta_M$ prior]}\\
\beta_{A} & \sim & Normal(0, 0.5) & \textrm{[$\beta_A$ prior]}\\
\sigma & \sim & Exponential(1) & \textrm{[$\sigma$ prior]}
\end{array}
$$

or compact notion 

$$
\mu_i = \alpha + \sum_{j = 1}^{n} \beta_jx_{ji}
$$

or even *matrix* notion

$$
m = Xb
$$
## Homework

**E1**

**E2**

**E3**

**E4**

**E5**

**E6**

**E7**

**M1**

**M2**

**M3**

**M4**

**M5**

**M6**

**H1**

**H2**

**H3**

**H4**

**H5**


## {brms} section

---

<div id="myModal" class="modal">
  <span class="close">&times;</span>
  <img class="modal-content" id="img01">
  <div id="caption"></div>
</div>

<script src="./js/zoom.js"></script>
