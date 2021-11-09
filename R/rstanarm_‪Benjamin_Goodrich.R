library(tidyverse)
library(rstanarm)
options(mc.cores = parallel::detectCores() - 1)

em_code <- c(`n/a` = 0,
             `< 1 year` = .5,
             `1 year` = 1,
             `2 years` = 2,
             `3 years` = 3,
             `4 years` = 4,
             `5 years` = 5,
             `6 years` = 6,
             `7 years` = 7,
             `8 years` = 8,
             `9 years` = 9,
             `10+ years` = 10)

data <- read_csv("data/loan.csv") %>% 
  dplyr::select(loan_amnt, annual_inc, loan_status, term, home_ownership, emp_length, installment ) %>% 
  mutate(emp_length = em_code[emp_length],
         term = factor(term),
         home_ownership = factor(home_ownership),
         y = c("NoProblem", "Problem")[(loan_status == "Charged Off") +1],
         y_num = as.numeric(loan_status == "Charged Off")) %>% 
  filter(row_number() < 10001) 

data %>% 
  group_by(y) %>%
  count()

summary(data[, c("loan_amnt", "annual_inc")])

y_tilde <- with(data, rbinom(nrow(data), size = 1,
                            prob = if_else(emp_length == 0, .5, .1)))

table(y_tilde)

post_sim <- stan_glm(y_tilde ~ I(data$emp_length != 0 ),
                     family = binomial(link = "logit"),
                     prior = normal(),
                     prior_intercept = normal())

post <- stan_glm(y_num ~ log(loan_amnt) + term + home_ownership + log(annual_inc) + I(emp_length != 0),
                 data = data,
                 family = binomial(link = "logit"),
                 prior = normal(),
                 prior_intercept = normal(),
                 QR = TRUE)

include <- c("log(loan_amnt)", "term60 months", "home_ownershipOWN", "home_ownershipRENT", "log(annual_inc)", "I(emp_length != 0)TRUE")

print(post)
plot(post, prob = .8, pars = include) +
  theme_bw()

# library(shinystan)
# launch_shinystan(post)

posterior_vs_prior(post, prob = .8, regex_pars = "^[h]") +
  scale_color_viridis_d()

ppd <- posterior_predict(post, 
                         newdata =  data, draws = 500)

profit <- sapply(1:ncol(ppd),
                 FUN = function(i){
                   ifelse(ppd[,i] == 1, - data$loan_amnt[i], data$installment[i] *
                            ifelse(data$term[i] == "36 months", 36, 60) -
                            data$loan_amnt[i])
                 })

hist(rowSums(profit) / sum(data$loan_amnt),
     prob = TRUE,
     main = "testing dataset",
     xlab = "Profit Margin")

# model comparison
post2 <-  update(post, formula. = . ~ . - I(emp_length != 0))
loo(post)
loo(post2)
