## -----------------------------------------------------------------------------
## Code for "A Quick Introduction to tidymodels" by Max Kuhn

## -----------------------------------------------------------------------------
## Part 2
## ----knitr, include = FALSE-----------------------------------------------------------------

library(tidymodels)

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)


## -----------------------------------------------------------------------------

# rsample and modeldata are loaded with tidymodels package
data(ames, package = "modeldata")
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

nrow(ames_train)/nrow(ames)


## -------------------------------------------------------------------------------------------
# result of initial_split()
# <training / testing / total>
data_split

## -----------------------------------------------------------------------------
training(data_split)

## -----------------------------------------------------------------------------

simple_lm <- lm(Sale_Price ~ Longitude + Latitude, data = ames_train)

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

head(simple_lm_values, n = 2)

tidy(simple_lm)

# But don't trust this too much!
glance(simple_lm)[1:3]


## -------------------------------------------------------------------------------------------
spec_lin_reg <- linear_reg()
spec_lin_reg

lm_mod <- set_engine(spec_lin_reg, "lm")
lm_mod

lm_fit <- fit(
  lm_mod,
  Sale_Price ~ Longitude + Latitude,
  data = ames_train
)

lm_fit

fit_xy(
  lm_mod,
  y = ames_train %>% dplyr::pull(Sale_Price),
  x = ames_train %>% dplyr::select(Latitude, Longitude)
)


## -----------------------------------------------------------------------------

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  Sale_Price ~ Longitude + Latitude,
  data = ames_train
)

coef(fit_stan$fit)

coef(lm_fit$fit)


## -------------------------------------------------------------------------------------------
fit_knn <- 
  nearest_neighbor(mode = "regression", neighbors = 5) %>%
  set_engine("kknn") %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

fit_knn

## -----------------------------------------------------------------------------

# Numeric predictions always in a df
# with column `.pred`
test_pred <- 
  lm_fit %>%
  predict(ames_test) %>%
  bind_cols(ames_test) 

test_pred %>% 
  dplyr::select(Sale_Price, .pred) %>% 
  slice(1:3)


# yardstick loaded by tidymodels

perf_metrics <- metric_set(rmse, rsq, ccc)

# A tidy result back:
test_pred  %>% 
  perf_metrics(truth = Sale_Price, estimate = .pred)

## -----------------------------------------------------------------------------
## Part 3

ggplot(ames_train, aes(x = Neighborhood)) + geom_bar() + coord_flip() + xlab("")

## -----------------------------------------------------------------------------

# recipes loaded by tidymodels
mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, ames_train) 

mod_rec <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

mod_rec

mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE)
mod_rec_trained

# Extracts processed version of `ames_train`
juice(mod_rec_trained)

bake(mod_rec_trained, new_data = ames_test)

## -----------------------------------------------------------------------------

ames_train %>%
  ggplot(aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess")

ames_train %>%
  group_by(Central_Air) %>%
  count() %>%
  ungroup() %>% 
  mutate(percent = n / sum(n) * 100)

library(MASS)

ames_train %>%
  ggplot(aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm") 


mod1 <- lm(Sale_Price ~ Year_Built + Central_Air,                          data = ames_train)
mod2 <- lm(Sale_Price ~ Year_Built + Central_Air + Year_Built:Central_Air, data = ames_train)
anova(mod1, mod2)

interact_rec <- recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built)

interact_rec %>%
  prep(training = ames_train) %>%
  juice() %>%
  # select a few rows with different values
  slice(153:157)


## -----------------------------------------------------------------------------

data(bivariate)

ggplot(bivariate_test, aes(x = A, y = B, color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() 

bivariate_rec <- recipe(Class ~ A + B, data = bivariate_train) %>%
  step_BoxCox(all_predictors())

bivariate_rec <- prep(bivariate_rec, training = bivariate_train, verbose = FALSE)

inverse_test <- bake(bivariate_rec, new_data = bivariate_test, everything())

ggplot(inverse_test, aes(x = 1/A, y = 1/B, color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlab("1/A") + ylab("1/B") 

## -----------------------------------------------------------------------------

bivariate_pca <- 
  recipe(Class ~ A + B, data = bivariate_train) %>%
  step_BoxCox(all_predictors()) %>%
  step_normalize(all_predictors()) %>% # center and scale
  step_pca(all_predictors()) %>%
  prep(training = bivariate_train)

pca_test <- bake(bivariate_pca, new_data = bivariate_test)

# Put components axes on the same range
pca_rng <- extendrange(c(pca_test$PC1, pca_test$PC2))

pca_test %>%
  ggplot(aes(x = PC1, y = PC2, color = Class)) +
  geom_point(alpha = .2, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlim(pca_rng) + ylim(pca_rng) + 
  xlab("Principal Component 1") + 
  ylab("Principal Component 2")

## -----------------------------------------------------------------------------

ggplot(ames_train, aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  )


ggplot(ames_train, aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::ns(x, df = 5), 
    se = FALSE
  )

## -----------------------------------------------------------------------------

ames_rec <- recipe(
  Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
    Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
    Central_Air + Longitude + Latitude,
  data = ames_train
) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_ns(Longitude, Latitude, deg_free = 5)

ames_rec <- prep(ames_rec)

lm_fit <- 
  lm_mod %>% 
  # Sale_Price is already on the log scale:
  fit(Sale_Price ~ ., data = juice(ames_rec))   

glance(lm_fit$fit)


ames_test_processed <- bake(ames_rec, ames_test, all_predictors())

# but let's not do this
# predict(lm_fit, new_data = ames_test_processed)

## -----------------------------------------------------------------------------

ames_wfl <- workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(lm_mod)

ames_wfl

## ames_wfl_fit <- fit(ames_wfl, ames_train)

## predict(ames_wfl_fit, ames_test) %>% slice(1:5)

## -----------------------------------------------------------------------------
## Part 4

set.seed(2453)
cv_splits <- vfold_cv(ames_train) #10-fold is default
cv_splits

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% 
  analysis() %>%
  dim()

cv_splits$splits[[1]] %>% 
  assessment() %>%
  dim()

## -----------------------------------------------------------------------------

knn_mod <- 
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

knn_wfl <- 
  workflow() %>% 
  add_model(knn_mod) %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

fit(knn_wfl, data = ames_train)

## -----------------------------------------------------------------------------

knn_res <- cv_splits %>%
  mutate(
    workflows = map(
      splits, 
      ~ fit(knn_wfl, data = analysis(.x))
    )
  ) 

knn_res

## -----------------------------------------------------------------------------

knn_pred <- map2_dfr(
  knn_res$workflows, 
  knn_res$splits,     
  ~ predict(.x, assessment(.y)),         
  .id = "fold"
)                                   

prices <- map_dfr(
  knn_res$splits,  
  ~ assessment(.x) %>% select(Sale_Price)
)

rmse_estimates <- knn_pred %>%
  bind_cols(prices) %>% 
  group_by(fold) %>% 
  do(rmse = rmse(., Sale_Price, .pred)) %>% 
  tidyr::unnest(cols = c(rmse)) 

mean(rmse_estimates$.estimate) #<< 

## -----------------------------------------------------------------------------

easy_eval <- fit_resamples(knn_wfl, resamples = cv_splits, control = control_resamples(save_pred = TRUE))
easy_eval

## -----------------------------------------------------------------------------

collect_predictions(easy_eval) %>% 
  arrange(.row) %>% 
  slice(1:5) 

collect_metrics(easy_eval)

## -----------------------------------------------------------------------------

collect_metrics(easy_eval, summarize = FALSE) %>% 
  slice(1:10)

## -----------------------------------------------------------------------------
## Part 5

penalty()
mixture()

glmn_param <- parameters(penalty(), mixture())
glmn_param

glmn_grid <- 
  grid_regular(glmn_param, levels = c(penalty = 10, mixture = 5))
glmn_grid %>% slice(1:4)

## -----------------------------------------------------------------------------

set.seed(7454)
glmn_sfd <- grid_max_entropy(glmn_param, size = 50)
glmn_sfd %>% slice(1:4)
# grid_latin_hypercube() can also be used
# grid_random() too


## -----------------------------------------------------------------------------

# The names can be changed:
glmn_set <- parameters(lambda = penalty(), mixture())

# The ranges can also be set by their name:
glmn_set <- 
  update(glmn_set, lambda = penalty(c(-5, -1)))


# Some parameters depend on data dimensions:
mtry()
rf_set <- parameters(mtry(), trees())

rf_set

# Sets the range of mtry to
# be the number of predictors
finalize(rf_set, mtcars %>% dplyr::select(-mpg))

## -----------------------------------------------------------------------------

knn_mod <- 
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

parameters(knn_mod)


## -----------------------------------------------------------------------------

nearest_neighbor(neighbors = tune("K"), weight_func = tune("weights")) %>% 
  set_engine("kknn") %>% 
  set_mode("regression") %>% 
  parameters()


set.seed(522)
knn_grid <- knn_mod %>% 
  parameters() %>% 
  grid_regular(
    levels = c(neighbors = 15, weight_func = 5)
  )

ctrl <- control_grid(verbose = TRUE, save_pred = TRUE)

knn_tune <- 
  knn_mod %>% 
  tune_grid(
    ames_rec,
    resamples = cv_splits, 
    grid = knn_grid, 
    control = ctrl
  )

knn_tune

# results for the first fold:
knn_tune$.metrics[[1]]

show_best(knn_tune, metric = "rmse", n = 3)

# Or pick the numerically best results
best_res <- select_best(knn_tune, metric = "rmse")
best_res


## -----------------------------------------------------------------------------

autoplot(knn_tune, metric = "rmse")

autoplot(knn_tune, metric = "rmse") + ylim(c(.07, .1))

## -----------------------------------------------------------------------------

knn_best_pred <- 
  knn_tune %>% 
  collect_predictions(parameters = best_res) %>% 
  mutate(resid = Sale_Price - .pred) %>% 
  arrange(desc(abs(resid)))

knn_best_pred %>% slice(1:4)

knn_best_pred %>%
  ggplot(aes(x = Sale_Price, y = .pred)) +
  geom_abline(col = "green") +
  geom_point(alpha = .3) +
  coord_obs_pred()

## -----------------------------------------------------------------------------

final_knn_mod <- finalize_model(knn_mod, best_res) 

final_knn_fit <- final_knn_mod %>% last_fit(ames_rec, split = data_split)


# Test set results
collect_metrics(final_knn_fit)

final_knn_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = Sale_Price, y = .pred)) +
  geom_abline(col = "green") +
  geom_point(alpha = .3) +
  coord_obs_pred()


## library(doParallel)
## 
## cl <- makeCluster(6)
## registerDoParallel(cl)
## 
## # run `tune_grid()`...
## 
## stopCluster(cl)

## library(usemodels)
## 
## use_xgboost(Sale_Price ~ ., data = ames)
