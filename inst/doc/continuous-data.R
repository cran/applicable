## ---- include = FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

prev_options <- options(width = 100)

library(ggplot2)
theme_set(theme_bw())

## -------------------------------------------------------------------------------------------------
library(applicable)

## ----ames_data, message=FALSE---------------------------------------------------------------------
library(AmesHousing)
ames <- make_ames()

## ----prep_data, message=FALSE---------------------------------------------------------------------
library(recipes)
library(dplyr)

ames_cols <- names(ames_new)

training_data <- 
  ames %>% 
  # For consistency, only analyze the data on new properties
  dplyr::select(one_of(ames_cols)) %>% 
  mutate(
    # There is a new neighborhood in ames_new
    Neighborhood = as.character(Neighborhood),
    Neighborhood = factor(Neighborhood, levels = levels(ames_new$Neighborhood))
  )


training_recipe <-
  recipe( ~ ., data = training_data) %>%
  step_dummy(all_nominal()) %>% 
  # Remove variables that have the same value for every data point.
  step_zv(all_predictors()) %>% 
  # Transform variables to be distributed as Gaussian-like as possible.
  step_YeoJohnson(all_numeric()) %>%
  # Normalize numeric data to have a mean of zero and
  # standard deviation of one.
  step_normalize(all_numeric())

## -------------------------------------------------------------------------------------------------
ames_pca <- apd_pca(training_recipe, training_data)
ames_pca

## -------------------------------------------------------------------------------------------------
ames_pca <- apd_pca(training_recipe, training_data, threshold = 0.25)
ames_pca

## ----autoplot, fig.align='center'-----------------------------------------------------------------

library(ggplot2)
autoplot(ames_pca)

## ---- echo = FALSE, results = "hold"--------------------------------------------------------------
autoplot(ames_pca, matches("PC0[1-5]"))
autoplot(ames_pca, distance) + scale_x_log10()

## ----new_sample-----------------------------------------------------------------------------------
ames_pca <- apd_pca(training_recipe, training_data)
pca_score <- score(ames_pca, ames_new)
pca_score %>% select(matches("PC00[1-3]"), contains("distance"))

## ---- echo = FALSE--------------------------------------------------------------------------------
training_scores <- score(ames_pca, training_data)
ggplot(training_scores, aes(x = PC001)) + 
  geom_histogram(col = "white", binwidth = .5) + 
  geom_vline(xintercept = pca_score$PC001, col = "red")

## -------------------------------------------------------------------------------------------------
# `ames_pca$pcs` is the output of `prcomp()`
comp_one <- ames_pca$pcs$rotation[, 1]
comp_one[order(abs(comp_one), decreasing = TRUE)] %>% head(5)

## ---- echo = FALSE--------------------------------------------------------------------------------
ggplot(training_data, aes(x = Garage_Area )) + 
  geom_histogram(col = "white", binwidth = 50) + 
  geom_vline(xintercept = ames_new$Garage_Area, col = "red")

## -------------------------------------------------------------------------------------------------
non_singular_recipe <- 
  training_recipe %>% 
  step_lincomb(all_predictors())

# Recipe interface
ames_hat <- apd_hat_values(non_singular_recipe, training_data)

## ----reset_options------------------------------------------------------------

options(prev_options)


