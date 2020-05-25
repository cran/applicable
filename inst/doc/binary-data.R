## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)
theme_set(theme_bw())

## ---- echo = FALSE------------------------------------------------------------
# TODO
#- Mention different input data types: data.frame, recipes, matrix, etc.
#- Maybe make a (better) conclusion?
#- Explain the reason why the training set is diverse.

## -----------------------------------------------------------------------------
library(applicable)

## -----------------------------------------------------------------------------
data(qsar_binary)

## -----------------------------------------------------------------------------
jacc_sim <- apd_similarity(binary_tr)
jacc_sim

## ----jac-plot, fig.width=5, fig.height=5.2,  out.width = '50%', fig.align='center'----
library(ggplot2)

# Plot the empirical cumulative distribution function for the training set
autoplot(jacc_sim)

## -----------------------------------------------------------------------------
# Summarize across all training set similarities
mean_sim <- score(jacc_sim, new_data = binary_unk)
mean_sim

