---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# metamed
<!-- badges: start -->
<!-- badges: end -->
The goal of **metamed** is to provide researchers and data analysts with a comprehensive and user-friendly R package for conducting meta-analyses. The package focuses on generating clear, publication-quality forest plots for both continuous and binary outcomes. Key features include:

- Contour-enhanced forest plots for visualizing effect sizes and confidence intervals.
- publication ready tables for binary outcome
- Automatic calculation and display of pooled effects, weights, and heterogeneity statistics (I², Tau², Q-test).
- Optional prediction intervals for anticipating the range of effect in new studies.
- Support for Odds Ratios (OR), Risk Ratios (RR), and standardized mean differences.
- Fully customizable plot aesthetics, including colors, labels, and square sizes.
- Integration with ggplot2 for flexible and reproducible visualizations.


## Installation
You can install the development version of metamed like so:
``` r
install.packages("remotes")
remotes::install_github("umarhussain-git/metamed")
```

## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
library(metamed)

dat <- data.frame(
Study = c("Shell 2021","Khalid 2020","Daniel 2009","Albert 2018","Khan 2011"),
events_t = c(10,25,5,30,18),
n_t = c(100,120,90,140,110),
events_c = c(15,20,22,28,25),
n_c = c(100,115,95,130,120)
)

forest.binary(
dat,
measure = "OR"
)

# Example for continuous outcome

# Continuous outcome example dataset
dat1 <- data.frame(
  Study = c("Study A", "Study B", "Study C", "Study D", "Study E", "Study F"),
  mean_c = c(5.2, 4.8, 6.1, 5.5, 4.9, 5.8),
  sd_c = c(1.8, 2.0, 2.1, 1.9, 2.2, 1.7),
  n_c = c(50, 60, 45, 55, 50, 40),
  mean_t = c(7.4, 5.9, 8.0, 3.8, 4.2, 8.1),
  sd_t = c(2.0, 1.9, 2.2, 2.1, 2.0, 2.3),
  n_t = c(50, 60, 45, 55, 50, 40)
)
 forest.continuous(dat1)
```


You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this.


