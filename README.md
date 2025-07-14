# REPLICATION PAPER [inset title, final part]

```{r,echo = FALSE }
want = c("rio", "haven", "janitor", "stargazer", "foreign", "magrittr",
         "nnet", "MASS", "MNLpred", "ggplot2", "ggplot2", "extrafont", "scales", "effects", "ggpubr",
         "jtools", "simEd", 'tidyverse', 'forcats', 'brglm2')

have = want %in% rownames(installed.packages())

if (any(!have)) { install.packages( want[!have] ) }

# load packages

junk <- lapply(want, library, character.only = TRUE)

rm(have, want, junk)


#important
set.seed(260423)
```

## Abstract [final part]

## Introduction

## Data and Methods: Original paper

## First part results

## Data and methods: Additional analysis

## Results

## Conclusions and limitations of the study.

## Bibliography
