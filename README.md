
title: "HW3"
author: "Bamba"
date: "2025-09-12"
output:
  pdf_document: default
  html_document: default
---


```{r setup, include=FALSE}

# Load required libraries
library(flexdashboard)
library(tidyverse)
library(readxl)
library(DBI)
library(duckdb)
library(janitor)
library(ggplot2)
library(plotly)
library(knitr)

tinytex::install_tinytex(force = TRUE)

# Install TinyTeX (lightweight LaTeX distribution)
install.packages("tinytex")
tinytex::install_tinytex()

# After installation, try rendering again
rmarkdown::render("Econometrics-HW3.Rmd")

# Set chunk options
knitr::opts_chunk$set(echo = TRUE)


library(ggplot2)
library(dplyr)
library(ggplot2)
library(tidyverse)


load("C:/Users/Faculty-Staff/Desktop/CUNY CLASS/ECONOMETRICS/d_HHP2020_24.Rdata")







p <- ggplot(data = d_HHP2020_24,
            mapping = aes(x = Education, fill = Mar_Stat))
```

```{r echo=FALSE}

p + geom_bar(position = "fill")

knitr::opts_chunk$set(echo = TRUE)
```

This graph creates a 100% stacked bar chart showing the proportion of marital statuses within each education level.it is not suitable any analysis.

```{r echo=FALSE}
p <- ggplot(data = d_HHP2020_24,
            mapping = aes(x = Education, fill = Mar_Stat))
p + geom_bar()
```
This graph creates a standard stacked bar chart showing raw counts (not proportions) of marital statuses for each education level.


```{r echo=FALSE}
p + geom_bar(mapping = aes(
  y = after_stat(prop),
  group = Mar_Stat)) 
```




This chart attempts to create proportional bars but will produce incorrect results because the grouping isn't properly configured for proportional calculations.

```{r echo=FALSE}
p + geom_bar( position = "dodge",
    mapping = aes(y = after_stat(prop), group = Mar_Stat, fill = Mar_Stat)
  ) +
  labs(title = "Education Level vs. Marital Status",
    x = "Education",y = "Proportion",fill = "Marital Status")
```

This Dodged Proportional Bar Chart creates a properly formatted dodged bar chart showing proportions of each marital status across education levels with appropriate labels and title.




Summary:
These code examples demonstrate different approaches to visualizing the relationship between education level and marital status:

Stacked percentage charts - Show part-to-whole relationships within each education category

Standard stacked charts - Display raw count distributions

Dodged proportional charts - Compare proportions across categories side-by-side



The Dodged proportional charts graph represents the most effective approach for comparing proportions across education levels, using dodged bars to make direct comparisons easier and proper labeling for clarity.
