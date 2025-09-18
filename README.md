
title: "HW3"
author: "Bamba"
date: "2025-09-12"
output:
  pdf_document: default
  html_document: default
---
**GROUP MEMBERS:**
Bamba Cisse
Riyesh Nath
Nasrin Khanam 
Marwan Kenawy



### Summary:

Summary: 
In this project, we will do a data exploration of household pulse data and try to find variables that can allows us to find a strong correlation with a person’s partnership status. 
The variables that we will test are: 

  - Effect of education on partnership status.
  - Effect of race on partnership status.
  - Effect of Age on partnership status 

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

```
This graph creates a 100% stacked bar chart showing the proportion of marital statuses within each education level.it is not suitable for any analysis.


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




---------------


```

#### Riyesh Nath -- Race and Gender's effect on partnership status

First we will look at race's effect on partnership status:
```{r}
data_groupby_race_part <- d_HHP2020_24 %>%
  count(Race, Mar_Stat) %>%
  group_by(Race) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()


ggplot(data = data_groupby_race_part, aes(x = Race, fill=Mar_Stat, y = prop)) +
  geom_col(position = "dodge")
```

Here we see that we have highest proportion of married in Asian community, then white community, then other and finally black community. We also see that in black community there is close proportion of never married and married. 

Maybe we can use chi sq test to see if race might have an affect on marriage rate.

```{r}
d_only_married_ornot <- d_HHP2020_24 %>%
  mutate(Mar_Stat = if_else(Mar_Stat == "Married", "Married", "Not Married"))

print(table(d_only_married_ornot$Race, d_only_married_ornot$Mar_Stat))
chisq.test(table(d_only_married_ornot$Race, d_only_married_ornot$Mar_Stat))

```

**Using p-value less than .05, it seems that we can state that race does seem to have an affect on married status.**

Now lets look at this further when we divide it by gender as well (we will filter trans due to the political and social complication which would make the analysis harder. Other is filter due to the ambiguity of other).

```{r}

d_HHP2020_24_female_male <- d_only_married_ornot %>%
  filter(Gender %in% c("male", "female"))


data_race_gender_partnership_black <- d_HHP2020_24_female_male %>%
  filter(Race == "Black", !is.na(Mar_Stat)) %>%
  count(Mar_Stat, Gender) %>%
  group_by(Gender) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

plot_black_demo <- ggplot(data = data_race_gender_partnership_black,
       mapping = aes(x = Gender, fill=Mar_Stat, y=prop)) +
  geom_col(position = "dodge") +
  labs(x = "Black demographic marriage status")

data_race_gender_partnership_white <- d_HHP2020_24_female_male %>%
  filter(Race == "white", !is.na(Mar_Stat)) %>%
  count(Mar_Stat, Gender) %>%
  group_by(Gender) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

plot_white_demo <-  ggplot(data = data_race_gender_partnership_white,
       mapping = aes(x = Gender, fill=Mar_Stat, y=prop)) +
  geom_col(position = "dodge") +
  labs(x = "White demographic marriage status")


data_race_gender_partnership_asian <- d_HHP2020_24_female_male %>%
  filter(Race == "Asian", !is.na(Mar_Stat)) %>%
  count(Mar_Stat, Gender) %>%
  group_by(Gender) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

plot_asian_demo <- ggplot(data = data_race_gender_partnership_white,
       mapping = aes(x = Gender, fill=Mar_Stat, y=prop)) +
  geom_col(position = "dodge") +
  labs(x = "Asian demographic marriage status")

grid.arrange(plot_asian_demo, plot_white_demo, plot_black_demo, ncol = 2)

```

**Using chi-square test for each subgroup for Race and then looking at Marriage or not Married, we see that gender has an affect.**

```{r}

d_HHP2020_24_female_male_black <- d_only_married_ornot %>%
  filter(Gender %in% c("male", "female"), Race == "Black") %>%
  droplevels()

chisq.test(table(
  d_HHP2020_24_female_male_black$Gender,
  d_HHP2020_24_female_male_black$Mar_Stat
))

d_HHP2020_24_female_male_white <- d_HHP2020_24 %>%
  filter(Gender %in% c("male", "female"), Race == "white") %>%
  droplevels()

chisq.test(table(
  d_HHP2020_24_female_male_white$Gender,
  d_HHP2020_24_female_male_white$Mar_Stat
))


d_HHP2020_24_female_male_asian <- d_HHP2020_24 %>%
  filter(Gender %in% c("male", "female"), Race == "Asian") %>%
  droplevels()

chisq.test(table(
  d_HHP2020_24_female_male_asian$Gender,
  d_HHP2020_24_female_male_asian$Mar_Stat
))

```

Looking at the ratio of female to male in Asian, White and Black demographic, we do see that there are a larger proportion of female vs male among the Black community than in other demographics. Could this be a factor for lack of marriage rate in Black community than other community? This needs to be tested as this hypothesis could claim that a person has a higher probability to marry someone from same Race. Unfortunately, our dataset does not give us information to test this claim. 

```{r}
data_black_community <- d_HHP2020_24_female_male %>%
  filter(Race == "Black") %>%
  count(Gender)

count_black_community <- ggplot(data = data_black_community,
       mapping = aes(x=Gender, y=n)) + 
  geom_col() +
  labs(x = "Black Demographic Gender Ratio")


data_white_community <- d_HHP2020_24_female_male %>%
  filter(Race == "white") %>%
  count(Gender)

count_white_community <- ggplot(data = data_white_community,
       mapping = aes(x=Gender, y=n)) + 
  geom_col() +
  labs(x = "White Demographic Gender Ratio")

data_asian_community <- d_HHP2020_24_female_male %>%
  filter(Race == "Asian") %>%





## Nasrin Khanam : Marital Status by Age Group

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
library(dplyr)

setwd("/Users/nasrinkhanam/Downloads")
unzip("d_HHP2020_24.zip")
load("d_HHP2020_24.Rdata")
```



## Marital Status by Age Group
```{r}
d_HHP2020_24 <- d_HHP2020_24 %>%
  mutate(Age_group = cut(Age,
                         breaks = c(-Inf, 24, 29, 34, 39, 44, 49, 59, 69, Inf),
                         labels = c("<=24","25-29","30-34","35-39",
                                    "40-44","45-49","50-59","60-69","70+")))

ggplot(d_HHP2020_24, aes(x = Age_group, fill = Mar_Stat)) +
  geom_bar(position = "dodge") +
  labs(title = "Marital Status by Age Group",
       y = "Count", x = "Age Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
# Marital Status by Age Groups (Selected Ranges)

d_HHP2020_24 <- d_HHP2020_24 %>%
  mutate(Age_group = cut(Age,
                         breaks = c(-Inf, 24, 29, 34, 39, 44, 49, 59, 69, Inf),
                         labels = c("<=24","25-29","30-34","35-39",
                                    "40-44","45-49","50-59","60-69","70+")))

# Define function filters
plot_age_group <- function(data, group_label) {
  df <- data %>% filter(Age_group == group_label,
                        Gender %in% c("male","female"),
                        Mar_Stat %in% c("Married","never","divorced"))
  
  ggplot(df, aes(x = Gender, fill = Mar_Stat)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Marital Status for Age Group", group_label),
         x = "Gender", y = "Count") +
    theme_minimal()
}

# Plots for each chosen age group
plot_age_group(d_HHP2020_24, "25-29")
plot_age_group(d_HHP2020_24, "35-39")
plot_age_group(d_HHP2020_24, "45-49")
plot_age_group(d_HHP2020_24, "50-59")
plot_age_group(d_HHP2020_24, "70+")

```
