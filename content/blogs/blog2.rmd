---
categories:
- ""
- ""
# date: "2017-10-31T22:26:09-05:00"
# description: Lorem Etiam Nullam
draft: false
image: sf.jpeg
keywords: ""
slug: rent_sf
title: San Francisco Rent Analysis
---

```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```

```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
```

# Rents in San Francsisco 2000-2018

```{r read_rent}
# download directly off tidytuesdaygithub repo
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

```

What are the variable types? Do they all correspond to what they really are? Which variables have most missing values?

> There are 8 character variables and 9 numeric variables. They all seem to correspond to what they really are. The variable with the most missing values is "descr".

```{r skim_rent}
# YOUR CODE GOES HERE
skimr::skim(rent)
summary(rent)

```

Make a plot that shows the top 20 cities in terms of % of classifieds between 2000-2018. You need to calculate the number of listings by city, and then convert that number to a %.

The final graph should look like this![](images/top_cities.png)

```{r top_cities}
# YOUR CODE GOES HERE
rent %>% 
  count(city, sort=TRUE) %>% 
  mutate(prop = n/sum(n)) %>% 
  slice_max(order_by = prop, n=20) %>% 
  mutate(city = fct_reorder(city, prop)) %>% 
  
  ggplot(aes(x=prop, y= city)) +
  geom_col() +
  labs(title = "San Francisco accounts for more than a quarter of all rental classifieds",
       subtitle = "% of Craigslist listings, 2000-2018", 
       caption = "Source: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018", y = "City", x = element_blank()) +
  scale_x_continuous(labels = scales::percent)
  
```

Make a plot that shows the evolution of median prices in San Francisco for 0, 1, 2, and 3 bedrooms listings. The final graph should look like this

![](images/sf_rentals.png)

```{r sf_median_prices}
# YOUR CODE GOES HERE
rent %>%
  filter(city == "san francisco", beds<=3) %>%
  group_by(year, beds) %>%
  summarise(median_price = median(price)) %>% 

ggplot( mapping=aes(x=year, y=median_price, colour = factor(beds))) +
  geom_line() +
  facet_wrap(~beds, nrow=1)+
  theme(legend.position = "none") +
  labs(title = "San Francisco rents have been steadily increasing",
       subtitle = "0- to 3-bed listings, 2000-2018", 
       caption = "Source: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018", y = element_blank(), x = element_blank())

```

Finally, make a plot that shows median rental prices for the top 12 cities in the Bay area. Your final graph should look like this

![](images/one_bed_bay_area.png)

```{r spirit_plot}
# YOUR CODE GOES HERE
rent %>%
  filter(city %in% c("san francisco", "san jose", "oakland", "santa rosa", "santa cruz", "san mateo", "sunnyvale", "mountain view", "berkeley", "santa clara", "palo alto", "union city"), beds == 1)%>%
  group_by(city,year) %>%
  summarise(median_price = median(price), city)%>%
  
  ggplot(aes(x=year, y=median_price, color=city)) +
  geom_line() +
  facet_wrap(~city, nrow=3)+
  theme(legend.position = "none") +
  labs(title = "Median rental prices for 1 bedroom flats in top 12 cities from Bay Area", x = "Year", y = "Median rental price", caption = "Source: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018")
  
```

What can you infer from these plots? Don't just explain what's in the graph, but speculate or tell a short story (1-2 paragraphs max).

> The general trend that can be seen across the cities in the Bay area is that the rent has decreased in the years following the dot.com bubble (2001-2004), after which it increased rapidly prior to the housing market crash and global financial crisis that started in 2007-2008. After 2010 rents started increasing rapidly again until the economic slowdown that commenced at the end of the decade.