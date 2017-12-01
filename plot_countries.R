# [name] with 4 dashes afterwards creates a type of toc
# test the toc ----
# libraries ----
library(tidyverse)

# data ----
url <- 'https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder.csv'
gapminder <- read_csv(url) # View(gapminder)

# ggplot: after filter by country ----
gapminder %>%
  filter(country == "Afghanistan") %>%
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Afghanistan")

cntry <- "Burundi"
# ggsave: after filter by country & plot ----
png <- paste0("gdp_",cntry,".png")

g <- gapminder %>%
  filter(country == cntry) %>%
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_point() +
  geom_smooth() +
  labs(title = cntry)

ggsave(filename = png, plot = g)

# function: for plotting any country ----
plot_country <- function(cntry){
  
  png <- paste0("gdp_", cntry, ".png")
  #browser()
  g <- gapminder %>%
    filter(country == cntry) %>%
    ggplot(aes(x = year, y = gdpPercap)) +
    geom_point() +
    geom_smooth() +
    labs(title = cntry,
         x = "Year",
         y = "Gross Domestic Product %")
  
  
  ggsave(filename = png, plot = g)
}

plot_country("Algeria")

countries2 <- unique(gapminder$country)

# for: loop to iterate over some countries ----
countries <- c("United States", "Mexico")

for (k in countries){
  
  plot_country(k)
  
}

# debug: inside function and for loop ----
plot_country <- function(cntry){
  
  png <- paste0("gdp_", cntry, ".png")
  
  #browser() 
  # debugging so you know what you are calling
  cat("plot_country(", cntry,") -> ", png, "\n")
  
  g <- gapminder %>%
    filter(country == cntry) %>%
    ggplot(aes(x = year, y = gdpPercap)) +
    geom_point() +
    geom_smooth() +
    labs(title = cntry,
         x = "Year",
         y = "Gross Domestic Product %")
  
  print(g)
  
  ggsave(filename = png, plot = g)
}

countries <- c("Fiji", "Peru", "Mexico")

for (k in countries){
  
  #browser()
  cat("for () { k: ", k," }\n")
  
  plot_country(k)
}

# if: output to different folder based on gdp ----
dir.create("developed")
dir.create("developing")

plot_country <- function(cntry, png){
  
  cat("plot_country(", cntry,", ", png, ")\n")
  
  g <- gapminder %>%
    filter(country == cntry) %>%
    ggplot(aes(x = year, y = gdpPercap)) +
    geom_point() +
    geom_smooth() +
    labs(title = cntry,
         x = "Year",
         y = "Gross Domestic Product %")
  
  ggsave(filename = png, plot = g)
}

# in the function is_developed, the argument threshold has a default

is_developed <- function(cntry, threshold = 12000){
  #browser() # cntry <- "Canada"
  gapminder %>%
    filter(country == cntry) %>%
    summarise(
      mean_gdp = mean(gdpPercap)) %>%
    .$mean_gdp >= threshold # dot refers to previous df
}


countries <- c("United States", "Canada", "Afghanistan", "Rwanda")

for (k in countries){
  
  if (is_developed(k)){
    png <- paste0("developed/gdp_", k, ".png")
  } else {
    png <- paste0("developing/gdp_", k, ".png")
  }
  
  plot_country(k, png)
}
