#1) SCRAPING CAR DATA-----------------------------------------------------------

library(stringr)
library(tidyverse)
library(rvest)

# This is how you get read the HTML into R
url <- "https://www.honeycarsmart.com/index.php/full-inventory"
html <- read_html(url)

#number of pages to scrape
num_pages <- 20
#-------------------------------------------------------------------------------
all_prices <- list()
for (i in 1:num_pages) { 
  url <- paste0("https://www.honeycarsmart.com/index.php/full-inventory/page/", i)
  page <- read_html(url)
  data <- page %>%
    html_elements(".results") %>%
    html_text2() 
  all_prices[[i]] <- data
}
# Notice that each element of this list contains two additional elements
# ("Monthly payment: $" and $"$), which we want to remove.
all_prices <- lapply(all_prices, function(x) x[grepl("^\\$\\d{1,3},\\d{3}$", x)])
prices <- unlist(all_prices)

# Clean up
prices <- 
  str_remove_all(prices, "[^0-9]") |>  # Remove non-numeric characters
  as.integer()
#-------------------------------------------------------------------------------
# Initialize lists for brands and years
all_brands <- list()
all_years <- list()

for (i in 1:num_pages) { 
  url <- paste0("https://www.honeycarsmart.com/index.php/full-inventory/page/", i)
  page <- read_html(url)
  data <- page %>% 
    html_elements(".vehicle-name") %>%
    html_text2() 
  
  # Extract years (4-digit numbers)
  years <- str_extract(data, "\\b\\d{4}\\b")  # This captures the year
  all_years[[i]] <- years
  
  # Remove the year from the brand names
  brands <- str_remove(data, "\\b\\d{4}\\b")  # This removes the year
  all_brands[[i]] <- str_trim(brands)  # Trim any extra spaces
}

# Unlist the results
brands <- unlist(all_brands)
years <- unlist(all_years)

# Convert years to integer
years <- as.integer(years)
#-------------------------------------------------------------------------------

all_mileages <- list()

for (i in 1:num_pages) { 
  url <- paste0("https://www.honeycarsmart.com/index.php/full-inventory/page/", i)
  page <- read_html(url)
  data <- page %>% 
    html_elements(".miles-style") %>%
    html_text2() 
  all_mileages[[i]] <- data
} 
mileages <- unlist(all_mileages)

  str_remove_all(mileages, " kms") |>  # Remove non-numeric characters
  str_remove_all( ",") |>  # Remove non-numeric characters
  as.integer()


#-------------------------------------------------------------------------------

all_colors <- list()

for (i in 1:num_pages) { 
  url <- paste0("https://www.honeycarsmart.com/index.php/full-inventory/page/", i)
  page <- read_html(url)
  data <- page %>% 
    html_elements(".car-info :nth-child(1)") %>%
    html_text2()
    data <- str_remove_all(data, "\\d{4}") 
  all_colors[[i]] <- data
}

colors <- unlist(all_colors)

#-------------------------------------------------------------------------------

# Put it all in a data frame
car_df <- tibble(
  price = prices,
  brands = brands,
  mileages = mileages,
  colors = colors
)

#2) ANALYZE DATA----------------------------------------------------------------


library(readxl)
car_df <- read_excel("car_df.xlsx") |>
  mutate(mileage = as.numeric(`MILEAGE (kms)`))
continent <- car_df$CONTINENT
#continent is manually inserted.

#objective 1 -> The latest the car, the more expensive it is.


model_1 <- lm(prices ~ years, data = car_df)

#objective 2 -> The lower the mileage, the more expensive the car is.


model_2 <- lm(prices ~ mileages, data = car_df)


#objective 3 -> The origin of the car can influence the price.


model_3 <- lm(prices ~ continent, data = car_df)


#3) GGPLOT----------------------------------------------------------------------

#model_1



#model_2

library(ggplot2)

ggplot(car_df, aes(x = mileage,
                   y = `PRICE($)`))+
  
  geom_point(colour= "red") +
  
  geom_smooth(method= "lm",
              se= FALSE,
              colour = "blue")+
  labs(
    title = "MODEL_2",
    subtitle = paste("MILEAGES & PRICES"),
    x = "MILEAGES(kms)",
    y = "PRICES($)"
  )
  


#model_3


ggplot(car_df,aes(y= `PRICE($)`,
                  x = CONTINENT)
)+
  geom_boxplot(colour = "red")+
theme_minimal()













