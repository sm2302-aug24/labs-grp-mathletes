library(tidyverse)
library(stringr)
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
#--------------------------------------------------------------------------------
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

# Clean up
mileages <- 
  str_remove_all(mileages, " kms") |>  # Remove non-numeric characters
  str_remove_all( ",") |>  # Remove non-numeric characters
  as.integer()
#--------------------------------------------------------------------------------
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
#--------------------------------------------------------------------------------
## Put it all in a data frame
 car_df <- tibble(
    price = prices,
    brands = brands,
    mileages = mileages,
    colors = colors,
    year = years
    )
#if you want to arrange
arrange(.data = car_df, mileages)

arrange(.data = car_df, brands)

arrange(.data = car_df, price)

#objective
#The latest the car, the expensive
#The higher the mileage the more expensive the car is

#---------------------------------------------------------------------
#Linear regression predicting price based on mileage, brands and colors
lm(price ~ mileages, data = car_df)
lm(price ~ brands, data = car_df)

write_csv(car_df, file = "car_df.csv")


