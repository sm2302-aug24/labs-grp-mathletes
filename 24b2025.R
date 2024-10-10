#SCRAPING CAR DATA-----------
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
#--------------------------------------------------------------------------------
all_brands <- list()
  
for (i in 1:num_pages) { 
    url <- paste0("https://www.honeycarsmart.com/index.php/full-inventory/page/", i)
    page <- read_html(url)
    data <- page %>% 
      html_elements(".vehicle-name") %>%
      html_text2() 
    all_brands[[i]] <- data
  }

brands <- unlist(all_brands)
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

  str_remove_all(mileages, " kms") |>  # Remove non-numeric characters
  str_remove_all( ",") |>  # Remove non-numeric characters
  as.integer()

mileages <- unlist(all_mileages)
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
#--------------------------------------------------------------------------------
# Put it all in a data frame
car_df <- tibble(
  price = prices,
  brands = brands,
  mileages = mileages,
  colors = colors
)