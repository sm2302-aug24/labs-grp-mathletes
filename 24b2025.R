#SCRAPING CAR DATA-----------


library(rvest)

# This is how you get read the HTML into R
url <- "https://www.honeycarsmart.com/index.php/full-inventory"
html <- read_html(url)

#number of pages to scrape

num_pages <- 21
all_data <- list()

for (i in 1:num_pages) { 
  page <- read_html(url)
  data <- page %>% 
    html_nodes(".css-selector") %>%
    html_text() 
  all_data[[i]] <- data
}

num_pages <- 20

#-------------------------------------------------------------------------------

all_prices <- list()

for (i in 1:num_pages) { 
  url <- paste0("https://www.honeycarsmart.com/index.php/full-inventory/page/", i)
  page <- read_html(url)
  data <- page %>% 
  html_elements(".results") %>%
  html_text2() 
  all_data[[i]] <- data
}
 str_remove_all(prices, "[^0-9]") |>  # Remove non-numeric characters
  str_remove_all("Monthly Payment: $") |>
  str_remove_all("$")
  as.integer()
  
combined_data <- unlist(all_data)
print(combined_data)

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

combined_data <- unlist(all_brands)
print(combined_data)

# Extract the car brand
brands <-
  html |>
  html_elements(".vehicle-name") |>
  html_text2() |>
  as.character()

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
print(combined_data)


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

combined_data <- unlist(all_colors)
print(combined_data)

# Extract the car colors
colors <-
  html |>
  html_elements(".car-info :nth-child(1)") |>
  html_text2() 

#--------------------------------------------------------------------------------

remarks <- 
  html |>
  html_elements("div p .mt-3") |>
  html_text2()

remarks <- tail(remarks, length(prices))

#--------------------------------------------------------------------------------

library(tidyverse)

# Put it all in a data frame
hsp_df <- tibble(
  price = prices,
  brands = brands,
  mileages = mileages,
  colors = colors,
  remarks = remarks

)




