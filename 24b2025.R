#SCRAPING CAR DATA-------


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

combined_data <- unlist(all_data)
print(combined_data)

# Extract the car price
prices <-
  html |>
  html_elements(".results") |>
  html_text2() 

# Clean up
prices <- 
  str_remove_all(prices, "[^0-9]") |>  # Remove non-numeric characters
  na_if("") #Replace empty string with na
as.integer()

# Do same thing for number of brands, mileages, colors, and other remarks
brands <-
  html |>
  html_elements(".vehicle-name") |>
  html_text2() |>
  as.character()


mileages <-
  html |>
  html_elements(".miles-style") |>
  html_text2() |>
  as.integer()
#smtg wrong

colors <-
  html |>
  html_elements(".mini-hide") |>
  html_text2()
str_remove_all(colors, "[0-9]")

remarks <- 
  html |>
  html_elements("div p .mt-3") |>
  html_text2()

remarks <- tail(remarks, length(prices))

library(tidyverse)

# Put it all in a data frame
hsp_df <- tibble(
  price = prices,
  brands = brands,
  mileages = mileages,
  colors = colors,
  remarks = remarks
)