library(rvest)
library(tidyverse)
# This is how you get read the HTML into R
url <- "https://www.honeycarsmart.com/index.php/full-inventory/"
html <- read_html(url)

# Extract the car price
prices <-
  html |>
  html_elements(".results") |>
  html_text2() 

# Clean up
prices <- 
  prices <- 
  str_remove_all(prices, "[^0-9]") |>  # Remove non-numeric characters
  na_if("") |>                         # Replace empty strings with NA
  as.integer()                         # Convert to integers

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
     str_remove_all("[^0-9]") |> 
    as.integer()

colors <-
  html |>
  html_elements(".mini-hide") |>
  html_text2()
str_remove_all(colors, "[0-9]")
str_trim(clean_colors)

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