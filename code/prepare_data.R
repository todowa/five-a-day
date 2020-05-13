

#---- Import Required Packages ----
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")


#---- Global Variables ----
url = "https://www.bbcgoodfood.com/seasonal-calendar/all"
OUTPUT_PATH <- "./output/"
SEASONAL_OUTPUT <- "seasonal_produce.csv"


#---- Download Seasonal Produce Data and Convert to Tidy Format ----
table <- read_html(url) %>% html_nodes("table")
table <- table %>% html_table %>% .[[1]]
table$produce <- table[, 1]
table <- table %>%
  select(produce, all_of(month.abb)) %>%
  gather(key = "month", value = "status", all_of(month.abb)) %>%
  mutate(status = ifelse(status == "", "Out of season", status))


#---- Prepare Variables for Analysis ----

# Identify meat, fish, fruit and veg in the list of produce
meat_and_fish <- c("Goose", "Halibut", "Lamb", "Mussels", "Oyster", "Salmon", "Tuna", "Beef", "Cod",
                   "Mackerel", "Spring lamb", "Turkey", "Whiting", "Chicken", "Grouse", "Kipper", "Venison",
                   "Crab", "Duck", "Guinea fowl", "Pork")
meat <- c("Goose","Lamb", "Beef", "Spring lamb", "Turkey", "Chicken", "Grouse", "Venison", "Duck", "Guinea fowl", "Pork")
fish <- setdiff(meat_and_fish, meat)
fruit_and_veg <- table %>% mutate(is_meat = ifelse(produce %in% meat, T, F),
                 is_fish = ifelse(produce %in% fish, T, F)) %>% filter(!is_meat & !is_fish) %>% group_by(produce) %>% count(.) %>% pull(produce)
fruit <- c("Apple", "Elderberries", "Gooseberry", "Pear", "Watermelon", "Apricot", "Crab apple", "Grapefruit",
           "Orange", "Raspberry", "Strawberry", "Tayberry", "Blackberry", "Cranberry", "Fig", "Lemon", "Redcurrant",
           "Tomato", "Blackcurrants", "Clementine", "Damson", "Nectarine", "Plum", "Quince", "Banana", "Bramley apple",
           "Cherry", "Date", "Loganberry", "Peach", "Pomegranate")
veg <- setdiff(fruit_and_veg, fruit)

# Add indicators for meat, fish, fruit and veg
table <- table %>% mutate(is_meat  = ifelse(produce %in% meat, T, F),
                 is_fish  = ifelse(produce %in% fish, T, F),
                 is_fruit = ifelse(produce %in% fruit, T, F),
                 is_veg   = ifelse(produce %in% veg, T, F))

confirm_all_labelled <- function(data) {
  #' Throws error if not all entries are either meat, fish, fruit or veg
  count <- data %>% filter(!is_meat & !is_fish & !is_fruit & !is_veg) %>% nrow()
  if (count != 0) { stop(call. = T) } else { cat("All entries are labelled as meat, fish, fruit or veg.")}
}
confirm_all_labelled(table)


#---- Save Data for Visualisation and Further Analysis ---- 
write_csv(table, file.path(OUTPUT_PATH, SEASONAL))
