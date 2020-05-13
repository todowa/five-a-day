

#---- Import Required Packages ----
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")


#---- Global Variables ----
OUTPUT_PATH <- "./output/"
SEASONAL_INPUT = "https://www.bbcgoodfood.com/seasonal-calendar/all"
SEASONAL_OUTPUT <- "seasonal_produce.csv"


#---- Download Seasonal Produce Data and Convert to Tidy Format ----
table <- read_html(SEASONAL_INPUT) %>% html_nodes("table")
table <- table %>% html_table %>% .[[1]]
table$produce <- table[, 1]
table <- table %>%
  select(produce, all_of(month.abb)) %>%
  gather(key = "month", value = "status", all_of(month.abb)) %>%
  mutate(status = ifelse(status == "", "Out of season", status))

# Add slugs for links to BBC Good Food website
table <- table %>% mutate(slug = str_replace(str_replace_all(tolower(produce), " ", "-"), "'", "")) %>%
  mutate(slug = ifelse(slug == "spring-lamb", "autumn-lamb", slug)) %>%
  mutate(slug = ifelse(slug == "white-asparagus", "asparagus-all-white", slug)) # exceptions to rule


#---- Prepare Seasonal Produce Variables for Analysis ----

# Identify meat, fish and seafood, fruit, veg, and herbs / nuts in the list of produce

meat_and_fish <- c("Goose", "Halibut", "Lamb", "Mussels", "Oyster", "Salmon", "Tuna", "Beef", "Cod",
                   "Mackerel", "Spring lamb", "Turkey", "Whiting", "Chicken", "Grouse", "Kipper", "Venison",
                   "Crab", "Duck", "Guinea fowl", "Pork")
meat <- c("Goose", "Lamb", "Beef", "Spring lamb", "Turkey", "Chicken", "Grouse", "Venison", "Duck", "Guinea fowl", "Pork")
fish <- setdiff(meat_and_fish, meat)

# Identify fruit and veg (and herbs / nuts) as those not meat or fish and seafood
fruit_and_veg <- table %>% mutate(is_meat = ifelse(produce %in% meat, T, F),
                 is_fish = ifelse(produce %in% fish, T, F)) %>% filter(!is_meat & !is_fish) %>% group_by(produce) %>% count(.) %>% pull(produce)

# Identify fruits
fruit <- c("Apple", "Elderberries", "Gooseberry", "Pear", "Watermelon", "Apricot", "Crab apple", "Grapefruit",
           "Orange", "Raspberry", "Strawberry", "Tayberry", "Blackberry", "Cranberry", "Fig", "Lemon", "Redcurrant",
           "Tomato", "Blackcurrants", "Clementine", "Damson", "Nectarine", "Plum", "Quince", "Banana", "Bramley apple",
           "Cherry", "Date", "Loganberry", "Peach", "Pomegranate")

# Identify herbs and/or seasoning and nuts
other <- c("Basil", "Chervil", "Garlic", "Mint", "Chestnut")

# Identify veg as remainder
veg <- setdiff(setdiff(fruit_and_veg, fruit), other)

# Add indicators for meat, fish, fruit and veg
table <- table %>% mutate(is_meat  = ifelse(produce %in% meat, T, F),
                 is_fish  = ifelse(produce %in% fish, T, F),
                 is_fruit = ifelse(produce %in% fruit, T, F),
                 is_veg   = ifelse(produce %in% veg, T, F),
                 is_other = ifelse(produce %in% other, T, F))

confirm_all_labelled <- function(data) {
  #' Throws error if not all entries are either meat, fish, fruit or veg
  count <- data %>% filter(!is_meat & !is_fish & !is_fruit & !is_veg & !is_other) %>% nrow()
  if (count != 0) { stop(call. = T) } else { cat("All entries are labelled as meat, fish, fruit, veg or other.\n")}
}
confirm_all_labelled(table)

# Add indicator for whether permitted as one of your five-a-day
# based on NHS website: https://www.nhs.uk/live-well/eat-well/5-a-day-what-counts/ (last accessed 2020-05-13)
# We exclude potato, yam, cassava:
excluded_starch <- c("Potato", "New potatoes", "Yam", "Cassava", "Plantain")
table <- table %>% mutate(is_five_a_day = ifelse(!is_meat & !is_fish & !is_other & !(produce %in% excluded_starch), T, F))

# Add indicator for whether produce is a bean
# According to the NHS website (https://www.nhs.uk/live-well/eat-well/beans-and-pulses-nutrition/), last accessed 2020-05-13:
# "One portion is 80g, which is equivalent to around 3 heaped tablespoons of cooked pulses.
# "But if you eat more than 3 heaped tablespoons of beans and pulses in a day, this still only counts as 1 portion of your 5 A Day.
# "This is because while pulses contain fibre, they don't give the same mixture of vitamins, minerals and other nutrients as fruit and vegetables.
# "This excludes green beans, such as broad beans and runner beans, which are counted as a vegetable and not a bean or pulse for 5 A Day."
# beans <- c() # No relevant beans in BBC Good Food data


#---- Save Data for Visualisation and Further Analysis ---- 
write_csv(table, file.path(OUTPUT_PATH, SEASONAL_OUTPUT))
