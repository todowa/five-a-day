

#---- Import Required Packages ----
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if(!require(emojifont)) install.packages("emojifont", repos = "http://cran.us.r-project.org")


#---- Global Variables ----
OUTPUT_PATH <- "./output/"
SEASONAL_INPUT <- "https://www.bbcgoodfood.com/seasonal-calendar/all"
SEASONAL_OUTPUT <- "seasonal_produce.csv"
EMOJI_PATH <- "https://emojipedia.org/search/?q="
# Set search terms
EMOJI_QUERYTERMS <- c("food", "fruit", "vegetables", "herbs", "meat", "fish",
                     "seafood", "salt", "pepper", "alien", "deer", "bird",
                     "onion", "berry", "cabbage", "crab", "nut", "corn", "oyster",
                     "pumpkin")
EMOJI_OUTPUT <- "emoji_list.csv"
DICT_OUTPUT <- "food_emoji_dictionary.csv"


#---- Scrape Seasonal Produce Data and Convert to Tidy Format ----
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


#---- Scrape Food Emojis ----

# Obtain food emoji unicode
foods <- lapply(EMOJI_QUERYTERMS, function(food) {
  read_html(paste0(EMOJI_PATH, food)) %>% html_nodes(".search-results h2") %>% html_text()
}) %>%
  unlist() %>%
  tibble::enframe(name = NULL) %>%
  separate(value, c("unicode", "name"), " ", extra = "merge") %>% 
  group_by(name, unicode) %>% count() %>% # Remove duplicates
  select(name, unicode)


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


#---- Prepare Food Emojis for Analysis ----

dictionary <- table %>% group_by(produce) %>% count() %>% select(produce) %>% mutate(name = produce) %>%
  mutate(name = ifelse(produce %in% c("Apple", "Bramley apple"), "Green Apple", name)) %>%
  mutate(name = ifelse(produce %in% c("Crab apple"), "Red Apple", name)) %>%
  mutate(name = ifelse(produce %in% c("Clementine", "Nectarine", "Orange", "Apricot"), "Tangerine", name)) %>%
  mutate(name = ifelse(produce %in% c("Cherry"), "Cherries", name)) %>%
  mutate(name = ifelse(produce %in% c("Beef"), "Cow", name)) %>%
  mutate(name = ifelse(produce %in% c("Goose"), "Duck", name)) %>%
  mutate(name = ifelse(produce %in% c("Blackcurrants", "Blackberry", "Cranberry", "Elderberries", "Tayberry",
                                      "Gooseberry", "Loganberry", "Redcurrant", "Raspberry"), "Blueberries", name)) %>%
  mutate(name = ifelse(produce %in% c("Venison"), "Deer", name)) %>%
  mutate(name = ifelse(produce %in% c("New potatoes", "Sweet potato", "Swede"), "Potato", name)) %>%
  mutate(name = ifelse(produce %in% c("Sweet potato", "Swede"), "Roasted Sweet Potato", name)) %>%
  mutate(name = ifelse(produce %in% c("Spring lamb", "Lamb"), "Ewe", name)) %>%
  mutate(name = ifelse(produce %in% c("Basil", "Mint", "Chervil", "Watercress"), "Herb", name)) %>%
  mutate(name = ifelse(produce %in% c("Pepper"), "Bell Pepper", name)) %>%
  mutate(name = ifelse(produce %in% c("Spring onion", "Garlic"), "Seedling", name)) %>%
  mutate(name = ifelse(produce %in% c("Grouse", "Guinea fowl"), "Rooster", name)) %>%
  mutate(name = ifelse(produce %in% c("Courgette", "Courgette flower"), "Cucumber", name)) %>%
  mutate(name = ifelse(produce %in% c("Salmon"), "Sushi", name)) %>%
  mutate(name = ifelse(produce %in% c("Pork"), "Pig", name)) %>%
  mutate(name = ifelse(produce %in% c("Mussels"), "Spiral Shell", name)) %>%
  mutate(name = ifelse(produce %in% c("Pumpkin"), "Jack-O-Lantern", name)) %>%
  mutate(name = ifelse(produce %in% c("Cauliflower", "Purple sprouting broccoli"), "Broccoli", name)) %>%
  mutate(name = ifelse(produce %in% c("Sweetcorn"), "Ear of Corn", name)) %>%
  mutate(name = ifelse(produce %in% c("Whiting", "Mackerel", "Tuna", "Cod", "Halibut", "Kipper"), "Fishing Pole", name)) %>%
  mutate(name = ifelse(produce %in% c("Swiss chard", "Asparagus", "White asparagus", "Kale", "Jerusalem artichoke",
                                      "Cavolo nero", "Spinach", "Globe artichoke", "Sorrel", "Chicory",
                                      "Celery", "Lamb's lettuce", "Cabbage", "Spring greens", "Leek",
                                      "Samphire", "Broad bean", "Runner bean", "Lettuce", "Mangetout",
                                      "Pak choi"), "Leafy Green", name)) %>%
  mutate(name = ifelse(produce %in% c("Aubergine"), "Eggplant", name)) %>%
  mutate(name = ifelse(produce %in% c("Salsify", "Turnip", "Parsnip", "Celeriac", "Radish", "Radicchio",
                                      "Fennel bulb", "Marrow", "Kohlrabi", "Brussels sprouts", "Beetroot"), "Alien", name)) %>%
  mutate(name = ifelse(produce %in% c("Damson", "Date", "Fig", "Grapefruit", "Quince", "Rhubarb", "Plum", "Peas",
                                      "Pomegranate"), "Alien Monster", name)) %>%
  mutate(name = ifelse(name %in% c("Blueberries", "Bell Pepper"), "T-Rex", name)) # Replace those emojis not yet in browser.


#---- Save Data for Visualisation and Further Analysis ---- 
write_csv(table, file.path(OUTPUT_PATH, SEASONAL_OUTPUT))
write_csv(foods, file.path(OUTPUT_PATH, EMOJI_OUTPUT))
write_csv(dictionary, file.path(OUTPUT_PATH, DICT_OUTPUT))
