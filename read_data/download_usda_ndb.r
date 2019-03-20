# Use API to get the nutrient information and hopefully serving size for the different foods in NDB

api_key <- 'xOQ7mUVeXtMwTCZZ7EKdtmr2LKJKBGhwMXrN6FId'
api_url <- 'http://api.nal.usda.gov/ndb'

library(httr)
library(jsonlite)
library(tidyverse)

# Get lists of codes for nutrients and food items
# -----------------------------------------------

nutlist_raw <- map(c(0, 50, 100, 150), function(offset) {
  req <- GET(paste0(api_url, '/list?format=json&lt=n&sort=id&api_key=', api_key, '&offset=', offset))
  fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)
})

nutlist <- map_dfr(nutlist_raw, ~ .x$list$item)

max_offset <- 247300 # there are around 5000 sub-lists.
# this won't work as brute force because only a certain number can be downloaded at any one time. (3600 requests per hour)
foodlist_raw <- map(seq(0, max_offset, 50), function(offset) {
  req <- GET(paste0(api_url, '/list?format=json&lt=f&sort=id&api_key=', api_key, '&offset=', offset))
  message('Downloaded number ', 1 + offset/50, ' of ', 1 + max_offset/50)
  Sys.sleep(0.5) # Keeping it to around 1 request per second.
  fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)
})

foodlist <- map_dfr(foodlist_raw, ~ .x$list$item)

foodgrouplist_raw <- map(c(0), function(offset) {
  req <- GET(paste0(api_url, '/list?format=json&lt=g&sort=id&api_key=', api_key, '&offset=', offset))
  fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)
})
