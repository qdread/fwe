# Read serving size data and match with LAFA data

fp_usda <- file.path(ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data'), 'USDAnutrients')

servsize <- read.csv(file.path(fp_usda, 'brandedfood/Serving_size.csv'), stringsAsFactors = FALSE)
# Water content data file contains lafa matching columns
watercontent <- read.csv(file.path(fp_usda, 'watercontent20mar2019_edited.csv'), stringsAsFactors = FALSE, skip = 6)
watercontent_old <- read.csv(file.path(fp_usda, 'watercontent.csv'), stringsAsFactors = FALSE, skip = 6)

# Check matches
descnew <- watercontent$Description
descold <- watercontent_old$Description
all(descold == descnew)
all(descold %in% descnew)
# They match but are in different orders.

# Correct the watercontent new number
watercontent <- watercontent %>%
  mutate(NDB_NO = as.integer(gsub('"', '', NDB_NO))) %>%
  left_join(watercontent_old)

watercontent %>% filter(!LAFA.equivalent %in% '')

write.csv(watercontent, file.path(fp_usda, 'watercontent_combined.csv'), row.names = FALSE)
