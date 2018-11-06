# Match categories of LAFA, FAOSTAT, and USEEIO (NAICS)

fao_crops_names <- unique(productionqty_crops_tidy$Item)
fao_cropsprocessed_names <- unique(productionqty_cropsprocessed_tidy$Item)
fao_livestockprimary_names <- unique(productionqty_livestockprimary_tidy$Item)
fao_livestockprocessed_names <- unique(productionqty_livestockprocessed_tidy$Item)

fao_price_names <- unique(producerpriceannual_tidy$Item)

# Names not in price table
setdiff(fao_crops_names, fao_price_names)
setdiff(fao_cropsprocessed_names, fao_price_names)
setdiff(fao_livestockprimary_names, fao_price_names)
setdiff(fao_livestockprocessed_names, fao_price_names)
# Basically the producer price only has the crops.

# LAFA
lafa_dairy_names <- sort(unique(dairy$Category))
lafa_fat_names <- sort(unique(fat$Category))
lafa_fruit_names <- sort(unique(fruit$Category))
lafa_grain_names <- sort(unique(grain$Category))
lafa_meat_names <- sort(unique(meat$Category))
lafa_sugar_names <- sort(unique(sugar$Category))
lafa_veg_names <- sort(unique(veg$Category))

# NAICS codes
bea_eeio_table <- read.csv('Q:/IO_tables/USEEIO/bea_useeio_lookuptable.csv', stringsAsFactors = FALSE)

# Write all three to a spreadsheet for manual cross referencing.
fao_item_names <- unique(c(fao_crops_names, fao_cropsprocessed_names, fao_livestockprimary_names, fao_livestockprocessed_names))
lafa_category_names <- unique(c(lafa_dairy_names, lafa_fat_names, lafa_fruit_names, lafa_grain_names, lafa_meat_names, lafa_sugar_names, lafa_veg_names))

write.csv(data.frame(fao_item_name = fao_item_names), file = 'Q:/crossreference_tables/fao_item_names.csv', row.names = FALSE)
write.csv(data.frame(fao_price_name = fao_price_names), file = 'Q:/crossreference_tables/fao_price_names.csv', row.names = FALSE)
write.csv(data.frame(lafa_category_name = lafa_category_names), file = 'Q:/crossreference_tables/lafa_category_names.csv', row.names = FALSE)

# Load the cross-referenced item names and add price names to this.
fao_item_names_crossref <- read.csv('Q:/crossreference_tables/fao_item_names.csv', stringsAsFactors = FALSE)
extranames <- setdiff(fao_price_names, fao_item_names_crossref$fao_item_name) # things in a not in b.

write.table(data.frame(fao_item_name = extranames), file = 'Q:/crossreference_tables/fao_item_names.csv', append = TRUE, row.names = FALSE, col.names = FALSE)