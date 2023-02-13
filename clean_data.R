

# Load packages and data --------------------------------------------------

library(tidyverse)

dir.proj = file.path("~/NYC Bagels")


# Raw Bagel Data ----------------------------------------------------------


# Full Bagel Data - not clean, has all raw text/scores
dat_bagel = read_csv(file.path(dir.proj, "Bagel Spreadsheet - Master Data Table.csv")) %>%
  select(Store_Name = `Store Name`,
         Cross_Streets = `Cross Streets`,
         Neighborhood, Borough, Time,
         Chars_XY = `Keyword X-Y Points`,
         Score_Store = `Score...10`,
         Score_Bagel = `Score...12`,
         Score_Cheese = `Score...14`,
         Score_Total = `Total Average`)

# Extract the characteristics given the data in Chars_XY
get_chars <- function(str){
  strs = str_split(string = str, pattern = "\n", simplify = FALSE)[[1]]
  strs = gsub(pattern = "N/A", replacement = "na,na", x = strs)
  strs = gsub(pattern = "[[:alpha:]]+", replacement = "", x = strs, perl = TRUE)
  strs = gsub(pattern = "[(|)]", replacement = "", x = strs)
  strs = str_split(string = strs, pattern = ",", simplify = FALSE)
  
  # Positive = more contemporary, negative = more classic
  c_contemporary_classic = strs[[1]][1]
  # Positive = more variety, negative = more focused
  c_variety_focused = strs[[1]][2]
  
  # Positive = more crackly, negative = more chewy
  c_crackly_chewy = strs[[2]][1]
  # Positive = x-large, negative = small
  c_xlarge_small = strs[[2]][2]
  
  # Positive = topping dense, negative = topping light
  c_toppingdense_light = strs[[3]][1]
  # Positive = high salt, negative = low salt
  c_highsalt_low = strs[[3]][2]
  
  # Positive = fine scallion, negative = coarse scallion
  c_finescallion_coarse = strs[[4]][1]
  # Positive = dairy forward, negative = dairy latent
  c_dairyfrwrd_latent = strs[[4]][2]
  
  chars = data.frame(c_contemporary_classic = c_contemporary_classic,
    c_variety_focused = c_variety_focused,
    c_crackly_chewy = c_crackly_chewy,
    c_xlarge_small = c_xlarge_small,
    c_toppingdense_light = c_toppingdense_light,
    c_highsalt_low = c_highsalt_low,
    c_finescallion_coarse = c_finescallion_coarse,
    c_dairyfrwrd_latent = c_dairyfrwrd_latent) %>%
    mutate_all(as.numeric)
  
  return(chars)
}

dat_chars = dat_bagel$Chars_XY %>%
  map(get_chars) %>%
  Reduce(rbind,.)
dat_chars$Store_Name = dat_bagel$Store_Name

dat_bagel2 = dat_bagel %>%
  inner_join(dat_chars, by = "Store_Name")

saveRDS(dat_bagel2, file.path(dir.proj, "dat_bagel_cleaned.RDS"))

# NFT Data ----------------------------------------------------------------

# NFT Data - more clean, less granular
dat_nft0 = read_csv(file.path(dir.proj, "NFT Data Bagel Spreadsheet - Master Data Sheet.csv"))


