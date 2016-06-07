fn_read_hs10_to_anzsic <- function()  {
  # purpose of this function is to read in a csv file; groom this so that the end result
  # is a HS10 to ANZSIC concordance file that is 14232 rows.

  # this is a helper function that changes: "98.83.48.3940" to "9883483940"
  # could be re-written with reg.ex
  source("R/fn_convert_to_HS10.R")
  
  df_concord <- read.csv("concordances/Concordance_HS_ANZSIC.csv", header = TRUE)
  df_concord_lu <- df_concord %>%  mutate(HS10 = fn_convert_to_HS10(HS10)) %>%  select(HS10, ANZSIC06)
  
  # identify duplicates
  df_duplicateHS10 <- df_concord_lu %>% group_by(HS10) %>% 
    summarise(count = n()) %>% filter(count != 1) %>% select(HS10) 
  
  # now eliminate duplicates
  df_concord_lu <- df_concord_lu %>% filter(!HS10 %in% df_duplicateHS10$HS10)
  
  # replace duplicates with specific values
  df_append <- data.frame(HS10 = df_duplicateHS10$HS10, ANZSIC06 = c("C189900", "Z999999", "Z999999"))
  
  # now attach back the three rows
  df_concord_lu <-  bind_rows(df_concord_lu, df_append)
  
  # check for uniqueness
  stopifnot(length(unique(df_concord_lu$HS10)) == nrow(df_concord_lu))
  
  return(df_concord_lu)

}