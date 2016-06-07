fn_read_me_data <- function() {
  # load in merchandise exports and call it "df_exports"  (3630442)
  load("inputs/raw_data.rda")
 # Exports_By_Country$Harmonised_System_Description <- NULL
#  Exports_By_Country$Re_exports_NZD_fob <- NULL
 # Exports_By_Country$Re_exports_Qty <- NULL
  
  # rename the data frames
  df_me_exports <- Exports_By_Country; rm(Exports_By_Country)

    
    df_me_exports <- df_me_exports %>% 
    select(c(Date, Harmonised_System_Code, Total_Export_NZD_fob)) %>%
    rename(HS10 = Harmonised_System_Code) %>%
    mutate(Year = year(Date)) %>%
    mutate(Value = as.numeric(Total_Export_NZD_fob)) %>%
    mutate(Value = ifelse(is.na(Value), 0, Value)) %>%
    mutate(HS10 = as.character(HS10)) %>%
    filter(Value != 0) %>%
    select(Date, Year, HS10,  Value) %>%
    group_by(Date,Year, HS10) %>%  summarise(Total = sum(Value))
   


  return(df_me_exports)
}