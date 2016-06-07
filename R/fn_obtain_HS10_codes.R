fn_obtain_HS10_codes <- function(a_df_concord_lu, a_df_tech_lu, int_row_num) {	
	# this function is passed 3 parameters:
	# 1) "a_df_concord_lu" - a look up table of HS10 and ANZSIC codes
	# 2) "a_df_tech_lu" - a look up table of ANZSIC patterns and high tech classifications
	# 3) "int_row_num" - the current row number of a_df_tech_lu

	# function operation
	# the function is called with a specific int_row_num. The function then
	# extract an ANZSIC pattern from "a_df_tech_lu" and then constructs a query which
	# is run against "a_df_concord_lu". The query then returns the HS10 codes associated 
	# with a specific set of ANZSIC numbers.


	# create an anzsic filter pattern based on the current row of "a_df_tech_lu"
	chr_anzic_pattern <- paste0("^", a_df_tech_lu[int_row_num, "sub_anzsic"] )
	
	# create a vector of HS10 codes associated with the anzsic pattern
	df_HS10 <- a_df_concord_lu %>% 
		filter(grepl(chr_anzic_pattern, ANZSIC06)) %>% select(HS10)
		
	vct_repeated_rows <- rep(int_row_num, nrow(df_HS10))
	# repeated the current row of a_df_tech_lu, nrow(df_HS10) number of times
	# club two data frames together
	rtn_df <- bind_cols(df_HS10, a_df_tech_lu[vct_repeated_rows,])
	return(rtn_df)
}