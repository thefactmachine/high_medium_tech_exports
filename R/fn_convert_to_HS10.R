fn_convert_to_HS10 <- function(a_string) {
# this function takes a value such as: "0101.10.00.24"
# and removes the periods (i.e ".") and returns: "0101100024"
	str1 <- substr(a_string, 1, 4)
	str2 <- substr(a_string, 6, 7)
	str3 <- substr(a_string, 9, 10)
	str4 <- substr(a_string, 12, 13)
	str_return <- paste0(str1, str2, str3, str4)
	return(str_return)	
}
