count <- function(cause = NULL) {
## Check that "cause" is non-NULL; else throw error
if (length(cause) == 0) {
	stop("NULL cause");
}
## Check that specific "cause" is allowed; else throw error
if (!(cause == "asphyxiation" || cause == "blunt force"
|| cause == "other" || cause == "shooting" || cause == "stabbing" || cause == "unknown")) {
	stop("cause is not allowed");
}
## Read "homicides.txt" data file
homicides <- readLines("homicides.txt");

regQuery <- sprintf("Cause: [%s|%s]%s", substr(cause,1,1), toupper(substr(cause,1,1)), substr(cause,2,nchar(cause)));

## Extract causes of death
ans <- grep(regQuery, homicides);
## Return integer containing count of homicides for that cause
return (length(ans));
}
