best <- function(state, rankArea) {
	## Read outcome data
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## Check that state and outcome are valid
	outcome <- outcome[outcome$State == state,]
	if (nrow(outcome) == 0)
		stop("invalid state")
	
	
	
	data <- NULL
	if (rankArea == "heart attack") {
		outcome <- outcome[complete.cases(outcome[,11]),]
		outcome[,11] <- as.numeric(outcome[,11])
		attach(outcome)
		data <- outcome[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name), ]
	} else if (rankArea == "heart failure") {
		outcome <- outcome[complete.cases(outcome[,17]),]
		outcome[,17] <- as.numeric(outcome[,17])
		attach(outcome)
		data <- outcome[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name), ]
	} else if (rankArea == "pneumonia") {
		outcome <- outcome[complete.cases(outcome[,23]),]
		outcome[,23] <- as.numeric(outcome[,23])
		attach(outcome)
		data <- outcome[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name), ]
	} else
		stop("invalid outcome")
	
	return (data[1,2])
	
	## Return hospital name in that state with lowest 30-day death
	## rate
}