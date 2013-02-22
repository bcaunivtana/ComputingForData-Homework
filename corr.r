corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
		result <- c()
		for (id in 1:332) {
			data <- getmonitor(id, directory)
			complete <- data[complete.cases(data),]
			if (nrow(complete) > threshold)	
				result <- append(result, cor(complete$nitrate, complete$sulfate))
		}
		return(result)
}

