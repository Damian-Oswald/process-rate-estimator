#' @title Get the optimal bandwidth parameters via hyperparameter tuning
#' 
#' @description
#' This function automatically optimizes the bandwidth for a specified combination of variables.
#' 
#' @param factors A character vector with variable names. Hyperparameters will be found for every combination of the specified variables.
#' @param data Data in a similar format as the [measurements] data.
#' 
#' @export
hypertune <- function(x = "N2O", data = PRE::measurements, factors = c("column", "depth"), bandwidths = 10^seq(1,4), k = 3, r = 10) {
    
    # create data frame to write results to
    results <- expand.grid(variable = x,
                           column = if("column"%in%factors) sort(unique(data[,"column"])) else NA,
                           depth = if("depth"%in%factors) sort(unique(data[,"depth"])) else NA,
                           bandwidth = bandwidths)
    results <- results[,apply(results, 2, function(x) all(!is.na(x)))]
    results$cost <- NA
    
    #' Define model evaluation function `FUN`
    FUN <- function(x_train, x_test, y_train, y_test, hyperparameter) {
        
        # Turn off messages by the npreg function
        options(np.messages = FALSE)
        
        # Create two data frames `train` and `test`
        train <- data.frame(x = x_train, y = y_train)
        test <- data.frame(x = x_test, y = y_test)
        
        # Calibrate the model given the `train` data frame
        model <- np::npreg(y ~ x, data = train, bws = hyperparameter)
        
        # Evaluate the model on the `test` data frame
        y_hat <- predict(model, newdata = test)
        
        # Return the Cost function value, here the Root Mean Squared Error (RMSE)
        sqrt(mean((y_hat - y_test)^2))
    }
    
    cat("\nSTARTING HYPERTUNING\n")
    
    #' Run all cross-validations
    for (i in 1:nrow(results)) {
        
        # create masks for column and depth, if wished
        columnmask <- if("column"%in%factors) data[,"column"] == results[i,"column"] else rep(TRUE, nrow(data))
        depthmask <- if("depth"%in%factors) data[,"depth"] == results[i,"depth"] else rep(TRUE, nrow(data))
        
        # Cache a subset of the data for this cross-validation
        subset <- na.omit(data[columnmask & depthmask, c("date",as.character(results[i,"variable"]))])
        
        # Run cross-validation
        cv <- PRE::crossValidate(FUN = FUN, 
                                 x = as.matrix(as.numeric(subset[,"date"])),
                                 y = as.matrix(subset[,as.character(results[i,"variable"])]),
                                 hyperparameter = results[i,"bandwidth"],
                                 k = k,
                                 r = r)
        
        # Calculate mean cost of all repeated model calibrations
        results[i,"cost"] <- mean(cv[,"cost"], na.rm = TRUE)
        
        # Print the progress
        PRE::progressbar(i, nrow(results))
    }
    
    class(results) <- "hyperParameterTable"
    
    return(results)
}

H = hypertune(x = c("N2O", "SP"), data = PRE::measurements, factors = NULL, bandwidths = 10^seq(1,4,0.25))
H[with(H, tapply(cost, variable, which.min)),]

