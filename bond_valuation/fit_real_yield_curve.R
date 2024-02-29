fit_real_yield_curve<- function(t0 = NULL, tn = NULL, country_abbrev = "US", seed = NULL, seed2 = NULL,
                      lower_bounds = c(-50, -50, -50, -50, 0.01, 5),
                      upper_bounds = c(75, 75, 75, 75, 5.5, 20), method = "NSS",
                      lambda_priors = c(5, 19), lambda_weights = c(0, 0), force_prior = FALSE) {
  
  # Function: This function estimates the NSS parameters by minimizing
  #           the sum of squared deviations of fitted prices from actual prices,
  #           weighted by duration
  
  # Inputs:
  # t0: time index for time at which you want to *start* estimating NSS parameters
  # tn: time index for time at which you want to *end* estimation of NSS parameters
  # country_abbrev: Eikon country abbreviation
  # seed: vector to initialize the second search
  # seed2: vector to initialize the third search. Ideally, a large distance away from "seed".
  # lower_bounds: lower bound on parameter space
  # upper_bounds: upper bound on parameter space
  # method: "NSS" - Nelson Siegel Svensson, "NS" - Nelson Siegel
  # lambda_priors: center prior on lambda_priors
  # lambda_weights: weights on lambda_priors (scale sd)
  # force_prior: Whether to force prior
  
  # Dependencies for csminwelNew() can be downloaded at: http://www.princeton.edu/~sims/
  
  library(lubridate)
  library(stats)
  library(zoo)
  
  country_abbrev = toupper(country_abbrev)
  countries = unique(profile$Domicile)
  num_countries = length(countries)
  residuals = list()
  ts_start_dates = matrix(0, num_countries, 2)
  
  
  #------------------------------------------------------
  # Loop through countries
  #------------------------------------------------------
  sc = which(countries == country_abbrev)
  sn = sc
  
  if (is.null(t0)) {
    t0 = 1
  }
  
  if (is.null(tn)) {
    tn = length(tsdates)
  }
  
  for (iC in sc:sn) {
    if (method == "NS") {
      nP = 4
    }
    if (method == "NSS") {
      nP = 6
    }
    
    nT = dim(price[[1]])[1]
    start = 0
    residuals[[iC]] = list(1:nT)
    mparam = array(NA, c(num_countries, nT, 6))  # regardless of t0, tn, mparam array has the same dimensions
    
    iC_ttm = ttm[[iC]]
    iC_real_ttm = realttm[[iC]]
    iC_ytm = ytm[[iC]]
    iC_price = price[[iC]]

    #------------------------------------------------------
    # Loop through time
    #------------------------------------------------------
    for (t in t0:tn) {
      
      # Here's where we cut by maturity
      p <- NULL
      index <- which(iC_real_ttm[t, ] > 1.5)
      ittm <- as.numeric(iC_ttm[t, index])
      ireal_ttm <- iC_real_ttm[t, index]
      y <- as.numeric(iC_ytm[t, index])
      p <- iC_price[t, index]
      
      maturity_dates = profile[index, ]$Maturity.Date
      cfs = profile[index, ]$Coupon.Frequency
      crs = profile[index, ]$Coupon.Rate / 100
      pps = profile[index, ]$Price.Factor
      today_date = tsdates[t]
      max_cfs = max(cfs)
      
      if (length(p) > 0) {
        p <- na.omit(as.vector(p))
      }
      
      # Choose initial conditions for the first optimization
      ndf = length(p)
            
        #------------------------------------------------------
        # Choose the seeed to initialize search for local optimum
        #------------------------------------------------------
        if (is.null(seed)) {
        # Initial seed from Sack et.al's work on *US* TIPS yields
        # It turns out to be a good seed for our country set.
        # Except lambda 2 which we can tweak. Changed lambda 1 also to 1.5.
        seed <- c(0.00000769, -0.85479195, 1.45657591, 7.31571326, 1.5, 12.41591914)
        }

        if (is.null(seed2)) {
        # Default seed2 was set about experimenting with various
        # other seeds. Seed2 turns out to be a good seed for our country set.
        seed2 <- c(50, 50, 50, -50, 5, 19)
        }

        init <- NA
        error = 0

        if (start == 0) {
        ts_start_dates[iC, 1] <- tsdates[t]
        ts_start_dates[iC, 2] <- t
        }

        # Sufficient degrees of freedom?
        if (ndf >= nP) {
        
        if (start == 1) {
            init <- mparam[iC, t - 1, ]
            
            if (is.na(init)) {
            for (e in 1:5) {
                if (is.na(init)) {
                init <- mparam[iC, t - e, ]
                
                if (e == 5) {
                    init <- seed
                }
                }
            }
          }
        }
        
        if (start == 0) {
          start = 1
          init <- seed
        }
     
        #------------------------------------------------------
        # Define important matrices
        #------------------------------------------------------   
        # cpyears_matrix: Indexed by coupon period number and bond number.
        #                 Given today's date, the matrix is populated
        #                 with the time to maturity of that coupon
        #                 payment (last coupon payment is coupon + principal)
        #                 cppayment_matrix: indexed by coupon period number and bond number.
        #                 gives the value of the coupon payment for that bond
        #                 at that period number.
        # duration: indexed by bond. Is the Macaulay duration.
        snB = length(maturity_dates)
        snT = ceiling(max(new_interval(today_date, maturity_dates) / years(1))) * max_cfs
        
        # Create matrices
        cpyears_matrix <- matrix(0, snB, snT)
        cppayment_matrix <- matrix(0, snB, snT)
        duration <- array()
        y_fitted <- array()
        
        # Populate matrices
        for (k in 1:snB) {
          maturity_date = mdy(maturity_dates[k])
          snTB = ceiling(max(new_interval(today_date, maturity_date) / years(1))) * cfs[k]
          cpyears = matrix(0, 1, snTB)
          cppayments = matrix(0, 1, snTB)
          
          for (j in 1:snTB) {
            if (j == 1) {
              cp_date = maturity_date
              tau = new_interval(today_date, cp_date) / years(1)
              cpyears[1, j] = tau
              cppayments[1, j] = pps[k] + crs[k] * pps[k] / cfs[k]
            } else {
              cp_date = maturity_date - months((j - 1) * (12 / cfs[k]))
              if (cp_date > today_date) {
                tau = new_interval(today_date, cp_date) / years(1)
                cpyears[1, j] = tau
                cppayments[1, j] = crs[k] * pps[k] / cfs[k]
              }
            }
          }
          
          duration0 = sum(cpyears * cppayments / ((1 + y[k]) ^ cpyears))
          duration0 = duration0 / sum(cppayments / ((1 + y[k]) ^ cpyears))
          duration[k] <- duration0 / (1 + (y[k] / cfs[k]))
        }
        
        duration = duration / sum(duration)  # transform into true weights
        
       
        #------------------------------------------------------
        # Choose curve parameters that optimize fit
        #------------------------------------------------------
        if (method == "NSS") {
          init1 = init
          
          if (force_prior == TRUE) {
            init1 = c(init1[1:4], lambda_priors[1])
          }
          
          test = optim(fn = NSS, par = init1, method = "L-BFGS-B", prevbetaV = prevbetaV, lower = lower_bounds,
                       upper = upper_bounds, pit = p, today_date = today_date, maturity_dates = maturity_dates,
                       max_cfs = max_cfs, cpyears_matrix = cpyears_matrix, cppayment_matrix = cppayment_matrix,
                       duration = duration, lambda_priors = lambda_priors, lambda_weights = lambda_weights,
                       force_prior = force_prior, verbosenss = FALSE)
        }
        
        if (method == "NS") {
          init1 = c(init[1:3], init[5])
          prevbetaV = init1
          
          if (force_prior == TRUE) {
            init1 = c(init1[1:3], lambda_priors[1])
          }
          
          lower_ns = c(lower_bounds[1:3], lower_bounds[5])
          upper_ns = c(upper_bounds[1:3], upper_bounds[5])
          
          test = optim(fn = NS, par = init1, method = "L-BFGS-B", lower = lower_ns, upper = upper_ns,
                       prevbetaV = prevbetaV, pit = p, today_date = today_date, maturity_dates = maturity_dates,
                       max_cfs = max_cfs, cpyears_matrix = cpyears_matrix, cppayment_matrix = cppayment_matrix,
                       duration = duration, lambda_priors = lambda_priors, lambda_weights = lambda_weights,
                       force_prior = force_prior, verbosenss = FALSE)
        }
        
        # Store estimation results
        if (!is.null(test$par)) {
          if (method == "NSS") {
            mparam[iC, t, ] <- test$par
            est <- NSS(betaV = test$par, prevbetaV = prevbetaV, pit = p, today_date = today_date,
                       maturity_dates = maturity_dates, max_cfs = max_cfs, cpyears_matrix = cpyears_matrix,
                       cppayment_matrix = cppayment_matrix, duration = duration, lambda_priors = lambda_priors,
                       lambda_weights = lambda_weights, force_prior = force_prior, verbosenss = TRUE)
          }
          
          if (method == "NS") {
            mparam[iC, t, c(1:3, 5)] <- test$par
            mparam[iC, t, c(4, 6)] <- c(0, 0)
            est <- NS(betaV = test$par, prevbetaV = prevbetaV, pit = p, today_date = today_date,
                      maturity_dates = maturity_dates, max_cfs = max_cfs, cpyears_matrix = cpyears_matrix,
                      cppayment_matrix = cppayment_matrix, duration = duration, lambda_priors = lambda_priors,
                      lambda_weights = lambda_weights, force_prior = force_prior, verbosenss = TRUE)
          }
          
          # Convert fitted prices (pfitted) to yields (yfitted)
          pfitted = est$fitted
          duration = est$duration
          param = est$betaV
          
          for (k in 1:snB) {
            tryCatch(
              root <- uniroot(function(x) (crs[k] / cfs[k]) * pps[k] * ((1 - 1 / (1 - x)^ittm[k]) / -x) +
                                   pps[k] / (1 - x)^ittm[k] - pfitted[k], c(-1, 1))$root, error = 0
            )
            y_fitted[k] = -root * cfs[k]  # gives us the annual frequency
          }
          
          # Save residuals
          res = (est$actual - est$fitted)
          res_yields = y - y_fitted
          
          residuals[[iC]][[t]] <- list(p_actual = est$actual, p_fitted = est$fitted, y = y, y_fitted = y_fitted,
                                        res = res, res_yields = res_yields, duration = duration,
                                        tau = cpyears_matrix[, 1], param = param)
        }
      }
    }
  }
  
  settings = list(seed = seed, seed2 = seed2, lower = lower_bounds, upper = upper_bounds, method = method,
                  lambda_priors = lambda_priors, lambda_weights = lambda_weights, force_prior = force_prior)
  
  #------------------------------------------------------
  # Return
  #------------------------------------------------------
  return(list(residuals = residuals, mparam = mparam, tsdates = tsdates, countries = countries, settings = settings))
}