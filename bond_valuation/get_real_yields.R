
get_real_yields <- function(data_directory, output_directory) {
  
  # Preamble: This function imports and formats Eikon data
  # on real bonds for the UK, Japan, Canada, Germany, and the United States
  # Inputs:
  #   - data_directory: Directory containing raw Eikon bond price and profile data
  #   - output_directory: Directory where the processed data will be stored

  #------------------------------------------------------  
  # Get data
  #------------------------------------------------------
  options(stringsAsFactors = FALSE)
  price_file_path <- paste(data_directory, "Eikon_bondprices.csv", sep = "")
  profile_file_path <- paste(data_directory, "Eikon_bondprofile.csv", sep = "")
  bid <- read.csv(file = price_file_path, header = TRUE, sep = ",", check.names = FALSE) 
  profile <- read.csv(file = profile_file_path, header = TRUE, sep = ",")

  # Get YTM and other variables
  library(xts)
  library(rootSolve)

  # Define variables
  countries <- unique(profile$Domicile)
  bond_data <- list()

  # Delete all U.K./Great Britain real bonds before September 2005
  gb_index <- which(profile$Domicile == "GB")
  gb_first_coupon_dates <- as.Date(profile$First.Coupon.Date[gb_index], format = "%m/%d/%Y")
  delete_index <- which(gb_first_coupon_dates < as.Date("09/01/2005", format = "%m/%d/%Y"))

  if (length(delete_index) > 0) {
    profile <- profile[-delete_index, ]
    bid <- bid[, -delete_index]
  }

  # Extract Data from Profile
  rics <- profile$X.1
  first_coupon_dates <- as.Date(profile$First.Coupon.Date, format = "%m/%d/%Y")
  coupon_frequencies <- profile$Coupon.Frequency
  day_counts <- profile$Day.Count
  maturity_dates <- as.Date(profile$Maturity.Date, format = "%m/%d/%Y")
  price_factors <- profile$Price.Factor
  coupon_rates <- profile$Coupon.Rate
  n_countries <- length(countries)
  ts_dates <- as.Date(bid[, 1], format = "%Y-%m-%d") 

  # Take out date
  bid <- xts(bid[, -1], order.by = ts_dates) 

  # D1 month is the month on which the previous coupon was paid
  # D2 month is today's month (i.e., month that corresponds to t)
  # D3 month is the month on which the next coupon shall be paid
  profile$d1_month <- as.numeric(format(first_coupon_dates, "%m"))
  profile$d3_month <- (as.numeric(format(first_coupon_dates, "%m")) + 12 / coupon_frequencies) %% 12
  profile$d3_month <- replace(profile$d3_month, profile$d3_month == 0, 12)

  #------------------------------------------------------
  # Construct real curve for each country
  #------------------------------------------------------
  n_t <- dim(bid)[1]
  n_s <- dim(bid)[2]
  n_d <- dim(profile)[2]
  price_list <- list()
  ytm_list <- list()
  ttm_list <- list()
  real_ttm_list <- list()
  cb_profile_list <- list()
  cb_profile_mat_list <- list()

  for (i in countries) {
    index <- which(profile$Domicile == i)
    bond_data[[i]] <- rics[index] 
  }

  for (i in countries) {
    i_rics <- bond_data[[i]]
    n_rics <- length(i_rics)
    i_c <- NULL 

    for (r in 1:n_rics) {
      i_c <- c(i_c, which(colnames(bid) == i_rics[r]))
    }

    i_data <- bid[, i_c]
    price_list[[i]] <- matrix(NA, n_t, n_s) # dirty price
    ytm_list[[i]] <- matrix(NA, n_t, n_s)
    ttm_list[[i]] <- matrix(NA, n_t, n_s)
    real_ttm_list[[i]] <- matrix(NA, n_t, n_s)
    cb_profile_mat_list[[i]] <- matrix(NA, n_d, n_s)

    for (j in 1:n_rics) {
      b <- i_c[j]

      for (t in 1:n_t) {
        cp <- as.numeric(i_data[t, j])
        index <- i_c[j]

        if (cp < 50 | is.na(cp) | abs(cp) > 500) {
          cp <- 0
        }

        if (!cp == 0) {
          CF <- NULL
          t_date <- ts_dates[t]
          t_month <- as.numeric(format(t_date, "%m"))
          t_d_date <- as.numeric(format(t_date, "%d"))
          t_year_0 <- as.numeric(format(t_date, "%y"))

          if (t_year_0 < 50) {
            t_year <- 2000 + t_year_0
          }

          if (t_year_0 > 50) {
            t_year <- 1900 + t_year_0
          }

          d2 <- format(t_date, "%m/%d/%Y")

          d1_month <- as.numeric(profile$d1_month[index])
          d1_d_date <- as.numeric(format(first_coupon_dates[index], "%d"))
          d3_month <- as.numeric(profile$d3_month[index])

          if (coupon_frequencies[index] == 2) {
            if ((t_month < d1_month && t_month < d3_month) | (t_month > d1_month && t_month > d3_month)) {
              if (t_month < d1_month && t_month < d3_month) {
                if (d1_month < d3_month) {
                  d1 <- paste(d3_month, d1_d_date, t_year - 1, sep = "/")
                  d3 <- paste(d1_month, d1_d_date, t_year, sep = "/")
                }
                if (d1_month > d3_month) {
                  d1 <- paste(d1_month, d1_d_date, t_year - 1, sep = "/")
                  d3 <- paste(d3_month, d1_d_date, t_year, sep = "/") 
                }
              }
              if (t_month > d1_month && t_month > d3_month) {
                if (d1_month < d3_month) {
                  d1 <- paste(d3_month, d1_d_date, t_year, sep = "/")
                  d3 <- paste(d1_month, d1_d_date, t_year + 1, sep = "/")
                }
                if (d1_month > d3_month) {
                  d1 <- paste(d1_month, d1_d_date, t_year, sep = "/")
                  d3 <- paste(d3_month, d1_d_date, t_year + 1, sep = "/")
                }
              }
            }
          } else {
            if (d1_month < 6) {
              if (t_month > d1_month | (t_month == d1_month & t_d_date > d1_d_date)) {
                d1 <- paste(d1_month, d1_d_date, t_year, sep = "/")
                d3 <- paste(d3_month, d1_d_date, t_year, sep = "/")
              }
              if (t_month < d1_month | (t_month == d1_month & t_d_date < d1_d_date)) {
                d1 <- paste(d3_month, d1_d_date, t_year - 1, sep = "/")
                d3 <- paste(d1_month, d1_d_date, t_year, sep = "/")
              }
            }

            if (d1_month > 6) {
              if (t_month > d1_month | (t_month == d1_month & t_d_date > d1_d_date)) {
                d1 <- paste(d1_month, d1_d_date, t_year, sep = "/") 
                d3 <- paste(d3_month, d1_d_date, t_year + 1, sep = "/")
              }
              if (t_month < d1_month | (t_month == d1_month & t_d_date < d1_d_date)) {
                d1 <- paste(d3_month, d1_d_date, t_year - 1, sep = "/")
                d3 <- paste(d1_month, d1_d_date, t_year, sep = "/")
              }
            }

            if (d1_month == 6 & t_d_date < d1_d_date) {
              d1 <- paste(d3_month, d1_d_date, t_year, sep = "/")
              d3 <- paste(d1_month, d1_d_date, t_year, sep = "/")
            }

            if (d1_month == 6 & t_d_date > d1_d_date) {
              d1 <- paste(d1_month, d1_d_date, t_year, sep = "/") 
              d3 <- paste(d3_month, d1_d_date, t_year, sep = "/")
            }
          }
        }

        if (f == 0) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        } else {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365 * f
        }

        if (f == 0) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 1) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 2) {
          em <- as.yearmon(d2) - as.yearmon(d1)
          ed2 <- as.numeric(format(d1, "%d")) 
          ed1 <- as.numeric(format(d2, "%d"))

          if (ed2 >= ed1) {
            ed <- ed2 - ed1
          }

          if (ed1 > ed2) {
            ed <- ed1
          }

          em <- as.numeric(em)
          ed <- as.numeric(ed)
          days_elapsed <- em * 30 + ed
          ttm_0 <- days_elapsed / 360
        }

        if (f == 0) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 0) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 0) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 1) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 1) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 1) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 1) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365
        }

        if (f == 2) {
          ttm_0 <- as.numeric(maturity_dates[index] - d2) / 365 * f
        }

        root <- 0

        if (ttm_0 > 0) {
          if (!f == 0) {
            tryCatch(
              root <- uniroot(function(x) cr * pp * ((1 - 1 / (1 - x)^ttm_0) / -x) + pp / (1 - x)^ttm_0 - dp,
                               c(-1, 1))$root,
              error = function(e) {
                root <- 0
              }
            )
          } else {
            tryCatch(
              root <- uniroot(function(x) pp / (1 - x)^ttm_0 - dp, c(-1, 1))$root,
              error = function(e) {
                root <- 0
              }
            )
          }
        }

        if (ttm_0 < 0) {
          root <- 0
        }

        if (!f == 0) {
          ytm_list[[i]][t, b] <- -root * f
        } else {
          ytm_list[[i]][t, b] <- -root
        }

        ttm_list[[i]][t, b] <- ttm_0
        real_ttm_list[[i]][t, b] <- as.numeric(maturity_dates[index] - d2) / 365

        if (abs(ytm_list[[i]][t, b]) > 0.20 | ytm_list[[i]][t, b] == 0) {
          ytm_list[[i]][, b] <- NA
          ttm_list[[i]][, b] <- NA
          real_ttm_list[[i]][, b] <- NA
        }
      }
      cb_profile_list[[i]][b] <- list(profile[index, ])
    }
  }

  #------------------------------------------------------
  # Output
  #------------------------------------------------------
  filename3 <- paste(output_directory, "bond_data.RData", sep = "")
  save(bid, profile, ts_dates, ytm_list, ttm_list, real_ttm_list, cb_profile_mat_list, price_list, file = filename3)

  return(list(bid, profile, ts_dates, ytm_list, ttm_list, real_ttm_list, cb_profile_mat_list, price_list))
}