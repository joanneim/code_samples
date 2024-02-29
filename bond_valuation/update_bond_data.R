update_bond_data <- function(lookback_days = 10, output_directory = "O:/", data_directory = "Z:/hypothetical",
                             uk_data_directory = "U:/hypothetical/Real Bonds/Bond data/project/data/", include_uk_data = FALSE) {

    # Preamble: Function to update bond price and profile data
    # Inputs:
    #   - lookback_days: Number of days to look back for historical data (default is 10)
    #   - output_directory: Directory where the updated data will be stored
    #   - data_directory: Directory containing raw bond price and profile data
    #   - uk_data_directory: Directory containing UK-specific bond price data
    #   - include_uk_data: Boolean indicating whether to include UK-specific data (default is FALSE)

  #------------------------------------------------------
  # Real bond price data from automated pull
  #------------------------------------------------------

  # Read bond price data from CSV
  raw_bond_price_lines <- readLines(paste(data_directory, 'Eikon_bondprices.csv', sep=''))
  raw_bond_price_csv <- read.csv(paste(data_directory, 'Eikon_bondprices.csv', sep=''), header=FALSE)
  raw_bond_price_lines = raw_bond_price_lines[-1]
  bond_prices_data <- read.csv(textConnection(raw_bond_price_lines), header=TRUE, stringsAsFactors=FALSE, na.strings=c("#N/A"))

  #------------------------------------------------------
  # Get RIC names
  #------------------------------------------------------

  # Extract RIC names and dates from the CSV
  ric_index <- which(!raw_bond_price_csv[, 1]=="")
  ric_names <- raw_bond_price_csv[ric_index, 1][-1]
  ric_index2 <- c(1:length(bond_prices_data[, 3]))[!bond_prices_data[, 3]=="" && !is.na(bond_prices_data[, 3])]
  ric_dates <- as.Date(bond_prices_data[ric_index2, 3], format="%m/%d/%Y")

  #------------------------------------------------------
  # Format
  #------------------------------------------------------

  # Remove unnecessary columns
  bond_prices_data <- bond_prices_data[, c(-1, -2, -3)]

  # Create a zoo object with dates as index and RIC names as columns
  prices_data_zoo <- zoo(bond_prices_data, order.by=ric_dates)
  colnames(prices_data_zoo) <- ric_names

  if (include_uk_data) {
    #----------------------------------------------
    # Get UK data if specified
    # UK data retrieved from automated pull of UKDOM's web-published gilt prices; these data are in a special form
    # and so must be distinguished from EIKON data
    #------------------------------------------------------

    # Read UK gilt price data from CSV
    uk_gilt_prices <- read.csv(paste(uk_data_directory, 'UKDOM_giltprices.csv', sep=''), header=TRUE)
    uk_gilt_prices <- unique(uk_gilt_prices)

    isin_codes <- unique(uk_gilt_prices$ISIN.Code)
    data_counter <- 0

    for (i in 1:length(isin_codes)) {
      isin_code <- isin_codes[i]
      isin_index <- which(uk_gilt_prices$ISIN.Code == isin_code)
      date_values <- as.Date(uk_gilt_prices$Close.of.Business.Date[isin_index], "%Y-%m-%d")
      price_values <- uk_gilt_prices$Clean.Price[isin_index]

      valid_index <- which(!is.na(price_values))
      if (length(isin_index) > 1) {
        uk_data <- zoo(price_values[valid_index], order.by=date_values[valid_index])

        if (index(uk_data)[1] > as.Date("2005-01-01", format="%Y-%m-%d")) {
          if (length(uk_data) > 100) {
            if (data_counter == 0) {
              uk_prices_data <- uk_data
              selected_isin_indices <- i
            }
            if (data_counter > 0) {
              uk_prices_data <- merge(uk_prices_data, uk_data)
              selected_isin_indices <- c(selected_isin_indices, i)
            }
            data_counter <- data_counter + 1
          }
        }
      }
    }

    # Name the frame
    selected_isin_codes <- isin_codes[selected_isin_indices]
    write.csv(selected_isin_codes, file="Z:/hypotheticaleikontofame_ukidentifiers.csv", row.names=FALSE)
    dimnames(uk_prices_data)[[2]] <- selected_isin_codes

    # Merge UK data with others
    combined_data <- merge(uk_prices_data, prices_data_zoo)
    colnames(combined_data) <- c(colnames(uk_prices_data), colnames(prices_data_zoo))
    all_dates <- index(combined_data)
    number_of_dates <- length(all_dates)
  } else {
    # If include_uk_data is FALSE, use only the data from others
    combined_data <- prices_data_zoo
    all_dates <- index(combined_data)
    number_of_dates <- length(all_dates)
  }

  if (is.null(lookback_days)) {
    selected_data <- combined_data
  } else {
    # Subset the data based on the specified lookback period
    selected_data <- combined_data[(number_of_dates - lookback_days):number_of_dates,]
  }

  #------------------------------------------------------
  # Output price data
  #------------------------------------------------------

  # Extract the identifiers from the selected_data
  selected_identifiers <- colnames(selected_data)
  write.csv(selected_identifiers, file="Z:/hypotheticaleikontofame_identifiers.csv", row.names=FALSE)

  # Output price data
  write.zoo(selected_data, file=paste(output_directory, "Eikon_bondprices.csv", sep=""), sep=",")

  #------------------------------------------------------
  # Get profile data
  #------------------------------------------------------

  # Read bond profile data from CSV
  raw_bond_profile_lines <- readLines(paste(data_directory, 'Eikon_bondprofile.csv', sep=''))
  raw_bond_profile_csv <- read.csv(paste(data_directory, 'Eikon_bondprofile.csv', sep=''), header=FALSE)
  raw_bond_profile_lines = raw_bond_profile_lines[-1]
  bond_profile_data <- read.csv(textConnection(raw_bond_profile_lines), header=TRUE, stringsAsFactors=FALSE, na.strings=c("#N/A"))

  #------------------------------------------------------
  # Get rid of all extra columns
  #------------------------------------------------------

  # Remove unnecessary columns
  bond_profile_data <- bond_profile_data[, c(-1, -2)]

  # Output profile data
  write.csv(bond_profile_data, file=paste(output_directory, "Eikon_bondprofile.csv", sep=""), row.names=FALSE)
}


