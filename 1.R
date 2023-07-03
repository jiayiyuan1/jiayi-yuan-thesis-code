rm(list=ls())
library(urca)
library(readrba)
library(FinTS)        # autocorrelogram - acf
library(fUnitRoots)   # ADF test - adfTest
library(vars)
library(readxl)

#import all variables
gdp<- read_excel("~/Desktop/log_activity_index.xlsx")
employment<- read_rba(series_id = "GLFSEPTSA") #already seasonally adjusted
creditcard <- read_rba(series_id = "CCCCSTTVSA")
debitcard<- read_rba(series_id = "CDCPTVSA")
cheques<- read_rba(series_id = "CCQCTVSA")

##--transfers
creditcardtransfer<- read_rba(series_id = "CCDEPDEPCTVSA")
debitcardtransfer<- read_rba(series_id = "CCDEPDEPDTVSA")
directentrypaymenttransfer<- read_rba(series_id = "CCDEPDEPTVSA")
npptransfer <- read_rba(series_id="CCDEPNPPTVSA")

##### add zero values for extending npptransfer time series
# Create a data frame with the desired time period and zero values
new_data <- data.frame(date = seq.Date(as.Date("2002-01-31"), 
                                       as.Date("2018-01-31"), 
                                       by = "month"), value = 0)

# Update npptransfer$value with the values from new_data$value based on date
npptransfer$value[npptransfer$date %in% new_data$date] <- new_data$value

# Replace missing values in the time period of interest with 0
time_period <- (npptransfer$date >= as.Date("2002-01-31")) & 
  (npptransfer$date <= as.Date("2018-01-31"))

npptransfer$value[time_period & is.na(npptransfer$value)] <- 0

# Merge the value columns from npptransfer and new_data based on date
npptransfer_new <- merge(npptransfer["date", "value"],
                         new_data, by = "date", all = TRUE)

# Replace missing values with zeros
npptransfer_new$value[is.na(npptransfer_new$value)] <- 0

# Remove the unnecessary value.y column
npptransfer_new <- npptransfer_new[, c("date", "value")]

# Rename the value column
colnames(npptransfer_new)[2] <- "value"

###combine 4 components of transfers and find its log-level time series
# Step 1: Combine the values from the three time series data frames into a matrix
transfers_matrix <- cbind.data.frame(creditcardtransfer$value, 
                                     debitcardtransfer$value, 
                                     directentrypaymenttransfer$value,
                                     npptransfer_new$value)

# Step 2: Convert the matrix to a data frame
transfers_df <- data.frame(transfers_matrix)

# Step 3: Create a new data frame with the combined time series data and the date/time column from one of the original data frames
transfers_ts <- data.frame(datetime = creditcardtransfer$date, 
                           values = transfers_df)

# Step 4: Use the log() function to find the logarithmic values of the combined time series data
log_transfers_values <- log(transfers_df)

# Convert the matrix of logarithmic values back into a data frame
log_transfers_df <- data.frame(log_transfers_values)

# Rename the columns of the log-transformed data frame
names(log_transfers_df) <- c("log_creditcardtransfer", 
                             "log_debitcardtransfer", 
                             "log_directentrypaymenttransfer",
                             "log_newplatformpaymenttransfer")

# Combine the log-transformed data with the datetime column from the original data
ltransfers <- data.frame(datetime = creditcardtransfer$date,
                         log_transfers_df)
# Replace -Inf values with 0 in the "log_newplatformpaymenttransfer" column
ltransfers$log_newplatformpaymenttransfer[is.infinite(ltransfers$log_newplatformpaymenttransfer)] <- 0

#define other variables in logarithm
##     gdp is already in log level
lemployment=log(employment$value)
lcreditcard=log(creditcard$value)
ldebitcard=log(debitcard$value)
lcheques=log(cheques$value)

#plot time series in logarithm levels
par(mfrow=c(2,3), mar=c(3,3,2,1))

gdp<- ts(gdp$value, start = c(1974,1),frequency = 12)
plot(gdp,
     main="Gross Domestic Product",
     ylab="Values (log)") 

employment<- ts(log(employment$value), start = c(1978,1),frequency = 12)
plot(employment,
     main = "Value of employment in Australia",
     ylab ="Values (log)")

TotalValue_creditcard<- ts(log(creditcard$value), start = c(1985,1),frequency = 12)
plot(TotalValue_creditcard,
     main = "Total value of creditcard transactions in Australia",
     ylab ="Values (log)")

TotalValue_debitcard<- ts(log(debitcard$value), start = c(2002,1),frequency = 12)
plot(TotalValue_debitcard,
     main = "Total value of debitcard transactions in Australia",
     ylab ="Values (log)")

TotalValue_cheques<- ts(log(cheques$value), start=c(2002, 1),frequency = 12)
plot(TotalValue_cheques,
     main = "Total value of cheques transactions in Australia",
     ylab ="Values (log)")

TotalValue_transfers <- ts(transfers_matrix, start = c(2002, 1), frequency = 12)
log_transfers <- log(TotalValue_transfers) # take the logarithm of the values
matplot(log_transfers, type = "l", lty = 1, 
        col = c("black", "blue", "green", "red"),
        main = "Total value of transfers in Australia",
        ylab = "Values (log)",
        xlab = "Time")
y_range <- range(log_transfers, na.rm = TRUE)
y_margin <- 0.1 * diff(y_range)
ylim <- c(y_range[1] - y_margin, y_range[2] + y_margin)
par(usr = c(par("usr")[1:2], ylim))
legend("topleft", legend = c("Credit transfer", "Debit transfer",
                             "Direct entry payment transfer", 
                             "New Platform Payment transfer"),
       col = c("black", "blue", "green", "red"), lty = 1,cex=0.35)


# time series for the sum of transfers
sum_transfers = ltransfers$log_creditcardtransfer+
  ltransfers$log_debitcardtransfer+
  ltransfers$log_directentrypaymenttransfer+
  ltransfers$log_newplatformpaymenttransfer
TotalValue_sum_transfers<- ts(sum_transfers, start = c(2002, 1), frequency = 12)
plot(TotalValue_sum_transfers,
     main="Total Value of the sum of transfers in Australia",
     ylab="Values(log)")


# the combined system for 6 variables
# download daily data
library(xts)
gdp_dwnld = readxl:: read_excel("~/Desktop/research report/gdp.xlsx")

# check if the first column is date
if (all(class(gdp_dwnld$Date) == "Date")){
  print("The first column is a date.")
} else {
  print("The first column is not a date.")
}
library(lubridate)
gdp_dwnld$Date <- ymd(gdp_dwnld$Date)
gdp_ = xts::xts(gdp_dwnld$value,gdp_dwnld$Date)

employment_dwnld = readrba:: read_rba(series_id = "GLFSEPTSA")
employment_ = xts::xts(log(employment_dwnld$value), employment_dwnld$date)

creditcard_dwnld = readrba:: read_rba(series_id = "CCCCSTTVSA")
creditcard_ = xts::xts(log(creditcard_dwnld$value), creditcard_dwnld$date)

debitcard_dwnld = readrba:: read_rba(series_id = "CDCPTVSA")
debitcard_ = xts::xts(log(debitcard_dwnld$value), debitcard_dwnld$date)

cheques_dwnld = readrba:: read_rba(series_id = "CCQCTVSA")
cheques_ = xts::xts(log(cheques_dwnld$value), cheques_dwnld$date)

creditcardtransfer_dwnld = readrba:: read_rba(series_id = "CCDEPDEPCTVSA")
creditcardtransfer_ = xts::xts(log(creditcardtransfer_dwnld$value), 
                               creditcardtransfer_dwnld$date)

debitcardtransfer_dwnld = readrba:: read_rba(series_id = "CCDEPDEPDTVSA")
debitcardtransfer_ = xts::xts(log(debitcardtransfer_dwnld$value), 
                              debitcardtransfer_dwnld$date)

directentrypaymenttransfer_dwnld = readrba:: read_rba(series_id = "CCDEPDEPTVSA")
directentrypaymenttransfer_ = xts::xts(log(directentrypaymenttransfer_dwnld$value), 
                                       directentrypaymenttransfer_dwnld$date)

npptransfer_dwnld =  readrba:: read_rba(series_id = "CCDEPNPPTVSA")
npptransfer_ = xts::xts(log(npptransfer_dwnld$value), 
                        npptransfer_dwnld$date)

transfers_ = xts::xts((ltransfers$log_creditcardtransfer+ 
                         ltransfers$log_debitcardtransfer+
                         ltransfers$log_directentrypaymenttransfer+
                         ltransfers$log_newplatformpaymenttransfer), 
                      ltransfers$datetime)
variables_all = na.omit(merge(gdp_, employment_, creditcard_, debitcard_, 
                              cheques_, transfers_))

colnames(variables_all)   = c("gdp", "employment", "credit card", "debit card",
                              "cheques", "transfers")

#  Create a covid dummy variable 
df <- as.Date(gdp_dwnld$Date)
COVID_dummy <- ifelse(df < as.Date("2020-01-31"), 0, 1)
covid <- data.frame(Date = df, value = COVID_dummy)
covid_matrix <- matrix(covid$value, ncol = 1)  # Convert to matrix
colnames(covid_matrix) <- c("covid")

# put covid into y matrix
covid_ = xts::xts(covid$value,covid$Date )
variables_all.1 = na.omit(merge(gdp_, employment_, creditcard_, debitcard_, 
                                cheques_, transfers_,covid_))
colnames(variables_all.1)   = c("gdp", "employment", "credit card", "debit card",
                                "cheques", "transfers","covid")

# Select the optimal lag order for the VAR model
VARselect(variables_all.1, lag.max = 12,
          type = "const",
          exogen = covid_matrix)

# all criteria suggests 4 lags.
p = 4
# Build the VAR model with the selected lag order
var_model <- VAR(variables_all.1, p = p, exogen = covid_matrix ) 
#summary(var_model) 

#diagnostics of heteroskedacity
#multivariate Portmanteau test
serial.test(x=var_model,lags.pt = p, type = "PT.asymptotic" )
arch.test(x=var_model, lags.single = 4, lags.multi = 10)

# Perform Johansen cointegration test
library(urca)
cointegration = ca.jo(variables_all.1, type="trace", 
                      ecdet="const", K = p, spec="transitory")
summary(cointegration)

# VECM model
# find the cointegrating rank
vecm_model<- ca.jo(variables_all.1, type="trace",
                   ecdet="const", K = p-1, spec="transitory")
summary(vecm_model)

# impluse response function
#convert VECM to VAR
var<- vec2var(vecm_model,r=4)

png("impulse_response_plots.png")
par(mfrow = c(2, 4))
irf.1 <- irf(var, impulse = "credit.card", response = "gdp", n.ahead = 40, ortho = TRUE,
             cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
plot(irf.1)

irf.2 <- irf(var, impulse = "debit.card", response = "gdp", n.ahead = 40, ortho = TRUE,
             cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
plot(irf.2)

irf.3 <- irf(var, impulse = "cheques", response = "gdp", n.ahead = 40, ortho = TRUE,
             cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
plot(irf.3)

irf.4 <- irf(var, impulse = "transfers", response = "gdp", n.ahead = 40, ortho = TRUE,
             cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
plot(irf.4)

irf.5 <- irf(var, impulse = "credit.card", response = "employment", n.ahead = 40, ortho = TRUE,
             cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
plot(irf.5)

irf.6 <- irf(var, impulse = "debit.card", response = "employment", n.ahead = 40, ortho = TRUE,
             cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
plot(irf.6)

irf.7 <- irf(var, impulse = "cheques", response = "employment", n.ahead = 40, ortho = TRUE,
             cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
plot(irf.7)

irf.8 <- irf(var, impulse = "transfers", response = "employment", n.ahead = 40, ortho = TRUE,
             cumulative = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
plot(irf.8)
dev.off()
