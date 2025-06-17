# set options for R markdown knitting
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(linewidth=80)

# set up line wrapping in MD knit output
library(knitr)
hook_output = knit_hooks$get("output")
knit_hooks$set(output = function(x, options) 
{
  # this hook is used only when the line width option is not NULL
  if (!is.null(n <- options$linewidth)) 
  {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) 
      x = strwrap(x, width = n)
    x = paste(x, collapse = "\n")
  }
  hook_output(x, options)
})

options(repos = c(CRAN = "https://cran.rstudio.com/"))

#### Install packages
install.packages("data.table")
# install.packages("tinytex")
# tinytex::install_tinytex(force = TRUE)

# --- Step 1: Load required libraries ---
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)

# Setting plot themes to format graphs
theme_set(theme_bw())

#### Point the filePath to where you have downloaded the datasets to and 
#### assign the data files to data.tables
filePath <- "C:/Users/User/OneDrive - Swinburne University/Desktop/forage/quantium/" 
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))

class(transactionData)

# --- Step 2: Exploratory data analysis ---
# --- 2.1 Examining transaction data ---
#### (1) Examining transaction data
str(transactionData) # check format of each column & see a sample of the data
head(transactionData) # look at the first 6 rows

### Convert DATE column from an integer to a date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30") 
# CSV and Excel integer dates begin on 30 Dec 1899

#### (2) Examining PROD_NAME, to see if we are looking for "chip"
colnames(transactionData)
# Count occurrences of each product name
transactionData[, .N, by = PROD_NAME]

# Examine the words in PROD_NAME to see if there are any incorrect entries
# such as products that are not chips
productWords <- data.table(
  word = unlist(strsplit(unique(transactionData[, PROD_NAME]), " "))
)
setnames(productWords, 'words')

# Keep only product words that relate to chip
# Remove digits
productWords <- productWords[grepl("\\d", words) == FALSE, ]
# Remove special characters
productWords <- productWords[grepl("[:alpha:]", words), ]
# Find the most common words
productWords[, .N, words][order(N, decreasing = TRUE)]
# Remove salsa products
# because the word "salsa" appears in the PROD_NAME column of the transactionData
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

#### (3) Check summary statistics to check for nulls and possible outliers
summary(transactionData)
# There are no nulls in the columns, but the product quantity of 200 packets 
  # appears to be an outlier due to its significantly higher value compared to 
  # the mean of 1.907 and the third quartile of 2.000, indicating an unusually 
  # large transaction that warrants further investigation.

# Filter the dataset to find the outlier
transactionData[PROD_QTY == 200, ]
# There are two transactions where 200 packets of chips are bought in one transaction
  # and both of these transactions where by the same customer.

# Check if the customer has had other transactions
transactionData[LYLTY_CARD_NBR == 226000, ]
# It looks like this customer has only had the two transactions over the year 
  # and is not an ordinary retail customer. The customer might be buying chips 
  # for commercial purposes instead. We’ll remove this loyalty card number from further analysis.

# Filter out the customer based on the loyalty card number
transactionData <- transactionData[LYLTY_CARD_NBR != 226000, ]
# Re‐examine transaction data
summary(transactionData)

# Check the number of transaction lines over time to see if there are any 
  # obvious data issues such as missing data.

# Count the number of transactions by date
transactionData[, .N, by = DATE]

# There are only 364 dates, which indicates a missing date.
# Create a sequence of dates from 1 Jul 2018 to 30 Jun 2019 and use this to create 
  # a chart of number of transactions over time to find the missing date.

# Create a sequence of dates and join this the count of transactions by date.
allDates <- data.table(seq(as.Date("2018/07/01"), as.Date("2019/06/30"), by = "day"))
setnames(allDates, "DATE")
transactions_by_day <- merge(allDates, transactionData[, .N, by = DATE], all.x = TRUE)

# Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# Plot transactions over time
ggplot(transactions_by_day, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# There is an increase in purchases in December and a break in late December. 
# Filter to December and look at individual days
ggplot(transactions_by_day[month(DATE) == 12, ], aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time (December)") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# The increase in sales occurs in the lead-up to Christmas and that there are zero sales
  # on Christmas day itself due to the shop close.
# So no more outliers, can continue on create other features

#### (4) Create pack size from PROD_NAME
# Taking the digits that are in PROD_NAME
transactionData[,PACK_SIZE:= parse_number(PROD_NAME)]
# check if the output look sensible
transactionData[,.N,PACK_SIZE][order(PACK_SIZE)]
# The largest size is 380g and the smallest size is 70g - seems sensible

# Check the output of the first few rows to see if we have indeed pickedout packsize.
transactionData
# Plot a histogram of PACK_SIZE since we know that it is a categorical variable and 
  # not a continuous variable eventhough it is numeric. 
hist(transactionData[,PACK_SIZE])
# Pack sizes looks reasonable

#### (5) Create brand, using the first word in PROD_NAME to work out the brand name
# Brands
transactionData[, BRAND := toupper(substr(PROD_NAME, 1, regexpr(pattern = ' ', PROD_NAME) - 1))]
# Checking brands
transactionData[, .N, by = BRAND][order(-N)]
# Some of the brand names look like they are of the same brands - such as RED and RRD, 
  # which are both Red Rock Deli chips. Let’s combine these together.

# Clean brand names
transactionData[BRAND == "RED", BRAND := "RRD"]
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]
transactionData[BRAND == "NCC", BRAND := "NATURAL"]
transactionData[BRAND == "DORITO", BRAND := "DORITOS"]
transactionData[BRAND == "GRAIN", BRAND := "GRNWVES"]
# Check again
transactionData[, .N, by = BRAND][order(BRAND)]

# Now that we are happy with the transaction dataset, let’s have a look at the customer dataset.

# --- 2.2 Examining customer data ---
# Examining customer data
str(customerData)

summary(customerData)

# Examining the values of lifestage and premium_customer
customerData[, .N, by = LIFESTAGE][order(-N)]
customerData[, .N, by = PREMIUM_CUSTOMER][order(-N)]

# As there are no issues with the customer data, we can now go ahead and join 
  # the transaction  and customer data sets together
# Merge transaction data to customer data
data <- merge(transactionData, customerData, all.x = TRUE)

# Since the number of rows in data is the same as that of transaction data, 
  # we can be sure that no duplicates were created. This is because we created data
  # by setting all.x = TRUE(inotherwords, aleftjoin) which means take all the rows 
  # in transactionData and find rows with matching values in shared columns and  
  # then joining the details in these rows to the x or the first mentioned table.

# Check if some customers were not matched on by checking for nulls.
data[is.null(LIFESTAGE), .N]
data[is.null(PREMIUM_CUSTOMER), .N]
# There are no nulls. So all our customers in the transaction data have been 
  # accounted for in the customer dataset.
# fwrite(data, paste0(filePath,"QVI_data.csv"))

# --- Step 3: Data analysis on customer segments ---

#### Data analysis metrics for the client:
# • Who spends the most on chips (total sales), describing customers by lifestage 
  # and how premium their general purchasing behaviour is
# • How many customers are in each segment
# • How many chips are bought per customer by segment
# • What’s the average chip price by customer segment
# We could also ask our data team for more information. Examples are:
# • The customer’s total spend over the period and total spend for each transaction 
  # to understand what proportion of their grocery spend is on chips
# • Proportion of customers in each customer segment overall to compare against 
  # the mix of customers who purchase chips

#### (1) Describe which customer segment contribute most to chip sales
# Calculate total sales by LIFESTAGE and PREMIUM_CUSTOMER
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]

# Create plot for proportion of sales by LIFESTAGE and PREMIUM_CUSTOMER
p <- ggplot(data = sales) +
  geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plot and label with proportion of sales
p + geom_text(data = ggplot_build(p)$data[[1]],
              aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2,
                  label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))

# Sales are coming mainly from Budget-olderfamilies,Mainstream-youngsingles/couples, 
  # and Mainstream-retirees

## (2) Check if the higher sales are due to there being more customers who buy chips.
# Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), 
                  .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]

# Create plot for proportion of customers
p <- ggplot(data = customers) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plot and label with proportion of customers
p + geom_text(data = ggplot_build(p)$data[[1]],
              aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2,
                  label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))

# There are more Mainstream - young singles/couples and Mainstream - retirees who buy chips. 
  # This contributes to there being more sales to these customer segments but this 
  # is not a major driver for the Budget Older families segment.
# Higher sales may also be driven by more units of chips being bought per customer. 

## (3) Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)),
                  .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
# Create plot
ggplot(data = avg_units, aes(weight = AVG, x = LIFESTAGE, fill =
                               PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per
 customer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Older families and young families in general buy more chips per customer
# Let’s also investigate the average price per unit chips bought for each customer 
  # segment as this is also a driver of total sales.

## (4) Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price <- data[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)), 
                  .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
# Create plot
ggplot(data = avg_price, aes(weight = AVG, x = LIFESTAGE, fill =
                               PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Mainstream mid-aged and young singles and couples pay more per chip packet.
# This may be due to premium shoppers prioritizing healthy snacks.
# They buy chips mainly for entertainment, not consumption.
# This is also supported by fewer premium mid-aged and young singles/couples 
  # buying chips compared to mainstream.
# As the average price per unit difference is small, a statistical test 
  # is needed to confirm its significance.

## (5) Perform an independent t‐test between mainstream vs premium and budget midage
  # and young singles and couples
pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER == "Mainstream", price],
       data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER != "Mainstream", price],
       alternative = "greater")
# The t-test results in a p-value < 2.2e-16, i.e. the unit price for mainstream, 
  # young and mid-age singles and couples are significantly higher than that of 
  # budget or premium, young and midage singles and couples.

# --- Step 4: Deep dive into specific customer segments for insights ---
# We might want to target customer segments that contribute the most to sales to retain them or further
# increase sales. Let’s look at Mainstream- young singles/couples. For instance, 
  # let’s find out if they tend to
# buy a particular brand of chips.

# Deep dive into Mainstream, young singles/couples
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == 
                   "Mainstream",]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER ==
                  "Mainstream"),]
#### (1) Brand affinity compared to the rest of the population
quantity_segment1 <- segment1[, sum(PROD_QTY)]

quantity_other <- other[, sum(PROD_QTY)]
quantity_segment1_by_brand <- segment1[, .(targetSegment =
                                             sum(PROD_QTY)/quantity_segment1), by = BRAND]

quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by
                                 = BRAND]
brand_proportions <- merge(quantity_segment1_by_brand,
                           quantity_other_by_brand)[, affinityToBrand := targetSegment/other]

brand_proportions[order(-affinityToBrand)]

# We can see that :
# • Mainstream young singles/couples are 23% more likely to purchase Tyrrells chips 
  # compared to the rest of the population
# • Mainstream youngsingles/couples are 56% less likely to purchase Burger Rings 
  # compared to the rest of the population

#### (2)  Find out if our target segment tends to buy larger packs of chips
#### Preferred pack size compared to the rest of the population
quantity_segment1_by_pack <- segment1[, .(targetSegment =
                                            sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]

quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by =
                                  PACK_SIZE]
pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[,
                    affinityToPack := targetSegment/other]

pack_proportions[order(-affinityToPack)]

#  It looks like Mainstream young singles/couples are 27% more likely to purchase 
  # a 270g pack of chips com pared to the rest of the population but let’s dive 
  # into what brands sell this pack size.
data[PACK_SIZE == 270, unique(PROD_NAME)]
# Twisties are the only brand offering 270g packs and so this may instead be 
  # reflecting a higher likelihood of purchasing Twisties

# --- Step 5: Conclusion ---
# Chip sales are primarily driven by Budget - older families, Mainstream - young singles/couples, 
# and Mainstream - retirees. High spend from mainstream young singles/couples and retirees is largely 
# due to their higher numbers. Mainstream mid-age and young singles/couples also show impulse buying behavior, 
# paying more per chip packet. Notably, Mainstream young singles/couples 
# are 23% more likely to buy Tyrrells chips.

# Recommendation: The Category Manager should increase visibility and impulse purchases 
# by strategically placing Tyrrells and smaller chip packs in areas frequented 
# by young singles/couples. Quantium can assist with segment location and trial impact measurement.

