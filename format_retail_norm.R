###################################################
# Formatting retail wide
#
#
###################################################
require(foreach)
require(doSNOW)
require(MASS)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create iterator 
#  Iterator used to only send chunks of data to workers
#   during parallel computing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
iretail<-function(items){
  iter_item <- iter(items)
  
  nextEl <- function(){
    item_current <- nextElem(iter_item)
    retail[retail$StockCode==item_current,]
    
  }
  obj        <-list(nextElem=nextEl)
  class(obj) <- c("iretail","abstractiter","iter")
  obj
  
}

# test_iter <- iretail(unique(retail$StockCode)[1:2])
# item <- nextElem(test_iter)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the for loop
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
registerDoSNOW(makeCluster(6, outfile=""))

t0 <- Sys.time()
retail_norm <- foreach(item = iretail(unique(retail$StockCode)), .combine = "rbind",
                        .packages = "MASS") %dopar% {
  
  #Only include items with greater than 3 prices
  if(length(unique(item$UnitPrice)) > 2){
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Create temporary dataset to figure out price and quantity span
    
    price_dat <- NULL
    for(price in unique(item$UnitPrice)){
      
      price_dat <- rbind(price_dat,
        data.frame(
          price,
          quantity = sum(item$Quantity[item$UnitPrice == price])
        ))
    }
    
    #Find normalized range for price and quantity
    # Find normalied slope of demand curve
    price_mod <- lm(scale(quantity) ~ scale(price), price_dat)
    
    price_mod_rob <- rlm(scale(quantity) ~ scale(price), price_dat, psi = psi.bisquare)
    
    
    price_range_normalized <- range(scale(price_dat$price))[2] - range(scale(price_dat$price))[1]
    quant_range_normalized <- range(scale(price_dat$quantity))[2] - range(scale(price_dat$quantity))[1]
    normalized_slope     <- summary(price_mod)$coef[2,1]
    norm_slope_rob       <- summary(price_mod_rob)$coef[2,1]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Find if the product is seasonal
    month_dat <- NULL
    
    for(month in 1:12){
      month_dat <- rbind(month_dat,
        data.frame(
          month,
          quantity = sum(item$Quantity[as.numeric(format(item$InvoiceDate,"%m")) == month],na.rm=T)
        )
      )  
    }
    
    seasonal <- IQR(scale(month_dat$quantity), na.rm=T)
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create variable that captures how long item has been available
    #   - counts the weeks from 1/1/2010 to min order date
    weeks_till_first_order <- as.numeric(difftime(min(item$InvoiceDate,na.rm=T), 
                                          as.POSIXct("2010-01-01"), units = "weeks"))
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Number unique customers purchased item 
    #  and number of unique orders
    #  ?repeat customers
    num_customers <- length(unique(item$CustomerID))
    num_orders    <- length(unique(item$InvoiceNo))
    
      
      
    dat_out <- data.frame(
      
      StockCode              = unique(item$StockCode),
      Description            = unique(item$Description)[1], 
      price_med              = median(price_dat$price), #Median price
      price_mean             = mean(price_dat$price), #Mean price
      quantity_med           = median(price_dat$quantity), #Median price
      quantity_mean          = mean(price_dat$quantity), #Mean price
      price_range_normalized,
      quant_range_normalized,
      normalized_slope,
      norm_slope_rob,
      seasonal,
      weeks_till_first_order,
      num_customers,
      num_orders
        
    )
    
    
    for(coun in levels(item$Country)){
      
      if(coun != "Unspecified"){
        col_name_hold <- c(colnames(dat_out), coun)
        
        dat_out <- data.frame(dat_out,ifelse(coun %in% item$Country, 1, 0))
        
        colnames(dat_out) <- gsub(" ","_",col_name_hold)
        
      }
    }
    dat_out
  }
  
  
}
t1 <- Sys.time()
gc()
t1-t0 #8.6 mins


write.csv(retail_norm,
  "retail_norm.csv", row.names = FALSE)






