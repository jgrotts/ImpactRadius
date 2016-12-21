#shiny server file
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(sqldf))

##############################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Formatting Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############################################


#Load data
retail <- read.csv("Online Retail.csv")
retail$InvoiceDate <- as.POSIXct(retail$InvoiceDate,format="%d/%m/%Y %H:%M")


#Remove country == "Unspecified"
retail <- sqldf("select * from retail where Country != 'Unspecified'")

#Remove negative prices
retail <- sqldf("select * from retail where UnitPrice > 0")

#Remove bogus quantities 
retail <- sqldf("select * from retail where (Quantity > 0 and Quantity < 10000)")

#Combine orders 
retail_orders <- sqldf("SELECT Country, SUM(Quantity * UnitPrice) FROM retail GROUP BY InvoiceNo")
dimnames(retail_orders)[[2]] <- c("Country","Order_Cost")

#Get stats for quantity by country
country_quant <- data.frame(
  min     = tapply(retail$Quantity,retail$Country,min),
  first_q = tapply(retail$Quantity,retail$Country,quantile,prob=.25),
  median  = tapply(retail$Quantity,retail$Country,median),
  mean    = tapply(retail$Quantity,retail$Country,mean),
  third_q = tapply(retail$Quantity,retail$Country,quantile,prob=.75),
  max     = tapply(retail$Quantity,retail$Country,max)
)


#Get stats for Unit price by country
country_price <- data.frame(
  min     = tapply(retail$UnitPrice,retail$Country,min),
  first_q = tapply(retail$UnitPrice,retail$Country,quantile,prob=.25),
  median  = tapply(retail$UnitPrice,retail$Country,median),
  mean    = tapply(retail$UnitPrice,retail$Country,mean),
  third_q = tapply(retail$UnitPrice,retail$Country,quantile,prob=.75),
  max     = tapply(retail$UnitPrice,retail$Country,max)
)



#Get stats for order cost by country
country_tot <- data.frame(
  min     = tapply(retail_orders$Order_Cost, retail_orders$Country, min),
  first_q = tapply(retail_orders$Order_Cost, retail_orders$Country, quantile,prob=.25),
  median  = tapply(retail_orders$Order_Cost, retail_orders$Country, median),
  mean    = tapply(retail_orders$Order_Cost, retail_orders$Country, mean),
  third_q = tapply(retail_orders$Order_Cost, retail_orders$Country, quantile,prob=.75),
  max     = tapply(retail_orders$Order_Cost, retail_orders$Country, max)
)




table1_fun <- function(name_hold){
  #More summary statistics
  
  out_table <- data.frame(
    c(format(length(unique(retail$InvoiceNo[retail$Country == name_hold])),big.mark=",",scientific=FALSE),
    
      format(length(unique(retail$CustomerID[retail$Country == name_hold])),big.mark=",",scientific=FALSE),
    
      format(length(unique(retail$StockCode[retail$Country == name_hold])),big.mark=",",scientific=FALSE))
  )
  
  dimnames(out_table)[[2]] <- c("N")
  dimnames(out_table)[[1]] <- c("Number of unique orders","Number of unique customers","Number of unique items")
  return(out_table) 
}

table2_fun <- function(name_hold){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Function to get summary statistics by country
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  out_table <- data.frame(rbind(
        country_quant[row.names(country_quant) == name_hold,],
        country_price[row.names(country_price) == name_hold,],
        country_tot[row.names(country_tot) == name_hold,]))
  dimnames(out_table)[[1]] <- c("Quantity","Unit Price","Cost of orders")
  return(out_table)
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define server logic required to summarize country
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyServer(function(input, output) {

  
  #Generate tables
  output$table1 <-  renderTable({table1_fun(input$Name)})
    
  output$table2 <-  renderTable({table2_fun(input$Name)})
    
  
  # Generate a distribution of order cost by country

   output$dist_plot <- renderPlot({
    ggplot(retail_orders[as.character(retail_orders$Country) == input$Name,],
      aes(x=Order_Cost))+
        geom_histogram(color = "black",fill = "lightgrey", binwidth = 25)+
        scale_x_continuous(limits = c(0,2500)) +
        xlab("Cost of order") +
        ylab("Number of orders")+
        ggtitle(paste("Distribution of the cost of the orders \n (",
          input$Name,")",sep=""))+
        theme_bw()+
        theme(plot.title  = element_text(size = 24, face = "bold"), 
              axis.title.x = element_text(size = 18, face = "bold"),
              axis.title.y = element_text(size = 18, face = "bold"),
              axis.text.x  = element_text(size = 14),
              axis.text.y  = element_text(size = 14)) 
  })
   
  
})
















