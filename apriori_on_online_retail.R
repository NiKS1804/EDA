# Transforming dataset for using Apriori algorithm
- Items with same Invoice no are grouped together using separator ','
- Only products are taken into a different dataframe
- We write that to a CSV file named 'Retail.csv'
- Then in Excel, we transformed that to a file containing multiple rows for different items
- Then that file is read again for Apriori Algorithm



data1 = retail1
data1 = data1 %>% group_by(InvoiceNo) %>% mutate(items = paste(Description, collapse =  ","))
View(data1)

data2 = as.data.frame(unique(data1$items))
colnames(data2) = "Items"
View(data2)

write.csv(data2,"D:/Retail.csv")


# Apriori algorithm

library('arules')
retail_data = read.csv("D:/Retail.csv", header = F)
str(retail_data)

# Take a sample of 20 
idx = sample(1:nrow(retail_data),20)
retail_data1 = retail_data[idx,]

rules = apriori(retail_data1,
                control = list(verbose=F),
                parameter = list(minlen=2, supp=0.2, conf=0.8))
