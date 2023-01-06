
#............................Data Cleaning.....................
#dropping waste no use columns

new_sales_data<-sales_data[,!names(sales_data) %in% c("Name.Prefix","User.Name","E.Mail","Middle.Initial","Name.prefix","sku","full_name","Last.Name","ref_num","Zip","Phone.No.","SSN")]
View(new_sales_data)

# checking if null values exist?

sum(is.null(new_sales_data$discount_amount))
sum(is.na(new_sales_data))

#adding sales column in data set
#Sales = Number of Units Sold * Average Selling Price Per Unit

sales<-new_sales_data
sales<-mutate(new_sales_data,sale=(qty_ordered*price)-discount_amount,cost_of_goods=(price*0.4),revenue=(qty_ordered*price))
sales<-mutate(sales,profit=revenue*0.6)

View(sales)

# 1.high no. of order on the base of region

high_sale<-new_sales_data %>% group_by(Region) %>% summarise(count=n_distinct(order_id))
View(high_sale)

ggplot(high_sale,aes(x=Region, y=count, label=count)) + 
  geom_col(width=0.5, position='dodge',fill="skyblue") +
  geom_text(position=position_dodge(0.5), vjust=-0.25) + 
  labs(y="Total No.Order", x="Region",title="Region Wise Sale")+
  theme_minimal()



# 2.getting  frequency of categories in data_set

ggplotly(
  ggplot(new_sales_data, aes(x =category, fill = category)) +
    geom_bar() +
    
    labs(x = "Category", y = "Frequency",title = "Frequency of Category"))



# 3.Status of orders to know either they are completed or canceled or others

modified_status<-filter(new_sales_data, status %in%  c("received","canceled","cod","complete","order_refunded","refund"))
View(modified_status)

#we use filter because there are too many statuses which have exist but in very less count like  closed,holded,payment_review,pending,pending_paypal,processing

ggplotly(
  ggplot(modified_status, aes(x =status, fill = status)) +
    geom_bar() +
    labs(x = "Order Status", y = "Count of Orders",title = "Status of Orders"))



# 4.Most preferred mode for payment

modified_payment<-new_sales_data%>%group_by(payment_method)%>%tally()
View(modified_payment)

rank_modified_payment<-modified_payment%>%arrange(desc(n))%>%head(5)

#we filtered the data because some of the payment methods are not used more than once in payment so thats why we dropped it.
ggplotly(ggplot(rank_modified_payment,aes(x=payment_method, y=n,label=n)) + 
           geom_col(width=0.5, position='dodge') +
           geom_text(position=position_dodge(0.5), vjust=-0.25) + 
           labs(y="Count", x="Payment Method",title=" Modes of Payment")+
           theme_minimal()+theme_gray())



# 5.most ordered categories

modified_category<-new_sales_data%>%group_by(category)%>%tally()

#In this we ranked top5 category in desc order

rank_modified_category<-modified_category%>%arrange(desc(n))%>%head(5)
View(modified_category)
View(rank_modified_category)

ggplotly(ggplot(rank_modified_category,aes(x=category, y=n,label =n)) + 
           geom_col(width=0.5, position='dodge',fill="purple") +
           geom_text(position=position_dodge(0.5), vjust=-0.25) + 
           labs(y="Total No.Order", x="Category",title="Category Wise Orders")+
           theme_minimal()+theme_dark())



# 6.Q.customer getting most of the orders depending on the last month
#it means we get count of orders based on the last month given in the data frame.
#top5 most frequent customers

max_month<-max(sales$month)
max_month

most_order <-new_sales_data %>%
  group_by(cust_id,First.Name) %>%filter(month==max_month)%>%
  summarize(count_orders = n_distinct(order_id))
View(most_order)

#ranking the top customers now
rank_most_order<-most_order%>%arrange(desc(count_orders))%>%head(5)

View(rank_most_order)

ggplotly(ggplot(rank_most_order,aes(x=First.Name, y=count_orders, label=count_orders)) + 
           geom_col(width=0.5, position='dodge',fill="orange") +
           geom_text(position=position_dodge(0.5), vjust=-0.25) + 
           labs(y="Order_Count", x="Customer_Name",title="Top 5 Cust. with Highest Orders in last Month")+
           theme_minimal())



# 7.top 5 cities with highest sales

View(top_sale_city<-sales%>%group_by(City)%>%summarise(sum=sum(sale)))
rank_modified_city<-top_sale_city%>%arrange(desc(sum))%>%head(5)
View(rank_modified_city)

library(scales)
ggplotly(
  ggplot(rank_modified_city,aes(x=City, y=sum, label=sum)) + 
    geom_col(width=0.5, position='dodge',fill="orange") +
    geom_text(position=position_dodge(0.5), vjust=-0.25) + 
    labs(y="Sale_Generated", x="City_Name",title="top 5 cities with highest sales")+
    theme_grey()+
    scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-6)))
View(new_sales_data)



# 8.Highest Orders in which Month?

now<-sales%>%group_by(month)%>%summarise(m_orders=n_distinct(order_id))#%>%arrange(desc(m_orders))
View(now)

ggplotly(
  ggplot(now,aes(x = month,y=m_orders,fill = month)) + 
    geom_bar(stat="identity")+
    geom_text(aes(label=m_orders),vjust=0)+
    labs(title = "",y="No of Orders",x="Month"))



# 9.top 5 customer who highly spent avg_money per order

avg_money_spent<-sales %>%
  mutate(subtotal = qty_ordered * price) %>%
  group_by(First.Name)%>%
  summarise(avg_purchase_values = as.integer(mean(subtotal)))
View(avg_money_spent)

rank_avg_money_spent<-avg_money_spent%>%arrange(desc(avg_purchase_values))%>%head(5)
View(rank_avg_money_spent)

ggplotly(ggplot(rank_avg_money_spent,aes(x = First.Name,y=avg_purchase_values,fill = First.Name)) + 
           geom_bar(stat="identity")+
           geom_text(aes(label=avg_purchase_values),vjust=-0.25)+
           labs(title = "",x="Customer Name",y="Avg Purchase Value"))



# 10.Count of customers in different regions

count_by_region<-sales%>%group_by(Region)%>%count(cust_id)%>%summarise(count_of_cust=sum(n)                                                                       )

ggplotly(ggplot(count_by_region,aes(x = Region,y=count_of_cust,fill = Region)) + 
           geom_bar(stat="identity")+
           geom_text(aes(label=count_of_cust),vjust=-0.25)+
           labs(x="Region",y="Count of Customers"))



