library(readxl)
library(lubridate)
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lme4)
library(quantreg)
library(caret)
library(WriteXLS)
library(zoo)
library(forecast)
library(h2o)
library(prophet)
library(tibble)
library(geepack)
library(vtreat)
library(xgboost)
library(featuretoolsR)
library(rpart)
library(kernlab)
library(glmnet)
library(RANN)
library(tensorflow)
library(keras)
library(xgboost)
library(correlationfunnel)
library(FSelector)


primary<-as.data.table(read_xlsx("D:/CSS/sales demand plan/data/March 2007 - July 2019.xlsx", sheet = 6))

prices<-as.data.table(read_xlsx("D:/CSS/sales prediction for march/data/0. Price Changes - Soft Drinks.xlsx", sheet = 3))[,1:5]

conversion<-as.data.table(read_xlsx("D:/CSS/discount/data/sku conversion.xlsx", skip=1))[,c(1,6)]

sale_15<-read.csv("D:\\CSS\\sales demand plan\\data created\\sale_27_with_july.csv")%>%filter(!(Year==2019 & Month==7))

rd_month_percs<-read.csv("D:\\CSS\\sales demand plan\\data created\\rd_month_percs_27.csv")

skus<-as.data.table(read_xlsx("D:/CSS/sales demand plan/data/CSD SKUs.xlsx"))

july<-as.data.table(read.csv("D:\\CSS\\sales demand plan\\data\\july_2019.csv"))

grouped_2016<-as.data.table(read.csv("D:\\CSS\\sales demand plan\\data created\\2016_grouped.csv"))

grouped_2015<-as.data.table(read.csv("D:\\CSS\\sales demand plan\\data created\\2015_grouped.csv"))


sale_15<-sale_15%>%
  rbind(july)%>%
  rbind(grouped_2016)%>%
  rbind(grouped_2015)


# creating primary sales: getting to long format
primary2<-primary%>%
  group_by(Material,`Materiall Desc`,Pack,Flavor,Year)%>%
  summarize(`4`=sum(`APR  L`,na.rm = T), `5`=sum(`MAY  L`,na.rm = T), `6`=sum(`JUN  L`,na.rm = T), 
            `7`=sum(`JUL  L`,na.rm = T), `8`=sum(`AUG  L`,na.rm = T), `9`=sum(`SEP  L`,na.rm = T),
            `10`=sum(`OCT  L`,na.rm = T), `11`=sum(`NOV  L`,na.rm = T), `12`=sum(`DEC  L`,na.rm = T),
            `1`=sum(`JAN  L`,na.rm = T), `2`=sum(`FEB  L`,na.rm = T), `3`=sum(`MAR  L`,na.rm = T))%>%
  filter(Year %in% c(2014,2015,2016,2017,2018,2019))


primary3<-primary2%>%gather(month,sales, 6:17)

primary3$Year<-ifelse(as.integer(primary3$month) %in% c(1,2,3),as.integer(primary3$Year)+1,primary3$Year)



# prepare prices
names(prices)<-c("date","sku","sku_name","old_price","new_price")
prices<-prices%>%na.omit()
prices$date<-ymd(prices$date)
prices$Year<-as.integer(format(prices$date, "%Y"))
prices$Month<-as.integer(format(prices$date, "%m"))
prices$Month<-as.character(prices$Month)
prices$old_price[prices$old_price==0]<-prices$new_price


primary4<-primary3%>%left_join(prices[,2:7], by=c("Material"="sku","Year"="Year","month"="Month"))
primary4$old_price<-as.integer(sub("-","",primary4$old_price))
primary4$new_price<-as.integer(sub("-","",primary4$new_price))


########################################################################3
# filter only for skus sold in 2019
# l<-sale_15%>%filter(Year==2019)

# primary4<-primary4%>%filter(Material %in% l$SKU.ID)

# primary4<-primary4%>%filter(!(sales==0))


# create date variable

primary4<-primary4%>%
  mutate(date_n=as.Date(paste0(Year,"-",month,"-","01")))

#downward filling
sales6<-primary4%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  fill(new_price)

#lead old price

sales6<-sales6%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  mutate(old_lead=shift(old_price,-1))


sales6$new_price<-ifelse(is.na(sales6$new_price), sales6$old_lead, sales6$new_price)

# upward filling
sales6<-sales6%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  fill(new_price, .direction = c("up"))


# filtering for zero sales ########################
# sales6<-
#   sales6%>%
#   filter(!(sales==0))


sales6$new_price2<-ifelse(is.na(sales6$new_price),as.numeric(gsub("([0-9]+).*$", "\\1", sales6$Pack))/5.5, sales6$new_price)
sales6$new_price2[sales6$new_price2<1]<-250

sales6$new_price2<-ifelse(is.na(sales6$new_price2),as.numeric(gsub("([0-9]+).*$", "\\1", sales6$`Materiall Desc`))/5.5, sales6$new_price2)
sales6$new_price2[is.na(sales6$new_price2)]<-100





# creating lagged price variables

sales6<-sales6%>%
  group_by(`Material`)%>%
  arrange(date_n)%>%
  mutate(lagp=shift(new_price2, type = "lag"))

sales6$lagp[is.na(sales6$lagp)]<-sales6$new_price2

sales6<-sales6%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  mutate(lagp2=shift(lagp, type = "lag"))

sales6$lagp2[is.na(sales6$lagp2)]<-sales6$lagp




# lagged sale variables


sales6<-sales6%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  mutate(lagsale=shift(sales, type = "lag"))



sales6$lagsale[is.na(sales6$lagsale)]<-sales6$sales

sales6<-sales6%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  mutate(lagsale2=shift(lagsale, type = "lag"))

sales6$lagsale2[is.na(sales6$lagsale2)]<-sales6$lagsale

sales6<-sales6%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  mutate(lagsale3=shift(lagsale2, type = "lag"))

sales6$lagsale3[is.na(sales6$lagsale3)]<-sales6$lagsale2


sales6<-sales6%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  mutate(lagsale4=shift(lagsale3, type = "lag"))%>%
  mutate(lag_ratio=lagsale3/lagsale2)

sales6$lagsale4[is.na(sales6$lagsale4)]<-sales6$lagsale3

sales6<-sales6%>%mutate(window=(lagsale+lagsale2+lagsale3+lagsale4)/4)

#standard dev feature

sales6<-sales6%>%
  group_by(Material)%>%
  mutate(stdev=sd(sales))


# combine uom
sales6<-sales6%>%
  left_join(conversion, by=c("Material"="Item Code1"))%>%
  mutate(bottles=sales/UOM,
         bottles=ifelse(is.na(bottles),sales,bottles))



# upward filling
sales6<-sales6%>%group_by(`Material`)%>%
  arrange(date_n)%>%
  fill(new_price, .direction = c("up"))


# cleaning for packs: filling missing values

sales6$Pack<-ifelse(is.na(sales6$Pack), paste0(gsub("([0-9]+).*$", "\\1", sales6$`Materiall Desc`),"ML"), sales6$Pack)

sapply(sales6,function(x)sum(is.na(x)))

sales6$price_per_liter<-sales6$new_price/sales6$UOM


sales6%>%
  ggplot(aes(sales, price_per_liter))+
  geom_smooth(method="lm")+
  facet_wrap(~Pack)+
  # geom_jitter(alpha=0.1)+
  theme_bw()




sales6$month<-as.numeric(sales6$month)


# pack total feature create datasets


totals<-sales6%>%
  group_by(Pack,Year,month)%>%
  arrange(Year,month)%>%
  summarize(pack_tots=sum(sales, na.rm=T))%>%
  group_by(Pack)%>%
  arrange(Year,month)%>%
  mutate(pack_tot_lag=shift(pack_tots, type = "lag"),
         pack_tot_lag2=shift(pack_tot_lag, type = "lag"),
         pack_tot_lag3=shift(pack_tot_lag2, type = "lag"),
         pack_tot_lag4=shift(pack_tot_lag3, type = "lag"),
         pack_tot_ly=shift(pack_tots,12))



# upward filling pack totals
totals<-totals%>%group_by(Pack)%>%
  arrange(Year,month)%>%
  fill(pack_tot_lag, .direction = c("up"))%>%
  fill(pack_tot_lag2, .direction = c("up"))%>%
  fill(pack_tot_lag3, .direction = c("up"))%>%
  fill(pack_tot_lag4, .direction = c("up"))%>%
  fill(pack_tot_ly, .direction = c("up"))

totals[is.na(totals)]<-1

# create monthly totals

monthly_tots<-sales6%>%
  group_by(Year,month)%>%
  arrange(Year,month)%>%
  summarize(m_tots=sum(sales, na.rm=T))%>%
  ungroup()%>%
  arrange(Year,month)%>%
  mutate(m_tot_lag=shift(m_tots, type = "lag"),
         m_tot_lag2=shift(m_tot_lag, type = "lag"),
         m_tot_lag3=shift(m_tot_lag2, type = "lag"),
         m_tot_lag4=shift(m_tot_lag3, type = "lag"),
         m_tot_ly=shift(m_tots,12))


# upward filling monthly tots
monthly_tots<-monthly_tots%>%
  arrange(Year,month)%>%
  fill(m_tots, .direction = c("up"))%>%
  fill(m_tot_lag, .direction = c("up"))%>%
  fill(m_tot_lag2, .direction = c("up"))%>%
  fill(m_tot_lag3, .direction = c("up"))%>%
  fill(m_tot_lag4, .direction = c("up"))%>%
  fill(m_tot_ly, .direction = c("up"))

# left join to sales6 and create features
sales6<-sales6%>%
  left_join(totals, by=c("Year"="Year","month"="month","Pack"="Pack"))%>%
  left_join(monthly_tots, by=c("Year"="Year","month"="month"))%>%
  mutate(pack_ratio_ly=pack_tot_ly/m_tot_ly,
         pack_ratio_window=(pack_tot_lag2+pack_tot_lag3+pack_tot_lag4)/(m_tot_lag2+m_tot_lag3+m_tot_lag4),
         pack_ratio_lag=pack_tot_lag2/m_tot_lag2)


# features of lagged skus
sales6<-sales6%>%
  group_by(Material)%>%
  arrange(Year,month)%>%
  mutate(sku_lag=shift(sales, type = "lag"),
         sku_lag_2=shift(sku_lag, type = "lag"),
         sku_lag_3=shift(sku_lag_2, type = "lag"),
         sku_lag_4=shift(sku_lag_3, type = "lag"),
         sku_lag_ly=shift(sku_lag, 12))

# upward filling and creating fdeatures
sales6<-sales6%>%
  group_by(Material)%>%
  arrange(Year,month)%>%
  fill(sku_lag, .direction = c("up"))%>%
  fill(sku_lag_2, .direction = c("up"))%>%
  fill(sku_lag_3, .direction = c("up"))%>%
  fill(sku_lag_4, .direction = c("up"))%>%
  fill(sku_lag_ly, .direction = c("up"))%>%
  mutate(sku_contrib_ly=sku_lag_ly/pack_tot_ly,
         sku_contrib_window=(sku_lag_2+sku_lag_3+sku_lag_4)/(pack_tot_lag2+pack_tot_lag3+pack_tot_lag4),
         sku_contrib_lag=sku_lag_2/pack_tot_lag2)


# flavors
sales6$Flavor[is.na(sales6$Flavor)]<-"none"

flvr_tots<-
  sales6%>%
  group_by(Flavor,Year,month)%>%
  arrange(Year,month)%>%
  summarize(flv_tots=sum(sales, na.rm=T))%>%
  group_by(Flavor)%>%
  arrange(Year,month)%>%
  mutate(flv_lag=shift(flv_tots, type = "lag"),
         flv_lag2=shift(flv_lag, type = "lag"),
         flv_lag3=shift(flv_lag2, type = "lag"),
         flv_lag4=shift(flv_lag3, type = "lag"),
         flv_ly=shift(flv_tots,12))

# upward filling
flvr_tots<-flvr_tots%>%
  arrange(Year,month)%>%
  fill(flv_lag, .direction = c("up"))%>%
  fill(flv_lag2, .direction = c("up"))%>%
  fill(flv_lag3, .direction = c("up"))%>%
  fill(flv_lag4, .direction = c("up"))%>%
  fill(flv_ly, .direction = c("up"))

flvr_tots[is.na(flvr_tots)]<-1


# join to sales6 and create features
sales6<-
  sales6%>%
  left_join(flvr_tots, by=c("Flavor"="Flavor","Year"="Year","month"="month"))%>%
  mutate(flv_contrib_ly=flv_ly/m_tot_ly,
         flv_contrib_window=(flv_lag2+flv_lag3+flv_lag4)/(m_tot_lag2+m_tot_lag3+m_tot_lag4),
         flv_contrib_lag=flv_lag2/m_tot_lag2)


# Join RD sales to sales6

sales6<-
  sales6%>%
  left_join(sale_15, by=c("Year"="Year","month"="Month","Material"="SKU.ID"))%>%
  left_join(rd_month_percs, by=c("month"="Month"))

sales6$sale15<-sales6$sale15*sales6$mult



sales6<-sales6%>%
  group_by(Material)%>%
  arrange(date_n)%>%
  mutate(lag_rd=shift(sale15, type="lag"),
         lag_rd2=shift(lag_rd, type="lag"),
         lag_rd3=shift(lag_rd2, type="lag"),
         lag_rd4=shift(lag_rd3, type="lag"))

# did not include lagrd removal#################

sales6<-
  sales6%>%
  filter(!(is.na(lag_rd)))


sales6<-sales6%>%
  fill(lag_rd, .direction = c("up"))%>%
  fill(lag_rd2, .direction = c("up"))%>%
  fill(lag_rd3, .direction = c("up"))%>%
  fill(lag_rd4, .direction = c("up"))%>%
  mutate(lagrd_window=(lag_rd2+lag_rd3+lag_rd4)/3)


sales6$bottles<-sales6$lag_rd/sales6$UOM




# create aggregating geatures
sales6<-sales6%>%group_by(Material,Year)%>%
  mutate(max_mat=max(lag_rd),
         min_mat=min(lag_rd),
         mean_mat=mean(lag_rd),
         sd_mat=sd(lag_rd),
         diff_mat=max_mat-min_mat)

# create trend features  
sales6<-sales6%>%
  mutate(trend=lag_rd-((lag_rd2+lag_rd3)/2),
         trend2=((lag_rd+lag_rd2)/2))

sales6$zero_removed_sale<-sales6$sales
sales6$zero_removed_sale[sales6$zero_removed_sale<=1]<-1

# did not include mean impute


# monthly means for xgboost
monthly_avg<-sales6%>%
  filter(!(Year==2019))%>%
  group_by(month)%>%
  summarize(monthly_avg=mean(sales,na.rm=T))


# 
flav_ratio1<-sales6%>%
  filter(!(Year==2019))%>%
  group_by(Flavor)%>%
  summarize(flav_tots=sum(sales,na.rm = T))


monthly_flav_ratio<-sales6%>%
  filter(!(Year==2019))%>%
  group_by(month,Flavor)%>%
  summarize(means=mean(sales,na.rm=T))%>%
  left_join(flav_ratio1)%>%
  mutate(flav_ratio=means/flav_tots)%>%
  select(month,Flavor,flav_ratio)





sales6<-sales6%>%
  left_join(monthly_avg)

sales6<-sales6%>%
  left_join(monthly_flav_ratio)


sales6$bottles<-sales6$lag_rd/sales6$UOM


sales6$Material<-as.factor(sales6$Material)
sales6$Pack<-as.factor(sales6$Pack)
sales6$Flavor<-as.factor(sales6$Flavor)


sales6<-sales6%>%
  group_by(Material)%>%
  mutate(max_price=max(price_per_liter))


# removing sugar free

# sales6<-sales6%>%filter(!(grepl("Sugar Free",`Materiall Desc`)))



# sales6<-read.csv("D:\\CSS\\sales demand plan\\data created\\sales6.csv")


tt<-sales6%>%filter(!(is.na(sale15)))

sales6[is.na(sales6)]<-1

sales6$Year<-as.factor(sales6$Year)

sales6$month<-as.factor(sales6$month)

# outlier treatment

sales6$zero_removed_sale<-sales6$sales
sales6$zero_removed_sale[sales6$zero_removed_sale<=1]<-1

# extra features
sales6$sugar<-ifelse(sales6$Year %in% c(2016,2017),1,0)


lam = BoxCox.lambda(sales6$sales)
sales6$salesbox = BoxCox(sales6$sales, lam)
sales6$salesbox<-BoxCox(sales6$zero_removed_sale)

min_sale<-min(sales6$sales)-1

sales6$logsale_real<-log(sales6$sales-min_sale)

sales6$sugar_free<-factor(ifelse(grepl("Sugar Free",sales6$`Materiall Desc`),1,0))


sales6<-sales6%>%
  group_by(Material,Year)%>%
  mutate(median_n=median(sales,na.rm = T),
         sales_wt_median=ifelse(sales<=1,median_n,sales))

sales6$sales_wt_median[sales6$sales_wt_median<=1]<-1

sales6<-sales6%>%mutate(Pack2=ifelse(grepl("1.5 Ltr", Pack),"2 Ltr",Pack))


sales6$pack2<-factor(sales6$Pack)
sales6$pack2<-factor(gsub("2 Ltr","1.5 Ltr",sales6$pack2))



sales7<-sales6


sales7$logsale<-log(sales7$zero_removed_sale)

e<-sales7%>%filter(!(Year %in% c(2014)))#%>%filter(!(grepl("Sugar Free",`Materiall Desc`)))#%>%filter(Material %in% skus$`Item Code`)
#%>%filter(!(grepl("2 Ltr",`Materiall Desc`) & date_n > as.Date("2018-05-01")))
#%>%filter(!(grepl("Sugar Free",`Materiall Desc`)))
e$id<-1:nrow(e)


tt<-e



## modelling ###






myControl  <-trainControl(method = "cv"
                          ,number=10
                          # ,repeats=5, 
                          ,verboseIter = T
                          ,savePredictions = 'final')


gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:22)*100,
                        shrinkage = c(0.01,0.1,0.2),
                        n.minobsinnode = 20)






xgbGrid <- expand.grid(
  nrounds = seq(from = 200, to = 2000, by = 100),
  eta = c(0.025, 0.05, 0.1, 0.3),# learning rate
  max_depth = c(2, 3, 4, 5, 6),# complexity of the trees
  gamma = 0,# regularization
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)



gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3,5),
                        n.trees = (1:10)*50,
                        shrinkage = c(0.01,0.1,0.2),
                         n.minobsinnode = c(20,10))





# rfgrid <- expand.grid(.mtry=c(1:15),.ntree=c(1000,1500))




mm<-7




e_2019<-e%>%
  filter(Year %in% c(2018,2019))%>%
  filter(!(Year==2019 & month==mm))
  # filter(month %in% c(12,1,2))

t1<-sample(e_2019$id,150,replace = F)
t2<-sample(e_2019$id,150,replace = F)
t3<-sample(e_2019$id,150,replace = F)
t4<-sample(e_2019$id,150,replace = F)
t5<-sample(e_2019$id,150,replace = F)
t6<-sample(e_2019$id,150,replace = F)



d1<-e_2019%>%
  filter(id %in% t1)

d2<-e_2019%>%
  filter(id %in% t2)

d3<-e_2019%>%
  filter(id %in% t3)

d4<-e_2019%>%
  filter(id %in% t4)

d5<-e_2019%>%
  filter(id %in% t5)

d6<-e_2019%>%
  filter(id %in% t6)


e2<-e%>%rbind(d1,d2,d3,d4,d5,d6)


gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3, 5),
                        n.trees = (1:10)*100,
                        shrinkage = c(0.01,0.1,0.2),
                        n.minobsinnode = c(20,10))


gbmGrid <-  expand.grid(interaction.depth = c(3),
                        n.trees = c(150),
                        shrinkage = c(0.1),
                        n.minobsinnode = c(10))


xgbGrid <- expand.grid(
  nrounds = c(2000),
  eta = c(0.05),# learning rate
  max_depth = c(4),# complexity of the trees
  gamma = 0,# regularization
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

lineargrid = expand.grid(nrounds = c(150,200,300,500), lambda= c(0.2,0.15,0.1,0.01,0.001), alpha = c(1,2), eta = c(0.4,0.3,0.2,0.1,0.05))


e3<-e%>%filter((Year==2019 & month  %in% c(8,mm)))


reg2 <- caret::train(#zero_removed_sale~
                        sales_wt_median~
                        # sales~
                       new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                       # factor(Material)+
                       # factor(Pack)+
                       # factor(Flavor)+
                       scale(window)+scale(lagsale3)
                     +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                       sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                       m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                     +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                     +pack_tot_lag2+pack_tot_lag3+pack_tot_lag+pack_tots
                     # +year2015+year2016+year2017+year2018
                     +scale(bottles)
                     +scale(lag_rd)
                     +scale(lag_rd2)
                     +lag_rd3
                     +lag_rd4
                     +lagrd_window
                     +lag_ratio
                     +max_mat
                     +min_mat
                     +trend
                     +trend2
                     +monthly_avg
                     +flav_ratio
                     +price_per_liter
                     # +max_price
                     +scale(mean_mat)
                     +sd_mat
                     +diff_mat
                     # +sugar_free
                     , 
                     # preProcess=c("center", "scale"),
                     
                     data = e%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
                     
                     # %>%filter(Material %in% skus$`Item Code`)
                     # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                     ,
                     method = "gbm",
                     # method = "rf",
                     # method="xgbTree",
                      # method="xgbLinear",
                     # method = "lm",
                     trControl = myControl
                     # ,preProc=c("scale")
                     ,na.action = na.pass
                     # ,tuneGrid = lineargrid
                     # ,tuneGrid = xgbGrid
                     # ,tuneGrid = gbmGrid
)









e2<-e%>%filter(!(Year==2019 & month  %in% c(8,mm)))

print(reg2)
reg2$results$RMSE
summary(reg2)

# e2$predict_gbm<-predict(reg2, newdata = e2)
e$predict_gbm<-predict(reg2, newdata = e)
e3$predict_gbm<-predict(reg2, newdata = e3)



init_model<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(varience=round(((abs(predict_gbm-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_gbm,varience)%>%
  arrange(-varience)

init2<-init_model%>%
  filter(Material %in% skus$`Item Code`)


mean(init2$varience)
init2%>%filter(varience>15)%>%nrow()








e2$OOF_pred_init<-reg2$pred$pred






############################# blend###########################################################

e$rand_n<-sample(1:nrow(e), nrow(e), replace=F)

data1<-e%>%filter(rand_n %in% 1:(nrow(e)/3))%>%
  filter(!(Year==2019 & month  %in% c(7,8,1)))

data2<-e%>%filter(rand_n %in% (nrow(e)/3):((nrow(e)/3)*2))%>%
  filter(!(Year==2019 & month  %in% c(7,8,1)))

data3<-e%>%filter(rand_n %in% ((nrow(e)/3)*2):nrow(e))%>%
  filter(!(Year==2019 & month  %in% c(7,8,1)))


predictors<-c("new_price2","lagp","month","lagsale2","Year","Pack","window","lagsale3","pack_ratio_ly","pack_ratio_window","sku_contrib_lag",
              "m_tot_lag2","pack_tot_lag2","sku_lag_2","sku_lag_3","sku_lag_4","flv_contrib_ly","flv_contrib_window","flv_contrib_lag",
              "bottles","lag_rd","lag_rd2","lag_rd3","lag_rd4","lagrd_window","max_mat","min_mat","trend","trend2","monthly_avg",
              "flav_ratio","price_per_liter","max_price","mean_mat","trend2","diff_mat")

outcome<-c("sales_wt_median")





feature_select <- rfe(e[,predictors], e[[outcome]],
                         rfeControl = control)





### feature selection ###

e2<-e%>%select(-Year)%>%
  rename("meterial_desc"="Materiall Desc")

e2<-e2[,2:89]

binarized_e <- e2 %>%
  select(c(-id,-date_n)) %>%
  binarize(n_bins = 4, thresh_infreq = 0.01)


k<- e[,c("sales","lagsale2","lagsale3","new_price","window","pack_ratio_ly","pack_ratio_window","pack_ratio_lag",
              "sku_contrib_ly","sku_contrib_window","sku_contrib_lag","m_tot_lag2","pack_tot_lag2","sku_lag_2","sku_lag_3",
              "flv_contrib_ly","flv_contrib_window","flv_contrib_lag","bottles","lag_rd","lag_rd2","lag_rd3","lag_rd4","lag_ratio",
              "max_mat","min_mat","trend","monthly_avg","flav_ratio","price_per_liter","flv_tots","sale15","flv_lag2",
         # "predict_gbm","predict_exp_st", "predict_exp1","expst_all",
         "trend2","max_price","mean_mat","diff_mat")]



correlated_e <- k%>%
  correlate(target = sales)







model1<-caret::train(data1[,predictors],
                y=data1$logsale,
                method='gbm',
                # data = e2%>%filter(!(Year==2019 & month  %in% c(7,8,1))),
                trControl=myControl
                # tuneLength=3,
                # na.action = na.pass
                )


model2<-caret::train(data2[,predictors],
                     y=data2$sales_wt_median,
                     method='gbm',
                     # data = e2%>%filter(!(Year==2019 & month  %in% c(7,8,1))),
                     trControl=myControl
                     # tuneLength=3,
                     # na.action = na.pass
                )

model3<-caret::train(data3[,predictors],
                     y=data3$sales,
                     method='gbm',
                     # data = e2%>%filter(!(Year==2019 & month  %in% c(7,8,1))),
                     trControl=myControl
                     # tuneLength=3,
                     # na.action = na.pass
)

e$mod1<-exp(predict(model1, newdata = e))
e$mod2<-predict(model2, newdata = e)
e$mod3<-predict(model3, newdata = e)




blend <- caret::train(
  sales_wt_median~
    mod1+mod2+mod3
  , 
  # preProcess=c("center", "scale"),
  
  data = e%>%filter(!(Year==2019 & month  %in% c(7,8,1)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
  
  # %>%filter(Material %in% skus$`Item Code`)
  # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
  ,
  # method = "lm",
  # method = "rf",
  method="xgbTree",
  # method = "lm",
  trControl = myControl
  # ,preProc=c("scale")
  ,na.action = na.pass
  # ,tuneGrid = xgbGrid
  # ,tuneGrid = gbmGrid
)

summary(blend)

e$pred_blend<-predict(blend, newdata = e)



blend_model<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(1))%>%
  mutate(var_blend=round(((abs(pred_blend-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,pred_blend,var_blend)%>%
  arrange(-var_blend)%>%
  filter(Material %in% skus$`Item Code`)



mean(blend_model$var_blend)
blend_model%>%filter(var_blend>15)%>%nrow()







sapply(e, function(x)sum(is.infinite(x)))










### log ensembling  ####################################################

e$id<-1:nrow(e)

t1<-sample(e$id,nrow(e)/1.4,replace = F)

t2<-sample(e$id,nrow(e)/1.4,replace = F)

t3<-sample(e$id,nrow(e)/1.4,replace = F)

t4<-sample(e$id,nrow(e)/1.4,replace = F)




data1<-e%>%
  filter(id %in% t1)

data2<-e%>%
  filter(id %in% t2)

data3<-e%>%
  filter(id %in% t3)

data4<-e%>%
  filter(id %in% t4)



gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 5, 7),
                        n.trees = (1:15)*100,
                        shrinkage = c(0.01,0.1,0.2),
                        n.minobsinnode = c(20,10))




xgbGrid <- expand.grid(
  nrounds = seq(from = 200, to = 2000, by = 100),
  eta = c(0.025, 0.05, 0.1, 0.3),# learning rate
  max_depth = c(2, 3, 4, 5, 6),# complexity of the trees
  gamma = 0,# regularization
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)




reg1 <- caret::train(#zero_removed_sale~
              #sales_wt_median~
              logsale~
                new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)
              # +Material
              # +Pack
              # +Flavor
              +scale(window)+scale(lagsale3)+scale(lagsale2)
              +pack_ratio_ly+pack_ratio_window+pack_ratio_lag
              +sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3
              +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
              # +year2015+year2016+year2017+year2018
              +scale(lag_rd)
              +lag_rd2
              +lag_rd3
              +lag_rd4
              +lagrd_window
              +lag_ratio
              +max_mat
              +min_mat
              +bottles
              +trend
              +trend2

              # +p_log3
              # +p_log2
              # +p_log1
              # +p_log4
              # +predict_gbm
              , 
              # preProcess=c("center", "scale"),
              data = e%>%filter(!(Year==2019 & month %in% c(8,mm)))#%>%filter(!(Year==2015))
              # %>%filter(!(grepl("Sugar Free",`Materiall Desc`)))
              # %>%filter(Material %in% skus$`Item Code`)
              ,
              # method = "xgbTree",
              method = "xgbLinear",
              # method="gbm",
              # method = "lm",
              trControl = myControl,
              na.action = na.pass
              # ,tuneGrid = gbmGrid
              # ,tuneGrid = xgbGrid
)

print(reg1)
reg1$results$RMSE
summary(reg1)


e$lgf<-exp(predict(reg1, newdata = e))
e3$lgf<-predict(reg1, newdata = e3)




init_model_log<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(varience_log=round(((abs(lgf-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,lgf,varience_log)%>%
  arrange(-varience_log)


log2<-init_model_log%>%
  filter(Material %in% skus$`Item Code`)

mean(log2$varience_log)
log2%>%filter(varience_log>15)%>%nrow()





#########################  end of log  ####################################################



gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3),
                        n.trees = (1:10)*50,
                        shrinkage = c(0.01,0.1,0.2),
                        n.minobsinnode = c(20,10))

gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3),
                        n.trees = (1:10)*50,
                        shrinkage = c(0.1),
                        n.minobsinnode = c(10))






reg_log <- caret::train(logsale~
                new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                Material+
                #  Pack+
                #Flavor+
                scale(window)+scale(lagsale3)
              +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
              +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
              # +year2015+year2016+year2017+year2018
              +bottles
              +scale(lag_rd)
              +lag_rd2
              +lag_rd3
              +lag_rd4
              +lagrd_window
              +lag_ratio
              +max_mat
              +min_mat
              +trend
              +trend2
              +monthly_avg
              +flav_ratio
              +price_per_liter
              +max_price
              +mean_mat
              +diff_mat
              
              # +sd_mat
              , 
              # preProcess=c("center", "scale"),
              
              data = e%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
              
              # %>%filter(Material %in% skus$`Item Code`)
              # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
              ,
              method = "gbm",
              # method="xgbTree",
              # method = "lm",
              trControl = myControl
               # ,preProc=c("scale")
              ,na.action = na.pass
              # ,tuneGrid = xgbGrid
              # ,tuneGrid = gbmGrid
              # ,maximize = FALSE
              # ,interaction.depth=5
              # ,n.trees=100
)


print(reg2)
reg2$results$RMSE
summary(reg2)


e$predict_gbm<-exp(predict(reg2, newdata = e))



init_model<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(varience=round(((abs(predict_gbm-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_gbm,varience)%>%
  arrange(-varience)

init2<-init_model%>%
  filter(Material %in% skus$`Item Code`)


mean(init2$varience)




rf <- caret::train(zero_removed_sale~
              new_price2+lagp+factor(Year)+factor(month)+scale(lagsale2)+
              # Material+
              #  Pack+
              #Flavor+
              scale(window)+scale(lagsale3)
            +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
              sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
              m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
            +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
            +bottles
            +scale(lag_rd)
            +lag_rd2
            +lag_rd3
            +lag_rd4
            +lagrd_window
            +lag_ratio
            +max_mat
            +min_mat
            +trend
            +trend2
            +monthly_avg
            +flav_ratio
            +price_per_liter
            # +mean_mat
            # +sd_mat
            +max_price
            +mean_mat
            +scale(mean_mat)
            +sd_mat
            +diff_mat
            , 
            # preProcess=c("center", "scale"),
            
            data = e
            %>%filter(!(Year==2019 & month %in% c(8,mm)))
            # %>%filter(Material %in% skus$`Item Code`)
            # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
            ,
            # method = "rf",
            # metric = 'rmse',
            method="xgbTree",
            # method = "xgbLinear",
            trControl = myControl
            # preProc=c("center","scale")
            ,na.action = na.pass
            # ,tuneGrid = lineargrid
            ,tuneGrid = xgbGrid
            # ,maximize = FALSE
)





print(rf)
e$predict_rf<-predict(rf, newdata = e)


rf_model<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(var_rf=round(((abs(predict_rf-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_rf,var_rf)%>%
  arrange(-var_rf)

rf2<-rf_model%>%
  filter(Material %in% skus$`Item Code`)
# mutate(duplicated=duplicated(Material))%>%
# filter(duplicated==FALSE)

mean(rf2$var_rf)




# sales7$id<-1:nrow(sales7)

t1<-sample(e$id,nrow(e)/1.4,replace = F)

t2<-sample(e$id,nrow(e)/1.4,replace = F)

t3<-sample(e$id,nrow(e)/1.4,replace = F)



data1<-e%>%
  filter(id %in% t1)

data2<-e%>%
  filter(id %in% t2)

data3<-e%>%
  filter(id %in% t3)


gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3),
                        n.trees = (1:10)*50,
                        shrinkage = c(0.1),
                        n.minobsinnode = c(10))


mm<-2


exp1 <- caret::train(zero_removed_sale~
                       new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                       # Material+
                       #  Pack+
                       #Flavor+
                       # factor(pack2)
                       scale(window)+scale(lagsale3)
                     +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                       sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                       m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                     +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                     # +year2015+year2016+year2017+year2018
                     # +bottles
                     # # +lagsale
                     # +scale(lag_rd)
                     # +lag_rd2
                     # +lag_rd3
                     +lag_rd4
                     +lagrd_window
                     +lag_ratio
                     +max_mat
                     +min_mat
                     +trend
                     +trend2
                     +monthly_avg
                     +flav_ratio
                     +price_per_liter
                     +max_price
                     +mean_mat
                     
                     # +sd_mat
                     , 
                     # preProcess=c("center", "scale"),
                     
                     data = data1%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
                     
                     # %>%filter(Material %in% skus$`Item Code`)
                     # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                     ,
                     method = "gbm",
                     # method="xgbTree",
                     # method = "lm",
                     trControl = myControl
                     # ,preProc=c("scale")
                     ,na.action = na.pass
                     # ,tuneGrid = xgbGrid
                     ,tuneGrid = gbmGrid
                     # ,maximize = FALSE
                     # ,interaction.depth=5
                     # ,n.trees=100
)


e$predict_exp1<-predict(exp1, newdata = e)



myControl  <-trainControl(method = "cv"
                          ,number=5
                          # ,repeats=5,
                          ,verboseIter = T
                          )


exp2 <- caret::train(sales_wt_median~
                       # new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                       # Material+
                       #  Pack+
                       #Flavor+
                      # scale(window)+scale(lagsale3)
                     +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                       #sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                       m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                     #+flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                     # +year2015+year2016+year2017+year2018
                     +bottles
                     +scale(lag_rd)
                     +lag_rd2
                     +lag_rd3
                     +lag_rd4
                     +lagrd_window
                     +lag_ratio
                     +max_mat
                     +min_mat
                     +trend
                     +trend2
                     +monthly_avg
                     +flav_ratio
                     +price_per_liter
                     +max_price
                     +mean_mat
                     
                     # +sd_mat
                     , 
                     # preProcess=c("center", "scale"),
                     
                     data = data2%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
                     
                     # %>%filter(Material %in% skus$`Item Code`)
                     # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                     ,
                     # method = "gbm",
                     method="xgbTree",
                     # method = "lm",
                     trControl = myControl
                     # ,preProc=c("scale")
                     ,na.action = na.pass
                     ,tuneGrid = xgbGrid
                     # ,tuneGrid = gbmGrid
                     # ,maximize = FALSE
                     # ,interaction.depth=5
                     # ,n.trees=100
)


e$predict_exp2<-predict(exp2, newdata = e)



exp3 <- caret::train(#logsale~ #changed log on 8/8 11.30 am
                       sales~
                       new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                       # Material+
                       #  Pack+
                       #Flavor+
                       # scale(window)+scale(lagsale3)
                     +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                       sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                       m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                     +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                     # +year2015+year2016+year2017+year2018
                     +bottles
                     +scale(lag_rd)
                     +lag_rd2
                     +lag_rd3
                     +lag_rd4
                     +lagrd_window
                     +lag_ratio
                     # +max_mat
                     # +min_mat
                     # +trend
                     # +trend2
                     # +monthly_avg
                     +flav_ratio
                     +price_per_liter
                     +max_price
                     # +mean_mat
                
                     , 
                     # preProcess=c("center", "scale"),
                     
                     data = data3%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
                     
                     # %>%filter(Material %in% skus$`Item Code`)
                     # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                     ,
                     method = "gbm",
                     # method="xgbTree",
                     # method = "lm",
                     trControl = myControl
                     # ,preProc=c("scale")
                     ,na.action = na.pass
                     # ,tuneGrid = xgbGrid
                     # ,tuneGrid = gbmGrid
                     # ,maximize = FALSE
                     # ,interaction.depth=5
                     # ,n.trees=100
)


# e$predict_exp3<-exp(predict(exp3, newdata = e))
e$predict_exp3<-predict(exp3, newdata = e)





gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3),
                        n.trees = (1:10)*50,
                        shrinkage = c(0.01,0.1,0.2),
                        n.minobsinnode = c(20,10))


myControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final' # To save out of fold predictions for best parameter combinantions
  # classProbs = T # To save the class probabilities of the out of fold predictions
  ,verboseIter = T
)


xgbGrid <- expand.grid(
  nrounds = seq(from = 200, to = 2000, by = 100),
  eta = c(0.025, 0.05, 0.1, 0.3),# learning rate
  max_depth = c(2, 3, 4, 5, 6),# complexity of the trees
  gamma = 0,# regularization
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

lineargrid = expand.grid(nrounds = c(10,20,30,40,50), lambda= c(0.1), alpha = c(1,2), eta = c(0.2,0.1,0.05))


exp_st <- caret::train(#sales_wt_median~
                       sales~ 
                     #   new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                     #   Material+
                     #   #  Pack+
                     #   #Flavor+
                     #   scale(window)+scale(lagsale3)
                     #   +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                     #   sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                     #   m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                     # +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                     # # +year2015+year2016+year2017+year2018
                     # +bottles
                     # +scale(lag_rd)
                     # +lag_rd2
                     # +lag_rd3
                     # +lag_rd4
                     # +lagrd_window
                     # +lag_ratio
                     # +max_mat
                     # +min_mat
                     # +trend
                     # +trend2
                     # +monthly_avg
                     # +flav_ratio
                     # +price_per_liter
                     # +max_price
                     # +mean_mat
                     
                     +predict_exp3
                     +predict_exp2
                     +predict_exp1

                     , 
                     # preProcess=c("center", "scale"),
                     
                     data = e%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
                     
                     # %>%filter(Material %in% skus$`Item Code`)
                     # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                     ,
                     # method = "gbm",
                     # method="xgbTree",
                     method="xgbLinear",
                     # method = "lm",
                     trControl = myControl
                     # ,preProc=c("scale")
                     ,na.action = na.pass
                     # ,tuneGrid = xgbGrid
                     # ,tuneGrid = lineargrid
                     # ,tuneGrid = gbmGrid
                     # ,maximize = FALSE
                     # ,interaction.depth=5
                     # ,n.trees=100
)

summary(exp_st)
exp_st
exp_st$results$RMSE

e$predict_exp_st<-predict(exp_st, newdata = e)
e3$predict_exp_st<-predict(exp_st, newdata = e3)



expst_model<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(var_expst=round(((abs(predict_exp_st-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_exp_st,var_expst)%>%
  arrange(var_expst)

expst2<-expst_model%>%
  filter(Material %in% skus$`Item Code`)%>%
  arrange(-var_expst)
# mutate(duplicated=duplicated(Material))%>%
# filter(duplicated==FALSE)

mean(expst2$var_expst)


# put all the exp models together 

e<-e%>%mutate(expst_all=(predict_exp3+predict_exp2+predict_exp1+predict_exp_st)/4)



expst_all2<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(var_expst=round(((abs(expst_all-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,`Materiall Desc`,sales,expst_all,var_expst)%>%
  filter(Material %in% skus$`Item Code`)%>%
  arrange(-var_expst)

mean(expst_all2$var_expst)






#### getting the 8th months ##


expst_all_aug<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(var_expst=round(((abs(expst_all-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,`Materiall Desc`,sales,expst_all,var_expst)%>%
  # filter(Material %in% skus$`Item Code`)%>%
  filter(!grepl("FITO|Water",`Materiall Desc`))


e2<-e%>%filter(!(Year==2019 & month  %in% c(8,mm)))



### real blend#################################  OOFs  #########################################

e3<-e%>%filter((Year==2019 & month  %in% c(8,mm)))



e2$OOF_pred_stack<-exp_st$pred$pred[order(exp_st$pred$rowIndex)]
e3$OOF_pred_stack<-predict(exp_st, newdata=e3)


e2$OOF_pred_init<-reg2$pred$pred[order(reg2$pred$rowIndex)]
e3$OOF_pred_init<-predict(reg2, newdata=e3)




e2$OOF_pred_log<-reg1$pred$pred[order(reg1$pred$rowIndex)]
e3$OOF_pred_log<-predict(reg1, newdata=e3)


e2$OOF_pred_linear<-rf$pred$pred[order(rf$pred$rowIndex)]
e3$OOF_pred_linear<-predict(rf, newdata=e3)



blend <- caret::train(sales_wt_median~
                         #   new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                         #   Material+
                         #   #  Pack+
                         #   #Flavor+
                         #   scale(window)+scale(lagsale3)
                         #   +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                         #   sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                         #   m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                         # +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                         # # +year2015+year2016+year2017+year2018
                         # +bottles
                       # +scale(lag_rd)
                       # +lag_rd2
                       # +lag_rd3
                       # +lag_rd4
                       # +lagrd_window
                       # +lag_ratio
                       # +max_mat
                       # +min_mat
                       # +trend
                       # +trend2
                       # +monthly_avg
                       # +flav_ratio
                       # +price_per_liter
                       # +max_price
                       # +mean_mat
                       
                       # +OOF_pred_stack
                       +OOF_pred_init
                       +OOF_pred_log
                        +OOF_pred_linear
                       
                       , 
                       # preProcess=c("center", "scale"),
                       
                       data = e2%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
                       
                       # %>%filter(Material %in% skus$`Item Code`)
                       # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                       ,
                       # method = "gbm",
                       # method="xgbTree",
                       # method = "lm",
                      method="xgbLinear",
                       trControl = myControl
                       # ,preProc=c("scale")
                       ,na.action = na.pass
                       # ,tuneGrid = xgbGrid
                      # ,tuneGrid = lineargrid
                       # ,tuneGrid = gbmGrid
                       # ,maximize = FALSE
                       # ,interaction.depth=5
                       # ,n.trees=100
)

summary(blend)


e2$predict_oof_pred<-predict(blend, newdata = e2)
e3$predict_oof_pred<-predict(blend, newdata = e3)



oof_model<-e3%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(var_oof=round(((abs(predict_oof_pred-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_oof_pred,var_oof)%>%
  filter(Material %in% skus$`Item Code`)%>%
  arrange(-var_oof)


mean(oof_model$var_oof)






blend_linear <- caret::train(sales_wt_median~
                        #   new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                        #   Material+
                        #   #  Pack+
                        #   #Flavor+
                        #   scale(window)+scale(lagsale3)
                        #   +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                        #   sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                        #   m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                        # +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                        # # +year2015+year2016+year2017+year2018
                        # +bottles
                      # +scale(lag_rd)
                      # +lag_rd2
                      # +lag_rd3
                      # +lag_rd4
                      # +lagrd_window
                      # +lag_ratio
                      # +max_mat
                      # +min_mat
                      # +trend
                      # +trend2
                      # +monthly_avg
                      # +flav_ratio
                      # +price_per_liter
                      # +max_price
                      # +mean_mat
                      
                      # +OOF_pred_stack
                      +OOF_pred_init
                      +OOF_pred_log
                      +OOF_pred_linear
                      
                      , 
                      # preProcess=c("center", "scale"),
                      
                      data = e2%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
                      
                      # %>%filter(Material %in% skus$`Item Code`)
                      # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                      ,
                      # method = "gbm",
                      # method="xgbTree",
                      # method = "lm",
                      method="xgbLinear",
                      trControl = myControl
                      # ,preProc=c("scale")
                      ,na.action = na.pass
                      # ,tuneGrid = xgbGrid
                      ,tuneGrid = lineargrid
                      # ,tuneGrid = gbmGrid
                      # ,maximize = FALSE
                      # ,interaction.depth=5
                      # ,n.trees=100
)



e2$predict_oof_pred_linear<-predict(blend_linear, newdata = e2)
e3$predict_oof_pred_linear<-predict(blend_linear, newdata = e3)



oof_model_linear<-e3%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(var_oof=round(((abs(predict_oof_pred_linear-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_oof_pred_linear,var_oof)%>%
  filter(Material %in% skus$`Item Code`)%>%
  arrange(-var_oof)


mean(oof_model_linear$var_oof)







blend_gbm <- caret::train(sales_wt_median~
                               #   new_price2+lagp+factor(month)+scale(lagsale2)+factor(Year)+
                               #   Material+
                               #   #  Pack+
                               #   #Flavor+
                               #   scale(window)+scale(lagsale3)
                               #   +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                               #   sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                               #   m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                               # +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                               # # +year2015+year2016+year2017+year2018
                               # +bottles
                             # +scale(lag_rd)
                             # +lag_rd2
                             # +lag_rd3
                             # +lag_rd4
                             # +lagrd_window
                             # +lag_ratio
                             # +max_mat
                             # +min_mat
                             # +trend
                             # +trend2
                             # +monthly_avg
                             # +flav_ratio
                             # +price_per_liter
                             # +max_price
                             # +mean_mat
                             
                             # +OOF_pred_stack
                             +OOF_pred_init
                             +OOF_pred_log
                             +OOF_pred_linear
                             
                             , 
                             # preProcess=c("center", "scale"),
                             
                             data = e2%>%filter(!(Year==2019 & month  %in% c(8,mm)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
                             
                             # %>%filter(Material %in% skus$`Item Code`)
                             # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                             ,
                             method = "gbm",
                             # method="xgbTree",
                             # method = "lm",
                             # method="xgbLinear",
                             trControl = myControl
                             # ,preProc=c("scale")
                             ,na.action = na.pass
                             # ,tuneGrid = xgbGrid
                             # ,tuneGrid = lineargrid
                             ,tuneGrid = gbmGrid
                             # ,maximize = FALSE
                             # ,interaction.depth=5
                             # ,n.trees=100
)

summary(blend_gbm)

e2$predict_oof_pred_gbm<-predict(blend_gbm, newdata = e2)
e3$predict_oof_pred_gbm<-predict(blend_gbm, newdata = e3)



oof_model_gbm<-e3%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(var_oof=round(((abs(predict_oof_pred_gbm-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_oof_pred_gbm,var_oof)%>%
  filter(Material %in% skus$`Item Code`)%>%
  arrange(-var_oof)


mean(oof_model_gbm$var_oof)



# combining all

e3$oof_combined<-(e3$predict_oof_pred_gbm+e3$predict_oof_pred_linear+e3$predict_oof_pred)/3



oof_model_combined<-e3%>%
  filter(Year==2019)%>%
  filter(month %in% c(mm))%>%
  mutate(var_oof=round(((abs(oof_combined-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,oof_combined,var_oof)%>%
  filter(Material %in% skus$`Item Code`)%>%
  arrange(-var_oof)


mean(oof_model_combined$var_oof)





##### featuretools implementation  #######################


k<- e[,c("id","date_n","sales","lagsale2","lagsale3","new_price","window","pack_ratio_ly","pack_ratio_window","pack_ratio_lag",
         "sku_contrib_ly","sku_contrib_window","sku_contrib_lag","m_tot_lag2","pack_tot_lag2","sku_lag_2","sku_lag_3",
         "flv_contrib_ly","flv_contrib_window","flv_contrib_lag","bottles","lag_rd","lag_rd2","lag_rd3","lag_rd4","lag_ratio",
         "max_mat","min_mat","trend","monthly_avg","flav_ratio","price_per_liter","flv_tots","sale15","flv_lag2",
         "trend2","max_price","mean_mat","diff_mat")]




l<- e[,c("Material","Pack","Flavor")]

l<-l%>%mutate(dup=duplicated(Material))%>%
  filter(dup==FALSE)%>%
  select(Material,Pack,Flavor)

n<-l[,c("Material","Flavor")]


m<-e[,c("id","date_n","Material","Pack","lagsale2","Year","month","lagsale3","new_price","window","pack_ratio_ly","pack_ratio_window","pack_ratio_lag",
            "sku_contrib_ly","sku_contrib_window","sku_contrib_lag","m_tot_lag2","pack_tot_lag2","sku_lag_2","sku_lag_3",
            "flv_contrib_ly","flv_contrib_window","flv_contrib_lag","bottles","lag_rd","lag_rd2","lag_rd3","lag_rd4","lag_ratio",
        "max_mat","min_mat","trend","monthly_avg","flav_ratio","price_per_liter","flv_tots","flv_lag2",
        "trend2","max_price","mean_mat","diff_mat")]


es <- as_entityset(
  l, 
  index = "Material", 
  entity_id = "l", 
  id = "demo"
)



es<-es%>%
  add_entity(entity_id = "m", df = m, index = "id")%>%
  add_relationship(set1 = "l",
                   set2 = "m",
                   idx = "Material")







# ft_matrix <- es %>%
#   dfs(
#     target_entity = "l", 
#     trans_primitives = c("and", "cum_sum","less_than","cum_mean","divide_by_feature","divide_numeric","percentile"),
#     agg_primitives = c("mean", "max", "min", "skew","n_most_common","count")
#   )




ft_matrix <- es %>%
  dfs(
    target_entity = "m", 
    trans_primitives = c("and", "cum_sum","divide_by_feature","percentile"),
    agg_primitives = c("mean", "max", "min", "skew","n_most_common","count")
  )




tidy <- tidy_feature_matrix(ft_matrix, remove_nzv = T, nan_is_na = T)

tidy$sales<-e$sales


nums <- unlist(lapply(tidy, is.numeric))  

tidy1<-tidy[ , nums]



correlated_e <- tidy1%>%
  correlate(target = sales)


tidy<-tidy%>%cbind(m)



tidy.pca <- prcomp(tidy[,c(7:17)])



gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3),
                        n.trees = (1:15)*50,
                        shrinkage = c(0.01,0.1,0.2),
                        n.minobsinnode = c(20,10))


featuretools_reg <- caret::train(#sales_wt_median~
  sales~ 
    factor(month)+scale(lagsale2)+factor(Year)+
    # Material+
    #  Pack+
    #Flavor+
    scale(window)+scale(lagsale3)
    +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
    sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
    m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3
  +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
  # +year2015+year2016+year2017+year2018
  +bottles
  +scale(lag_rd)
  +lag_rd2
  +lag_rd3
  +lag_rd4
  # +lagrd_window
  +lag_ratio
  +max_mat
  +min_mat
  +trend2
  +monthly_avg
  +flav_ratio
  +price_per_liter
  +max_price
  +mean_mat
  +`PERCENTILE(sku_contrib_lag)`
  +`PERCENTILE(1 / sku_contrib_lag)`
  +`l.MEAN(m.window)`
  +`l.MAX(m.window)`
  +`PERCENTILE(window)`
  +`PERCENTILE(1 / lagsale3)`
  +`l.MAX(m.pack_ratio_ly)`
  +`PERCENTILE(pack_ratio_window)`
  +`l.MEAN(m.sku_contrib_ly)`
  +`l.MEAN(m.sku_contrib_lag)`
  +`l.MAX(m.m_tot_lag2)`
  +`PERCENTILE(pack_tot_lag2)`
  +`l.MAX(m.sku_lag_2)`
  +`l.MAX(m.flv_contrib_ly)`
  +`l.MEAN(m.monthly_avg)`
  +`PERCENTILE(1 / flav_ratio)`

  
  , 
  # preProcess=c("center", "scale"),
  
  data = tidy%>%filter(!(Year==2019 & month  %in% c(8,6)))#%>%filter(!(lagsale2==0))%>%filter(!(sales==0))
  
  # %>%filter(Material %in% skus$`Item Code`)
  # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
  ,
  # method = "gbm",
  method="xgbTree",
  # method="xgbLinear",
  # method = "lm",
  trControl = myControl
  # ,preProc=c("scale")
  ,na.action = na.pass
  ,tuneGrid = xgbGrid
  # ,tuneGrid = lineargrid
  # ,tuneGrid = gbmGrid
  # ,maximize = FALSE
  # ,interaction.depth=5
  # ,n.trees=100
)

summary(featuretools_reg)

tidy$predict_featuretools<-predict(featuretools_reg, newdata = tidy)



featuretools_list<-tidy%>%
  filter(Year==2019)%>%
  filter(month %in% c(6))%>%
  mutate(var_oof=round(((abs(predict_featuretools-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_featuretools,var_oof)%>%
  filter(Material %in% skus$`Item Code`)%>%
  arrange(-var_oof)


mean(featuretools_list$var_oof)




list_primitives <- function() {
  .ft$list_primitives()
}








###stack model 2

# e$id<-1:nrow(e)

t1<-sample(e$id,nrow(e)/1.4,replace = F)

t2<-sample(e$id,nrow(e)/1.4,replace = F)

t3<-sample(e$id,nrow(e)/1.4,replace = F)



data1<-e%>%
  filter(id %in% t1)

data2<-e%>%
  filter(id %in% t2)

data3<-e%>%
  filter(id %in% t3)







st1<- caret::train(zero_removed_sale~
              new_price2+lagp+factor(Year)+factor(month)+scale(lagsale2)+
              # Material+
              #  Pack+
              #Flavor+
              scale(window)+scale(lagsale3)
            +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
              sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
              m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
            +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
            +bottles
            # +lagsale
            +scale(lag_rd)
            +lag_rd2
            +lag_rd3
            +lag_rd4
            +lagrd_window
            +lag_ratio
            +max_mat
            +min_mat
            +trend
            +trend2
            +monthly_avg
            +flav_ratio
            +price_per_liter
            # +mean_mat
            # +sd_mat
            , 
            # preProcess=c("center", "scale"),
            
            data = data1
            %>%filter(!(Year==2019 & month %in% c(8,7,6)))
            # %>%filter(Material %in% skus$`Item Code`)
            # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
            ,
            method = "gbm",
            # method="xgbTree",
            # method = "lm",
            trControl = myControl
            # preProc=c("center","scale")
            ,na.action = na.pass
            # ,tuneGrid = xgbGrid
            
            # ,maximize = FALSE
            # ,interaction.depth=5
            # ,n.trees=100
)



e$pred_st1<-predict(st1, newdata = e)


st2<- caret::train(zero_removed_sale~
              new_price2+lagp+factor(Year)+factor(month)+scale(lagsale2)+
              # Material+
              #  Pack+
              #Flavor+
              scale(window)+scale(lagsale3)
            +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
              sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
              m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
            +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
            +bottles
            # +lagsale
            +scale(lag_rd)
            +lag_rd2
            +lag_rd3
            +lag_rd4
            +lagrd_window
            +lag_ratio
            +max_mat
            +min_mat
            +trend
            +trend2
            +monthly_avg
            +flav_ratio
            +price_per_liter
            # +mean_mat
            # +sd_mat
            , 
            # preProcess=c("center", "scale"),
            
            data = data2
            %>%filter(!(Year==2019 & month %in% c(8,7,6)))#%>%filter(!(Year==2016))
            # %>%filter(Material %in% skus$`Item Code`)
            # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
            ,
            method = "gbm",
            # method="xgbTree",
            # method = "lm",
            trControl = myControl
            # preProc=c("center","scale")
            ,na.action = na.pass
            # ,tuneGrid = gbmGrid
            # ,tuneGrid = xgbGrid
            # ,maximize = FALSE
            # ,interaction.depth=5
            # ,n.trees=100
)


e$pred_st2<-predict(st2, newdata = e)





st3<- caret::train(sales~
              new_price2+lagp+factor(Year)+factor(month)+scale(lagsale2)+
              # Material+
              #  Pack+
              #Flavor+
              scale(window)+scale(lagsale3)
            +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
              sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
              m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
            +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
            +bottles
            # +lagsale
            +scale(lag_rd)
            +lag_rd2
            +lag_rd3
            +lag_rd4
            +lagrd_window
            +lag_ratio
            +max_mat
            +min_mat
            +trend
            +trend2
            +monthly_avg
            +flav_ratio
            +price_per_liter
            # +mean_mat
            # +sd_mat
            , 
            # preProcess=c("center", "scale"),
            
            data = data3
            %>%filter(!(Year==2019 & month %in% c(8,7,6)))
            # %>%filter(Material %in% skus$`Item Code`)
            # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
            ,
            method = "gbm",
            # method="xgbTree",
            # method = "lm",
            trControl = myControl
            # preProc=c("center","scale")
            ,na.action = na.pass
            # ,tuneGrid = gbmGrid
            # ,tuneGrid = xgbGrid
            # ,maximize = FALSE
            # ,interaction.depth=5
            # ,n.trees=100
)





print(st3)
reg2$results$RMSE
summary(st3)


e$pred_st3<-predict(st3, newdata = e)





st4<- caret::train(sales~
                     # new_price2+lagp+factor(Year)+factor(month)+scale(lagsale2)+
                     # # Material+
                     # #  Pack+
                     # #Flavor+
                     # scale(window)+scale(lagsale3)
                   +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
                     sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
                     m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
                   +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
                   # +bottles
                   # # +lagsale
                   # +scale(lag_rd)
                   # +lag_rd2
                   # +lag_rd3
                   # +lag_rd4
                   # +lagrd_window
                   # +lag_ratio
                   # +max_mat
                   # +min_mat
                   # +trend
                   # +trend2
                   # +monthly_avg
                   # +flav_ratio
                   # +price_per_liter
                   # +mean_mat
                   # +sd_mat
                   , 
                   # preProcess=c("center", "scale"),
                   
                   data = e
                   %>%filter(!(Year==2019 & month %in% c(8,7,6)))
                   # %>%filter(Material %in% skus$`Item Code`)
                   # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
                   ,
                   method = "gbm",
                   # method="xgbTree",
                   # method = "lm",
                   trControl = myControl
                   # preProc=c("center","scale")
                   ,na.action = na.pass
                   # ,tuneGrid = xgbGrid
                   
                   # ,maximize = FALSE
                   # ,interaction.depth=5
                   # ,n.trees=100
)



e$pred_st4<-predict(st4, newdata = e)






st_ens<- caret::train(sales~
               #   new_price2+lagp+factor(Year)+factor(month)+scale(lagsale2)+
               #   # Material+
               #   #  Pack+
               #   #Flavor+
               #   scale(window)+scale(lagsale3)
               # +pack_ratio_ly+pack_ratio_window+pack_ratio_lag+
               #   sku_contrib_ly+sku_contrib_window+sku_contrib_lag+
               #   m_tot_lag2+pack_tot_lag2+sku_lag_2+sku_lag_3+sku_lag_4
               # +flv_contrib_ly+flv_contrib_window+flv_contrib_lag
               # +bottles
               # # +lagsale
               # +scale(lag_rd)
               # +lag_rd2
               # +lag_rd3
               # +lag_rd4
               # +lagrd_window
               # +lag_ratio
               # +max_mat
               # +min_mat
               # +trend
               # +trend2
               # +monthly_avg
               # +flav_ratio
               # +price_per_liter
               # +mean_mat
               # +sd_mat
               +pred_st2
               +pred_st1
               # +predict_rf
               +pred_st3
               +pred_st4
               , 
               # preProcess=c("center", "scale"),
               data = e
               %>%filter(!(Year==2019 & month %in% c(8,7,6)))
               # %>%filter(Material %in% skus$`Item Code`)
               # %>%filter(Material %in% c("S42337","S42335","S40605","S44708","S44508","S44608","S41700","S42336","S42345","S40381"))
               ,
               method = "gbm",
               # method="xgbTree",
               # method = "lm",
               trControl = myControl
               # ,preProc=c("knnImpute")
               ,na.action = na.pass
               # ,tuneGrid = gbmGrid
               # ,tuneGrid = xgbGrid
               # ,maximize = FALSE
               # ,interaction.depth=5
               # ,n.trees=100
)



summary(st_ens)
st_ens$results$RMSE
print(st_ens)


e$pred_stack<-predict(st_ens, newdata = e)



stack_model<-e%>%
  filter(Year==2019)%>%
  filter(month %in% c(6))%>%
  mutate(var_stack=round(((abs(pred_stack-sales)/sales))*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,pred_stack,var_stack)%>%
  arrange(-var_stack)

stack2<-stack_model%>%
  filter(Material %in% skus$`Item Code`)

mean(stack2$var_stack)









# averaging


e$average<-(e$predict_exp_st*2/12)+(e$predict_gbm*2/12)+(e$predict_log*2/12)+(e$expst_all*2/12)+(e$predict_exp2*2/12)+(e$pred_stack*2/12)



avg_2<-e%>%as.data.frame()%>%
  filter(Material %in% skus$`Item Code`)%>%
  filter(Year==2019)%>%
  filter(month %in% c(2))%>%
  mutate(varience_avg=round((abs(average-sales)/sales)*100,2))%>%
  arrange(Material)%>%
  select(Year,month,Material,`Materiall Desc`,sales,average,varience_avg)%>%
  arrange(-varience_avg)

mean(avg_2$varience_avg)













h2o.init()


test_id<-sample(e_2019$id,250,replace = F)

testh2o<-e%>%filter(Year==2019)%>%
  filter(!(month==6))%>%
  filter(id %in% test_id)


trainh2o<-e2%>%
  filter(!(Year==2019 & month==6))
  # filter(!(id %in% test_id))






train<-e[c('zero_removed_sale','new_price2', 'lagp', 'Year','month','lagsale2','lagsale3',
                    # 'Material',
                    # 'Pack',
                    'window',
                    'pack_ratio_ly','pack_ratio_window','pack_ratio_lag','sku_contrib_ly','sku_contrib_window','sku_contrib_lag',
                    'm_tot_lag2','pack_tot_lag2','sku_lag_2','sku_lag_3','sku_lag_4',
                    'flv_contrib_ly','flv_contrib_window','flv_contrib_lag',
                    'lag_rd','lag_rd2','lag_rd3','lag_rd4','lagrd_window','trend2','trend',
                    'max_mat','min_mat','bottles','monthly_avg','flav_ratio','price_per_liter','max_price','mean_mat')]%>%
  filter(!(Year==2019 & month==6))%>%
  as.h2o()


test<-testh2o[c('zero_removed_sale','new_price2', 'lagp', 'Year','month','lagsale2','lagsale3',
                  # 'Material',
                  # 'Pack',
                  'window',
                  'pack_ratio_ly','pack_ratio_window','pack_ratio_lag','sku_contrib_ly','sku_contrib_window','sku_contrib_lag',
                  'm_tot_lag2','pack_tot_lag2','sku_lag_2','sku_lag_3','sku_lag_4',
                  'flv_contrib_ly','flv_contrib_window','flv_contrib_lag',
                  'lag_rd','lag_rd2','lag_rd3','lag_rd4','lagrd_window','trend2','trend',
                  'max_mat','min_mat','bottles','monthly_avg','flav_ratio','price_per_liter','max_price','mean_mat')]%>%as.h2o()



y<-'zero_removed_sale'
x<-setdiff(names(train), y)





aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train,
                   # leaderboard_frame = test,
                  max_runtime_secs = 300)



aml@leaderboard


f<-as.h2o(e%>%
            filter(Material %in% skus$`Item Code`))

f$predict_ensemble<-h2o.predict(aml, newdata = f)

h2o_model<-f%>%as.data.frame()%>%
  filter(Year==2019)%>%
  filter(month %in% c(6))%>%
  mutate(varience_h2o=(abs(predict_ensemble-sales)/sales)*100)%>%
  arrange(Material)%>%
  select(Year,month,Material,sales,predict_ensemble,varience_h2o)%>%
  arrange(-varience_h2o)


mean(h2o_model$varience_h2o)






