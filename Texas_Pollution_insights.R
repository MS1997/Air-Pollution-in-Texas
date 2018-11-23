library(dplyr)
library(ggplot2)
library(tidyr)

#import data
tp <- read.csv("texas_pollution.csv")

#EDA
dim(tp) 
head(tp)
tail(tp)
sum(is.na(tp)) #no missing values as sum is zero
names(tp)
str(tp)

summary(tp$product.description)
summary((tp$county))
summary(tp$company)
summary(tp$site)

#cleaning data
tp3 <- gather(tp, "pollutant","quantity_emitted", -c(X, company, site, reporting.year, county,product.description))
write.csv(tp3, "NewTexas_pollution.csv")

#EDA on clean data
dim(tp3) 
head(tp3)
tail(tp3)
sum(is.na(tp3)) #no missing values as sum is zero
names(tp3)
str(tp3)

#************************************************************************************************
#insight 1

tp3 %>% 
  group_by(product.description) %>%
  summarise(sum=sum(quantity_emitted))%>%
  arrange(desc(sum))%>%
  setNames(c("Product", "Total pollutants emitted"))

ggplot(tp3, aes(x = pollutant, y = quantity_emitted,color=pollutant)) + #plot 1 
  geom_point()+
  facet_grid(.~product.description)+
  ggtitle("DISTRIBUTION OF POLLUTANTS FOR DIFFERENT PRODUCTS") +
  xlab("Pollutant") +
  ylab("Quantity of pollutant emitted (in tpy)") 



#***********************************************************************************************
#insight 2

max_min_emission<-function(type) #the argument type takes pollutant type as input
{ #gives the total quantity of pollutant emitted for each product in descending order
  p<-paste("total emission of",type,sep=" ")
  tp3 %>%
    filter(tp3[["pollutant"]]==type)%>%
    group_by(product.description) %>%
    summarise(sum=sum(quantity_emitted))%>%
    arrange(desc(sum))%>%
    setNames(c("Product",p))
}

#calling functions
max_min_emission("co")

max_min_emission("nox")

max_min_emission("pm10")

max_min_emission("pm2.5")

max_min_emission("so2")

max_min_emission("voc")

#stacked graph for comparison of emssion levels of pollutants by product
ggplot(tp3, aes(x=pollutant, y=quantity_emitted, fill=product.description))+ 
  geom_col(position = "fill")+ 
  ggtitle("COMPARION OF EMSSION LEVELS OF POLLUTANTS FOR DIFFERENT PRODUCTS") +
  xlab("Pollutant") +
  ylab("Quantity of pollutant emitted") #plot 2


#************************************************************************************************
#insight 3

tp3 %>% 
  group_by(pollutant) %>%
  summarise(sum=sum(quantity_emitted))%>%
  arrange(desc(sum))%>%
  setNames(c("Pollutant","Total quantity emitted"))

ggplot(tp3, aes(x = pollutant, y = quantity_emitted,fill=pollutant)) + #plot 3
  geom_col()+
  ggtitle("DISTRIBUTION OF POLLUTANTS FOR DIFFERENT PRODUCTS") +
  xlab("Pollutant") +
  ylab("Quantity of pollutant emitted (in tpy)") 


#************************************************************************************************
#insight 4

Plot_for_product<-function(product) #the argument place takes product type as input 
 { #gives graph for quantity of pollutants emitted for the required product
  title<-paste("EMISSION LEVELS OF POLLUTANTS FOR",product, sep=" ")
  tp3 %>%
    filter(tp3[["product.description"]]==product) %>%
    ggplot(aes(x = pollutant, y = quantity_emitted, fill=pollutant)) + #plot 4
    geom_boxplot()+
    ggtitle(title) +
    xlab("Pollutant") +
    ylab("Quantity Emitted (in tpy)")
}

#calling functions
Plot_for_product("CRUDE PETROLEUM & NATURAL GAS")
#checking for outlier quantity emission for voc for crude petroleum and natural gas
tp3 %>%
  select(company, site, county, pollutant, quantity_emitted) %>%
  filter(pollutant=="voc" & tp3$quantity_emitted==max(tp3$quantity_emitted) & tp3$product.description=="CRUDE PETROLEUM & NATURAL GAS")

Plot_for_product("NATURAL GAS TRANSMISSION")

Plot_for_product("NATURAL GAS LIQUIDS")

Plot_for_product("PLASTICS MATERIALS AND SYNTHETIC RESINS")

Plot_for_product("INDUSTRIAL GASES")


#************************************************************************************************  
 #insight 5

  #functions have been called for each type at the end of the insight
 above_90_percentile<-function(type) #the argument type takes pollutant type as input
 { #gives the details of the companies in the top ten percentile emissions for each pollutant
   s<-filter(tp3,tp3[["pollutant"]]==type)
   q<-quantile(s$quantity_emitted,0.9)
   f<- (s %>%
          filter(s[["quantity_emitted"]]>q) %>%
          arrange(desc(quantity_emitted)))
 f
 }
 
 percentage_of_total<-function(type) #the argument type takes pollutant type as input
 {#gives the contribution of the top 10 percentile polluters in the total emission of the required polluatant 
   d<-above_90_percentile(type)
   sum1=sum(d$quantity_emitted)
   d1<-filter(tp3, tp3$pollutant==type)
   sum2=sum(d1$quantity_emitted)
   per=(sum1/sum2)*100
   per
 }
 
 draw_graph<-function(type) #the argument type takes pollutant type as input
 { #gives a graph giving an overview of the emssion levels for each pollutant
   title<-paste("Emission levels of",type, sep=" ")
   tp3 %>%
     filter(tp3[["pollutant"]]==type) %>%
     ggplot(aes(quantity_emitted, fill=county))+ #plot 5
     geom_dotplot()+ ggtitle(title) +xlab("Quantity Emitted (in tpy)")
}
 
 #calling functions
 above_90_percentile("co")
 draw_graph("co")
 percentage_of_total("co")
 
 above_90_percentile("nox")
 draw_graph("nox")
 percentage_of_total("nox")
 
 above_90_percentile("pm10")
 draw_graph("pm10")
 percentage_of_total("pm10")
 
 above_90_percentile("pm2.5")
 draw_graph("pm2.5")
 percentage_of_total("pm2.5")
 
 above_90_percentile("so2")
 draw_graph("so2")
 percentage_of_total("so2")
 
 above_90_percentile("voc")
 draw_graph("voc")
 percentage_of_total("voc")
 
 
#************************************************************************************************ 
#insight 6
 
#functions have been called at the end of the insight
 Cumulate_Pollutants<-function()
   {#gives cumulative emission for each county for each pollutant and the total pollutant emission for the county
   countyarea<-character()
   sumco<-numeric()
   sumnox<-numeric()
   sumpm10<-numeric()
   sumpm2.5<-numeric()
   sumso2<-numeric()
   sumvoc<-numeric()  
   total<-numeric()
   u<-unique(tp3$county)
   a<-as.character(u) #storing all county names in one vector
   for(i in 1:length(a))
   {
     check<-a[i]
     new<-tp3 %>%
       filter(county==check)%>%
       group_by(pollutant) %>%
       summarise(sum=sum(quantity_emitted)) #for a each county, grouping has been done on the basis of pollutant type
     
     new2<-new$sum #storing the cumulative quantities of pollutants emitted in one county
     
     tot<-sum(new2) #total quanity of pollutants emitted in one county
     countyarea<-c(countyarea,a[i])
     sumco<-c(sumco,new2[1])
     sumnox<-c(sumnox,new2[2])
     sumpm10<-c(sumpm10,new2[3])
     sumpm2.5<-c(sumpm2.5,new2[4])
     sumso2<-c(sumso2,new2[5])
     sumvoc<-c(sumvoc,new2[6])
     total<-c(total,tot)
   }
   data.frame(countyarea, sumco, sumnox, sumpm10, sumpm2.5, sumso2, sumvoc,total)
 }
 
 Plot_for_county<-function(place) #The argument place takes county name as an input
   {#gives the graph for emission levels of pollutants for the required county
    title<-paste("EMISSION LEVELS OF POLLUTANTS FOR",place, sep=" ")
   tp3 %>%
     filter(tp3[["county"]]==place) %>%
     ggplot(aes(x = pollutant, y = quantity_emitted, fill=pollutant)) + #plot 6
     geom_col()+
     ggtitle(title) +
     xlab("Pollutant") +
     ylab("Quantity Emitted (in tpy)")
 }
 
 #calling functions
 P<-Cumulate_Pollutants()
 
 #finding the most polluted county
 P1<-P%>%
   arrange(desc(P$total))%>%
   head()
Plot_for_county(P1[1,1])#graph1 for emission levels of pollutants for the most polluted county
Plot_for_county(P1[2,1])#graph2 for emission levels of pollutants for the second most polluted county

#finding the least polluted county
P2<-P%>%
  arrange(desc(P$total))%>%
  tail()
Plot_for_county(P2[6,1])#graph3 for emission levels of pollutants for the least polluted county
Plot_for_county(P2[5,1])#graph4 for emission levels of pollutants for the second least polluted county

#END***************************************************************************************************************
