library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

transactions <- read_csv("transaction.csv")
products <- read_csv("products.csv")

options(stringsAsFactors = FALSE)

#we join the transactions and products datasets into one dataset called transactionsANDproducts
transactionsANDproducts <-full_join(transactions, products, by ="UPC")
class(transactionsANDproducts$WEEK_END_DATE)

# Then use as.Date() to convert text strings into dates, 
transactionsANDproducts$WEEK_END_DATE<-as.Date(transactionsANDproducts$WEEK_END_DATE,"%d-%b-%y")

# and format() to change the way the dates are formatted we use %b(month abbreviated name)-%Y(all 4 digits of the year)
transactionsANDproducts$month_year<-format(transactionsANDproducts$WEEK_END_DATE,"%b-%Y")

t2<-write.csv(transactionsANDproducts,"transactionsproducts.csv", row.names=FALSE)

#We import transactionsproducts and change month_year from character to date as "%b-%Y"

transactionsproducts %>%
  ggplot(aes(x=month_year, y=PRICE, group=CATEGORY, color=CATEGORY))+
  geom_line(show.legend=FALSE)+
  labs(title="Prices over Time")+
  facet_wrap(CATEGORY ~ .)+
  theme(axis.text.x=element_text(angle=90),
        plot.title= element_text(hjust=0.5),
        strip.text.x= element_text(size=5))

# we observe the price through 1 year to observe seasonal changes
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

transactions <- read_csv("transaction.csv")
products <- read_csv("products.csv")

options(stringsAsFactors = FALSE)

#After we join the transactions and products datasets into one dataset,
#we take a subset of year 2010 by extracting  the relevant rows and columns

transactionsANDproducts <-full_join(transactions, products, by ="UPC")
year_2010 <- transactionsANDproducts[c(164275:343688),c(1,3,8,15)]
class(year_2010$WEEK_END_DATE)

year_2010$WEEK_END_DATE<-as.Date(year_2010$WEEK_END_DATE,"%d-%b-%y")
year_2010$month_year<-format(year_2010$WEEK_END_DATE,"%b-%Y")

t2<-write.csv(year_2010,"transactionsproducts_2010.csv", row.names=FALSE)

#We turn our 'month_year' axis into a character vector
year_2010$month_year<- as.character(year_2010$month_year)

#Then turn it back into a factor with the levels in the correct order
year_2010$month_year <- factor(year_2010$month_year, levels=unique(year_2010$month_year))

#As before we import transactionsproducts and change month_year to date as "%b-%Y"and plot the graph

year_2010 %>%
  ggplot(aes(x=month_year, y=PRICE, group=CATEGORY, color=CATEGORY))+
  geom_line(show.legend=FALSE)+
  labs(title="Prices over year 2010")+
    facet_wrap(CATEGORY ~ .)+
  theme(axis.text.x=element_text(angle=90),
        plot.title= element_text(hjust=0.5),
        strip.text.x= element_text(size=5))
  
#finally to observe if the changes are consistent we also look at year 2011
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

transactions <- read_csv("transaction.csv")
products <- read_csv("products.csv")

options(stringsAsFactors = FALSE)

#After we join the transactions and products datasets into one dataset,we take a subset of year 2011 
transactionsANDproducts <-full_join(transactions, products, by ="UPC")
year_2011 <- transactionsANDproducts[c(343689:521623),c(1,3,8,15)]
class(year_2011$WEEK_END_DATE)

year_2011$WEEK_END_DATE<-as.Date(year_2011$WEEK_END_DATE,"%d-%b-%y")
year_2011$month_year<-format(year_2011$WEEK_END_DATE,"%b-%Y")

t2<-write.csv(year_2011,"transactionsproducts_2011.csv", row.names=FALSE)

#As before we import transactionsproducts and change month_year to date as "%b-%Y"and plot the graph
#We turn our 'month_year' axis into a character vector
year_2011$month_year<- as.character(year_2011$month_year)

#Then turn it back into a factor with the levels in the correct order
year_2011$month_year <- factor(year_2011$month_year, levels=unique(year_2011$month_year))

#As before we import transactionsproducts and change month_year to date as "%b-%Y"and plot the graph
year_2011 %>%
  ggplot(aes(x=month_year, y=PRICE, group=CATEGORY, color=CATEGORY))+
  geom_line(show.legend=FALSE)+
  labs(title="Prices over year 2011")+
  facet_wrap(CATEGORY ~ .)+
  theme(axis.text.x=element_text(angle=90),
        plot.title= element_text(hjust=0.5),
        strip.text.x= element_text(size=5))




