#install&loading all packages needed 
library(tidyverse)
library(here)
library(ggplot2)
library(plotly)
library(RColorBrewer)
#load the data set
vgsales<-read.csv(here("data","vgsales.csv"))
#show first few rows of the raw data set
head(vgsales)
#Clean up important missing rows in raw data
vgsales <- vgsales[vgsales$Year!='N/A',]
vgsales <- vgsales[vgsales$Publisher!='N/A',]
#check the data
head(vgsales)

###Preparation for Visualisation2
#Group and sum data by year and sales
sales_by_year <- vgsales %>% 
  group_by(Year) %>% 
  summarize(total_sales = sum(Global_Sales))
#Count the number of games by year
games_by_year <- vgsales %>% 
  group_by(Year) %>% 
  summarize(num_games = n())
#Merge the data to get a new dataset with release year, release number and their sales by year
total_sales_by_year <- merge(sales_by_year, games_by_year, by = "Year")

#Visualisation 1
#use ggplot to create a bar chart with line
Release_Sales<-ggplot(total_sales_by_year, aes(x = Year)) +
  geom_bar(aes(y = num_games), stat = "identity", fill = "lightskyblue1") +
  geom_line(aes(y = total_sales, group = 1), color = "cornflowerblue") +
  #Add a second y axis
  scale_y_continuous(breaks=seq(0,1500,by=150),sec.axis = sec_axis(~./10, name = "Total Sales (millions)")) +
  labs(x = "Year", y = "Number of Games", 
       title = "Number of Games Released and Total Sales by Year(million)")+
  theme(plot.title = element_text(size = 13, face = "bold"),
        #cancelling the display of the year for the sake of beauty
        axis.text.x=element_blank())
#Use plotly to make the year display on mouse hover
ggplotly(Release_Sales, tooltip = c("x", "total_sales"))


###Preparation for Visualisation2
#Extract the top ten publishers by sales and their global sales
Top10_Pub <-vgsales %>%
  group_by(Publisher) %>%
  summarize(total_sales = sum(Global_Sales)) %>%
  top_n(10, total_sales) %>%
  arrange(desc(total_sales))
#Extract global sales of other publishers and combine into one
Other_Pub <- vgsales %>%
  group_by(Publisher) %>%
  summarize(total_sales = sum(Global_Sales)) %>%
  filter(!Publisher %in% Top10_Pub$Publisher) %>%
  summarize(Publisher = "other", total_sales = sum(total_sales))
#Merge the data of the top ten publishers and other publishers
Global_Pub <- bind_rows(Top10_Pub, Other_Pub)

#Visualisation 2
#Use ggplot to create a bar chart and convert it to a pie chart
ggplot(Global_Pub, aes(x = "", y = total_sales, fill = Publisher)) +
  geom_bar(stat = "identity",color="white") +
  coord_polar("y", start = 0) +
  #settin color of pie chart
  scale_fill_brewer(palette = "RdYlGn") +
  theme_classic() +
  #Hide unnecessary elements in the chart
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 1.2, size = 20, face = "bold")) +
  ggtitle("Total Sales by Global Publisher")+#setting title
  #Setting the percentage ratio in the center of pie chart
  geom_text(aes(label = paste0(round(total_sales/sum(total_sales) * 100), "%")), 
            position = position_stack(vjust = 0.5),color="grey22",size=3)
#Save a clean version of the chart
ggsave("figs/GlobalSales_by_pubs.png",units = "cm",width =20,height = 10,dpi=1000 )

#Code book
library(codebook)
cb1<-codebook(Global_Pub)
print(cb1,type='html')

cb2<-codebook(total_sales_by_year)
print(cb2,type='html')
