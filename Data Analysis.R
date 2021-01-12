library(data.table)
library(dplyr)
library(ggplot2)
library(cowplot)

RawData<-fread("./bestsellers with categories.csv",header =TRUE)

RawData<-tbl_df(RawData)
RawData$Genre<-as.factor(RawData$Genre)

getData<-function(i){
  DataFrameName<-RawData%>%filter(Year == i)%>%arrange(Name)
  }

Df2009<-getData(2009)
Df2010<-getData(2010)
Df2011<-getData(2011)
Df2012<-getData(2012)
Df2013<-getData(2013)
Df2014<-getData(2014)
Df2015<-getData(2015)
Df2016<-getData(2016)
Df2017<-getData(2017)
Df2018<-getData(2018)
Df2019<-getData(2019)


SummaryGern<-function(DataFrameName){
    Count<-DataFrameName%>%group_by(Genre)%>%summarise(count = n())
    
    DataMean<-DataFrameName%>%select(Year,Genre,`User Rating`,Reviews,Price)%>%group_by(Genre)%>%
              summarise_each(mean)%>%relocate(Year,.before = Genre)%>%
              rename(User.Rating.Mean=`User Rating`,
                     Reviews.Mean=Reviews,
                     Price.Mean=Price)
    
    DataMedian<-DataFrameName%>%select(Year,Genre,`User Rating`,Reviews,Price)%>%group_by(Genre)%>%
                summarise_each(median)%>%relocate(Year,.before = Genre)%>%
                rename(User.Rating.Median=`User Rating`,
                       Reviews.Median=Reviews,
                       Price.Median=Price)

    Data<-inner_join(DataMean,DataMedian)
    Data<-inner_join(Data,Count)
    
    return(Data)
}

a<-SummaryGern(Df2009)
b<-SummaryGern(Df2010)
c<-SummaryGern(Df2011)
d<-SummaryGern(Df2012)
e<-SummaryGern(Df2013)
f<-SummaryGern(Df2014)
g<-SummaryGern(Df2015)
h<-SummaryGern(Df2016)
i<-SummaryGern(Df2017)
j<-SummaryGern(Df2018)
k<-SummaryGern(Df2019)

summarydata<-rbind(a,b,c,d,e,f,g,h,i,j,k)
summarydata<-summarydata[order(summarydata$Year),]
summarydata$Year<-as.Date(as.character(summarydata$Year),format= "%Y")

rm(a,b,c,d,e,f,g,h,i,j,k)

graph1<-ggplot(summarydata,aes(x=Year,y=count,color=Genre))
graph1<-graph1+geom_line()+scale_x_date(date_break= "1 year",date_labels = "%Y")
graph1<-graph1+geom_point()+ylim(c(15,35))
graph1<-graph1+ggtitle("Count of Amazon Top 50 Best Selling Book Based on Genre from 2009 to 2019")

graph2<-ggplot(summarydata,aes(x=Year,y=User.Rating.Mean,color=Genre))
graph2<-graph2+geom_line()+scale_x_date(date_break= "1 year",date_labels = "%Y")
graph2<-graph2+geom_point()+theme(legend.position = "none")+ylim(c(4.4,5.0))

graph3<-ggplot(summarydata,aes(x=Year,y=User.Rating.Median,color=Genre))
graph3<-graph3+geom_line()+scale_x_date(date_break= "1 year",date_labels = "%Y")
graph3<-graph3+geom_point()+ylim(c(4.4,5.0))

plot_row1<-plot_grid(graph2,graph3,rel_widths = c(0.8,1))

titlebindplot1<-ggdraw()+draw_label("Average and Median User Rating of Amazon Top 50 Best Selling Book Based on Genre from 2009 to 2019")
BindGraph1<-plot_grid(titlebindplot1,plot_row1,ncol=1,rel_heights = c(0.1,1))

graph4<-ggplot(summarydata,aes(x=Year,y=Reviews.Mean,color=Genre))
graph4<-graph4+geom_line()+scale_x_date(date_break= "1 year",date_labels = "%Y")
graph4<-graph4+geom_point()+theme(legend.position = "none")+ylim(c(1500,25000))

graph5<-ggplot(summarydata,aes(x=Year,y=Reviews.Median,color=Genre))
graph5<-graph5+geom_line()+scale_x_date(date_break= "1 year",date_labels = "%Y")
graph5<-graph5+geom_point()+ylim(c(1500,25000))

plot_row2<-plot_grid(graph4,graph5,rel_widths = c(0.8,1))
titlebindplot2<-ggdraw()+draw_label("Average and Median User Reviews of Amazon Top 50 Best Selling Book Based on Genre from 2009 to 2019")
BindGraph2<-plot_grid(titlebindplot2,plot_row2,ncol=1,rel_heights = c(0.1,1))


graph6<-ggplot(summarydata,aes(x=Year,y=Price.Mean,color=Genre))
graph6<-graph6+geom_line()+scale_x_date(date_break= "1 year",date_labels = "%Y")
graph6<-graph6+geom_point()+theme(legend.position = "none")+ylim(c(6,25))

graph7<-ggplot(summarydata,aes(x=Year,y=Price.Median,color=Genre))
graph7<-graph7+geom_line()+scale_x_date(date_break= "1 year",date_labels = "%Y")
graph7<-graph7+geom_point()+ylim(c(6,25))

plot_row3<-plot_grid(graph6,graph7,rel_widths = c(0.8,1))
titlebindplot3<-ggdraw()+draw_label("Average and Mean Price of Amazon Top 50 Best Selling Book Based on Genre from 2009 to 2019")
BindGraph3<-plot_grid(titlebindplot3,plot_row3,ncol=1,rel_heights = c(0.1,1))

png("Plot1.png",width = 720,height = 420)
print(graph1)
dev.off()

png("Plot2.png",width = 800,height=500)
print(BindGraph1)
dev.off()

png("Plot3.png",width = 800,height = 500)
print(BindGraph2)
dev.off()

png("Plot4.png",width = 800,height = 500)
print(BindGraph3)
dev.off()
