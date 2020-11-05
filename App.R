library(tidyverse)
create_data<-function(sample_size,min_size,max_size,intial_value){
  xdata<-sample(x = min_size:max_size,size=sample_size,replace = TRUE)
  ydata<-sample(x = min_size:max_size,size=sample_size,replace = TRUE)
  zdata<-sample(x = min_size:max_size,size=sample_size,replace = TRUE)
  data.frame(
    id = 1:sample_size,
    x=xdata,
    y=ydata,
    z=zdata)->sample_data
  plot.dat= data.frame()
  head(sample_data)
  for (i in 1:sample_size) {
    m = 1+((i-1)*10)+intial_value
    n = i*10+intial_value
    x = seq(m,n,0.1)
    sinx = sin(x)*xdata[i]
    siny = cos(x)*ydata[i]
    sinz = sin(x)*zdata[i]
    values = c(sinx,siny,sinz)
    # Create a variable that indicates what is being plotted
    function.type = rep(c("x","y","z"), each = length(x))
    xval = c(x,x,x)
    plot.dat = rbind(plot.dat,data.frame(values, xval, function.type))
  }
  plot.dat
}
sample_size<-20
cases<-1
plot.dat=data.frame()
data<- sample(x=c(TRUE,FALSE),size=cases,replace = TRUE)
for(i in 1:cases){
  if(data[i]){
    plot.dat=rbind(plot.dat,create_data(sample_size,20,50,sample_size*10*i))
  }
  else{
    plot.dat=rbind(plot.dat,create_data(sample_size,-19,19,sample_size*10*i))
  }
}

read.csv("data.csv")->plot.dat
ggplot(plot.dat, 
       aes(x = xval, y = values, colour = function.type))+
  geom_line()+
  theme_minimal()
colMeans(sample_data)->sample_mean
#write.csv(plot.dat,"data.csv")
