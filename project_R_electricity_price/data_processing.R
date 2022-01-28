package.check <- lapply(
  c("xts","DT","plyr","shiny","splitstackshape","shinyWidgets","plotly","ggpubr","Metrics","rstudioapi","dplyr","data.table","ggplot2","timeDate","stringr","tseries","stats","MASS","strucchange","forecast","dygraphs","readxl","scales","glmnet","parsedate","lubridate"),
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

file_names = list.files()
file_list  = list()

for (i in file_names){
  if (endsWith(i,".csv")){
    print(i)
    file_list[length(file_list)+1]=i
    assign(i,read.csv(i,sep=",",header=TRUE))
  }
}


spain_price             = list()
germany_price           = list()
netherlands_price       = list()

spain_generation        = list()
germany_generation      = list()
netherlands_generation  = list()


for (i in (file_list)){
  if (grepl("Forecast_germany", i, fixed=TRUE)){
    germany_generation[length(germany_generation)+1]=i
  }
  if (grepl("Forecast_spain", i, fixed=TRUE)){
    spain_generation[length(spain_generation)+1]=i
  }
  if (grepl("Forecast_netherlands", i, fixed=TRUE)){
    netherlands_generation[length(netherlands_generation)+1]=i
  }
}



for (i in (file_list)){
  if (grepl("germany", i, fixed=TRUE) && !grepl("Forecast", i, fixed=TRUE)){
    germany_price[length(germany_price)+1]=i
  }
  if (grepl("spain", i, fixed=TRUE) && !grepl("Forecast", i, fixed=TRUE)){
    spain_price[length(spain_price)+1]=i
  }
  if (grepl("netherlands", i, fixed=TRUE) && !grepl("Forecast", i, fixed=TRUE)){
    netherlands_price[length(netherlands_price)+1]=i
  }
}




price_data_processing<-function(list){
  
  size = length(list)
  df = get(list[[1]])[,1:2]
  
  for (i in (2:size)){
    df = rbind(df, get(list[[i]])[,1:2])
  }  
  df$MTU..CET.<-as.POSIXlt(substr(df$MTU..CET.,1,16),tz="CET",'%d.%m.%Y %H:%M')
  
  return(df)
}


generation_data_processing<-function(list){
  
  size = length(list)
  df = get(list[[1]])[,1:2]
  
  for (i in (2:size)){
    df = rbind(df, get(list[[i]])[,1:2])
  }  
  df$MTU<-as.POSIXlt(substr(df$MTU,1,16),tz="UTC",'%d.%m.%Y %H:%M')
  
  return(df)
}


spain_price            = price_data_processing(spain_price)

germany_price          = price_data_processing(germany_price)

netherlands_price      = price_data_processing(netherlands_price)


spain_generation       = generation_data_processing(spain_generation)

germany_generation     = generation_data_processing(germany_generation)

netherlands_generation = generation_data_processing(netherlands_generation)




#explanatory variables

VE_austria<-get("at.csv")
VE_belgium<-get("be.csv")
VE_switzerland<-get("ch.csv")
VE_germany<-get("de.csv")
VE_danemark<-get("dk.csv")
VE_spain<-get("es.csv")
VE_france<-get("fr.csv")
VE_england<-get("gb.csv")
VE_ireland<-get("ie.csv")
VE_italy<-get("it.csv")
VE_luxembourg<-get("lu.csv")
VE_netherlands<-get("nl.csv")
VE_norway<-get("no.csv")
VE_portugal<-get("pt.csv")
VE_sweden<-get("se.csv")

VE_list<-list(VE_austria,
              VE_belgium,
              VE_switzerland,
              VE_germany,
              VE_danemark,
              VE_spain,
              VE_france,
              VE_england,
              VE_ireland,
              VE_italy,
              VE_luxembourg,
              VE_netherlands,
              VE_norway,
              VE_portugal,
              VE_sweden
              )


VE_names_list<-list("VE_austria",
              "VE_belgium",
              "VE_switzerland",
              "VE_germany",
              "VE_danemark",
              "VE_spain",
              "VE_france",
              "VE_england",
              "VE_ireland",
              "VE_italy",
              "VE_luxembourg",
              "VE_netherlands",
              "VE_norway",
              "VE_portugal",
              "VE_sweden"
)




for (i in 1:length(VE_list)){
    VE_list[[i]]$end<-NULL
    VE_list[[i]]$start<-as.POSIXlt(VE_list[[i]]$start,tz="CET",'%Y-%m-%d %H:%M')
    colnames(VE_list[[i]])[2]<-paste0('load_',VE_names_list[[i]])
    VE_list[[i]]<-VE_list[[i]][VE_list[[i]]$start$min==0,]
    rownames(VE_list[[i]])<-1:nrow(VE_list[[i]])
    
}


load_variables <- VE_list[[1]]


for (i in 2:length(VE_list)){
  print(nrow(load_variables))

  VE_distinct <- VE_list[[i]] %>% distinct(start, .keep_all = TRUE)
  load_variables<-inner_join(load_variables, VE_distinct, by = c("start" = "start"))
}



germany_price$Day.ahead.Price..EUR.MWh.<-as.numeric(germany_price$Day.ahead.Price..EUR.MWh.)

germany_price<-na.omit(germany_price)
spain_price<-na.omit(spain_price)
netherlands_price<-na.omit(netherlands_price)


netherlands_generation$Scheduled.Generation..MW...D....BZN.NL <- as.numeric(netherlands_generation$Scheduled.Generation..MW...D....BZN.N) 
spain_generation$Scheduled.Generation..MW...D....BZN.ES       <- as.numeric(spain_generation$Scheduled.Generation..MW...D....BZN.ES )



germany_generation<-na.omit(germany_generation)
spain_generation<-na.omit(spain_generation)
netherlands_generation<-na.omit(netherlands_generation)




germany_dataset<-inner_join(load_variables,germany_price, by = c("start"="MTU..CET.") )
colnames(germany_dataset)[colnames(germany_dataset)=="Day.ahead.Price..EUR.MWh."]<-"germany_price"


all_datasets<-inner_join(germany_dataset,spain_price, by = c("start"="MTU..CET.") )
colnames(all_datasets)[colnames(all_datasets)=="Day.ahead.Price..EUR.MWh."]<-"spain_price"


all_datasets<-inner_join(all_datasets,netherlands_price, by = c("start"="MTU..CET.") )
colnames(all_datasets)[colnames(all_datasets)=="Day.ahead.Price..EUR.MWh."]<-"netherlands_price"



all_datasets<-inner_join(all_datasets,germany_generation, by = c("start"="MTU") )
colnames(all_datasets)[colnames(all_datasets)=="Scheduled.Generation..MW...D....BZN.DE.AT.LU"]<-"germany_generation"

all_datasets<-inner_join(all_datasets,spain_generation, by = c("start"="MTU") )
colnames(all_datasets)[colnames(all_datasets)=="Scheduled.Generation..MW...D....BZN.ES"]<-"spain_generation"

all_datasets<-inner_join(all_datasets,netherlands_generation, by = c("start"="MTU") )
colnames(all_datasets)[colnames(all_datasets)=="Scheduled.Generation..MW...D....BZN.NL"]<-"netherlands_generation"


df<-all_datasets


df['day']<-format (x=df$start,format= "%A")
df['month']<-format(x=df$start,format= "%B")
df['hour']<-format(x=df$start,format= "%H")


day_list<-unique(df['day'])
month_list<-unique(df['month'])
hour_list<-unique(df['hour'])

day_function<-function(x,day){
  if  (x==day){
    return(1)
  }
  else{
    return(0)
  }
}


for ( i in (1:nrow(day_list))){
  df[day_list[[i,1]]]<-sapply(df$day,function(x) day_function(x,day_list[[i,1]]))
}

for ( i in (1:nrow(month_list))){
  df[month_list[[i,1]]]<-sapply(df$month,function(x) day_function(x,month_list[[i,1]]))
}



for ( i in (1:nrow(hour_list))){
  df[paste0("hour",hour_list[[i,1]])]<-sapply(df$hour,function(x) day_function(x,hour_list[[i,1]]))
}


unique(df$hour)

df<- df[colnames(df)!="month"]
df<- df[colnames(df)!="day"]
df<- df[colnames(df)!="hour"]


df<-na.omit(df)
nrow(df)

write.csv(df,"load_variables.csv", row.names = FALSE)


