





coeff_lm<-function(country,explanatory_variables){
  OLS1 = lm(country ~ ., data = as.data.frame(explanatory_variables))
  
  names(OLS1$coefficients)<-gsub("explanatory_variables","",names(OLS1$coefficients))
  return(OLS1)
}




boxplot_month<-function(country,explanatory_variables){
  country<-as.data.frame(country)
  VE1<-as.data.frame(explanatory_variables)
  month_list<-c('janvier','fevrier','mars','avril','mai','juin',
                'juillet','aout','septembre','octobre','novembre','decembre')
  
  VE_month<-cbind(country,VE1[,month_list])
  for (i in (1:length(month_list))){
    assign(paste0('VE',month_list[i]),as.data.frame(subset(VE_month[,1],VE_month[month_list[i]]==1)))
    dat<-get(paste0('VE',month_list[i]))
    colnames(dat)<-'price'
    dat['month']<-paste0(i%/%10,i%%10,"_",month_list[i])
    assign(paste0('VE',month_list[i]),dat)
  }
  VE_final<-rbind(VEjanvier,VEfevrier,VEmars,VEavril,VEmai,VEjuin,VEjuillet,VEaout,VEseptembre,
                  VEoctobre,VEnovembre,VEdecembre)
  return(VE_final)
}  






date_difference<-function(beginning,end){
  answer<-as.numeric(as.POSIXct(end)-as.POSIXct(beginning))%/%365
  return(answer)
}








splitting_years<-function(load_variables){
  beginning<-load_variables$start[1]
  end<-load_variables$start[nrow(load_variables)]
  diff_date<-date_difference(beginning,end)
  dates<-list()
  for (i in (1:diff_date)){
    print(i)
    d<-beginning 
    d<-d %m+% years(i)
    if (d %in% load_variables$start){
      dates[length(dates)+1]<- as.numeric(rownames(load_variables[load_variables$start==d,]))
    }
    else{
      while (!d %in% load_variables$start){
        hour(d)<-hour(d)+1
      }
      dates[length(dates)+1]<- as.numeric(rownames(load_variables[load_variables$start==d,]))
      
    }
  }
  return(dates)
}


data_split<-function(years,data,i){
  if (i==1){
    histo_data<-data[1:years[[i]],] 
  }
  else if (i==length(years)){
    histo_data<-data[years[[(i-1)]]:nrow(data),] 
  }
  else{
    histo_data<-data[years[[(i-1)]]:years[[(i)]],]     
  }
  histo_data<-as.data.frame(histo_data)
  histo_data$years<-i
  return(histo_data)
}




histo_prep<-function(tab,years,beginning,end){
  diff_date<-date_difference(beginning,end)
  tab<-as.data.frame(tab)
  first_histo<-data_split(years,tab,1)
  if (diff_date>1){
    for (i in (2:diff_date)){
      first_histo<-as.matrix(rbind(first_histo,data_split(years,tab,i)))
    }
    first_histo<-as.data.frame(first_histo)
    df <- data.frame(
      years=factor(first_histo$years),
      histo_data=round(first_histo$histo_data)
    )
    mu <- ddply(df, "years", summarise, grp.mean=mean(histo_data))
    return(list(df,mu))
  }
  else{
    first_histo<-as.data.frame(first_histo)
    df <- data.frame(
      years=factor(first_histo$years),
      histo_data=round(first_histo$histo_data)
    )
    mu <- ddply(df, "years", summarise, grp.mean=mean(histo_data))
    return(list(df,mu))
  }
}




get_load_variables<-function(coeff_list){
  res<-coeff_list[grepl("load_VE",rownames(coeff_list)),]
  return(res)  
}

get_date_variables<-function(coeff_list){
  res<-coeff_list[!grepl("load_VE",rownames(coeff_list)) & !grepl("generation",rownames(coeff_list)) ,]
  return(res)  
}

get_generation_variables<-function(coeff_list){
  res<-coeff_list[grepl("generation",rownames(coeff_list)),]
  return(res)  
}





predicting_evolution<-function(country, explanatory_variables, years){
  
  OLS1      <- coeff_lm(country[1 : years[[1]]],explanatory_variables[1 : years[[1]],])
  names(OLS1$coefficients) <- gsub("explanatory_variables","",names(OLS1$coefficients))
  model     <- predict(OLS1,as.data.frame(explanatory_variables)[1: years[[1]], ])
  RMSE_list <- list( round(rmse(model,country[1: years[[1]]]),2))
  
  for (i in (1 : (length(years)-1))){
    OLS1                           <- coeff_lm(country[1 : years[[i]]],explanatory_variables[1 : years[[i]],])
    names(OLS1$coefficients)       <- gsub("explanatory_variables","",names(OLS1$coefficients))
    predictions                    <- predict(OLS1,as.data.frame(explanatory_variables)[(years[[i]]+1) : years[[i+1]], ])
    model                          <- c(model, predictions)
    RMSE_list[length(RMSE_list)+1] <- list( round(rmse(predictions,country[(years[[i]]+1) : years[[i+1]]]),2))
  }
  
  OLS1                           <- coeff_lm(country[1 : years[[length(years)]]],explanatory_variables[1 : years[[length(years)]],])
  names(OLS1$coefficients)       <- gsub("explanatory_variables","",names(OLS1$coefficients))
  predictions                    <- predict(OLS1,as.data.frame(explanatory_variables)[(years[[length(years)]]+1) : length(country), ])
  model                          <- c(model,predictions)
  RMSE_list[length(RMSE_list)+1] <- list( round(rmse(predictions,country[(years[[length(years)]] + 1) : length(country)]),2))
  
  return(list(model, RMSE_list))
}





