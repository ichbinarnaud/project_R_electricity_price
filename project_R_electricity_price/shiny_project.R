


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
# source("project_functions.R")



load_variables<-read.csv("load_variables.csv",sep=",",header=TRUE)

load_variables$start<-as.POSIXlt(load_variables$start,tz="CET",'%Y-%m-%d %H:%M')


country_list = list("spain","germany","netherlands")

spain<-load_variables$spain_price
germany<-load_variables$germany_price
netherlands<-load_variables$netherlands_price


explanatory_variables<-load_variables[!(colnames(load_variables) %in% paste0(country_list,"_price"))]

explanatory_variables<-as.matrix(explanatory_variables[colnames(explanatory_variables)!="start"])



coeff_lm<-function(country,explanatory_variables){
  OLS1 = lm(country ~ ., data = as.data.frame(explanatory_variables))
  
  names(OLS1$coefficients)<-gsub("explanatory_variables","",names(OLS1$coefficients))
  return(OLS1)
}



boxplot_month<-function(country,explanatory_variables){
  country<-as.data.frame(country)
  VE1<-as.data.frame(explanatory_variables)
  month_list<-c('janvier','février','mars','avril','mai','juin',
                'juillet','août','septembre','octobre','novembre','décembre')
  
  VE_month<-cbind(country,VE1[,month_list])
  for (i in (1:length(month_list))){
    assign(paste0('VE',month_list[i]),as.data.frame(subset(VE_month[,1],VE_month[month_list[i]]==1)))
    dat<-get(paste0('VE',month_list[i]))
    colnames(dat)<-'price'
    dat['month']<-paste0(i%/%10,i%%10,"_",month_list[i])
    assign(paste0('VE',month_list[i]),dat)
  }
  VE_final<-rbind(VEjanvier,VEfévrier,VEmars,VEavril,VEmai,VEjuin,VEjuillet,VEaoût,VEseptembre,
                  VEoctobre,VEnovembre,VEdécembre)
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
  res<-coeff_list[!grepl("load_VE",rownames(coeff_list)),]
  return(res)  
}








#############################################
#####              Shiny             ########
#############################################



server<-function(input, output) {
  
  ##Ici on augmente la taille maximale des données possibles à traiter
  
  options(shiny.maxRequestSize = 3000*1024^2)
  

  cbp1  <- c("#000000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#AD4F09")

  beginning_date<-load_variables$start[1]
  ending_date<-load_variables$start[nrow(load_variables)]
  years#<-splitting_years(load_variables)
  years<-list(8602, 17242,25948,32475,41080)
  
  
  traductions <- c(
    search = 'Chercher:', 
    decimal = ',',
    thousands = "&#8239;",  # small unbreakable space
    lengthMenu = 'Montrer _MENU_',
    paginate = list(previous = 'PrÃ©cÃ©dent', `next` = 'Suivant'),
    emptyTable = "Aucune donnÃ©e Ã  afficher",
    processing = "mise Ã  jour...",
    infoEmpty = ""
  )
  dt_header_color <- function(bg_color){
    list(initComplete = htmlwidgets::JS(
      "function(settings, json) {",
      paste0("$(this.api().table().header()).css({'background-color': '", bg_color, "', 'color': '#fff'});"),
      "}"))
  }
  
  dt_header_bleu <- dt_header_color('#214770')
  
  options_globales_DT <- list(language = traductions,
                              # autoWidth = TRUE,
                              scrollX = T,
                              columnDefs = list(list(className = 'dt-center', targets = "_all")),
                              dom = "Bfrtip",
                              buttons = list("pageLength", 'copy', 'excel', 'pdf'),
                              lengthMenu = list(c(15, 30, 50, -1), c("15 lignes", "30 lignes", "50 lignes", "Toutes les lignes"))
  )
  
  
  
  
  
  #############################################
  #####        Every country           ########
  #############################################
  
  ## Le renderUI renvoie tous les éléments affichés mais aucun calcul n'est fait dedans
  
  vec <- unlist(country_list)
  
  sapply(seq_along(vec), function(x) {
    name <-vec[[x]]
    output[[name]] <- renderUI({
      tab<-get(name)
      sidebarLayout(
        sidebarPanel(
          
          textOutput(paste0("R_squared_true", x)),
          textOutput(paste0("RMSE",x)),
          downloadButton(paste0("export_data",x), "Générer les données en csv"),
          
        ),
        
        
        mainPanel( dygraphOutput(paste0("dygraph",x)), 
                   plotOutput(paste0('boxplot',x)),
                   plotlyOutput(paste0("histo",x)),
                   DT::dataTableOutput(paste0("tableau_flux_physiques",x)),
                   DT::dataTableOutput(paste0("tableau_tous_coeff",x)),
                   DTOutput(paste0("prediction_MW",x)),
                   DTOutput(paste0("tableau_editable",x)),
                   plotOutput(paste0("residus", x))
                   
        )
      )
    })
  })
  
  sapply(seq_along(vec), function(x) {

    tab<-eventReactive(vec[x],{
      return(as.matrix(get(vec[x])))
    })
    
    OLS1<-eventReactive(vec[x],{
      OLS1<-coeff_lm(tab(),explanatory_variables)
      return(OLS1)
    })
    
    coeff_list<-eventReactive(vec[x],{
      coeff_list<-as.data.frame(list(summary(OLS1())$coef)[[1]])
      return(coeff_list)
    })
    

    output[[paste0("export_data", x)]] <- downloadHandler(
      filename = function(){
        paste(vec[x], Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(coeff_list(),file, na = "")
      },
      contentType = "csv"
    )
    
    output[[paste0("boxplot", x)]]<- renderPlot({
      VE_final<-boxplot_month(tab(),explanatory_variables)
      p <- ggplot(VE_final, aes(x=price, y=month)) +
        geom_boxplot()
      return(p)
    })
    
    
    ## renderDT est là pour renvoyer les tableaux de données, mais en permettant des options plus dynamiques
    output[[paste0("tableau_tous_coeff", x)]] <- renderDT({
      datatable(get_date_variables(coeff_list()),
                extensions = 'Buttons',
                selection = list(mode = 'multiple', selected = NULL, target = 'row'),
                rownames = TRUE,
                filter = "top",
                options = c(dt_header_bleu, options_globales_DT))
    })
    


    
    output[[paste0("residus", x)]]<-renderPlot(
      hist(OLS1()$residuals,50,main = "residual histogram",freq=TRUE,xlab="error (euros)")
    )
    
    ## On affiche les histogrammes de chaque année
    output[[paste0("histo", x)]]<-renderPlotly(
      {
        df_mu<-histo_prep(tab(),years,beginning_date,ending_date)
        df<-df_mu[[1]]
        mu<-df_mu[[2]]
        ggplot(df, aes(x=histo_data, color=years, fill=years)) +
          geom_histogram(aes(y=..density..), position="identity", alpha=0.5,binwidth=3)+
          geom_density(alpha=0.6)+
          geom_vline(data=mu, aes(xintercept=grp.mean, color=years),
                     linetype="dashed")+
          scale_color_manual(values=cbp1)+
          scale_fill_manual(values=cbp1)+
          labs(title="Histogramme des prix pour chaque année",x="Prix (MWH)", y = "Densité")+
          theme_classic()
      }
    )
    
 
    
    
    
    output[[paste0("RMSE", x)]]<-renderText({
      model<-OLS1()$fitted.values
      R<-round(rmse(model,tab()),2)
      return(paste(" La racine de l'erreur quadratique moyenne est ",R))
    })
    
    output[[paste0("R_squared_true", x)]]<-renderText({
      p<-ncol(explanatory_variables)
      return(paste(" Le coefficient de détermination est ", rbind(summary(OLS1())$adj.r.squared)))  
    })
    
    ##Affichage du graohiques des prédictions et des observés

    
    output[[paste0("dygraph", x)]]<-  renderDygraph({
      model= predict(OLS1(),as.data.frame(explanatory_variables))
      model_observations<-cbind(tab(),model)
      traff = xts(model_observations, order.by = load_variables$start)
      
      colnames(traff)<-c("observed","predicted")
      dygraph(traff, main = paste0("MWH price in ", vec[x])) %>%
        dyRangeSelector() %>%
        dyAxis("y",label="Price MWH (euros)")  %>%
        dyEvent(load_variables$start[unlist(years)])
    })
  
    output[[paste0("tableau_flux_physiques", x)]] <- renderDT({
      datatable(get_load_variables(coeff_list()),
                extensions = 'Buttons',
                selection = list(mode = 'multiple', selected = NULL, target = 'row'),
                rownames = TRUE,
                filter = "top",
                options = c(dt_header_bleu, options_globales_DT))
    })    
    
    
    
    ## v$data contient le tableau éditable avec comme valeur par défaut les médianes de chaque 
    ## colonne
    v <- reactiveValues(data = {
      data.frame(t(apply(explanatory_variables,2,median)))
    })
    
    #On renvoie un tableau éditable contenant les valeurs de v
    output[[paste0("tableau_editable",x)]] <- renderDT({
      DT::datatable(v$data, editable = TRUE)
    })
    
    #On récupère les changements effectués dans le tableau precedent et on les enregistre dans v
    
    observeEvent(input[[paste0("tableau_editable",x,"_cell_edit")]], {
      info = input[[paste0("tableau_editable",x,"_cell_edit")]]
      i = as.numeric(info$row)
      j = as.numeric(info$col)
      k = as.numeric(info$value)
      v$data[i,j] <- k
    })
    
    ## On renvoie la valeur prédite ainsi que les bornes supérieures et inférieures avec une proba de 99%
    output[[paste0("prediction_MW",x)]] <- renderDT ({
      return(data.frame(predict(OLS1(),newdata=v$data,interval="confidence",level = 0.99)))
    })
  })
  
}





ui<-navbarPage("Analysis",
               
               tabPanel("spain",
                        basicPage(
                          uiOutput("spain")                          
                        )
               ),
               tabPanel("germany",
                        basicPage(
                          uiOutput("germany")
                        )
               ),
               tabPanel("netherlands",
                        basicPage(
                          uiOutput("netherlands")
                        )
               )               
)

runApp(list(ui = ui, server = server), launch.browser = TRUE)









