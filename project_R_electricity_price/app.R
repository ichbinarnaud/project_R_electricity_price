
package.check <- lapply(
  c("xts","DT","plyr","shiny","corrplot","splitstackshape","shinyWidgets","ggpubr","Metrics","rstudioapi","dplyr","data.table","ggplot2","timeDate","stringr","tseries","stats","MASS","strucchange","forecast","dygraphs","readxl","scales","glmnet","parsedate","lubridate"),
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)
source("shiny_functions.R")




load_variables<-read.csv("load_variables.csv",sep=",",header=TRUE)

load_variables$start<-as.POSIXlt(load_variables$start,tz="CET",'%Y-%m-%d %H:%M')


country_list = list("spain","germany","netherlands")

spain<-load_variables$spain_price
germany<-load_variables$germany_price
netherlands<-load_variables$netherlands_price


explanatory_variables<-load_variables[!(colnames(load_variables) %in% paste0(country_list,"_price"))]

explanatory_variables<-as.matrix(explanatory_variables[colnames(explanatory_variables)!="start"])



#############################################
#####              Shiny             ########
#############################################



server<-function(input, output) {
  
  options(shiny.maxRequestSize = 3000*1024^2)
  

  cbp1 <- c("#000000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#AD4F09")

  beginning_date <- load_variables$start[1]
  ending_date    <- load_variables$start[nrow(load_variables)]
  #years         <- splitting_years(load_variables)
  years          <- list(8559, 17086,25748,29058,37643)
  
  
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
  
  
  
  

  vec <- unlist(country_list)
  
  sapply(seq_along(vec), function(x) {
    name <-vec[[x]]
    output[[name]] <- renderUI({
      tab<-get(name)
      sidebarLayout(
        sidebarPanel(
          
          textOutput(paste0("R_squared_true", x)),
          textOutput(paste0("RMSE1",x)),
          textOutput(paste0("RMSE2",x)),
          textOutput(paste0("RMSE3",x)),
          textOutput(paste0("RMSE4",x)),
          textOutput(paste0("RMSE5",x)),
          downloadButton(paste0("export_data",x), "Generer les donnees en csv"),
          
        ),
        
        
        mainPanel( dygraphOutput(paste0("dygraph",x)), 
                   plotOutput(paste0('boxplot',x)),
                   plotlyOutput(paste0("histo",x)),
                   DT::dataTableOutput(paste0("load_tab",x)),
                   DT::dataTableOutput(paste0("generation_tab",x)),
                   DT::dataTableOutput(paste0("date_tab",x)),
                   DTOutput(paste0("prediction_price",x)),
                   DTOutput(paste0("predicting_tab",x)),
                   plotOutput(paste0("errors_are_centered", x)),
                   plotOutput(paste0("homoscedasticity", x)),
                   plotOutput(paste0("errors_are_uncorrelated", x)),
                   plotOutput(paste0("errors_are_gaussian", x)),
                   plotOutput(paste0("residuals", x))
                   
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

    prediction_result<-eventReactive(vec[x],{
      predictions<-predicting_evolution(tab(),explanatory_variables, years)
      return(predictions)
    })    
    
        
    coeff_list<-eventReactive(vec[x],{
      coeff_list<-as.data.frame(list(summary(OLS1())$coef)[[1]])
      return(coeff_list)
    })

    output[[paste0("errors_are_centered", x)]]<-renderPlot(
      plot(OLS1(),1)
    )
    
    output[[paste0("homoscedasticity", x)]]<-renderPlot(
      plot(OLS1(),3)
    )   
    
    output[[paste0("errors_are_uncorrelated", x)]]<-renderPlot(
      acf(residuals(OLS1()),main="Auto-correlation plot")
    )  
    
    output[[paste0("errors_are_gaussian", x)]]<-renderPlot(
      plot(OLS1(),2)
    )       


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
      p <- ggplot(VE_final, aes(x=price, y=month)) + geom_boxplot()

      return(p + ggtitle("Repartition du prix (EUR / MWH) en fonction du mois") 
             + theme(plot.title = element_text(color="black", size=18, face="bold")))
    })
    
    
    output[[paste0("date_tab", x)]] <- renderDT({
      datatable(get_date_variables(coeff_list()),
                extensions = 'Buttons',
                selection = list(mode = 'multiple', selected = NULL, target = 'row'),
                rownames = TRUE,
                filter = "top",
                options = c(dt_header_bleu, options_globales_DT))
    })
    
    output[[paste0("load_tab", x)]] <- renderDT({
      datatable(get_load_variables(coeff_list()),
                extensions = 'Buttons',
                selection = list(mode = 'multiple', selected = NULL, target = 'row'),
                rownames = TRUE,
                filter = "top",
                options = c(dt_header_bleu, options_globales_DT))
    }) 


    output[[paste0("generation_tab", x)]] <- renderDT({
      datatable(get_generation_variables(coeff_list()),
                extensions = 'Buttons',
                selection = list(mode = 'multiple', selected = NULL, target = 'row'),
                rownames = TRUE,
                filter = "top",
                options = c(dt_header_bleu, options_globales_DT))
    })    
    
    
    
        
    output[[paste0("residuals", x)]]<-renderPlot(
      hist(OLS1()$residuals,50,main = "residual histogram",freq=TRUE,xlab="erreur (EUR / MWH)")
    )
    
    
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
          labs(title="Histogramme des prix pour chaque annee", x="Prix ( EUR / MWH )", y = "Densite")+
          theme_classic()
      }
    )
  

    
    
    
    output[[paste0("RMSE1", x)]]<-renderText({
      R<-prediction_result()[[2]][2]
      return(paste(" RMSE seconde annee : ",R))
    })

    output[[paste0("RMSE2", x)]]<-renderText({
      R<-prediction_result()[[2]][3]
      return(paste(" RMSE troisieme annee : ",R))
    })    

    output[[paste0("RMSE3", x)]]<-renderText({
      R<-prediction_result()[[2]][4]
      return(paste(" RMSE quatrieme annee : ",R))
    })
    
    output[[paste0("RMSE4", x)]]<-renderText({
      R<-prediction_result()[[2]][5]
      return(paste("RMSE cinquieme annee : ",R))
    })    
    
    output[[paste0("RMSE5", x)]]<-renderText({
      R<-prediction_result()[[2]][6]
      return(paste(" RMSE sixieme annee : ",R))
    })    
        
    output[[paste0("R_squared_true", x)]]<-renderText({
      p<-ncol(explanatory_variables)
      return(paste(" Le coefficient de determination est ", round(rbind(summary(OLS1())$adj.r.squared),4)))  
    })
    

    
    output[[paste0("dygraph", x)]]<-  renderDygraph({
      model              <- prediction_result()[[1]]
      model_observations <- cbind(tab(),model)
      traff              <- xts(model_observations, order.by = load_variables$start)
      
      colnames(traff)<-c("observed","predicted")
      dygraph(traff, main = paste0("Price (EUR / MWH) in ", vec[x], " over the years" )) %>%
        dyRangeSelector() %>%
        dyAxis("y",label="Price (EUR / MWH) ")  %>%
        dyEvent(load_variables$start[unlist(years)])
    })
  
   
    
    
    

    v <- reactiveValues(data = {
      data.frame(t(apply(explanatory_variables,2,median)))
    })

    
    
        
    output[[paste0("predicting_tab",x)]] <- renderDT({
      DT::datatable(v$data, editable = TRUE)
    })
    

    observeEvent(input[[paste0("predicting_tab",x,"_cell_edit")]], {
      info = input[[paste0("predicting_tab",x,"_cell_edit")]]
      i = as.numeric(info$row)
      j = as.numeric(info$col)
      k = as.numeric(info$value)
      v$data[i,j] <- k
    })
    
    output[[paste0("prediction_price",x)]] <- renderDT ({
      return(data.frame(predict(OLS1(),newdata=v$data,interval="confidence",level = 0.99)))
    })
  })
  
  output$correlation  <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Tableau de visualisation des correlations entre les variables explicatives")),
    
      mainPanel( plotOutput("correlation_tab")))
      
  })

  output[["correlation_tab"]]<-renderPlot(
    corrplot(round(cor(explanatory_variables[,1:20]),2),method="ellipse")
  )

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
               ,
               tabPanel("Correlation",
                        basicPage(
                          uiOutput("correlation")
                        )
               )                
)





runApp(list(ui = ui, server = server), launch.browser = TRUE)





