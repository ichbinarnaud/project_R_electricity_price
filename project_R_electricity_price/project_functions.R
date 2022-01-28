
#######################################################
# liste des fonctions pour le fichier "Outil_shiny.R" #
#######################################################

##########################################################################################
# "rsq" calcule le coefficient de détermination ajusté
##########################################################################################


rsq <- function (x, y, p) {
  n=length(x)
  r_squared<-cor(x,y, method = "pearson")**2
  Adj_R_squared<-1-n*(1-r_squared)/(n-p)
  return(Adj_R_squared)
}
##########################################################################################
# "coeff_ouvrage" renvoie toutes les données de la régression linéaire multiple effectuée 
##########################################################################################

coeff_ouvrage<-function(variable_endogene,VE1){
  variable_endogene<-as.matrix(variable_endogene)
  OLS1 = lm(variable_endogene~-1+VE1)
  names(OLS1$coefficients)<-gsub("VE1","",names(OLS1$coefficients))
  return(OLS1)
}

##########################################################################################
# "calcul_R2_ouvrage" renvoie le R² à partir des données de la régression multiple
##########################################################################################

calcul_R2_ouvrage<-function(OLS1){
  Adj_R2=rbind(summary(OLS1)$adj.r.squared)  
  return(Adj_R2)
}


##########################################################################################
# "separation_donnees" renvoie toutes les dates de "données" présentes dans l'année "i"
# le paramètre "annees" correspond à la sortie de la fonction "separation_annees"
##########################################################################################

separation_donnees<-function(annees,donnees,i){
  if (i==1){
    donnees_histo<-donnees[1:annees[[i]],] 
  }
  else if (i==length(annees)){
    donnees_histo<-donnees[annees[[(i-1)]]:nrow(donnees),] 
  }
  else{
    donnees_histo<-donnees[annees[[(i-1)]]:annees[[(i)]],]     
  }
  donnees_histo<-as.data.frame(donnees_histo)
  donnees_histo$annees<-i
  return(donnees_histo)
}

##########################################################################################
# "preparation_histo" renvoie les 2 variables nécessaires à la création des histogrammes
# dans "Outil_shiny.R".
# Le paramètre "annees" correspond à la sortie de la fonction "separation_annees", 
# date_debut est la première date de "donnees" et date_fin est sa dernière date.
# tab correspond à la liaison dont on veut connaître les histogrammes en fonction du temps
##########################################################################################

preparation_histo<-function(tab,annees,date_debut,date_fin){
  diff_date<-difference_date(date_fin,date_debut)
  tab<-as.data.frame(tab)
  premier_histo<-separation_donnees(annees,tab,1)
  if (diff_date>1){
    for (i in (2:diff_date)){
      premier_histo<-as.matrix(rbind(premier_histo,separation_donnees(annees,tab,i)))
    }
    premier_histo<-as.data.frame(premier_histo)
    df <- data.frame(
      annees=factor(premier_histo$annees),
      donnees_histo=round(premier_histo$donnees_histo)
    )
    mu <- ddply(df, "annees", summarise, grp.mean=mean(donnees_histo))
    return(list(df,mu))
  }
  else{
    premier_histo<-as.data.frame(premier_histo)
    df <- data.frame(
      annees=factor(premier_histo$annees),
      donnees_histo=round(premier_histo$donnees_histo)
    )
    mu <- ddply(df, "annees", summarise, grp.mean=mean(donnees_histo))
    return(list(df,mu))
  }
}

##########################################################################################
# "recuperation_ouvrages" renvoie un dataframe avec les coefficients,
# la t value et l'impact en MW des groupes de production, trié par ordre décroissant 
# de l'impact. liste_coeff correspond à la liste des coefficients de la liaison
##########################################################################################

recuperation_ouvrages<-function(liste_coeff){
  liste_nom<-c("Gravelines",
               "Chooz",
               "Cattenom",
               "Fessenheim",
               "Paluel",
               "Penly",
               "Flamanville",
               "Nogent",
               "Chinon",
               "Saint.Laurent",
               "Dampierre",
               "Belleville",
               "Civaux",
               "Blayais",
               "Golfech",
               "Bugey",
               "Saint.Alban",
               "Cruas",
               "Tricastin",
               
               ##On passe à l'hydraulique
               "S.Bissorte",
               "Grand_Maison",
               "Cheylas",
               "Revin",
               "Montezic",
               
               ##On passe au thermique
               "Bouchain",
               "Pont.Sambre",
               "E.Huchet",
               "Blenod",
               "Croix_de_Metz",
               "Grande.Rivière_Montoir",
               "Cordemais",
               "Amfard_Havre",
               "Martigues",
               "Gracieuse",
               "Fos"
  )
  liste_megawatt<-c(900,
                    1500,
                    1300,
                    900,
                    1300,
                    1300,
                    1300,
                    1300,
                    900,
                    900,
                    900,
                    1300,
                    1500,
                    900,
                    1300,
                    900,
                    1300,
                    900,
                    900,
                    
                    ##on passe à l'hydraulique
                    150,
                    150,
                    230,
                    180,
                    200,
                    
                    ##On passe au thermique
                    580,
                    420,
                    600,
                    420,
                    420,
                    420,
                    565,
                    115,
                    460,
                    420,
                    480
  )
  liste_nom_megawatt<-data.frame(liste_nom,liste_megawatt)
  liste_ouvrages<- liste_coeff[rownames(liste_coeff) %in% liste_nom,]
  liste_ouvrages['liste_nom']<-rownames(liste_ouvrages)
  liste_complete<-inner_join(liste_ouvrages,liste_nom_megawatt,"liste_nom")
  liste_complete['impact(MW)']<-liste_complete$liste_megawatt*liste_complete$Estimate
  rownames(liste_complete)<-liste_complete$liste_nom
  liste_complete<-liste_complete[,c("impact(MW)","liste_megawatt","t value","Estimate")]
  colnames(liste_complete)<-c("Impact(MW)","Delta MW","t value","Coefficient")
  liste_complete <- liste_complete[order(-abs(liste_complete$`t value`)),]
  return(liste_complete)
}

##########################################################################################
# "recuperation_flux_physiques" renvoie un dataframe avec les coefficients,
# la t value et l'impact en MW des flux physiques, trié par ordre décroissant de l'impact.
# liste_coeff correspond à la liste des coefficients de la liaison
##########################################################################################

recuperation_flux_physiques<-function(liste_coeff){
  res<-liste_coeff[grepl("flux_phy",rownames(liste_coeff)),]
  res<-rbind(res,liste_coeff[grepl("IFA2",rownames(liste_coeff)),])
  rownames(res)[rownames(res)=="flux_phy_ANG"]<-"IFA_2000"
  res['Impact(MW)']<-res$Estimate*1000
  res['MW choisi']<-1000
  liste_complete<-res[,c("Impact(MW)","MW choisi","t value","Estimate")]
  colnames(liste_complete)<-c("Impact(MW)","Delta MW","t value","Coefficient")
  liste_complete <- liste_complete[order(-abs(liste_complete$`t value`)),]
  return(liste_complete)  
}

##########################################################################################
# "recuperation_conso_region" renvoie un dataframe avec les coefficients,
# la t value et l'impact en MW des consommations régionales, ainsi que 
# de la production éolienne, trié par ordre décroissant de l'impact.
# liste_coeff correspond à la liste des coefficients de la liaison
##########################################################################################

recuperation_conso_region<-function(liste_coeff,eolien){
  
  res<-liste_coeff[grepl("conso",rownames(liste_coeff)),]
  res1<-liste_coeff[grepl("eole",rownames(liste_coeff)),]
  
  res['Impact(MW)']<-res$Estimate*1000
  res['MW choisi']<-1000
  res1['MW choisi']<-round(apply(eolien[,rownames(res1)],2,mean))
  res1['Impact(MW)']<-res1$Estimate*res1['MW choisi']
  
  res<-rbind(res,res1)
  liste_complete<-res[,c("Impact(MW)","MW choisi","t value","Estimate")]
  colnames(liste_complete)<-c("Impact(MW)","Delta MW","t value","Coefficient")
  liste_complete <- liste_complete[order(-abs(liste_complete$`t value`)),]
  return(liste_complete)  
}

##########################################################################################
# "recuperation_conso_region" renvoie un dataframe avec les coefficients et
# la t value de toutes les variables exogènes utilisées.
# liste_coeff correspond à la liste des coefficients de la liaison
##########################################################################################




##########################################################################################
# Les récupérations calendaires fonctionnent sur le même principe que les fonctions
# précédentes, mais ne renvoient que la t_value et le coefficient. 
# Elles ne sont pas utilisées dans l'outil, mais je les ai laissées au cas où 
# étudier cet impact intéresse quelqu'un
##########################################################################################

recuperation_mois<-function(liste_coeff){
  liste_mois<-c("mai","juin", "juillet","août","septembre","octobre","novembre","décembre","janvier","février","mars","avril")       
  liste_ouvrages<- liste_coeff[rownames(liste_coeff) %in% liste_mois,]
  liste_complete<-liste_ouvrages[,c("t value","Estimate")]
  colnames(liste_complete)<-c("t value","Coefficient")
  liste_complete <- liste_complete[order(-abs(liste_complete$`t value`)),]
  return(liste_complete)  
}

recuperation_jour<-function(liste_coeff){
  liste_jour<-c("lundi","mardi","mercredi", "jeudi","vendredi","samedi","dimanche")
  liste_ouvrages<- liste_coeff[rownames(liste_coeff) %in% liste_jour,]
  liste_complete<-liste_ouvrages[,c("t value","Estimate")]
  colnames(liste_complete)<-c("t value","Coefficient")
  liste_complete <- liste_complete[order(-abs(liste_complete$`t value`)),]
  return(liste_complete)  
}

recuperation_heure<-function(liste_coeff){
  liste_ouvrages<-liste_coeff[grepl("heure",rownames(liste_coeff)),]
  liste_complete<-liste_ouvrages[,c("t value","Estimate")]
  colnames(liste_complete)<-c("t value","Coefficient")
  liste_complete <- liste_complete[order(-abs(liste_complete$`t value`)),]
  return(liste_complete)  
}




##########################################################################################
# "impact_variable" renvoie les coefficients du fichier "total_des_coefficients" 
##########################################################################################


impact_variable<-function(variable,coeff_total){
  liste_coeff<-coeff_total[variable]
  liste_coeff<-liste_coeff[order(-abs(liste_coeff[variable])),,drop=FALSE]
  return(liste_coeff)
}

##########################################################################################
# "decoupage_transit_faible" renvoie les coefficients de la régression linéaire 
# uniquement appliquée aux données associées au transit supérieur à 20 MW
##########################################################################################


decoupage_transit_faible<-function(variable_endogene_non_nulle,VE1,indices_transit_nul){
  VE1_non_nul<-as.matrix(VE1[-indices_transit_nul,])
  OLS1 = lm(as.matrix(variable_endogene_non_nulle)~-1+.,data=as.data.frame(VE1_non_nul))
  # names(OLS1$coefficients)<-gsub("VE1_non_nul","",names(OLS1$coefficients))
  
  return(OLS1)
}

