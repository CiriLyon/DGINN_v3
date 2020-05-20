

library(shinyalert)
library(VennDiagram)
library(tools)
library(shinythemes)
library(gplots)
library(shiny)
library(shinyjs)
library(openxlsx)
library(V8)

# if (!require("KernSmooth")) install.packages("KernSmooth")
# library(KernSmooth)


server = function(input , output ,session){
  
  
  observeEvent(input$resetData, {
    js$reset()
  })
  
  observeEvent(input$quit, {
    js$closeWindow()
    stopApp()
    
  })
  
  #_______________________________
  observeEvent(input$boutonUpload,{
    monDoc           <- input$boutonUpload 
    
    if(is.null(monDoc)) {
      return()
    }
    
    monDocPath       <- monDoc$datapath #chemin + nom temporaires du fichier uploadé assigné par l'ordi.
    
    monDocName       <- monDoc$name #nom du fichier uploadé tel qu'il existe naturellement.
    
    trueNames        <- file.path(dirname(monDocPath), monDocName) #chemin + nom naturel
    
    file.copy(from = monDocPath , to = trueNames)
    
    if(!exists("PathLecture")){
      PathLecture      <- paste0(dirname(monDoc$datapath) , '/', monDoc$name)
      
      file.rename(from =  monDocPath  , to = PathLecture)
    }
    
    if(file_ext(monDoc$name) == "xlsx"){
      leaTab    <- read.xlsx(PathLecture , 1)
    }else{
      leaTab    <- read.table(PathLecture, header = T, sep = "\t")
    }
    #--Message d'erreur en cas de fichier qui ne contient pas les colonnes obligatoires.
    obligatoyCol <- c('Gene', 'GeneSize', 'Omega', 'Method', 'PosSel', 'NbSites' , 'PSS')
    differ       <- setdiff(obligatoyCol  , colnames(leaTab))
    
    if(!!length(differ)){
      showModal(
        modalDialog(
          title = "Columns below are required but not in the uploaded file - Couldn't process data!",
          paste(differ, collapse = " | "),
          footer = modalButton("OK"),
          size = "l"
        )
      )
      return()
    }
    
    #--output relatif à la table des données
    
    
    #-- Création du pdf à télécharger  
    pdf(file = "leaGraphs.pdf", paper = 'a4r' , h = 0 , w = 0)
    
    
    
    # recoder le na en N.
    leaTab$PosSel <- gsub("na" , "N" , leaTab$PosSel)
    
    #Supprimer les "[ ]".
    leaTab$PSS <- gsub("\\[|\\]" , "",  leaTab$PSS)
    
    leaTab$Omega <- round(leaTab$Omega,2)
    # -- Fin de l'importation et de la préparation des données.
    # colnames(leaTab)
    # [1] "Gene"     "GeneSize" "Method"   "PosSel"   "NbSites"  "PSS"  
    
    omeg <- unique(data.frame(leaTab$Gene , leaTab$Omega ))
    
    
    #-- les cellules vides dans le document original de Léa sont codées "". Dans Excel elles sont codées
    # soit "" soit NA. Si le codage est NA je le convertis en "", sinon je le laisse.
    if(!!sum(is.na(leaTab$PSS))){
      leaTab[which(is.na(leaTab$PSS )), "PSS"] <- ""
    }
    #-- Fin recodage des cellules vides dans la colonne PSS.
    
    
    #------------------------------------------------------
    
    #################################################################################
    #################################################################################
    ############################1ère figure##########################################
    #################################################################################
    #-- début du graphique "Number of methods detecting positive selection"
    
    
    TAB1 <- droplevels(leaTab[leaTab$Method != "MEME" , ])
    
    TAB1$PosSel <- factor(TAB1$PosSel , levels = c("Y", "N"))
    
    YNbyGene     <- table(TAB1$Gene , TAB1$PosSel)
    YNbyGene_new <- as.data.frame.matrix(YNbyGene)
    nbrMeth <- max(rowSums(YNbyGene_new , na.rm =T)) #le nombre de méthodes sans la méthode 'MEME'
    
    
    #-- introduire le Omega dans cette table juste pour pouvoir trier selon lui, et le supprimer ensuite.
    YNbyGene_new <- merge(YNbyGene_new, omeg , by.x  ="row.names" , by.y = "leaTab.Gene", all =T, sort =F)
    
    #-- Ordre pour l'affichage graphique: (1) nbr de méthodes donnant 'Yes' et (2) ordre orthographique des noms des génes.
    YNbyGene_new           <- YNbyGene_new[order(YNbyGene_new$Y, YNbyGene_new$leaTab.Omega , rownames(YNbyGene_new), decreasing =  c(F,F,T) , method = "radix") , ]
    
    rownames(YNbyGene_new) <- YNbyGene_new$Row.names
    
    YNbyGene_new[, c("Row.names" , "leaTab.Omega")] <- NULL
    Ord          <- rownames(YNbyGene_new) #-- Pour ordonner le 2e graphique.
    
    
    #################################################################################
    #################################################################################    
    
    
    TAB1$PosSel2 <- ifelse(TAB1$PosSel == "Y", 1, 0)
    
    xtable <- with(TAB1 , xtabs(PosSel2 ~  Gene +  Method))
    
    xtable <- xtable[Ord, ]
    
    tabHeat <- as.data.frame.matrix(xtable[rev(Ord) , unique(TAB1$Method )])
    
    #################################################################################
    #################################################################################
    ############################Heatmap##########################################
    #################################################################################
    
    par( oma = c(0,0,0,0) , mgp = c(0,0.5,0))
    heatmap.2(as.matrix(tabHeat),
              # add.expr = list(
              #   abline (h = 1:nrow(tabHeat)+0.5 , col = adjustcolor("black" , 0.1))
              # ),
              trace = "none", 
              colsep = 1:ncol(tabHeat),
              rowsep = 1:nrow(tabHeat),
              sepcolor = "grey55",
              dendrogram = "none", 
              Rowv = F , 
              Colv = F, 
              key          = F                  ,
              
              
              col = adjustcolor( c( "grey90" , "grey20"), alpha.f = 0.7), 
              sepwidth = 0.0001 ,
              margins  = c(7, 14)
              ,
              # offsetRow = -50, 
              # offsetCol = -50  #
              # , 
              srtCol=40
              ,
              cexCol = 1.2
              ,
              main = "Detection of positive selection",
              ylab = "Genes" ,
              xlab = "Methods",
              cexRow = min(20/nrow(tabHeat), 1.5)
    )
    
    
    legend("topleft" , 
           legend = c("Not Detected" , "Detected") ,
           fill = c( "grey90" , "grey20") ,
           xpd =T, 
           inset = c(-0.01 ,-0.001),
           bg = NA,
           bty = "n",
           title = "Detection status",
           title.col = "tomato" ,
           #text.col = c( "grey90" , "grey20") ,
           cex = 1,
           border = NA)
    
    
    #################################################################################
    #################################################################################
    ############################fin Heatmap##########################################
    #################################################################################
    
    
    #################################################################################
    #################################################################################
    ########################### 1ère Figure ##########################################
    #################################################################################
    
    
    ##-- Couleur des méthodes
    #-- Noms des méthodes, c'est pour assigner une couleur à une méthode et qui reste la même dans la figure 1 et 3.
    #set.seed(1)
    #coul <- sample(colors(distinct = T), length(unique(leaTab$Method))) #Couleurs des métohdes
   
    #coul <- 1:length(unique(leaTab$Method))
    
     coul <- c("deepskyblue4", "darkorange" , "deepskyblue3" , "mediumseagreen" , "yellow3" , "black")
    
    
     
     metNames <- unique(leaTab$Method)                    #noms des méthodes
    
    metColor <- data.frame(Nom = metNames , coul = coul , stringsAsFactors = FALSE) #data frame qui contient 2 colonnes : (1) nom et (2) couleur des méthodes.
    ##-- Fin Couleur des méthodes
    
    xt2 <- as.data.frame.matrix(xtable[,unique(TAB1$Method)])
    xt2_col <- metColor[which(metColor$Nom %in% colnames(xt2)) , ]
    
    par(xpd = T , mar=c(4,10,8,0) , mfrow  =c(1,2) ,   oma = c(0,0,0,0) , mgp = c(3,0.3,0))
    
    
    h =  barplot(
      t(xt2),
      border = NA ,
      axes = F ,
      col = adjustcolor(xt2_col$coul, alpha.f = 1),
      horiz = T ,
      las  = 2 ,
      main = "Methods detecting positive selection" ,  
      #c(
      #   "Detection of positive selection",
      #   paste( nbrMeth ,  "methods")
      # ),
      cex.main = 0.9 ,
      cex.names  = min(20/nrow(tabHeat), 1.5),
      
      legend.text = paste0(1:ncol(xt2), ") ",  xt2_col$Nom),
      
      args.legend = list(
        x = "topleft",
        bg = NA,
        bty = "n",
        title = "Used Methods",
        title.col = "black" ,
        text.col = xt2_col$coul ,
        horiz = F ,
        cex = 0.8,
        border = NA,
        inset = c(-0.5 ,-0.3)
      )
    )
    
    axis(3, line = 0, at = c(0:nbrMeth), label = c("0", rep("", nbrMeth -1), nbrMeth), tck = 0.02)
    
    
    
    #-- Fin du traitement du point 1 et du graphique "Number of methods detecting positive selection"
    
    #################################################################################
    #################################################################################
    ############################2ème figure##########################################
    #################################################################################
    #-- Début du graphique "Percentage of positively selected sites"
    
    par(mar=c(4,6,8,2))
    
    #-- Supprimer de la table les lignes de la méthode 'MEME' où 'Method' == 'BUSTED' et 'PosSel' ==  'N' (Léa l'a demandé à l'oral).
    GN   <- unique(leaTab[leaTab$Method == "BUSTED" & leaTab$PosSel ==  'N'  , "Gene"]) #génes concernés par la suppression.
    
    TAB3 <- leaTab
    
    if (!!length(GN)) {
      TAB3 <- TAB3[-which(TAB3$Gene %in% GN & TAB3$Method == 'MEME'),]
    } 
    TAB3 <- droplevels(TAB3[TAB3$Method != "BUSTED" , ])
    
    
    #-- Vérification que le nombre de site donné dans la colonne 'NbSites' correspond à la somme des sites dans 'PSS'.
    sumSiteByGene            <- data.frame(tapply(TAB3$NbSites,TAB3$Gene , sum , na.rm =T))
    uniqueGeneSize           <- unique(data.frame(TAB3$Gene , TAB3$GeneSize)) 
    rownames(uniqueGeneSize) <- uniqueGeneSize[,"TAB3.Gene"] 
    uniqueGeneSize           <- uniqueGeneSize[,-1, drop = F]
    
    
    lstGene <- split(TAB3 , TAB3$Gene)
    checksiteNbr <- data.frame(matrix(NA, length(lstGene), 2))
    
    for(t in 1:length(lstGene)){
      ff                <- table(as.numeric(unlist(strsplit(lstGene[[t]]$PSS , split = "," , fixed  = T))))
      
      checksiteNbr[t,1] <- names(lstGene)[t]
      
      checksiteNbr[t,2] <- sum(ff)
    }
    
    if(identical(rownames(sumSiteByGene) , checksiteNbr[,1])){
      if(!sum(sumSiteByGene[,1] - checksiteNbr[,2])){print(paste("Correspondance parfaite"))}else{ 
        showModal(modalDialog(title = "Inconsistency!", "Conflicting values about the number of sites between the NbSites & PSS columns!"))
        #shinyalert (title = "Incohérence!" , text =  "Valeurs contradictoires à propos du nombre des sites entre les colonnes NbSites &	PSS" , type ="error") 
      } 
    }else{stop("Revoir le check pour le nbr des sites")}
    #Fin de la vérif.
    
    if(identical(dim(sumSiteByGene) , dim(uniqueGeneSize))){
      tabFin <- merge(sumSiteByGene , uniqueGeneSize , by = "row.names" , all = T , sort = F)
      rownames(tabFin) <- tabFin$Row.names
      tabFin$Row.names <- NULL
    }else{stop ("")}
    
    #-- ordonner les génes selon la régle définie par Léa en point 1.
    if(all(rownames(tabFin) %in% Ord) & all(Ord %in% rownames(tabFin))){
      tabFin <- tabFin[Ord ,]
    }else{stop("")}
    
    
    tabFin_v2 <- merge(tabFin, omeg , by.x  ="row.names" , by.y = "leaTab.Gene", all =T, sort =F)
    rownames(tabFin_v2) <- tabFin_v2$Row.names
    tabFin_v2$Row.names <- NULL
    
    tabFin_v2$ratio <- tabFin_v2[,1]/tabFin_v2[,2]
    
    #-- Pour ^tre sûr que les 2 barplot soient ordonnés pareil.
    tabFin_v2 <- tabFin_v2[Ord,]
    
    #-- Borne supérieure : pour indixer la borne supérieure du xlim. 
    bornSup <- round(max(tabFin_v2$ratio) + 0.06,1)
    
    xx = barplot(
      tabFin_v2$ratio  ,
      names.arg = NULL ,
      las = 1,
      cex.names = 1,
      col = adjustcolor("blue4", alpha.f = 0.5),
      border = NA,
      horiz = T ,
      axes = F,
      main = "Percentage of all positively selected sites over alignment length",
      #expression(paste("Positively selected sites ratio: Sites number of"~ Gene[i] %/% Gene[i], " length" )) ,
      xlim = c(0, bornSup) ,
      cex.main = 0.9
    )
    
    axis(3, line = 0, labels =  c("0%" , paste0(bornSup*100,"%")), at = seq(0,bornSup, length.out = 2), las  = 1, tck = 0.02)
    
    #Cex <- log2(tabFin_v2$leaTab.Omega*100)/2
    posX <- -0.05
    
    text(rep(posX, length(xx)) , xx,  tabFin_v2$leaTab.Omega , cex = min(20/nrow(tabHeat), 1.5))
    
    par(mar = rep(0,4), xpd  =T)
    
    text(   posX , max(xx) + 4 , expression(paste(omega)), cex = 2.5)
    #arrows( posX , max(xx) + 3 , posX , max(xx) + 1, code = 2 , col = "magenta" , lwd = 1, length = 0.15)
    
    
    
    #-- Fin du graphique "Percentage of positively selected sites"
    
    # dev.off()
    # 
    # system('open "justHeat.pdf" ')
    
    
    #################################################################################
    #################################################################################
    ############################3ème figure##########################################
    #################################################################################
    
    
    
    #-- début du graphique FreqSiteByGene : les batonnets sur les positions des génes.
    TAB4 <- leaTab
    
    #-- Supprimer de la table les lignes de la méthode 'MEME' pour les gènes où 'Method' == 'BUSTED' et 'PosSel' ==  'N' (Léa l'a demandé à l'oral).
    GN   <- unique(leaTab[leaTab$Method == "BUSTED" & leaTab$PosSel ==  'N'  , "Gene"]) #gènes concernés par la suppression.
    
    if (!!length(GN)) {
      TAB4 <- TAB4[-which(TAB4$Gene %in% GN & TAB4$Method == 'MEME'),]
    }
    
    
    #-- On peut maintenant supprimer la méthode BUSTED de ce graphique. 
    TAB4 <- TAB4[-which(TAB4$Method == "BUSTED"), ]
    
    #-- liste de un data.frame par gene
    lstGene <- split(TAB4 , TAB4$Gene)
    
    
    if(!identical(sort(Ord), sort(names(lstGene)))){
      rm(list =ls())
      stop("Erreur de correspondance dans les noms des génes!")
    }
    
    lstGene <- lstGene[rev(Ord)] #inverser l'ordre, car le barplot commence du bas vers le haut.
    
    #-----------------------------------------------------------------------------------------------------------------------------
    #-- Trouver et supprimer les génes pour lesquels on ne trouve aucun site.
    
    #si, après avoir supprimé la méthode MEME pour les gènes où la méthode BUSTED ne donne pas de sélection positive 
    # (i.e. PosSel== N), alors si PSS est vide pour toutes les méthodes restantes alors ce gène doit être supprimé de cette figure.
    toRemove <- c()        # positions des gènes à supprimer dans la liste.
    cpt <- 0               # compteur pour les gènes à supprimer.
    nameRemovedGene <- c() # noms des gènes à supprimer.
    for(k in 1:length(lstGene)){
      
      
      if(sum(lstGene[[k]]$PSS == "") == nrow(lstGene[[k]])){
        cpt <- cpt + 1
        toRemove[cpt] <- k
        nameRemovedGene[cpt] <- names(lstGene)[k]
      }
    }#for(k in 1:length(lstGene )){
    
    #-- suppression des gènes, s'ils existent.
    if(!is.null(toRemove)){ lstGene <- lstGene[-toRemove]}
    
    #--Fin de la suppression des génes pour lesquels on ne trouve aucun site
    
    #-----------------------------------------------------------------------------------------------------------------------------
    
    #-- Détermination de la position maximale présente dans le jeu de données, c'est pour adapter l'axe des x en fonction.
    
    maxPos <- c() #position maximale trouvée dans tous les jeu de données.
    maxDet <- c() #valeur du nombre de détection maximale trouvé, c'est pour ajuster le cadre.
    for(tpp in 1:length(lstGene)){
      tabPos = table(as.numeric(unlist(strsplit(lstGene[[tpp]]$PSS , split = "," , fixed  = T))))
      maxPos[tpp] <- max(as.numeric(names(tabPos)) , na.rm = T) 
      maxDet[tpp] <- max(as.numeric(tabPos)        , na.rm = T)
    }
    
   # maxPos <- max(maxPos) + log2(max(maxPos)) #j'ajoute log10 pour élargir le cadre un petit chouïa.
    maxGeneSize <- max(leaTab$GeneSize) #largeur de la page est la taille du plus grand gène. 
    maxDet <- max(maxDet)
    #-- Fin de la détermination de la position maximale.
    
    #-----------------------------------------------------------------------------------------------------------------------------
    
    
    ##-- Couleur des méthodes
    #-- Noms des méthodes, c'est pour assigner une couleur à une méthode et qui reste la même dans la figure 1 et 3.
    
    TAB4_col <- metColor[which(metColor$Nom %in% unique(TAB4$Method)) , ]
    
    
    ##-- fin des couleurs des méthodes
    #-----------------------------------------------------------------------------------------------------------------------------
    
    
    PAS <- input$nbGene #= 10 #length(lstGene)+2 # PAS designe le nombre de génes pas page
    # observeEvent(input$nbGene, {
    #   PAS <<- input$nbGene
    #   if (PAS == 0 | is.na(PAS)){
    #     updateNumericInput(session, "val", value = 12)
    #   }
    # })
    
    par(mfrow = c(PAS,1) ,#c(length(lstGene) , 1) , #c(length(lstGene) , 1), # c(10,1)
        oma = c(1,2,0,0) + 0.1,
        mar = c(20/PAS,8,0,4) + 0.1
    )
    
    #-----------------------------------------------------------------------------------------------------------------------------
    #-- La légende
    plot(0,0, axes  = F , xlim = c(0 , maxGeneSize), ylim = c(0,maxDet+1 ), bty="n", xlab = "" , ylab = "" , type = "n") #main = names(lstGene)[1],
    
    par(xpd = T)
    legend("topright", legend  = TAB4_col$Nom , fill = TAB4_col$coul,   horiz = T , title = "Used Methods",
           bg = NA,
           bty = "n",
           title.col = "black" ,
           text.col = TAB4_col$coul ,
           border = NA,
           inset = c(+0.1 ,+0.3),
           cex  = min(10/PAS, 1.5))
    par(xpd = F)
    #-- Fin de la légende.
    #-----------------------------------------------------------------------------------------------------------------------------
    compteur <- 0
    cum <- NULL #Pour un géne[t], 'cum' est une concaténation de matrices, où chaque matrice est une colonne où les rownames 
    #sont les positions des sites et le contenu de la colonne est le nombre 1 ou NA. Le résultat de la concaténation des méthodes
    #est une matrice où les colnames sont les noms des méthodes et les rownames sont toutes les positions trouvées par toutes 
    #les méthodes du géne[t].
    
    #-- itération sur les génes.
    for(t in 1:length(lstGene)){
      
      compteur <- compteur +1  
      #-- Je vérifie que la taille du géne est la méme dans toutes les cases de la colonne 'GeneSize' d'un géne donné.
      sizeCurrentGene <- unique(leaTab[leaTab$Gene==names(lstGene)[t],"GeneSize"])
      if(length(sizeCurrentGene)>1){
        rm(list =ls())
        stop(paste("Incohérence dans le ficher de sortie du Pipeline : Plusieurs tailles pour un méme gène!! Cf. le gène nommé", names(lstGene)[t] ))
      }
      #Fin vérification de la taille du gène
      
      compMet <- 0 #compter les méthodes qui donnent des positions dans la colonne PSS.
      rm(cum)
      
      #-- itération sur les les méthodes du géne[t].
      for(met in 1:nrow(lstGene[[t]])){#met parcourt les méthodes d'un géne.
        
        if(lstGene[[t]][met,"PSS"]!= ""){# si la méthode[t] donne des sélections positives.
          compMet <- compMet + 1
          
          ff = as.matrix(table(as.numeric(unlist(strsplit(lstGene[[t]][met,"PSS"] , split = "," , fixed  = T)))))
          colnames(ff) <- lstGene[[t]][met, "Method"]
          
          if(compMet == 1){ cum <-  ff }
          
          if(compMet > 1){
            
            cum <- merge( cum , ff  , by ="row.names", all = T , sort = F)
            rownames(cum) <- cum$Row.names 
            cum$Row.names <- NULL
          }
        }
      }
      
      #-- la fonction cumsum avec des NA ne donne pas exactement ce é quoi on s'attend, du coup je convertis les NA en 0.
      x2 <- cum
      x2[is.na(x2)] <- 0
      
      #-- Si la table ne contient qu'une colonne, car une seule méthode donne des positions.
      if (ncol(x2) > 1) { #on fait le cumsum seulement lorsqu'il y a 2 colonnes et plus dans la table.
        
        x3 <- (t(apply(x2 , 1 , cumsum)))
        
        x4 <- x3
        
        #-- Avec la bouble boucle for ci-dessous je remets les NA lé où il y avais les zéros.
        # Si la valeur de la ligne i et de la colonne j a été vue dans une colonne < j de la méme ligne ce qu'il y avait un 0. 
        for (i in 1:nrow(x4)) {
          for (j in 2:ncol(x4)) {
            if (x4[i, j] %in% x4[i, (j - 1):1]) {
              x4[i, j] <- 0
            }
          }
        }
        
        X5          <- x4
        X5[X5 == 0] <- NA
      } else{
        X5 <- x2
      }#Sinon du "Si la table ne contient qu'une colonne"
      
      
      for(CC in 1:ncol(X5)){
        
        if(CC == 1){
          plot(0,0, axes  = F , xlim =  c(0 , maxGeneSize), ylim = c(0,maxDet ), bty="n", xlab = "" , ylab ="",type ="n") #main = names(lstGene)[1],
          mtext(names(lstGene)[t],side=2,las=1,line=-2, cex  = 1 )
        }
        
        X <- as.numeric(rownames(X5)) #Les positions des traits verticaux.
        Y <- X5[,CC]                  #La hauteur des traits verticaux.
        
        
        #-- couleur selon la méthode.
        CLR <- metColor[which(metColor$Nom == colnames(X5)[CC]), "coul"]
        
        segments(X , Y-1+0.1 , X , Y-0.2 , col = adjustcolor(CLR, alpha.f = 0.8), lwd = 2) #Traits vérticaux
        segments(0 , -0.1 , sizeCurrentGene , -0.1 , col = "black" , lwd = 1) #le Trait horizontal du bas.
       
        petitDist <- maxGeneSize/0.4e3 #Petite distance pour écarter un peu le cadre gris à droite et à gauche.   
        #-- Cadre
        segments(0-petitDist , 0-petitDist , 0-petitDist , maxDet  , col = adjustcolor("grey50", alpha.f = 1), lwd = 0.1)  # le trait Gauche
        segments(0-petitDist , maxDet  , sizeCurrentGene+petitDist ,maxDet  , col = "grey50" , lwd = 0.1)          # le trait haut
        segments(sizeCurrentGene+petitDist , maxDet  , sizeCurrentGene+petitDist ,0  , col = "grey50" , lwd = 0.1) # le trait Droit
        #-- fin cadre
        
        #Affichage du nom du géne
        #mtext(paste(ordre[names(lstGene)[t],"Y"] , "'Yes' & " , sum(ff), "Sites "),side=4,las=1,line=-10, cex  = 0.6 )
        #axis(1, labels = seq(0,5000, 1000) , at = seq(0,5000, 1000), cex.axis = 0.6)
        #axis(1, labels = NA , tick= F, col.ticks ="red", lwd = 0.001)
      }  #for(CC in 1:ncol(X5)){
      
      # if(t %% 11 == 0){plot.new()}
      
      #}#for(t in 1:length(lstGene)){
      
      
      if(t %in%  c(PAS - 1, (PAS-1)+(1:length(lstGene))*(PAS) , length(lstGene))){ # pour le placement des axes de bas de page.
        
        #-- le tout dernier plot de la page, c'est pour poser l'axe qui porte la graduation des positions. 
        #plot(X,Y, axes  = F , xlim =  c(0 , maxGeneSize), ylim = c(0,maxDet ), bty="n", xlab ="" , ylab ="" , type = "n")
        
        
        
        #-- étendue de l'axe
        etend <- seq(0,maxGeneSize, length.out = 5) - (seq(0,maxGeneSize, length.out = 5) %% 100)
        
        #segments(0 , 0 , maxGeneSize , 0 , col ="royalblue" , lwd = 2) #pour afficher le trait horizontal.
        
        axis(side  = 1 , labels = etend , at = etend , cex.axis = 1 , outer = T ,  tick = T , pos = -0.5 , col ="royalblue", lwd.ticks= 0 ) #uniquement pour afficher les chiffres ,line = -6
      }
    }#for(t in 1:length(lstGene)){
    
    
    dev.off() 
    #########################################fin pdf##############################################################   
    #########################################fin pdf##############################################################
    #########################################fin pdf##############################################################
    #########################################fin pdf##############################################################
    #########################################fin pdf##############################################################
    
    ###################################################################################### pdf à télécharger
    output$download_PDF <- downloadHandler(
      
      #-- Nom du fichier de sortie
      filename = paste0(monDocName,"_", Sys.Date(), ".pdf")
      ,
      #--createdTempFilePath est un chemin temporaire créer par la fonction 'downloadHandler' pour stocker les éléments zippés.
      #Exemple : "C:\\Users\\oallatif\\AppData\\Local\\Temp\\RtmpEvac1y\\file3f4458725b.zip"
      content = function(createdTempFilePath) {
        
        file.copy("leaGraphs.pdf", createdTempFilePath)
        
      }
      ,
      contentType = "image/pdf"
    )#downloadHandler
    ######################################################################################
    
    
    
##==========================================================================================================================================================    
                          ##################### Affichage WEB #####################################    
##==========================================================================================================================================================    
    #-- output relatif au barPlots
    
    
    output$tableCsv    <- renderTable(leaTab)
    hide("tableCsv")
    
    observeEvent(
      input$showHideData, {
        toggle("tableCsv")
      })#observeEvent
    
    #################################################################################
    #################################################################################
    ########################### Heatmap ##########################################
    #################################################################################
    output$heat  = renderPlot({
      
      par(mar =c(4,4,2,4), oma = c(0,0,0,0) , mgp = c(2,1,0)) #mgp est la distance entre les rownames et le plot. 
      heatmap.2(as.matrix(tabHeat),
                # add.expr = list(
                #   abline (h = 1:nrow(tabHeat)+0.5 , col = adjustcolor("black" , 0.1))
                # ),
                trace = "none", 
                colsep = 1:ncol(tabHeat),
                rowsep = 1:nrow(tabHeat),
                sepcolor = "grey55",
                dendrogram = "none", 
                Rowv = F , 
                Colv = F, 
                key          = F                  ,
                
                
                col = adjustcolor( c( "grey90" , "grey20"), alpha.f = 0.7), 
                sepwidth = 0.0001 ,
                margins  = c(7, 14)
                ,
                # offsetRow = -50, 
                # offsetCol = -50  #
                # , 
                srtCol=40
                ,
                cexCol = 1.2
                ,
                main = "Detection of positive selection",
                ylab = "Genes" ,
                xlab = "Methods"
                ,
                cexRow =  min(50/nrow(tabHeat), 1.5)
      )
      
      
      legend("topleft" , 
             legend = c("Not Detected" , "Detected") ,
             fill = c( "grey90" , "grey20") ,
             xpd =T, 
             inset = c(0.1 ,0.1),
             bg = NA,
             bty = "n",
             title = "Detection status",
             title.col = "tomato" ,
             #text.col = c( "grey90" , "grey20") ,
             cex = 1.5,
             border = NA
             )
      
    })
    hide("heat")
    
    
    #################################################################################
    #################################################################################
    ########################### barplots ##########################################
    #################################################################################
    #-- barPlots ce n'est pas le diagramme de venn, c'est le 1er barplot de gauche, j'ai gardé le nom par facilité.
    
    
    
    #################################################################################
    #################################################################################
    ########################### 1er barplot ##########################################
    #################################################################################
    output$barPlots  = renderPlot({ 
      
      
      
      par(xpd = T , mar=c(4,14,12,0) , mfrow  =c(1,2) ,   oma = c(0,0,0,0) , mgp = c(3,0.3,0))
      
      h =  barplot(
        t(xt2),
        border = NA ,
        axes = F ,
        col = adjustcolor(xt2_col$coul, alpha.f = 1),
        horiz = T ,
        las  = 2 ,
        main = "Methods detecting positive selection" ,  
        #c(
        #   "Detection of positive selection",
        #   paste( nbrMeth ,  "methods")
        # ),
        cex.main = 0.9 ,
        
        cex.names  = min(50/nrow(tabHeat), 1.5) ,
        
        legend.text = paste0(1:ncol(xt2), ") ",  xt2_col$Nom),
        
        args.legend = list(
          x = "topleft",
          bg = NA,
          bty = "n",
          title = "Used Methods",
          title.col = "gold" ,
          text.col = xt2_col$coul ,
          horiz = F ,
          cex = 1,
          border = NA,
          inset = c(-0.2 ,-0.15)
        )
      )
      
      axis(3, line = 0, at = c(0:nbrMeth), label = c("0", rep("", nbrMeth -1), nbrMeth), tck = 0.02)
      
      
      #-- Fin du traitement du point 1 et du graphique "Number of methods detecting positive selection"
      
      #################################################################################
      #################################################################################
      ############################2ème barplot##########################################
      #################################################################################
      #-- Début du graphique "Percentage of positively selected sites"
      
      par(mar=c(4,6,12,2))
      
      
      xx = barplot(
        tabFin_v2$ratio  ,
        names.arg = NULL ,
        las = 1,
        cex.names = 1,
        col = adjustcolor("blue4", alpha.f = 0.5),
        border = NA,
        horiz = T ,
        axes = F,
        main = "Percentage of all positively selected sites over alignment length",
        #expression(paste("Positively selected sites ratio: Sites number of"~ Gene[i] %/% Gene[i], " length" )) ,
        xlim = c(0, bornSup) ,
        cex.main = 0.9
      )
      
      axis(3, line = 0, labels =  c("0%" , paste0(bornSup*100,"%")), at = seq(0,bornSup, length.out = 2), las  = 1, tck = 0.02)
      
      #Cex <- log2(tabFin_v2$leaTab.Omega*100)/2
      posX <- -0.015 #-0.05 #-0.05
      
      text(rep(posX, length(xx)) , xx,  tabFin_v2$leaTab.Omega)
      
      par(mar = rep(0,4), xpd  =T)
      
      text(   posX , max(xx, na.rm =T) + 4 , expression(paste(omega)), cex = 2.5)
      arrows( posX , max(xx, na.rm =T) + 3 , posX , max(xx, na.rm =T) + 1, code = 2 , col = "magenta" , lwd = 1, length = 0.15)
      
    })
    hide("barPlots")
    #################################################################################
    #################################################################################
    ########################### Fin des barplots ####################################
    #################################################################################   
    
    
    
    
    #################################################################################
    #################################################################################
    ########################### Position des sites ##########################################
    #################################################################################
    
    
    output$sitePos <- renderPlot({
      
      #-----------------------------------------------------------------------------------------------------------------------------
      
      par(mfrow = c(length(lstGene)+2,1) ,#c(length(lstGene) , 1) , #c(length(lstGene) , 1), # c(10,1)
          oma = c(0,2,0,0) + 0.1,
          mar = c(0,8,0,2) + 0.1,
          xpd =T
      )
      
      #-----------------------------------------------------------------------------------------------------------------------------
      #-- La légende
      plot(0,0, axes  = F , xlim =  c(0 , maxGeneSize), ylim = c(0,maxDet+1 ), bty="n", ylab ="",type ="n") #main = names(lstGene)[1],

      par(xpd = T)
      legend("topright", legend  = TAB4_col$Nom , fill = TAB4_col$coul,   horiz = T , title = "Used Methods",
             bg = NA,
             bty = "n",
             title.col = "indianred" ,
             text.col = TAB4_col$coul ,
             border = NA,
             inset = c(+0.1 ,0),
             cex  = 2)
      par(xpd = F)
      #-- Fin de la légende.
      #-----------------------------------------------------------------------------------------------------------------------------
      
      cum <- NULL #Pour un gène[t], 'cum' est une concaténation de matrices, où chaque matrice est une colonne où les rownames 
      #sont les positions des sites et le contenu de la colonne est le nombre 1 ou NA. Le résultat de la concaténation des méthodes
      #est une matrice où les colnames sont les noms des méthodes et les rownames sont toutes les positions trouvées par toutes 
      #les méthodes du gène[t].
      
      #-- itération sur les gènes.
      for(t in 1:length(lstGene)){
        
        #-- Je vérifie que la taille du gène est la même dans toutes les cases de la colonne 'GeneSize' d'un gène donné.
        sizeCurrentGene <- unique(leaTab[leaTab$Gene==names(lstGene)[t],"GeneSize"])
        if(length(sizeCurrentGene)>1){
          rm(list =ls())
          stop(paste("Incohérence dans le ficher de sortie du Pipeline : Plusieurs tailles pour un même gène!! Cf. le gène nommé", names(lstGene)[t] ))
        }
        #Fin vérification que la taille du gène
        
        compMet <- 0 #compter les méthodes qui donnent des positions dans la colonne PSS.
        rm(cum)
        
        #-- itération sur les les méthodes du gène[t].
        for(met in 1:nrow(lstGene[[t]])){#met parcourt les méthodes d'un gène.
          
          if(lstGene[[t]][met,"PSS"]!= ""){# si la méthode[t] donne des sélections positives.
            compMet <- compMet + 1
            
            ff = as.matrix(table(as.numeric(unlist(strsplit(lstGene[[t]][met,"PSS"] , split = "," , fixed  = T)))))
            colnames(ff) <- lstGene[[t]][met, "Method"]
            
            if(compMet == 1){ cum <-  ff }
            
            if(compMet > 1){
              
              cum <- merge( cum , ff  , by ="row.names", all = T , sort = F)
              rownames(cum) <- cum$Row.names 
              cum$Row.names <- NULL
            }
          }
        }
        
        #-- la fonction cumsum avec des NA ne donne pas exactement ce à quoi on s'attend, du coup je convertis les NA en 0.
        x2 <- cum
        x2[is.na(x2)] <- 0
        
        #-- Si la table ne contient qu'une colonne, car une seule méthode donne des positions.
        if (ncol(x2) > 1) { #on fait le cumsum seulement lorsqu'il y a 2 colonnes et plus dans la table.
          
          x3 <- (t(apply(x2 , 1 , cumsum)))
          
          x4 <- x3
          
          #-- Avec la bouble boucle for ci-dessous je remets les NA là où il y avais les zéros.
          # Si la valeur de la ligne i et de la colonne j a été vue dans une colonne < j de la même ligne ce qu'il y avait un 0. 
          for (i in 1:nrow(x4)) {
            for (j in 2:ncol(x4)) {
              if (x4[i, j] %in% x4[i, (j - 1):1]) {
                x4[i, j] <- 0
              }
            }
          }
          
          X5          <- x4
          X5[X5 == 0] <- NA
        } else{
          X5 <- x2
        }#Sinon du "Si la table ne contient qu'une colonne"
        
        
        for(CC in 1:ncol(X5)){
          
          if(CC == 1){
            plot(0,0, axes  = F , xlim =  c(0 , maxGeneSize), ylim = c(0,maxDet ), bty="n", ylab ="",type ="n") #main = names(lstGene)[1],
            mtext(names(lstGene)[t],side=2,las=1,line = -2 , cex  = 1 )
          }
          
          X <- as.numeric(rownames(X5)) #Les positions des traits verticaux.
          Y <- X5[,CC]                  #La hauteur des traits verticaux.
          
          
          #-- couleur selon la méthode.
          CLR <- metColor[which(metColor$Nom == colnames(X5)[CC]), "coul"]
          
          segments(X , Y-1+0.1 , X , Y-0.2 , col = adjustcolor(CLR, alpha.f = 1), lwd = 2) #Traits vérticaux
          segments(0 , -0.1 , sizeCurrentGene , -0.1 , col = "black" , lwd = 1) #Trait horizontal.
          
          
          petitDist <- maxGeneSize/0.4e3 #Petite distance pour écarter un peu le cadre gris à droite et à gauche.   
          #-- Cadre
          segments(0-petitDist , 0-petitDist , 0-petitDist , maxDet  , col = adjustcolor("grey50", alpha.f = 1), lwd = 0.1)  # le trait Gauche
          segments(0-petitDist , maxDet  , sizeCurrentGene+petitDist ,maxDet  , col = "grey50" , lwd = 0.1)          # le trait haut
          segments(sizeCurrentGene+petitDist , maxDet  , sizeCurrentGene+petitDist ,0  , col = "grey50" , lwd = 0.1) # le trait Droit
          #-- fin cadre
          
          #Affichage du nom du gène
          #mtext(paste(ordre[names(lstGene)[t],"Y"] , "'Yes' & " , sum(ff), "Sites "),side=4,las=1,line=-10, cex  = 0.6 )
          #axis(1, labels = seq(0,5000, 1000) , at = seq(0,5000, 1000), cex.axis = 0.6)
          #axis(1, labels = NA , tick= F, col.ticks ="red", lwd = 0.001)
        }  #for(CC in 1:ncol(X5)){
        
        
      }#for(t in 1:length(lstGene)){
      
      
      #-- le tout dernier plot de la page, c'est pour poser l'axe qui porte la graduation des positions. 
      plot(X,Y, axes  = F , xlim =  c(0 , maxGeneSize), ylim = c(0,maxDet ), bty="n", xlab ="" , ylab ="" , type = "n")
      
      
      
      #-- étendue de l'axe
      etend <- seq(0,maxGeneSize, length.out = 5) - (seq(0,maxGeneSize, length.out = 5) %% 100)
      
      #segments(0 , maxDet , maxGeneSize , maxDet , col ="royalblue" , lwd = 2) #pour afficher le trait horizontal.
      
      axis(side  = 1 , labels = etend , at = etend , cex.axis = 1 , outer = F ,  tick = T , pos = maxDet , col ="royalblue", lwd.ticks= 0 ) #uniquement pour afficher les chiffres ,line = -6
      
      # mtext(side = 1 ,"Gene Size", adj = 0.5, padj=0, cex = 1)
      # Fin du tout dernier plot de la page
      
      
    })
    
    hide("sitePos")
    
    observeEvent(
      input$showHideVenn , {
        toggle("heat")
        toggle("barPlots")
        toggle("sitePos")
      })#observeEvent
    
    
  })#observe (le 1er 'observe' dans la fonction 'server')
}#server
