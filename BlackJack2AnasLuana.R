####  Bienvenue ###
### MERCI DE BIEN LIRE CECI, AVANT DE COMMENCER ###

### Vous allez jouer au Black Jack ###
### Rules : en fonction de vos parametres, vous allez passer en mode MANUEL ou AUTOMATIQUE###
### Pour passer en mode AUTOMATIQUE, exemple: playBlackJack(17,17,1) ###
### Pour passer en mode MANUEL, exemple: playBlackJack(17,,4) ---> donc ne completez pas la limite du joueur
### c'est a vous de trouver vos propres strategies!###


########################## PLAY BLACK JACK ################################################
playBlackJack <- function(limite.croupier,limite.joueur,n, affichage = TRUE){

  #on definit un vecteur avec tous les points totaux du joueurs
  point.joueur <- rep(NA,n)
  
  #Quelques fonctions bien utiles:
  
  #Calcule les points pour le(s) joueur(s) et pour le croupier
  calculPoint <- function(mains, joueur) {
    
    main <- mains[,joueur]
    main[main %in% c("Valet", "Dame", "Roi")] <- 10
    sub.main <- as.numeric(main[main != "As"])
    sub.tot <- sum(sub.main, na.rm = TRUE)
    
    main[main == "As"] <- ifelse(sub.tot > 10, 1, 11)
    
    return(sum(as.numeric(main), na.rm = TRUE))
    
  }

  #Affiche les points du croupier et du/des joueur(s), dans un tableau
  affichagePoints <- function(){
    
    matrice.pts <- matrix(nrow=2, ncol=(n + 1))
    colnames(matrice.pts) <- c("Croupier", paste("Joueur", 1:n))
    rownames(matrice.pts) <- c("Somme totale des points","Status")
    
    matrice.pts[1,1] <- nb.point  #on indexe les points du croupier
    matrice.pts[1,(2:(n+1))] <- point.joueur #on indexe les points du joueur
    matrice.pts[2,(2:(n+1))] <- Status() #on indexe le status
    matrice.pts[2,1] <- "Banque" 
    print(matrice.pts)
  }

  #Donne un status a chaque joueur
  Status <- function(){
    
   #prend le status des joueurs, par rapport a la banque (gagnant, perdant, egalite)
   status <- rep(NA,n)
    
    for (i in 1:n){ 
      if((point.joueur[i] > nb.point & point.joueur[i] <= 21)
         | (nb.point > 21 & point.joueur[i] <= 21)){
        status[i] <- "Gagnant"
      } else if(point.joueur[i] == nb.point & point.joueur[i] <= 21){
        status[i] <- "Egalite"
      } else{
        status[i] <- "Perdant" }
    }
   return(status)
  }
  
  
  #Une petite fonction qui affiche le jeu
  afficheJeu <- function()
    print(mains[1:tour,])
  
  jeu <- rep(c("As", 2:10, "Valet", "Dame", "Roi"), 4)
  jeu <- sample(jeu, 52)
  
  mains <- matrix(NA, 52, n+1)
  colnames(mains) <- c("Croupier", paste("Joueur", 1:n))
  
  #la premiere distribution des 2 cartes au croupier et n joueurs
  mains[1,] <- jeu[1:(n+1)] #distribution 1ere carte
  jeu <- jeu[-(1:(n+1))]
  mains[2,] <- jeu[1:(n+1)]  #distribution 2eme carte
  jeu <- jeu[-(1:(n+1))]
  
  tour <- 2
  
  if (affichage)
    afficheJeu()
  
  ## Commencons a jouer - LET THE GAME BEGIN ##
  
  ###### Ici c'est le mode MANUEL
  if (missing(limite.joueur)) { 
    
    cat("Vous etes pass�s en mode MANUEL.
        C'est a vous de choisir les strategies de chaque joueur!")
    
    for (i in 1:n) #i c'est le joueur
    {
      tour <- 3
      
      while (TRUE){
        veuxCarte <- readline("Prenez-vous une carte ? (Carte / Non) : ")
        
        point.joueur[i] <- calculPoint(mains, (i+1))
        
        if (veuxCarte == "Non") {
          break 
        } else if (veuxCarte == "Carte") {
          
#          point.joueur[i] <- calculPoint(mains, (i+1))
          
          #distribution de carte au joueur i 
          if (point.joueur[i] <= 21) {         
            mains[tour, (i+1)] <- jeu[1]   
            jeu <- jeu[-1]  
            tour <- tour + 1 
          } else {
            cat("Tu as perdu. On passe au prochain joueur.")
            break   
          }  
          
          afficheJeu()
        } else {
          cat("Mauvaise reponse. Re-essayez : Carte/Non")
        }
      }
    }
    tour.save <- tour
    
    tour <- 3
  
    ## Le croupier joue
    while (TRUE){
      nb.point <- calculPoint(mains, 1)
      
      if (nb.point < limite.croupier){
        mains[tour, 1] <- jeu[1]
        jeu <- jeu[-1]
        tour <- tour + 1
        
        afficheJeu()
      } else
        break   
    }
    
    mains <- mains[1:max(tour.save, tour),]

    Status()
    affichagePoints()
    
###### Ici c'est le mode AUTOMATIQUE
    
  } else {
    for (i in 1:n) 
    {
      
      tour <- 3
      while (TRUE) {
        
        point.joueur[i] <- calculPoint(mains, (i+1)) 
        
        if (point.joueur[i] < limite.joueur) { 
          mains[tour, (i + 1)] <- jeu[1]   
          jeu <- jeu[-1]  
          tour <- tour + 1 
        } else if (point.joueur[i] > 21) {
          break
        } else {
          break
        }
  
        
        if (affichage)
          afficheJeu()
      }
    }
    
    tour.save <- tour


    ## Le croupier joue
    tour <- 3
    
    while (TRUE){
      nb.point <- calculPoint(mains, 1)
      
      if (nb.point < limite.croupier){
        mains[tour, 1] <- jeu[1]
        jeu <- jeu[-1]
        tour <- tour + 1
        
        
        
        if (affichage)
          afficheJeu()
        
      } else
        break   
    }
    
    mains <- mains[1:max(tour.save, tour),]
    
    
    if (affichage){
      Status()
       affichagePoints()}
    
  }
  
  
  invisible(Status())
  # ca tourne le Status en mode invisible
  # pour pouvoir etre utilis� dans le Monte Carlo 
}




############################################### MONTE CARLO ##########################################"

MonteCarlo <- function(nb.strategies,essais){ 

# nb.strategies = le nb de strategies que vous allez tester; 
# essais = le nombre de fois que vous allez tourner le code, par strategie
# la matrice qui va donner le nombre total de gains et la probabilite, par strategie
  
  Comptage <- matrix (0, nrow = nb.strategies, ncol = 4)
  colnames(Comptage) <- c(" Strategie ","Egalite","Gagnant","Perdant")
  
  matriceProba <- matrix (0, nrow = nb.strategies, ncol = 4)
  colnames(matriceProba) <- c("Strategie","Egalite","Gagnant","Perdant")
  
  
  for (i in 1:nb.strategies){
    lim.jou <- as.numeric(readline("Quelle limite pour le joueur? (exemple: 17):"))
    
    
    Comptage[i,1] <- lim.jou  #la premiere colonne reprz les stratgies

    VecteurGains <- rep(NA,essais)
    
    for (j in 1:essais){
      
      VecteurGains[j] <- playBlackJack(17,lim.jou,1, affichage = FALSE)
    }
    
    # VecteurGains ordonn� E G P
    sorted.vectgains <- as.vector(sort(t(table(VecteurGains)) , 
                                       ord = 0 , decreasing = FALSE )) 
    if ( length(sorted.vectgains) == 2) {
    Comptage[i,3:4] <- sorted.vectgains
    } else {
    Comptage[i,2:4] <- sorted.vectgains
      
    }
  }
  
  rownames(Comptage) <- paste(Comptage[,1])
  rownames(matriceProba) <- paste(Comptage[,1])
  
  
   matriceProba[1:nb.strategies, 2:4] <- Comptage[,2:4]/essais
   matriceProba[,1] <- Comptage[1:nb.strategies,1]

   print("Voici le nombre de gains en fonction de chaque strategie:")
   prmatrix(Comptage , rowlab=rep("", nb.strategies))
   print("Voici la probabilit� de gain en fonction de chaque strategie:")
   prmatrix(matriceProba , rowlab=rep("", nb.strategies))
   

   barplot(t(matriceProba[,2:4]), ylim = c(0, 0.1 + max(matriceProba[,2:4])) 
           ,main = "Probabilite de gain, par strategie (Monte Carlo)",
           xlab="Strategies choisies", col=c("white","green","red"), 
           beside=TRUE)
   legend ("topleft",c("Egalite", "Gagnant" , "Perdant"), fill =c("white","green","red"))
   
}



############### ESSAIS : 

playBlackJack(17,17,3)  # mode AUTOMATIQUE
playBlackJack(17, ,3) # mode MANUEL 
MonteCarlo(3,1000) # Monte Carlo pour 3 strategies et 1000 essais

#### Controle d'optimisation :
system.time(playBlackJack(17,17,2)) # 0.14s pour 10 joueurs ! Pas mal hein ?!
system.time(MonteCarlo(1,1000))  # 0.33s pour 1000 essais. Ca a l'air bon.