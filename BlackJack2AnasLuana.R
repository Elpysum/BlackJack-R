####  Bienvenue ###
####  Welcome ###

### MERCI DE BIEN LIRE CECI, AVANT DE COMMENCER ###
### THANKS FOR READING THIS? BEFORE PLAYING ###

### Vous allez jouer au Black Jack ###
### Rules : en fonction de vos parametres, vous allez passer en mode MANUEL ou AUTOMATIQUE###
### Explication : playBlackJack(limite croupier, limite joueur, nombre joueurs) (voir les régles) ###
### Pour passer en mode AUTOMATIQUE, exemple: playBlackJack(17,17,1) ###
### Pour passer en mode MANUEL, exemple: playBlackJack(17,,4) ---> donc ne completez pas la limite du joueur
### C'est a vous de trouver vos propres strategies!###

### You will play Black Jack ###
### Rules : in fonction of your parametres, you'll pass in MANUAL mode or AUTOMATIC one ###
### Some explanation : playBlackJack(house limit, player limit, number of players) ###
### Some explanation : house/player limit = the limit of numbers of each (see the rules) ###
### For AUTOMATIC, example :  playBlackJack(17,17,1) ###
### For MANUAL, example : playBlackJack(17,,4) ---> so, donc complete the player limit
### You'll have to find your own strategy ###


########################## PLAY BLACK JACK ################################################
playBlackJack <- function(limite.croupier,limite.joueur,n, affichage = TRUE){

  #On definit un vecteur avec tous les points totaux du joueurs
  #We define a vector with the total players numbers
  point.joueur <- rep(NA,n)
  
  #Quelques fonctions bien utiles:
  #Some usefull functions:
  
  #Calcule les points pour le(s) joueur(s) et pour le croupier
  #Calculates the points for the players AND the house
  calculPoint <- function(mains, joueur) {
    
    main <- mains[,joueur]
    main[main %in% c("Valet", "Dame", "Roi")] <- 10
    sub.main <- as.numeric(main[main != "As"])
    sub.tot <- sum(sub.main, na.rm = TRUE)
    
    main[main == "As"] <- ifelse(sub.tot > 10, 1, 11)
    
    return(sum(as.numeric(main), na.rm = TRUE))
    
  }

  #Affiche les points du croupier et du/des joueur(s), dans un tableau
  #Shows the points of the house AND players, in a table
  affichagePoints <- function(){
    
    matrice.pts <- matrix(nrow=2, ncol=(n + 1))
    colnames(matrice.pts) <- c("Croupier", paste("Joueur", 1:n))
    rownames(matrice.pts) <- c("Somme totale des points","Status")
    
    matrice.pts[1,1] <- nb.point  #on indexe les points du croupier/we index the house points
    matrice.pts[1,(2:(n+1))] <- point.joueur #on indexe les points du joueur/we index the players points
    matrice.pts[2,(2:(n+1))] <- Status() #on indexe le status/we index the Status (Gain, Loss, Equality)
    matrice.pts[2,1] <- "Banque" 
    print(matrice.pts)
  }

  #Donne un status a chaque joueur
  #Gives a status to each player (Gain, Loss, Equality)
  Status <- function(){
    
   #Prend le status des joueurs, par rapport a la banque (gagnant, perdant, egalite)
   #Takes the status of players, rapporting to the house (Gain = Gagnant, Loss = Perdant, Equality = Egalité)
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
  #Small function who shows the game
  afficheJeu <- function()
    print(mains[1:tour,])
  
  jeu <- rep(c("As", 2:10, "Valet", "Dame", "Roi"), 4)
  jeu <- sample(jeu, 52)
  
  mains <- matrix(NA, 52, n+1)
  colnames(mains) <- c("Croupier", paste("Joueur", 1:n))
  
  #La premiere distribution des 2 cartes au croupier et "n" joueurs
  #First distribution of 2 cards to the house and "n" players
  mains[1,] <- jeu[1:(n+1)] #distribution 1ere carte/card
  jeu <- jeu[-(1:(n+1))]
  mains[2,] <- jeu[1:(n+1)]  #distribution 2eme carte/card
  jeu <- jeu[-(1:(n+1))]
  
  tour <- 2
  
  if (affichage)
    afficheJeu()
  
  ## Commencons a jouer - LET THE GAME BEGIN ##
  
  ###### Ici c'est le mode MANUEL
  ###### MANUAL mode
  if (missing(limite.joueur)) { 
    
    # You pass in Manual mode, it's up to you to chose the best strategy to gain
    cat("Vous etes passés en mode MANUEL.
        C'est a vous de choisir les strategies de chaque joueur!")
    
    for (i in 1:n) #i c'est le joueur/ i it's the player
    {
      tour <- 3
      
      while (TRUE){
        veuxCarte <- readline("Prenez-vous une carte ? (Carte / Non) : ") #You'll chose a card ? Yes / No 
        
        point.joueur[i] <- calculPoint(mains, (i+1))
        
        if (veuxCarte == "Non") {
          break 
        } else if (veuxCarte == "Carte") {
                    
          #Distribution de carte au joueur "i" 
          #Card distribution to player "i"
          if (point.joueur[i] <= 21) {         
            mains[tour, (i+1)] <- jeu[1]   
            jeu <- jeu[-1]  
            tour <- tour + 1 
          } else {
            cat("Tu as perdu. On passe au prochain joueur.") #You had lost, we pass to the next player
            break   
          }  
          
          afficheJeu()
        } else {
          cat("Mauvaise reponse. Re-essayez : Carte/Non") #Bad response, try again : Card/No
        }
      }
    }
    tour.save <- tour
    
    tour <- 3
  
    ## Le croupier joue
    ## The house plays
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
###### Here is the AUTOMATIC mode
    
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
    ## The house plays
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
  # Ca tourne le Status en mode invisible
  # This turns the Status in invisible mode 
  # pour pouvoir etre utilisé dans le Monte Carlo 
  # for being used in the Monte Carlo analysis
}




############################################### MONTE CARLO ##########################################"

MonteCarlo <- function(nb.strategies,essais){ 

# nb.strategies = le nb de strategies que vous allez tester / number of strategies you'll chose to test
# essais = le nombre de fois que vous allez tourner le code, par strategie / number of times you'll compile the code
# la matrice qui va donner le nombre total de gains et la probabilite, par strategie 
  # matrix that gives the total number of gains and probability, per strategy
  
  Comptage <- matrix (0, nrow = nb.strategies, ncol = 4)
  colnames(Comptage) <- c(" Strategie ","Egalite","Gagnant","Perdant")
  # strategy, equality, winner, looser
  
  matriceProba <- matrix (0, nrow = nb.strategies, ncol = 4)
  colnames(matriceProba) <- c("Strategie","Egalite","Gagnant","Perdant")
  
  
  for (i in 1:nb.strategies){
    lim.jou <- as.numeric(readline("Quelle limite pour le joueur? (exemple: 17):")) #Which limit for the player?
    
    
    Comptage[i,1] <- lim.jou  #la premiere colonne reprz les strategies/ the first column shows the strategies

    VecteurGains <- rep(NA,essais)
    
    for (j in 1:essais){
      
      VecteurGains[j] <- playBlackJack(17,lim.jou,1, affichage = FALSE)
    }
    
    # VecteurGains ordonné E G P
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
  # These are the number of gains in fonction of each strategy
   prmatrix(Comptage , rowlab=rep("", nb.strategies))
   print("Voici la probabilité de gain en fonction de chaque strategie:")
  # These are the probabilities in fonction of each strategy
   prmatrix(matriceProba , rowlab=rep("", nb.strategies))
   

   barplot(t(matriceProba[,2:4]), ylim = c(0, 0.1 + max(matriceProba[,2:4]))  
           ,main = "Probabilite de gain, par strategie (Monte Carlo)", 
           xlab="Strategies choisies", col=c("white","green","red"), 
           beside=TRUE)
   legend ("topleft",c("Egalite", "Gagnant" , "Perdant"), fill =c("white","green","red"))
   # main = probability of gain, per strategy 
   # xlab = chosen strategies
}



############### ESSAIS : 

playBlackJack(17,17,3)  # mode AUTOMATIQUE
playBlackJack(17, ,3) # mode MANUEL 
MonteCarlo(3,1000) # Monte Carlo pour 3 strategies et 1000 essais

#### Controle d'optimisation :
system.time(playBlackJack(17,17,2)) # 0.14s pour 10 joueurs ! Pas mal hein ?!
system.time(MonteCarlo(1,1000))  # 0.33s pour 1000 essais. Ca a l'air bon.
