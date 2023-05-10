#CSR
library(readxl)
library(openxlsx)

# CSR de base:
CSR_base = function(CSR_marche, CSR_concentration, CSR_contrepartie, CSR_souscription_vie, CSR_souscription_nonvie, rho_mat){
  
  #le variable dans lequelle je vais stocker le resultat
  resultat = 0 
  # recuperation du matrice de correlation d'un fichier excel (on va parametre cette etape)
  rho_mat = as.matrix(read_excel("Rho_Mat.xlsx","rho_mat",col_names = FALSE))
  #ecrire les CSR_Parametres pour recuperer ses indices  
  CSR = c(CSR_marche, CSR_concentration, CSR_contrepartie, CSR_souscription_vie, CSR_souscription_nonvie)
  for (i in seq_along(CSR)) {
    for (j in seq_along(CSR)){
      resultat = resultat + rho_mat[i,j]*CSR[i]*CSR[j]
    }
  }
  resultat = sqrt(resultat)
  print(paste("la valeur de CSR_base est :",resultat))
  return(resultat)
}
#petit test CSR_base
#CSR_base(10,10,10,10,10)


# CSR_marche
CSR_marche = function(CSR_action, CSR_taux, CSR_change, CSR_immobilier, CSR_ecart_taux, rho_mat){
  resultat = 0
  rho_mat = as.matrix(read_excel("Rho_Mat.xlsx","rho_mat",col_names = FALSE))
  CSR = c(CSR_action, CSR_taux, CSR_change, CSR_immobilier, CSR_ecart_taux)
  for (i in seq_along(CSR)) {
    for (j in seq_along(CSR)){
      resultat = resultat + rho_mat[i,j]*CSR[i]*CSR[j]
    }
  }
  resultat = sqrt(resultat)
  print(paste("la valeur de CSR_marche est :",resultat))
  return(resultat)
}

# CSR_action 
CSR_action = function(BE_action_Baisse, BE_action){
  return(BE_action_Baisse - BE_action)
}

#CSR_taux
CSR_taux = function(BE_taux_baisse, BE_taux_hausse,BE ){
  CSR_taux_hausse = BE_taux_hausse - BE
  CSR_taux_baisse = BE_taux_baisse - BE
  return(max(CSR_taux_hausse -CSR_taux_baisse))
}

#CSR_immobilier:
CSR_immobilier = function(BE_Baisse, BE){
  return(BE_Baisse - BE)
}

#CSR_ecart_taux: a faire 

#CSR_change: a faire 

#CSR_contrepartie 
CSR_contrepartie = function(CSR_cpt_type1, CSR_cpt_type2,rho_mat){ 
  resultat = 0 
  rho_mat = as.matrix(read_excel("Rho_Mat.xlsx","rho_mat",col_names = FALSE))
  CSR=c(CSR_cpt_type1, CSR_cpt_type2)
  for (i in seq_along(CSR)) {
    for (j in seq_along(CSR)){
      resultat = resultat + rho_mat[i,j]*CSR[i]*CSR[j]
    }
  }
  resultat = sqrt(resultat)
  print(paste("la valeur de CSR_contrepartie est :",resultat))
  return(resultat)

  }

#CSR_contrepartie type 1: annexe 1

#CSR_contrepartie type 2:
CSR_cpt_type2 = function(CSR_cpt_assures, CSR_cpt_intermediaires, CSR_cpt_autres_contreparties){
  somme = CSR_cpt_assures + CSR_cpt_intermediaires + CSR_cpt_autres_contreparties
  return(somme)
}

#CSR_concentration:
#' @param CSR doit etre un vecteur nemurique (on doit parametre cette etape) 
CSR_concentration = function(CSR){
  print(paste("la valeur de CSR_concentration est:",sqrt(sum(CSR^2))))
  return(sqrt(sum(CSR^2)))
}

#CSR_souscription_vie
CSR_souscription_vie = function(CSR_mortalite, CSR_longevite, CSR_rachat, CSR_frais, CSR_catastrophe, rho_mat){
  resultat = 0
  rho_mat = as.matrix(read_excel("Rho_Mat.xlsx","rho_mat",col_names = FALSE))
  CSR = c(CSR_mortalite, CSR_longevite, CSR_rachat, CSR_frais, CSR_catastrophe)
  for (i in seq_along(CSR)) {
    for (j in seq_along(CSR)){
      resultat = resultat + rho_mat[i,j]*CSR[i]*CSR[j]
    }
  }
  resultat = sqrt(resultat)
  print(paste("la valeur de CSR_souscription_vie est :",resultat))
  return(resultat)
}

#CSR_mortalite: 
CSR_mortalite = function(BE_hausse, BE){
  resultat = BE_hausse - BE
  print(paste("la valeur de CSR_mortalite est :",resultat))
  return(resultat)
  
}


#CSR_longevite:
CSR_longevite = function(BE_baisse, BE){
  resultat = BE_baisse - BE
  print(paste("la valeur de CSR_longevite est :",resultat))
  return(resultat)
}

#CSR_rachat:
CSR_rachat = function(BE_hausse, BE_baisse, BE){
  CSR_hausse = BE_hausse - BE
  CSR_baisse = BE_baisse - BE
  resultat = max(CSR_hausse, CSR_baisse)
  print(paste("la valeur de CSR_rachat est :",resultat))
  return(resultat)
}

#CSR_frais:
CSR_frais = function(BE_choc, BE){
  resultat = BE_choc - BE
  print(paste("la valeur de CSR_frais est :",resultat))
  return(resultat)
}

#CSR_catastrophe: (la relation n'est pas cliare a revoir)


#CSR_souscription_non vie
CSR_souscription_nonvie = function(CSR_primes, CSR_provisions, CSR_catastrophe,rho_mat){
  resultat = 0
  rho_mat = as.matrix(read_excel("Rho_Mat.xlsx","rho_mat",col_names = FALSE))
  CSR=c(CSR_primes, CSR_provisions, CSR_catastrophe)
  for (i in seq_along(CSR)) {
    for (j in seq_along(CSR)){
      resultat = resultat + rho_mat[i,j]*CSR[i]*CSR[j]
    }
  }
  resultat = sqrt(resultat)
  print(paste("la valeur de CSR_souscription_vie est :",resultat))
  return(resultat)
}

#CSR_primes:
#CSR_provisions
#CSR_catastrophe

# CSR_operationnel
CSR_operationnel = function(CSR_base, X){
  resultat = CSR_base * X
  print(paste("la valeur de CSR_operationnel est :",resultat))
  return(resultat)
}


#Ajustement du capital de solvabilité requis
#Adj_assures
Adj_assures=function(CSRB_nettes, CSRB_bruttes, BDF){
  resultat = min(abs(CSRB_nettes - CSRB_bruttes ),BDF)
  print(paste("la valeur de Adj_assures est :",resultat))
  return(resultat) 
}


#Adj_impotsDifferes
Adj_impotsDifferes = function(TX_impots, CSR_base,CSR_operationnel,Adj_assures,impots_Actifs, impos_Passif){
  if(impos_Passif- impots_Actifs>0) {ecart = impos_Passif- impots_Actifs
  }else {ecart = 0}
  resultat = TX_impots * min((CSR_base + CSR_operationnel - Adj_assures),ecart)
  print(paste("la valeur de Adj_impotsDifferes est :",resultat))
 return(resultat)
}














