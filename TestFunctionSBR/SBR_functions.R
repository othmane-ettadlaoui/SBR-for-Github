#CSR




# CSR de base:
#' Title
#'
#' @param CSR_marche 
#' @param CSR_concentration 
#' @param CSR_contrepartie 
#' @param CSR_souscription_vie 
#' @param CSR_souscription_nonvie 
#' @param rho : coefficient de correlation entre les sous-risques
#'
#' @return
#' @export
#'
#' @examples
CSR_base = function(CSR_marche, CSR_concentration, CSR_contrepartie, CSR_souscription_vie, CSR_souscription_nonvie, rho){
 CSR = c(CSR_marche, CSR_concentration, CSR_contrepartie, CSR_souscription_vie, CSR_souscription_nonvie)
 return(sqrt(sum(rho_mat * outer(CSR, CSR)))) 
  
}


# CSR_marche
CSR_marche = function(CSR_action, CSR_taux, CSR_change, CSR_immobilier, CSR_ecart_taux, rho_mat){
  CSR = c(CSR_action, CSR_taux, CSR_change, CSR_immobilier, CSR_ecart_taux)
  return(sqrt(sum(rho_mat * outer(CSR, CSR)))) 
  
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
   CSR=c(CSR_cpt_type1, CSR_cpt_type2)
  return(sqrt(sum(rho_mat * outer(CSR,CSR))))
}

#CSR_contrepartie type 1: annexe 1

#CSR_contrepartie type 2:
CSR_cpt_type2 = function(CSR_cpt_assures, CSR_cpt_intermediaires, CSR_cpt_autres_contreparties){
  somme = CSR_cpt_assures + CSR_cpt_intermediaires + CSR_cpt_autres_contreparties
  return(somme)
}

#CSR_concentration:
CSR_concentration = function(CSR){
  return(sqrt(sum(CSR^2)))
}

#CSR_souscription_vie
CSR_souscription_vie = function(CSR_mortalite, CSR_longevite, CSR_rachat, CSR_frais, CSR_catastrophe, rho_mat){
  CSR = c(CSR_mortalite, CSR_longevite, CSR_rachat, CSR_frais, CSR_catastrophe)
  return(sqrt(sum(rho_mat * outer(CSR, CSR)))) 
}

#CSR_mortalite: a voir 
#CSR_longevite:
#CSR_rachat:
#CSR_frais:
#CSR_catastrophe:


#CSR_souscription_non vie
CSR_souscription_nonvie = function(CSR_primes, CSR_provisions, CSR_catastrophe,rho_mat){
  CSR=c(CSR_primes, CSR_provisions, CSR_catastrophe)
  return(sqrt(sum(rho_mat * outer(CSR, CSR))))
}

#CSR_primes:
#CSR_provisions
#CSR_catastrophe

# CSR_operationnel
CSR_operationnel = function(CSR_base, X){
  return(CSR_base * X)
}


#Ajustement du capital de solvabilité requis
#Adj_assures
Adj_assures=function(CSRB_nettes, CSRB_bruttes, BDF){
 return(min(abs(CSRB_nettes - CSRB_bruttes ),BDF)) 
}


#Adj_impotsDifferes
Adj_impotsDifferes = function(TX_impots, CSR_base,CSR_operationnel,Adj_assures,impots_Actifs, impos_Passif){
  if(impos_Passif- impots_Actifs>0) {ecart = impos_Passif- impots_Actifs
  }else {ecart = 0}
 return(TX_impots * min((CSR_base + CSR_operationnel - Adj_assures),ecart))
}













