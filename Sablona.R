# cviceni 7, 18.11.2021
#################################################
#  DE NOVO ASSEMBLY  #
#################################################

# znaková metoda SSP - Shortest Supersting Problem
# greedy algoritmus, takze jdu prvek po prvku


SuperString = function(sek){
  # sek - soubor readu, ktere skladame do contigu
  sek = c('CATGC', 'CTAAGT', 'GCTA', 'TTCA', 'ATGCATC')
  while (sek > 1) {
    mat <- MaticePrekryvu(sek)
    # najit nejdelsi prvek v matici
    # urcit, ktere ready byly vybrane
    spojeni <- Prekryv(sek1,sek2)
    # vytvoreni noveho S vektoru = pradani spojeni a smazani 2 readu
  }

}


################################################################################
Prekryv = function(sek1,sek2){
  # 1 for cyklus

  return(prekryv)
}


################################################################################
MaticePrekryvu = function(sek){
  # 2 for cykly
  pocet <- length(sek)
  matice <- matrix(data = NA, nrow = (pocet-1), ncol = (pocet-1))
  # vyplneni matice
  for (i in 1:(pocet-1)) {      # radky matice
    for (j in 1:(pocet-1)) {    # sloupce matice
      # pokud je sek2 delsi vymeni se s sek1
      if (length(sek[j+1]) > length(sek[i])){ 
        pom <- sek[i]; sek[i] <- sek[j+1]; sek[j+1] <- pom
      }
      s1 <- sek[i]              # sekvence 1 = read 1
      s2 <- sek[j+1]            # sekvence 2 = read 2
      L1 <- length(sek[i])      # delka - vybere jeden read
      L2 <- length(sek[j+1])    # delka - vybere dalsi read
      
    }
    
  }
  
    
  
  return(matice)
}


