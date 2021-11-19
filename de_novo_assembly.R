# cviceni 7, 19.11.2021
#################################################
#  DE NOVO ASSEMBLY  #
#################################################

# znaková metoda SSP - Shortest Supersting Problem
# greedy algoritmus, takze jdu prvek po prvku

# Hlavni funkce se bude skladat z 3 dilcich fci:
#       - vypocet prekryvu Prekryv()
#       - vytvoreni matice MaticePrekryvu()
#       - spojeni sekvenci Spojeni()

################################################################################
# sek - soubor readu, ktere skladame do contigu
sek = c('CATGC', 'CTAAGT', 'GCTA', 'TTCA', 'ATGCATC')
sek1 <- sek[4]
sek2 <- sek[5]

################################################################################
Prekryv = function(sek1,sek2){
  # musim vyresit, kdyz jsou delky ready rozdilne o vic nez 1
  sek1 = strsplit(sek1,"")[[1]]
  sek2 = strsplit(sek2,"")[[1]]
  
  if (length(sek2) > length(sek1)){ 
    pom <- sek1; sek1 <- sek2; sek2 <- pom
  }
  L1 <- length(sek1)      # delka delsiho readu
  L2 <- length(sek2)      # delka kratsiho readu
  Lpre1 <- 0
  Lpre2 <- 0
  
  for (i in L2:1) {   # prekryv z leva
    if(isTRUE(sek1[1:i] == sek2[(L2+1-i):L2])){
      prekryv1 <- sek1[1:i]           # prekryvajici usek
      Lpre1 <- length(prekryv1)       # delka useku
      break
    }
  }
  for (j in 1:L2) {   # prekryv z prava
    if(isTRUE(sek1[(L1-L2+j):L1] == sek2[1:(L2+1-j)])){
      prekryv2 <- sek1[(L1-L2+j):L1]  # prekryvajici usek
      Lpre2 <- length(prekryv2)       # delka useku
      break
    }
  }
  if (Lpre1 == 0 && Lpre2 == 0){
    return('-')
  } else if (Lpre2 >= Lpre1){
    return(prekryv2)
  } else{
    return(prekryv1)}
}


################################################################################
MaticePrekryvu <- function(sek){
  pocet <- length(sek)
  matice <- matrix(data = NA, nrow = (pocet-1), ncol = (pocet-1))
  polovina <- 1
  # vyplneni matice
  for (i in 1:(pocet-1)) {      # radky matice
    for (j in polovina:(pocet-1)) {    # sloupce matice
      matice[i,j] <- Prekryv(sek[i],sek[j+1])
    }
    polovina <- polovina + 1
  }
  return(matice)
}
  

################################################################################
Spojeni <- function(prekryv, sek1, sek2){
  
  
}


################################################################################
SuperString <- function(sek){
  while (sek > 1) {
    matice <- MaticePrekryvu(sek)
    # najit nejdelsi prvek v matici which.max()
    prvek <- max(max(matice))
    # urcit, ktere ready byly vybrane
    
    # vytvoreni noveho S vektoru = pradani spojeni a smazani 2 readu
  }

}

