
non.nas <- function(x=extendCoals){
    nn <- lapply(x, sum)
    ##nn <- lapply(extendCoals, sum)
    nn <- unlist(nn)
    ##table(is.na(nn))
    ##nn                     # debug
    ##extendCoals[[206]]     # debug: 20jul2021 NA due to unreported sole sección in cps municipio
    ##which(is.na(nn)==TRUE) # debug
    nn <- which(is.na(nn)==FALSE)
    ##length(nn)
    return(nn)
}






SPLITS

###########
##   1   ##
###########

###########
## 6 # 7 ##
###########

ahora hago split.to en 2006: a sección 1 le pongo 6+7 de 2009 en adelante (que no me sirve para autorregresión fwd de sección 1); serviría para autorregresión backward de sección 1

necesito manipular split.from en 2006: que a secciones 6 y 7 les ponga votos de sección 1 en años < 2006




MERGE

###########
## 2 # 3 ##
###########

###########
##   3   ##
###########

no hay que hacerle nada para regresiones




