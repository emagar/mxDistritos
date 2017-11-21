########################################################################
# script with functions to get 2005 and 2010 seccion-level populations #
########################################################################
#
#########################################################
#########################################################
## should edit pop objects to fix changed secciones... ##
#########################################################
#########################################################

# function to get conteo 2005 pop (or other info) --- adapted from red.r
get2005 <- function(edon=NA){
    edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
    fls <- c("01_ags_pob.csv", "02_bc_pob.csv", "03_bcs_pob.csv", "04_camp_pob.csv", "05_coah_pob.csv", "06_col_pob.csv", "07_chiap_pob.csv", "08_chih_pob.csv", "09_df_pob.csv", "10_dgo_pob.csv", "11_gto_pob.csv", "12_gro_pob.csv", "13_hgo_pob.csv", "14_jal_pob.csv", "_5_mex_pob.csv", "16_mich_pob.csv", "17_mor_pob.csv", "18_nay_pob.csv", "19_nl_pob.csv", "20_oax_pob.csv", "21_pue_pob.csv", "22_qro_pob.csv", "23_qroo_pob.csv", "24_slp_pob.csv", "_5_sin_pob.csv", "26_son_pob.csv", "27_tab_pob.csv", "28_tamp_pob.csv", "29_tlax_pob.csv", "30_ver_pob.csv", "31_yuc_pob.csv", "_2_zac_pob.csv")
    edo <- edos[edon]
    fl <- fls[edon]
    message(paste("Getting seccion-level 2005 pop. for", edo))
    c05 <- c("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/conteo2005")
    tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE, stringsAsFactors = FALSE)
    pob05 <- data.frame(edon    =tmp[,grep("ENTIDAD", colnames(tmp))],  
                        disn2006=tmp[,grep("DISTRITO", colnames(tmp))], 
                        munn    =tmp[,grep("MUNICIPIO", colnames(tmp))],
                        seccion =tmp[,grep("SECCION", colnames(tmp))],  
                        ptot    =tmp[,grep("POB_TOT", colnames(tmp))])
    pob05 <- pob05[order(pob05$seccion),]
    return(pob05)
}    

## function to get 2010 pop --- adapted from red.r
get2010 <- function(edon=NA){
    edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
    fls <- c("secciones_01.csv", "secciones_02.csv", "secciones_03.csv", "secciones_04.csv", "secciones_05.csv", "secciones_06.csv", "secciones_07.csv", "secciones_08.csv", "secciones_09.csv", "secciones_10.csv", "secciones_11.csv", "secciones_12.csv", "secciones_13.csv", "secciones_14.csv", "secciones_15.csv", "secciones_16.csv", "secciones_17.csv", "secciones_18.csv", "secciones_19.csv", "secciones_20.csv", "secciones_21.csv", "secciones_22.csv", "secciones_23.csv", "secciones_24.csv", "secciones_25.csv", "secciones_26.csv", "secciones_27.csv", "secciones_28.csv", "secciones_29.csv", "secciones_30.csv", "secciones_31.csv", "secciones_32.csv")
    edo <- edos[edon]
    fl <- fls[edon]
    message(paste("Getting seccion-level 2010 pop. for", edo))
    c10 <- c("~/Dropbox/data/mapas/seccionesIfe")
    tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE, stringsAsFactors = FALSE)
    pob10 <- data.frame(edon=tmp[,c("ENTIDAD")],
                        disn=tmp[,c("DISTRITO")],
                        seccion=tmp[,c("CLAVEGEO")],
                        ptot=tmp[,c("POBTOT")],
                        p18=tmp[,c("P_18YMAS")])
    pob10$munn <- as.integer(pob10$seccion/100000) - as.integer(pob10$seccion/100000000)*1000
    pob10$seccion <- pob10$seccion - as.integer(pob10$seccion/10000)*10000;
    if (edon==7) pob10 <- pob10[which(duplicated(pob10$seccion)==FALSE),]; # por alguna razón los datos de chiapas vienen duplicados
    pob10 <- pob10[order(pob10$seccion),]
    return(pob10)
}

## ## poblacion del censo 2000
## c00 <- c("/home/eric/Dropbox/data/mapas/seccionesIfe/2000popThatIFEused/secciones.dat.ife.csv")
## tmp <- read.csv(c00, stringsAsFactors = FALSE)
## pob00 <- data.frame(edon=tmp[,c("ENT_CLAVE")],
##                     seccion=tmp[,c("SEC_CLAVE")],
##                     ptot=tmp[,c("SEC_POBLACION")])
## rm(tmp)

## # fusiona pob05 y pob10
## pob <- pob10; colnames(pob) <- c("edon", "disn10", "seccion",  "ptot10", "p1810", "munn")
## #summary(pob$seccion)
## #pob$tmp <- pob$edon*10000 + pob$seccion
## #summary(pob05$seccion); colnames(pob05)
## #pob05$tmp <- pob05$edon*10000 + pob05$seccion;
## #tmp <- pob05[,c("tmp", "ptot", "seccion")]; colnames(tmp) <- c("tmp","ptot05", "seccion05")
## tmp <- pob05[, c("edon","seccion","disn","ptot")]; colnames(tmp) <- c("edon", "seccion", "disn05", "ptot05")
## pob <- merge(x = pob, y = tmp, by = c("edon","seccion"), all = TRUE)
## dim(pob05); dim(pob10); dim(pob); # debug
## pob <- pob[, c("edon","seccion","disn05","disn10","ptot05","ptot10")] # drops munn and p1810
## #
## # merge pob2000 into pop object
## colnames(pob00) <- c("edon","seccion","ptot00")
## pob <- merge(x = pob, y = pob00, by = c("edon","seccion"), all = TRUE)
## pob <- pob[, c("edon","seccion","disn05","disn10","ptot00","ptot05","ptot10")] # drops munn and p1810
## drop <- which(pob$edon==0); pob <- pob[-drop,]; rm(drop) # drops state aggregates that came with pob00 
## #
## # sections that are absent from one or the anoother population file
## pob$dcheck <- 0
## pob$dcheck[which(is.na(pob$ptot00)==TRUE | is.na(pob$ptot05)==TRUE | is.na(pob$ptot10)==TRUE)] <- 1
## table(pob$dcheck)
## pob[is.na(pob)==TRUE] <- 0 # cambia los NAs por ceros para las sumas
## # Las secciones cambiadas involucran a millones de habitantes
## sum(pob$ptot00[pob$dcheck==1]); sum(pob$ptot05[pob$dcheck==1]); sum(pob$ptot10[pob$dcheck==1])
## ## # list them by state
## ## for(i in 1:32){
## ##     print(paste("edon =", i));
## ##     print(pob$seccion[pob$edon==i & pob$dcheck==1]);
## ## }
## #
## # pob$disn05 - pob$disn10 # suggests pob05 reports 1997 districts and pob10 reports 2005 districts
