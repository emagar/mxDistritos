version 8.0

cd "D:\01\Dropbox\data\elecs\redistrict\planesEdomex\"
*cd "C:\Users\emagarm\Documents\Dropbox\data\elecs\redistrict\planesEdomex\"
*cd "C:\01\"
*cd "/home/eric/Dropbox/data/elecs/redistrict/planesEdomex/"

** IMPORTA LOS DATOS O ABRE LOS GUARDADOS, SEGUN SEA EL CASO
*insheet using "pedro.esquivel.v1.csv", clear comma
insheet using "emmPobOk.csv", clear comma
*use "ericMagarPobOk.dta", clear
*** ELIMINA VARIABLES DUPLICADAS O INUTILES
*drop munn clave

gen seccion=v1-int(v1/10000)*10000
*drop v1
rename v2 newdisn
rename v3 dcontig

** ORDENA DATOS POR SECCION
sort seccion

** IMPORTA DATOS CENSALES 2010 SECCIONALES
merge seccion using "censales2010edomex.dta"
tab _merge
drop _merge

sort seccion

** FUSIONA CON LOS DATOS ELECTORALES DE 2012 SECCION POR SECCION
merge seccion using "mexDfSeccion2012.dta"
tab _merge
drop _merge

rename disn olddisn

** RECOMPONE LOS DATOS DE COALICIONES
gen pric =  pri+ pvem+ pripvem
move pric pri
drop pri pvem pripvem

gen prdc =  prd + pt + mc + prdptmc + prdpt + prdmc + ptmc
move prdc prd
*drop prd pt mc prdptmc prdpt prdmc ptmc

gen efec=pan+pric+prdc+panal
move efec nr

** AGREGA DATOS ELECTORALES POR DISTRITO ELECTORAL
sort olddisn
by olddisn: egen panold=sum(pan)
by olddisn: egen pricold=sum(pric)
by olddisn: egen prdcold=sum(prdc)
by olddisn: egen panalold=sum(panal)
by olddisn: egen efecold=sum(efec)
by olddisn: egen ptotold=sum(pob_tot)
gen tmp=0
replace tmp=1 if olddisn==olddisn[_n-1]
*replace panold=. if tmp==1
*replace pricold=. if tmp==1
*replace prdcold=. if tmp==1
*replace panalold=. if tmp==1
*replace efecold=. if tmp==1
rename tmp dropold

sort newdisn
by newdisn: egen pannew=sum(pan)
by newdisn: egen pricnew=sum(pric)
by newdisn: egen prdcnew=sum(prdc)
by newdisn: egen panalnew=sum(panal)
by newdisn: egen efecnew=sum(efec)
by newdisn: egen ptotnew=sum(pob_tot)
gen tmp=0
replace tmp=1 if newdisn==newdisn[_n-1]
*replace pannew=. if tmp==1
*replace pricnew=. if tmp==1
*replace prdcnew=. if tmp==1
*replace panalnew=. if tmp==1
*replace efecnew=. if tmp==1
rename tmp dropnew


* EXPORTA DATOS AGREGADOS
preserve
drop if dropold==1
drop pan pric prdc panal efec pannew pricnew prdcnew panalnew efecnew dropnew
rename olddisn disn
rename panold pan
rename pricold pric
rename prdcold prdc
rename panalold panal
rename efecold efec
rename ptotold ptot
drop if disn==.
sort disn
outsheet disn pan pric prdc panal efec ptot using "old.csv", comma replace
restore

preserve
drop if dropnew==1
drop pan pric prdc panal efec
rename newdisn disn
rename pannew pan
rename pricnew pric
rename prdcnew prdc
rename panalnew panal
rename efecnew efec
rename ptotnew ptot
sort disn
*outsheet disn pan pric prdc panal efec ptot using "new.magar.csv", comma replace
outsheet disn pan pric prdc panal efec ptot using "new.esquivel.csv", comma replace
restore

