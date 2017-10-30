clear all
set more off

use C:\Users\User\Desktop\300297683\Claudia\Documents\Magar\Seminario\Edomex17.dta 

merge 1:1 seccion using "C:\Users\User\Desktop\300297683\Claudia\Documents\Magar\Seminario\Edomex12.dta" 
keep if _merge == 3
drop _merge

save "C:\Users\User\Desktop\300297683\Claudia\Documents\Magar\Seminario\Edomex17.dta", replace

gen ceros = o
replace ceros = 1 if Distrito17 == 1
tab Distrito12 if ceros == 1


*identificar el viejo distrito que mas secciones le dio al nuevo*
* c = secciones que comparte, n = secciones del nuevo distrito, p = seeciones del distrito padre
* s= c/ p+n-c*

gen unos = 1
bysort Distrito12 dis4015100: egen c = sum(unos)

preserve
duplicates drop Distrito12 dis4015100, force
restore

bysort dis4015100: egen max = max(c)

bysort dis4015100: egen nuevo = count(seccion)
bysort Distrito12: egen padre = count(seccion)

gen padre1 = .
replace padre1 = 130 if dis4015100 == 1
replace padre1 = 91 if dis4015100 == 2
replace padre1 = 139 if dis4015100 == 3
replace padre1 = 75 if dis4015100 == 4
replace padre1 = 104 if dis4015100 == 5
replace padre1 = 92 if dis4015100 == 6
replace padre1 = 105 if dis4015100 == 7
replace padre1 = 95 if dis4015100 == 8
replace padre1 = 81 if dis4015100 == 9
replace padre1 = 81 if dis4015100 == 10
replace padre1 = 90 if dis4015100 == 11
replace padre1 = 296 if dis4015100 == 12
replace padre1 = 210 if dis4015100 == 13
replace padre1 = 210 if dis4015100 == 14
replace padre1 = 135 if dis4015100 == 15
replace padre1 = 122 if dis4015100 == 16
replace padre1 = 254 if dis4015100 == 17
replace padre1 = 296 if dis4015100 == 18
replace padre1 = 254 if dis4015100 == 19
replace padre1 = 133 if dis4015100 == 20
replace padre1 = 135 if dis4015100 == 21
replace padre1 = 118 if dis4015100 == 22
replace padre1 = 184 if dis4015100 == 23
replace padre1 = 194 if dis4015100 == 24
replace padre1 = 279 if dis4015100 == 25
replace padre1 = 184 if dis4015100 == 26
replace padre1 = 188 if dis4015100 == 27
replace padre1 = 227 if dis4015100 == 28
replace padre1 = 279 if dis4015100 == 29
replace padre1 = 289 if dis4015100 == 30
replace padre1 = 98 if dis4015100 == 31
replace padre1 = 289 if dis4015100 == 32
replace padre1 = 114 if dis4015100 == 33
replace padre1 = 98 if dis4015100 == 34
replace padre1 = 192 if dis4015100 == 35
replace padre1 = 242 if dis4015100 == 36
replace padre1 = 242 if dis4015100 == 37
replace padre1 = 187 if dis4015100 == 38
replace padre1 = 244 if dis4015100 == 39
replace padre1 = 167 if dis4015100 == 40
replace padre1 = 187 if dis4015100 == 41
replace padre1 = 115 if dis4015100 == 42
replace padre1 = 146 if dis4015100 == 43
replace padre1 = 146 if dis4015100 == 44
replace padre1 = 104 if dis4015100 == 45

gen aux1 = padre1 + nuevo - max
gen s = max/aux1

