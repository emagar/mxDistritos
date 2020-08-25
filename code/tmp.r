##############################################
## CREATION OF NEW MUNICIPIOS IN THE PERIOD ##
##############################################
## In order to prevent NAs when estimating expected vote with five previous races, this code block
## splits and aggregates secciones belonging to new/old municipalities. In many cases the municipal
## split went to court. IFE/INE are conservative, only incorporating new units after courts have
## ruled. Others usually do this before courts rule: local authorities (prior to 2016) elected municipal
## authorities, INEGI incorporates them in census/counts. These manipulations will use the less conservative
## criterion (in order to analyze municipal races), splitting federal election returns earlier than IFE/INE
## would.

1. [DONE] in eq, subset children and parents
2. [DONE] merge mun94 into v94, mun97 into v97...
3. [DONE] agg municpios: v94m... (non manip)
4. [DONE] agg municpios: v94manip...
5. consolidate square vmanip matrix 
6. list all secciones needing new mun manip in any year 1994:2018 (so that v..manip all have same nrows)
7. fix/save mun data
8. regress vhat for non-manip and manip data
9. using yrchg, identify 5 obs that need replacement
10. save m regs
   
##############################################
## REPLACE WRONG REGRESSION ESTIMATES       ##
## KEEPING THE TRUE VOTE RETURNS INSTEAD    ##
## OF MANIPULATIONS TO GET ESTIMATES RIGHT  ##
##############################################

