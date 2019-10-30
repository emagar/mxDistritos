
# Table of Contents

1.  [Description of *Recent Mexican electoral geography* repository](#orgd45d6b1)
2.  [Files in the repository and how to cite them](#org9c4e935)
    1.  [Measures of recent party performance for use in maps](#org7f6f4a9)
    2.  [Redistricting and *reseccionamiento*](#orgca854e1)
    3.  [Comparative maps (shapefiles etc.)](#org62ac647)
    4.  [Descriptive plots and literature](#org40df9f0)
3.  [Variables in the datasets ](#orga6031fd)
    1.  [Observation identifiers](#org6e35dee)
    2.  [Vote returns and party performance](#org2e4056c)
    3.  [Redistricting and *reseccionamiento*](#org5d49417)
4.  [Note on coalitions ](#org5aff893)
5.  [Acknowledgements](#orge9b553b)
6.  [below are elements to copy/emulate in this readme file](#orge37d198)

Last revision: 2019-10-30

**>>> Under construction (expected completion end of Oct. 2019) <<<**


<a id="orgd45d6b1"></a>

# Description of *Recent Mexican electoral geography* repository

-   Author: Eric Magar
-   Email: emagar at itam dot mx

The repository contains maps of Mexican districts used to elect representatives to various offices and code for data systematization and analysis. The primary source are shapefiles publicly distributed by INE (formerly IFE, Mexico's national election board, page [here](https://cartografia.ife.org.mx/sige7/?cartografia)). Data in this repo is prepared for mapping and preliminary/basic statistical analysis.


<a id="org9c4e935"></a>

# Files in the repository and how to cite them

You are free to download and modify the data (see the LICENSE document) provided you give proper credit to this source. Unless otherwise noted next to the file descriptor, the cite is Eric Magar (2019) Recent Mexican electoral geography repository, <https://github.com/emagar/mxDistritos>.


<a id="org7f6f4a9"></a>

## Measures of recent party performance for use in maps

Election data at two geographic levels were used: the units of observation in this set of files are municipalities (files with `mu` in name) or secciones electorales (files with `se` in name). Quantities of interest are available for 2009, 2012, 2015, and 2018.

-   `code/elec-data-for-maps.r` <a id="org33aad5a"></a> = code manipulates polling place vote returns in federal deputy elections 1994&#x2013;2018 for use in maps. Data are aggregated up to the municipal- and sección-levels for analysis.
-   `code/resecc-deal-with-splits.r` = code to re-aggregate split oversized secciones in order to preserve time-series in the analysis (invoked within `elec-data-for-maps.r`).
-   `code/triplots-etc.r` = code to plot recent party performance quantities (plots saved in `graph/` folder).
-   `data/dipfed*-vhat.csv` files = electoral statistics calculated from federal diputado single-member district elections. Measures of interest are (a) the parties' vote shares in the unit-year; (b) the change in vote share in the unit-year since last election; (c) the predicted vote share for the unit-year out of each party's performance in the unit in the previous five federal diputado elections; (d) estimates of each party's core support in the unit for the whole period; (e) the frequency each party won the vote plurality/majority in the unit in the period (`code/elec-data-for-maps.r` was used to prepare these files).
-   `data/dipfed-mu-regs-*.RData` files = summary statistics of yearly federal deputy municipal-level regressions in [R](https://www.r-project.org/) format. (Regression coefficient estimates were used to predict vote shares reported in `vhat` files.) Reading each file into an R environment imports a list with three objects named `pan`, `morena`, and `oth`. Each object is a sub-list with ![img](./graph/readme-math/about-2500.svg) class 'lm' objects, one for each regression fitted. [This post in Spanish](https://emagar.github.io/residuales-2018/) elaborates the method.
-   `data/dipfed-mu-mean-regs.RData` = summary statistics of 1994&#x2013;2018 federal deputy municipal-level regressions in [R](https://www.r-project.org/) format. (Regression coefficient estimates were used to estimate party core support in each municipality reported in `vhat` files.) Reading each file into an R environment imports a list with three objects named `pan`, `morena`, and `oth`. Each object is a sub-list with ![img](./graph/readme-math/about-2500.svg) class 'lm' objects, one for each regression fitted. [This post in Spanish](https://emagar.github.io/residuales-2018/) elaborates the method.
-   (`data/dipfed-se-regs-*.RData` and `data/dipfed-se-mean-regs.RData` files = sección-level regression summary statistics **not included** in repository due to large size (![img](./graph/readme-math/about-66k.svg) regressions per party-year). Contact me and I will send the files.)
-   Variable description [here](#org66dece1).


<a id="orgca854e1"></a>

## Redistricting and *reseccionamiento*

-   `equivSecc/docsRedistReseccRemunic/` = folder contains numerous documents prepared by INE/IFE explaining redistricting criteria, changes in sección delimitations (*reseccionamiento*), and changes in a state's municipalities.
    -   **Citation for these documents**: [Instituto Nacional Electoral (various years)](https://ine.mx).
-   `equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv` and `.xlsx` = historical record of *secciones electorales* nationwide since 1994 in comma-separated and excel format, respectively. Secciones, which do not traverse municipal borders, are the basic building blocks for districting at both the federal and state levels (see Magar et al. 2017, fn. 9). Each row reports one sección (approx. 69,000 total) and the district it belonged to in four federal congressional district maps (maps inaugurated in 1979, 1997, 2006, and 2018). It also reports the district it would have belonged to in the 2013 map that was rejected prior to adoption. A small but important number of secciones suffered changes through time (the official term is *reseccionamiento*)&#x2014;most frequently due to under- or over-population, but also after court rulings modifying state or municipal borders; see the \`OBSERVACIONES\` and its right-adjacent variables). For this reason, the dataset maps secciones-to-districts at each federal election since 1994.  
    -   This dataset consolidates and extends excel sheets that IFE/INE distributes periodically.
    -   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.


<a id="org62ac647"></a>

## Comparative maps (shapefiles etc.)

-   `mapasComparados/` =
-   `redisProcess/` =


<a id="org40df9f0"></a>

## Descriptive plots and literature

-   `graph/` = folder with descriptive plots of some of the measures distributed here.
-   `graph/readme-math` = images used to render math equations across this `README.md` file. Ignore them.
-   `lit/` = folder with relevant literature.


<a id="orga6031fd"></a>

# Variables in the datasets <a id="org66dece1"></a>

Variables are not necessarily included in every dataset distributed.


<a id="org6e35dee"></a>

## Observation identifiers

-   \`ord\` = observation counter.
-   \`edon\` = state number 1:32.
-   \`edo\` = state abbreviation (may differ from official abbreviations so that sorting them alphabetically preserves the order set by *edon*).
-   \`seccion\` = sección identifier, starts at 1 in each state.
-   \`edosecn\` = string identitying \`edon\` and \`seccion\` period separated; distinguishes units with same \`seccion\` value across states.
-   \`inegi\` = municipality identifier used by INEGI (census bureau).
-   \`ife\` = municipality identifier used by IFE/INE (election board).
-   \`mun\` = municipality's name.


<a id="org2e4056c"></a>

## Vote returns and party performance

-   \`pan\` = vote share won by the PAN and allies, see the [note on parties and coalitions](#orgfe9497c).
-   \`pri\` = vote share won by the PRI and allies, see the [note on parties and coalitions](#orgfe9497c).
-   \`morena\` = vote share won by the left and allies, see [note on parties and coalitions](#orgfe9497c).
-   (\`oth\` = vote share of candidates fielded by minor parties is not reported. It is 1 &#x2013; pan &#x2013; pri &#x2013; morena and therefore fully - \`d.pan\`, \`d.pri\`, \`d.morena\` = party's vote share change since last election (first differences, i.e., pan<sub>yr</sub> &#x2013; pan<sub>yr-3</sub> and so forth).
-   \`vhat.pan\`, \`vhat.pri\`, \`vhat.morena\` = vote share predicted for the current year (\`yr\`) from a linear estimation of the party's performance in five immediately previous elections in the unit. Letting v<sub>i,t</sub> denote party i's vote share in year t, the equation fitted with OLS in each unit looks thus: ![img](./graph/readme-math/5-yr.svg). (A compositional variable specifiction was used, so the actual equation is slightly different, see [this post in Spanish](https://emagar.github.io/residuales-2018/) for details.) The variable reports ![img](./graph/readme-math/vhat.svg), the point prediction for the current year.
-   \`bhat.pan\` and \`bhat.morena\` variables = point estimates of the slope coefficient from the regression described in the bullet above. (There is no estimate for the PRI, see [this post in Spanish](https://emagar.github.io/residuales-2018/).)
-   \`alphahat.pan\`, \`alphahat.pri\`, \`alphahat.mprena\` = party's core support group estimate for the unit in 2000&#x2013;2018 federal diputado elections. See [this post in Spanish](https://emagar.github.io/residuales-2018/) for estimation details.
-   \`betahat.pan\`, \`betahat.morena\` = party's volatility to national swings estimate for the unit in 2000&#x2013;2018 federal diputado elections. (There is no estimate for the PRI, see [this post in Spanish](https://emagar.github.io/residuales-2018/).)

determined.)


<a id="org5d49417"></a>

## Redistricting and *reseccionamiento*

-   \`split\` = equals 0 for secciones that remained unchanged in the period, otherwise indicates the year sección was split into smaller units due to oversize. Estimation of some of the quantities reported involved re-aggregating new units into their oversized parent sección in order to preserve the full vote returns time series. See [code](#org33aad5a) for details.
-   \`new\` = equals 0 for secciones that remained unchanged in the period, otherwise indicates the year sección was created by splitting an oversized sección into smaller units. Estimation of some of the quantities reported involved re-aggregating these smaller units into their oversized parent in order to preserve the full vote returns time series. See [code](#org33aad5a) for details.
-   \`dis1994\` = federal district of the 1976 map that sección belonged to at the 1994 election.
-   \`dis1997\` = federal district of the 1997 map that sección belonged to at the 1997 election.
-   \`dis2000\` = federal district of the 1997 map that sección belonged to at the 2000 election.
-   \`dis2003\` = federal district of the 1997 map that sección belonged to at the 2003 election.
-   \`dis2006\` = federal district of the 2006 map that sección belonged to at the 2006 election.
-   \`dis2009\` = federal district of the 2006 map that sección belonged to at the 2009 election.
-   \`dis2012\` = federal district of the 2006 map that sección belonged to at the 2012 election.
-   \`dis2013\` = federal district of the 2013 map that sección belonged to; 2013 map was never adopted.
-   \`dis2015\` = federal district of the 2006 map that sección belonged to at the 2015 election.
-   \`dis2018\` = federal district of the 2018 map that sección belonged to at the 2018 election.
-   \`OBSERVACIONES\` = character string describing changes that a sección may have suffered through time in the source;
-   \`action\` = character string indicates change that sección may have suffered: *merged* if it was integrated into a neighboring sección due to under-population; *split* if it was subdivided into two or more new secciones due to over-population; *new* if it was created from a split sección; *munic* if it arose to accommodate a change in intermunicipal border lines; *stateChg* if it arose to accommodate a change in interstate border lines.
-   \`fr.to\` = character string indicates whether sección arose *from* another sección (eg. after latter was split) or was incorported *to* another sección (eg. latter absorbed it when both were merged).
-   \`orig.dest\` = when sección was subdivided, indicates sección number(s) that arose; when sección was merged, indicates which one absorbed it.
-   \`when\` = year the change took place.
-   \`color\` = character string indicated the cell color in the original excel sheet (secciones that suffered contemporaneous changes shared the same color).
-   \`coment\` = character string with comments (in mostly Spanish).


<a id="org5aff893"></a>

# Note on coalitions <a id="orgfe9497c"></a>

Electoral alliances are extended nationwide and, in some cases, to other years for convenience in the analysis. Details for each party follow.

-   Partido Acción Nacional (PAN) fielded candidates jointly with the Green party (PVEM) nationwide in 2000 and with the PRD and minor MC in select districts in 2018. The 2018 coalition was extended nationwide (i.e. PAN + PRD + MC votes added) for analysis. PAN did not ally in any other year in the period.
-   Partido Revolucionario Institucional (PRI) never allied before 2003. It then fielded joint candidates with the PVEM nationwide (in 2006) or in select districts (in 2003, 2009, 2012, and 2015). In 2018 it fielded joint candidates with the PVEM and the PNA in select districts. Alliances were extended nationwide for analysis.
-   The left is generically called by its latest incarnation's name MORENA, the Movimiento de Regeneración Nacional (which is a splinter from PRD, the Partido de la Revolución Democrática). The left's vote up to 1997 is the PRD's, which ran solo. In 2000 the PRD fielded joint candidates nationwide with Partido del Trabajo (PT), Movimiento Ciudadano (MC, then called Convergencia), and two now-extinct minor parties. From 2003 to 2012 the left is the sum of PRD, PT, and MC (an artificial sum in 2003 and 2009 for comparability). In 2015 the left is the sum of votes for the PRD, PT, MORENA, and PES (only the first two fielded joint candidates in select districts). In 2018 the left is the sum of MORENA, PT, and PES which fielded joint candidates in most districts.
-   A residual "others" category sums the votes for parties other than those listed above for analysis. In 1991 the parties whose votes are summed are PARM, PDM, PFCRN, PPS, PEM, and PRT; in 1994 PPS, PFCRN, PARM, UNO-PDM, PT, and PVEM; in 1997 PC, PT, PVEM, PPS, and PDM; in 2000 PCD, PARM and DS; in 2003 PSN, PAS, MP, PLM, and FC; in 2006 PNA and ASDC; in 2009 PNA and PSD; in 2012 PNA only; in 2015 MC, PNA, PH, and a handful of independent candidates; and in 2018 a handful of independent candidates.  
    -   `data/dipfed2015mu-vhat.csv` = 2015 federal diputado single-member district election statistics. The units are municipalities. Data are municipal aggregates of sección-level returns (i.e. votes from all secciones belonging to a given municipality are added up). The PRI fielded joint candidates with the Green party in some states only, as did the PRD with the PT; both coalitions are extended nationwide for convenience in the analysis (`code/elec-data-for-maps.r` was used to prepare this file).
    -   Variables in the dataset:<a id="org6c02083"></a>


<a id="orge9b553b"></a>

# Acknowledgements

I acknowledge financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. Files distributed here systematize data from the [Instituto Nacional Electoral](https://ine.mx), I am sincerely grateful for their excellent work producing and distributing election results and associated metadata. I am responsible for mistakes and shortcomings in the data. 


<a id="orge37d198"></a>

# below are elements to copy/emulate in this readme file

-   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.
-   **Citation for this dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.

-   `data/prdf2006-on.csv`
-   <del>`datosBrutos/` = large directory containing primary sources</del> (dropped from repo due to large size&#x2026; [mail me](mailto:emagar@itam.mx) if you need this).

