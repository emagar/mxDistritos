- [Description of *Recent Mexican electoral geography* repository](#org7f5d16f)
- [Recent changes](#orgdb14357)
- [Files in the repository and how to cite them](#orga3aaaa6)
  - [Measures of recent party performance for use in maps](#org907b90e)
    - [Code](#org2b41ed6)
    - [Data](#org003aba3)
  - [Redistricting and *reseccionamiento*](#org887a67d)
  - [Comparative maps and shapefiles](#org03e8cd2)
  - [Descriptive plots and literature](#org153f28c)
- [Variables in the datasets <a id="org92f3af6"></a>](#org31936eb)
  - [Observation identifiers](#org7b238a4)
  - [Vote returns and party performance (in `vhat` files)](#org10ae681)
  - [Vote returns (in `vraw` files)](#orga930097)
  - [Redistricting and *reseccionamiento* <a id="orgec13380"></a>](#orgba8187e)
- [Note on electoral coalitions <a id="org2006603"></a>](#org197a914)
- [Acknowledgements](#orga4eb57c)

Last revision: 2024-10-17


<a id="org7f5d16f"></a>

# Description of *Recent Mexican electoral geography* repository

-   Author: Eric Magar
-   Location <https://github.com/emagar/mxDistritos>
-   Email: emagar at itam dot mx
-   Citation for the data: see 'About' on the repository landing page

The repository contains maps of Mexican districts used to elect representatives to various offices and code for data systematization and analysis. The primary source are shapefiles publicly distributed by INE (formerly IFE, Mexico's national election board, page [here](https://cartografia.ife.org.mx/sige7/?cartografia)). Data in this repo is prepared for mapping and statistical analysis.


<a id="orgdb14357"></a>

# Recent changes

-   2024-10-17: Reseccionamiento 2023 is in! Hundreds of new secciones created from splitting overcrowded parents [here](./equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv).
-   2024-05-08: Shapefiles for 2024 federal district map produced [here](./mapasComparados/fed/shp/disfed2024/).
-   2024-02-15: New 2024 federal district map now in. The seccion&#x2013;district relation is [here](./equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv).
-   2023-05-26: All code and files relate to v.hat and alpha regressions migrated away from this repository. They now inhabit <https://github.com/emagar/elecRetrns>.

-   2023-02-15: Appended new secciones electorales created since 2020 in files below `equivSecc`. Corresponding shapefiles still pending.


<a id="orga3aaaa6"></a>

# Files in the repository and how to cite them

You are free to download and modify the data (see the LICENSE document) provided you give proper credit to this source. Unless otherwise noted below the file descriptor, the cite is Eric Magar (2019) Recent Mexican electoral geography repository, <https://github.com/emagar/mxDistritos>.


<a id="org907b90e"></a>

## Measures of recent party performance for use in maps


<a id="org2b41ed6"></a>

### Code

-   `code/elec-data-for-maps.r` <a id="org54182e3"></a> = code manipulates polling place vote returns in federal deputy elections 1994&#x2013;2018 for use in maps. Data are aggregated up to the municipal- and sección-levels for analysis.
-   `code/get-winners.r` = code with sub-routine to produce unit winners (invoked within `elec-data-for-maps.r`).
-   `code/resecc-deal-with-splits.r` = code to re-aggregate split oversized secciones in order to preserve time-series in the analysis (invoked within `elec-data-for-maps.r`).
-   `code/triplots-etc.r` = code to plot recent party performance quantities (plots saved in `graph/` folder).


<a id="org003aba3"></a>

### Data

Measures of federal deputy elections at two geographic levels are distributed: municipalities (files with `municipio` in name) and secciones electorales (files with `seccion` in name). Quantities of interest are available for 2006, 2009, 2012, 2015, and 2018. Variable descriptions [here](#org92f3af6).

-   `data/*-vhat-*.csv` files = electoral statistics calculated from federal diputado single-member district elections 2006&#x2013;2018. Measures of interest are (a) the parties' **vote shares** in the unit-year; (b) the **change** in vote share in the unit-year since last election; (c) the **predicted vote share** for the unit-year out of each party's performance in the unit in the previous five federal diputado elections; (d) estimates of each **party's core support** in the unit for the whole period; (e) estimates of each **party's beta volatility** in the unit for the whole period (`code/elec-data-for-maps.r` was used to prepare these files; 2006 unavailable at sección level due to data missing comparable sección IDs for 1991).
-   `data/*-win.csv` files = more measures of interest: (a) unit-year **winners** in the period (i.e., party with most votes); (b) **margins** of victory in the unit-year (i.e., winner's vote share minus runner-up's vote share); (c) the **frequency of party victories** in the unit between 1994 and 2018 (`code/get-winners.r` was used to prepare these files).
-   `data/*-vraw-*.csv` files = unmanipulated (raw) votes received by parties in federal diputado single-member district elections 1991&#x2013;2018. Unlike `vhat` files, which simplify the field of parties by adding up the votes of minor parties, this file reports them individually when they did not coalesce with a major party (`code/elec-data-for-maps.r` was used to prepare these files; 1991 unavailable at sección level due to data missing comparable sección IDs).
-   `data/*-municipio-regs-*.RData` files = summary statistics of yearly federal deputy municipal-level regressions in [R](https://www.r-project.org/) format. (Regression coefficient estimates were used to predict vote shares reported in `vhat` files.) Reading each file into an R environment imports a list with three objects named `pan`, `left`, and `oth`. Each object is a sub-list with ![img](./graph/readme-math/about-2500.svg) class 'lm' objects, one for each regression fitted. [This post](https://emagar.github.io/residuales-2018-english/) elaborates the method.
-   `data/*-municipio-mean-regs.RData` = summary statistics of 1994&#x2013;2018 federal deputy municipal-level regressions in [R](https://www.r-project.org/) format. (Regression coefficient estimates were used to estimate party core support and beta volatility in each municipality reported in `vhat` files.) Reading each file into an R environment imports a list with three objects named `pan`, `left`, and `oth`. Each object is a sub-list with ![img](./graph/readme-math/about-2500.svg) class 'lm' objects, one for each regression fitted. [This post](https://emagar.github.io/residuales-2018-english/) elaborates the method.
-   (`data/dipfed-seccion-regs-*.RData` and `data/dipfed-seccion-mean-regs.RData` files = sección-level regression summary statistics **not included** in repository due to large size (![img](./graph/readme-math/about-66k.svg) regressions per party-year). Files are available upon [request](mailto:emagar@gmail.com).)


<a id="org887a67d"></a>

## Redistricting and *reseccionamiento*

-   `equivSecc/docsRedistReseccRemunic/` = folder contains numerous documents prepared by INE/IFE explaining redistricting criteria, changes in sección delimitations (*reseccionamiento*), and changes in a state's municipalities.
    -   **Citation for these documents**: [Instituto Nacional Electoral (various years)](https://ine.mx).
-   `equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv` and `.xlsx` = historical record of *secciones electorales* nationwide since 1994 in comma-separated and excel format, respectively. Secciones, which do not traverse municipal borders, are the basic building blocks for districting at both the federal and state levels (see Magar et al. 2017, fn. 9). Each row reports one sección (approx. 71,000 total) and the district it belonged to in four federal congressional district maps (maps inaugurated in 1979, 1997, 2006, and 2018). It also reports the district it would have belonged to in the 2013 map that was rejected prior to adoption. A small but important number of secciones suffered changes through time (the official term is *reseccionamiento*)&#x2014;most frequently due to under- or over-population, but also after court rulings modifying state or municipal borders; see the \`OBSERVACIONES\` and its right-adjacent variables). For this reason, the dataset maps secciones-to-districts at each federal election since 1994.
    -   This dataset consolidates and extends excel sheets that IFE/INE distributes periodically. While built upon that IFE circulated in the 2010s, it has been drastically edited to include recent information.
    -   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.


<a id="org03e8cd2"></a>

## Comparative maps and shapefiles

-   `mapasComparados/` = **DESCRIPTION UNDER CONSTRUCTION**
-   `mapasComparados/fed/shp/disfed2006/` = contains one folder per state with IFE/INE-produced 2006 federal district digital maps in shapefile format. Maps include polygons for federal single-member diputado districts (`DISTRITO`), state borders (`ENTIDAD`), municipalities (`MUNICIPIO`), secciones electorales (`SECCION`), and polling places (`CASILLA`). The 2006 map was used in the 2006, 2009, 2012, and 2015 congressional races.
-   `mapasComparados/fed/shp/disfed2018/` = contains one folder per state with IFE/INE-produced 2018 federal district digital maps in shapefile format. Maps include polygons for federal single-member diputado districts (`DISTRITO`), state borders (`ENTIDAD`), municipalities (`MUNICIPIO`), and secciones electorales (`SECCION`). The 2018 map was used in the 2018 congressional races.
-   `mapasComparados/loc/shp/0code/dissolveSecciones.r` = code to generate state legislative district maps.
-   `mapasComparados/loc/shp/` = contains folders with states' legislative district digital maps in shapefile format. District polygons were prepared by dissolving the borders of municipalities and secciones electorales belonging to each district. Two sets of polygons are included for each state: the map that was last used in the 2012&#x2013;2014 state legislative races and the map that the 2015&#x2013;2017 redistricting redrew for each state. (A few states include additional sets of polygons.)
-   `redisProcess/` = **DESCRIPTION UNDER CONSTRUCTION**
-   `redisProcess/maps-with-all-poposals/2005/fed/` = one comma-separated file for each state describing the federal redistricting process in 2005 (i.e., how the 2006 map was made). Each file lists the district that each sección electoral belongs to in different versions of the map: the machine-generated first blueprint (escenario1); the second version that incorporated a first round of party proposals (escenario2); the final map that was submitted for approval by the Election Board's Council General after a second round of party proposals (escenario3); and the full set of proposals that state and national parties made to the blueprint and the second version of the map (e.g. in file `agsFed.csv`, pan1 is PAN's proposal to the first blueprint; pan2 is that party's proposal to the second version of the map; and so forth.)
-   `redisProcess/maps-with-all-poposals/2017/fed/` = one comma-separated file for each state describing the federal redistricting process in 2017 (i.e., how the 2018 map was made). Files have the same format as those above, with one difference: both state and national parties made proposals (e.g. in file `jalFed.csv`, morena<sub>clv1</sub> is the MORENA Jalisco state ("l" for local) chapter's proposal to the first blueprint; pan<sub>cnv2</sub> is the national ("n") PAN's proposal to the second version of the map; and so forth.)
-   `redisProcess/maps-with-all-poposals/2017/loc/` = one comma-separated file for each state describing the state redistricting processes in 2015&#x2013;17. Files have the same format as those above.
-   `redisProcess/maps-with-all-poposals/2013/fed/` = one comma-separated file for each state describing the federal redistricting process in 2013 (i.e., a map that was never adopted). Files have the same format as those above.


<a id="org153f28c"></a>

## Descriptive plots and literature

-   `graph/` = folder with descriptive plots of some of the measures distributed here.
-   `graph/readme-math` = images used to render math equations across this `README.md` file. Ignore them.
-   `lit/` = folder with relevant literature.


<a id="org31936eb"></a>

# Variables in the datasets <a id="org92f3af6"></a>

Variables are not necessarily included in every dataset distributed.


<a id="org7b238a4"></a>

## Observation identifiers

-   `ord` = observation counter.
-   `edon` = state number 1:32.
-   `edo` = state abbreviation (may differ from official abbreviations so that sorting them alphabetically preserves the order set by *edon*).
-   `seccion` = sección identifier, starts at 1 in each state.
-   `edosecn` = string identitying `edon` and `seccion` period separated; distinguishes units with same `seccion` value across states.
-   `inegi` = municipality identifier used by INEGI (census bureau).
-   `ife` = municipality identifier used by IFE/INE (election board).
-   `mun` = municipality's name.


<a id="org10ae681"></a>

## Vote returns and party performance (in `vhat` files)

`vhat` files report vote shares and simplify the party field to three major competitors and a residual \`others\` category.

-   `pan` = vote share won by the PAN and allies, see the [note on parties and coalitions](#org2006603).
-   `pri` = vote share won by the PRI and allies, see the [note on parties and coalitions](#org2006603).
-   `left` = vote share won by the left and allies, see [note on parties and coalitions](#org2006603).
-   (`oth` = vote share of candidates fielded by minor parties is not reported. It is 1 &#x2013; pan &#x2013; pri &#x2013; left and therefore fully determined.)
-   `efec` = valid votes in the unit (total votes cast for parties minus void and null ballots).
-   `d.pan`, `d.pri`, `d.left` = party's vote share change since last election (first differences, i.e., pan<sub>yr</sub> &#x2013; pan<sub>yr-3</sub> and so forth).
-   `vhat.pan`, `vhat.pri`, `vhat.left` = vote share predicted for the current year (`yr`) from a linear estimation of the party's performance in five immediately previous elections in the unit. Letting v<sub>i,t</sub> denote party i's vote share in year t, the equation fitted with OLS in each unit looks thus: ![img](./graph/readme-math/5-yr.svg). (A compositional variable specifiction was used, so the actual equation is slightly different, see [this post](https://emagar.github.io/residuales-2018-english/) for details.) The variable reports ![img](./graph/readme-math/vhat.svg), the point prediction for the current year.
-   `bhat.pan` and `bhat.left` variables = point estimates of the slope coefficient from the regression described in the bullet above. (There is no estimate for the PRI, see [this post](https://emagar.github.io/residuales-2018-english/).)
-   `alphahat.pan`, `alphahat.pri`, `alphahat.mprena` = party's core support group estimate for the unit in 2000&#x2013;2018 federal diputado elections. See [this post](https://emagar.github.io/residuales-2018-english/) for estimation details.
-   `betahat.pan`, `betahat.left` = party's volatility to national swings estimate for the unit in 2000&#x2013;2018 federal diputado elections. (There is no estimate for the PRI, see [this post](https://emagar.github.io/residuales-2018-english/).)
-   `w94`, `w97`, &#x2026;, `w18` = string with the name of the party that won the largest vote share in the unit in 1994, 1997, &#x2026;, 2018, respectively.
-   `mg94`, `mg97`, &#x2026;, `mg18` = margin of victory (i.e. difference between winner's and runner-up's vote shares) in the unit in 1994, 1997, &#x2026;, 2018, respectively.
-   `n.win.pan` = number of times the PAN (with or without coalition partners) finished first in the unit between 1994 and 2018.
-   `n.win.pri` = number of times the PRI (with or without coalition partners) finished first in the unit between 1994 and 2018.
-   `n.win.left` = number of times the left finished first in the unit between 1994 and 2018. Up to 2012, this was the PRD (with or without coalition partners). In 2018, this was MORENA (with or without coalition partners). In 2015, it was either (with or without coalition partners).
-   `n.win.oth` = number of time some party or coalition other than those listed above finished first in the unit between 1994 and 2018.


<a id="orga930097"></a>

## Vote returns (in `vraw` files)

`vraw` files report absolute votes for **all** parties in the congressional race. See the note on [coalitions](#org2006603) in different years.

-   `edon`, `seccion`, `ife`, `inegi` = unit identifiers (see above).
-   `disn` = federal district the unit belongs to in the current congressional election.
-   `d94`, `d97`, `d00`, `d03`, `d06`, `d09`, `d12`, `d15`, and `d18` = dummies equal 1 if the sección was utilized in the 1994, 1997, &#x2026;, 2018 congressional elections, respectively; equal 0 otherwise. Indicates [*reseccionamiento*](#orgec13380). Only included in the 1994 file to economize on redundancy.
-   `efec` = valid votes in the unit (total votes cast for parties minus void and null ballots).
-   `lisnom` = total registered voters (*lista nominal*) in the unit. Available for selected years only.
-   `pan`, `pri`, `prd`, and `left` = votes cast for major parties running without partners in the district the unit belongs to.
-   `panc`, `pric`, `prdc`, and `leftc` = votes cast for major parties and their allied partner(s) in the district the unit belongs to.
-   `dpanc`, `dpric`, `dprdc`, and `dleftc` = dummy equal 1 if the major party allied (fielded a joint candidate) with partners in the district the unit belongs to; equal 0 otherwise.
-   Remainder columns report votes cast for minor parties.


<a id="orgba8187e"></a>

## Redistricting and *reseccionamiento* <a id="orgec13380"></a>

Apart from redistricting, the election board routinely adopts [changes](https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DS/DS-CG/DS-SesionesCG/CG-acuerdos/2016/08_Agosto/CGor201608-26/CGor201608-26-ap-6-x1.pdf) in its geographic units when *secciones electorales* become over- or under-sized. Variables in this set are sección-level.

-   `split` = equals 0 for secciones that remained unchanged in the period, otherwise indicates the year sección was split into smaller units due to oversize. Estimation of some of the quantities reported involved re-aggregating new units into their oversized parent sección in order to preserve the full vote returns time series. See [code](#org54182e3) for details.
-   `new` = equals 0 for secciones that remained unchanged in the period, otherwise indicates the year sección was created by splitting an oversized sección into smaller units. Estimation of some of the quantities reported involved re-aggregating these smaller units into their oversized parent in order to preserve the full vote returns time series. See [code](#org54182e3) for details.
-   `dis1979` = district the sección belonged in the 1979 map (used in the 1979 to 1994 federal elections, inclusive).
-   `dis1997` = district the sección belonged in the 1997 map (used in the 1997 to 2003 federal elections, inclusive).
-   `dis2006` = district the sección belonged in the 2006 map (used in the 2006 to 2015 federal elections, inclusive).
-   `dis2013` = district the sección belonged in the 2018 map (used in the 2018 and 2021 federal elections).
-   `dis2018` = district the sección belonged in the 2013 map (the 2013 map was never adopted).
-   `OBSERVACIONES` = character string describing changes that a sección may have suffered through time in the source;
-   `action`, `action2`, `action3` = character string indicates change that sección may have suffered: *merged.to* if integrated into a neighboring sección, usually due to under-population; *split.to* if subdivided into new secciones, usually due to over-population; *split.from* if it arose from a split sección; *mun.chg* if municipal delimitations changed around the sección; *state.chg* if it arose to accommodate a change in interstate border lines.
-   `orig.dest`, `orig.dest2`, `orig.dest3` = when sección was subdivided, indicates sección number(s) that arose; when sección was merged, indicates which one absorbed it.
-   `when`, `when2`, `when3` = year the change took place.
-   `coment` = character string with comments (in mostly Spanish).


<a id="org197a914"></a>

# Note on electoral coalitions <a id="org2006603"></a>

Electoral alliances in congressional races, which often were limited to a subset of single-member districts, are extended nationwide for analyticial convenience. In some cases, noted below, they are extended to a year it did not occur for the same reason. Details for each party follow.

-   Partido Acción Nacional (PAN) fielded candidates jointly with the Green party (PVEM) nationwide in 2000 and with the PRD and minor MC in select districts in 2018. The 2018 coalition was extended nationwide (i.e. PAN + PRD + MC votes added) for analysis. PAN did not forge alliances in other years in the period.
-   Partido Revolucionario Institucional (PRI) never allied before 2003. It has then fielded joint candidates with the PVEM nationwide (in 2006) or in select districts (in 2003, 2009, 2012, and 2015). In 2018 it fielded joint candidates with the PVEM and the PNA in select districts. Alliances were extended nationwide for analysis.
-   For most of the period, the left corresponds to PRD, the Partido de la Revolución Democrática, and recently became MORENA, the Movimiento de Regeneración Nacional (which is a splinter from PRD). The left's vote up to 1997 is the PRD's, which ran solo. In 2000 the PRD fielded joint candidates nationwide with Partido del Trabajo (PT), Movimiento Ciudadano (MC, then called Convergencia), and two now-extinct minor parties. From 2003 to 2012 the left is the sum of PRD, PT, and MC (an artificial sum in 2003 and 2009 for comparability). In 2015 the left is the sum of votes for the PRD, PT, MORENA, and PES (only the first two fielded joint candidates in select districts). In 2018 the left is the sum of MORENA, PT, and PES which fielded joint candidates in most districts.
-   A residual "others" category sums the votes for parties other than those listed above for analysis. In 1991 the parties whose votes are summed are PARM, PDM, PFCRN, PPS, PEM, and PRT; in 1994 PPS, PFCRN, PARM, UNO-PDM, PT, and PVEM; in 1997 PC, PT, PVEM, PPS, and PDM; in 2000 PCD, PARM and DS; in 2003 PSN, PAS, MP, PLM, and FC; in 2006 PNA and ASDC; in 2009 PNA and PSD; in 2012 PNA only; in 2015 MC, PNA, PH, and a handful of independent candidates; and in 2018 a handful of independent candidates.


<a id="orga4eb57c"></a>

# Acknowledgements

I acknowledge financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. Files distributed here systematize/analyze a massive volume of data from the [Instituto Nacional Electoral](https://ine.mx), I am sincerely grateful for their excellent work producing and distributing election results, maps, and associated metadata. I am responsible for mistakes and shortcomings.
