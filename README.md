
# Table of Contents

1.  [Description of *Recent Mexican federal election geography* repository](#org8b135a6)
2.  [Files in the repository and how to cite them](#orgaaf9aef)
3.  [Acknowledgements](#org83c170c)

Last revision: 2019-09-27


<a id="org8b135a6"></a>

# Description of *Recent Mexican federal election geography* repository

-   Author: Eric Magar
-   Email: emagar at itam dot mx

The repository contains maps of Mexican districts used to elect representatives to various offices and code for data systematization and analysis. The primary source are shapefiles publicly distributed by INE (formerly IFE, Mexico's national election board, page [here](https://cartografia.ife.org.mx/sige7/?cartografia)). Data in this repo is prepared for mapping and preliminary/basic analysis.


<a id="orgaaf9aef"></a>

# Files in the repository and how to cite them

-   `equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv` = historical record of *secciones electorales* nationwide since 1994. Secciones, which do not traverse municipal borders, are the the basic building blocks for districting at both the federal and state levels (see Magar et al. 2017, fn. 9). Each row reports one sección (\(N \approx 69,000\)) and the district it belonged to in four federal congressional district maps (maps inaugurated in 1979, 1997, 2006, and 2018). It also reports the district it would have belonged to in the 2013 map that was rejected prior to adoption. A small but important number of secciones suffered changes through time (the official term is *reseccionamiento*)&#x2014;most frequently due to under- or over-population, but also after court rulings effecting modifications in state or municipal borders; see the \`OBSERVACIONES\` and its right-adjacent variables). For this reason, the dataset maps secciones-to-districts at each federal election since 1994.  
    -   This dataset builds upon an excel sheet that IFE/INE distributes (included in the repository and listed next).
    -   Variables in the dataset:
        -   \`ord\` = observation counter.
        -   \`edon\` = state number.
        -   \`edo\` = state abbreviation (may differ from the 'official' abbreviations so that sorting them alphabetically preserves the order set by *edon*).
        -   \`seccion\` = IFE's sección number starting at 1 for each state.
        -   \`munn\` = municipality's number.
        -   \`ife\` = municipality's code used by IFE/INE.
        -   \`inegi\` = municipality's code used by INEGI.
        -   \`mun\` = municipality's name.
        -   \`edosecn\` = string identitying \`edon\` and \`seccion\` period separated; distinguishes units with same \`seccion\` value across states.
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
        -   \`coment\` = character string with comments.
    -   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.


<a id="org83c170c"></a>

# Acknowledgements

Eric Magar acknowledges financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. He is responsible for mistakes and shortcomings in the data. 

