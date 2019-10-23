
# Table of Contents

1.  [Description of *Recent Mexican electoral geography* repository](#orge15083c)
2.  [Files in the repository and how to cite them](#org269b1f8)
3.  [Acknowledgements](#org3095bb0)
4.  [below are elements to copy/emulate in this readme file ---](#org5f3c04c)
5.  [Files in the repository and how to cite them](#orgbd21f32)
6.  [Codebook](#org8287abd)
7.  [Coding procedure for the incumbent's status](#org0589965)
8.  [Procedimiento para codificar el estatus del ocupante](#org2c02832)
9.  [Sources](#org3d213ae)

Last revision: 2019-10-16

**>>> Under construction (expected completion end of Oct. 2019) <<<**


<a id="orge15083c"></a>

# Description of *Recent Mexican electoral geography* repository

-   Author: Eric Magar
-   Email: emagar at itam dot mx

The repository contains maps of Mexican districts used to elect representatives to various offices and code for data systematization and analysis. The primary source are shapefiles publicly distributed by INE (formerly IFE, Mexico's national election board, page [here](https://cartografia.ife.org.mx/sige7/?cartografia)). Data in this repo is prepared for mapping and preliminary/basic statistical analysis.


<a id="org269b1f8"></a>

# Files in the repository and how to cite them

-   `code/elec-data-for-maps.r` = code manipulates sección-level vote returns in federal deputy elections 1994&#x2013;2018 for use in maps.
-   `vhat` files = electoral statistics for different years and units of aggregation calculated from federal diputado single-member district elections. Measures of interest are (a) the parties' vote shares in the unit-year; (b) the predicted vote share for the unit-year out of each party's performance in the unit in the previous five federal diputado elections; (c) estimates of each party's core support in the unit for the whole period; (d) number of times each party won the vote plurality/majority in the unit in the period (`code/elec-data-for-maps.r` was used to prepare these files). 
    -   Variables in the datasets:<a id="org6aa117a"></a> most variables described here are included in every year and level of aggregation file. When this is not true, the corresponding file mentions the exception. 
        -   \`yr\` = election year.
        -   \`edon\` = state number 1:32.
        -   \`inegi\` = municipality's code used by INEGI (census bureau).
        -   \`ife\` = municipality's code used by IFE/INE (election board).
        -   \`pan\` = vote share won by the PAN and allies, see the note on coalitions.
        -   \`pri\` = vote share won by the PRI and allies, see the note on coalitions.
        -   \`morena\` = vote share won by the left and allies, see the note on coalitions. 
            -   The PRD and its PT ally's votes, MORENA's, and the PES's are aggregated (in order to ease comparison to the 2018 election, when Morena+PT+PES fielded joit candidates).
        -   (\`oth\` = vote share of candidates fielded minor parties is omitted, it is 1 - pan - pri - morena.)
            -   by MC, PNA, PH, and independents
        -   \`d.pan\`, \`d.pri\`, \`d.morena\` = first differences, party's vote share change since last election (i.e., pan<sub>yr</sub> - pan<sub>yr-3</sub> and so forth).
        -   \`vhat\` variables = vote share predicted for the current year (\`yr\`) from a linear estimation of the party's performance in five immediately previous elections in the unit. Letting v<sub>i,t</sub> denote party i's vote share in year t, the equation fitted with OLS in each unit looks thus: ![img](./graph/readme-math/5-yr.svg). (A compositional variable specifiction was used, so the actual equation is slightly different, see [this post in Spanish](https://emagar.github.io/residuales-2018/) for details.) The variable reports ![img](./graph/readme-math/vhat.svg), the point prediction for the current year.
            -   \`vhat.pan\` = PAN's vote share estimate.
            -   \`vhat.pri\` = PRI's vote share estimate.
            -   \`vhat.morena\` = left's vote share estimate.
        -   \`bhat\` variables = point estimates of the slope (b) coefficient from the regression described in the bullet above.
            -   \`alphahat\` variables = party core support estimate for the unit in 2000-2018 federal diputado elections. See XXXX for estimation details.
            -   \`bhat.pan\` = PAN's slope estimate.
            -   \`bhat.morena\` = left's slope estimate.
        -   \`alphahat\` variables = vote share estimates from a linear prediction for the current year (\`yr\`) from the party's performance in five immediately previous federal diputado elections in the unit. The equation v<sub>i,t</sub> = a + bt, t = (yr - 15, yr - 12, yr - 9, yr - 6, yr - 3) estimated via OLS in each unit (municipality/sección) for this purpose. Compositional variable specification used for estimation, see [this post in Spanish](https://emagar.github.io/residuales-2018/) for details.
            -   \`alphahat.pan\` = PAN's core support estimate.
            -   \`alphahat.pri\` = PRI's core support estimate.
            -   \`alphahat.morena\` = left's core support estimate.
        -   \`betahat\` variables = 
            -   \`betahat.pan\` = PAN's
            -   \`betahat.morena\` = left's
    -   Files available for year-levels of aggregation:
        -
    -   Note on coalitions: electoral alliances are extended nationwide and, in some cases, to other years for convenience in the analysis. Details for each party follow:
        -   Partido Acción Nacional (PAN) fielded candidates jointly with the Green party (PVEM) nationwide in 2000 and with the PRD and minor MC in select districts in 2018. The 2018 coalition was extended nationwide (i.e. PAN + PRD + MC votes added) for analysis. PAN did not ally in any other year in the period.
        -   Partido Revolucionario Institucional (PRI) never allied until 2000. It then fielded joint candidates with the PVEM nationwide (in 2006) or in select districts (in 2003, 2009, 2012, and 2015). In 2018 it fielded joint candidates with the PVEM and the PNA in select districts. Partial alliances were extended nationwide for analysis.
        -   The left is generically called MORENA, as its latest incarnation, the Movimiento de Regeneración Nacional which is a splinter from the Partido de la Revolución Democrática (PRD). The left's vote up to 1997 is the PRD's, which ran solo. In 2000 the PRD fielded joint candidates nationwide with Partido del Trabajo (PT), Movimiento Ciudadano (MC, then called Convergencia), and two now-extinct minor parties. From 2003 to 2012 the left is the sum of PRD, PT, and MC (an artificial sum in 2003 and 2009 for comparability). In 2015 the left is the sum of votes for the PRD, PT, MC, MORENA, and PES (only the first two fielded joint candidates in select districts). In 2018 the left is the sum of MORENA, PT, and PES which fielded joint candidates in most districts.
        -   A residual "others" category sums the votes for parties other than those listed above for analysis. In 1991 the parties whose votes are summed are PARM, PDM, PFCRN, PPS, PEM, and PRT; in 1994 PPS, PFCRN, PARM, UNO-PDM, PT, and PVEM; in 1997 PC, PT, PVEM, PPS, and PDM; in 2000 PCD, PARM and DS; in 2003 PSN, PAS, MP, PLM, and FC; in 2006 PNA and ASDC; in 2009 PNA and PSD; in 2012 PNA only; in 2015 MC, PNA, PH, and a handful of independent candidates; and in 2018 a handful of independent candidates.

-   `data/dipfed2015mu-vhat.csv` = 2015 federal diputado single-member district election statistics. The units are municipalities. Data are municipal aggregates of sección-level returns (i.e. votes from all secciones belonging to a given municipality are added up). The PRI fielded joint candidates with the Green party in some states only, as did the PRD with the PT; both coalitions are extended nationwide for convenience in the analysis (`code/elec-data-for-maps.r` was used to prepare this file). 
    -   Variables in the dataset:<a id="org1a8954a"></a>
-   `equivSecc/docsRedistReseccRemunic/` = folder contains numerous documents prepared by INE/IFE explaining redistricting criteria, changes in sección delimitations (*reseccionamiento*), and changes in a state's municipalities.
-   `equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv` = historical record of *secciones electorales* nationwide since 1994. Secciones, which do not traverse municipal borders, are the the basic building blocks for districting at both the federal and state levels (see Magar et al. 2017, fn. 9). Each row reports one sección (approx. 69,000 total) and the district it belonged to in four federal congressional district maps (maps inaugurated in 1979, 1997, 2006, and 2018). It also reports the district it would have belonged to in the 2013 map that was rejected prior to adoption. A small but important number of secciones suffered changes through time (the official term is *reseccionamiento*)&#x2014;most frequently due to under- or over-population, but also after court rulings effecting modifications in state or municipal borders; see the \`OBSERVACIONES\` and its right-adjacent variables). For this reason, the dataset maps secciones-to-districts at each federal election since 1994.  
    -   This dataset builds upon an excel sheet that IFE/INE distributes (included in the repository and listed next).
    -   Variables in the dataset:<a id="orgc57e02c"></a>
        -   \`ord\` = observation counter.
        -   \`edon\` = state number 1:32.
        -   \`edo\` = state abbreviation (may differ from the 'official' abbreviations so that sorting them alphabetically preserves the order set by *edon*).
        -   \`seccion\` = IFE's sección number starting at 1 for each state.
        -   \`munn\` = municipality's number.
        -   \`ife\` = municipality's code used by IFE/INE (election board).
        -   \`inegi\` = municipality's code used by INEGI (census bureau).
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


<a id="org3095bb0"></a>

# Acknowledgements

Eric Magar acknowledges financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. He is responsible for mistakes and shortcomings in the data. 


<a id="org5f3c04c"></a>

# below are elements to copy/emulate in this readme file ---

Maps of federal and state legislative districts, and code voting data for recent Mexican elections for certain offices at different levels of aggregation. Data has been compiled from many sources. More recent years tend to be coded from official vote returns. Earlier elections tend to be from secondary sources (see Souces section). Data inludes district-level federal deputy vote returns since 1979 and district-level presidential vote returns since 2006; and municipality-level municipal president vote returns (except in the state of Nayarit, votes cast for municipal president also elect a municipal council in a fused ballot). 

*Important note:* older incarnations of this this repository contain LFS (Large File System) parts. Make sure to install [LFS](https://git-lfs.github.com/) in your machine before cloning previous commits of the repository.


<a id="orgbd21f32"></a>

# Files in the repository and how to cite them

You are free to download and modify the data (see the LICENSE document for details) provided you give proper credit to this source. Unless otherwise noted next to the file descriptor, the cite is Eric Magar (2018) Recent Mexican election vote returns repository, <https://github.com/emagar/elecReturns>.

In general, file names identify the office elected (i.e., **df**, **se**, **pr**, **dl**, **go**, **ay** for *diputados federales*, *senadores*, *presidente*, *diputados locales*, *gobernador*, and *ayuntamiento*, respectively), followed by the unit of observation (i.e., **ed**, **df**, **dl**, **mu**, **de**, **se**, **ca** for *estado*, *distrito federal*, *distrito local*, *municipio*, *demarcación*, *sección*, and *casilla* respectively), and the years included. Other than in Nayarit since 2008 (and, pending a court case, Mexico City since 2018), *ayuntamientos* are elected in fused ballots for a *presidente municipal* and a fraction of the municipal council (*regidores* and *síndicos*). Nayarit elects these members of the municipal council in single-member plurality districts called *demarcaciones*.

-   `data/aymu1977-present.csv` = updated to 2018, can be processed with code/ay.r in order to systematize coalitions (ie., aggregate votes when member parties' returns are reported separately and remove redundant columns).
-   `data/aymu1997-present.coalAgg.csv` = pre-processed version of the above (starting in 1997) so that coalition votes appear properly aggregated.
-   `data/aymu1989-present.incumbents.csv` = names of municipal election winning candidates since 1989 (work in progress).
-   `data/ayde2008-presentNayRegid.csv` = Nayarit's municipal demarcaciones vote returns since 2008.
-   `code/ay.r` = script to manipulate *ayuntamiento* returns.
-   `code/ayClean.r` = script used to clean *ayuntamiento* returns, should be unnecessary unless new data are added because output has been saved into csv file.
-   `data/dfdf1979-on.csv`
    -   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.
-   `data/dfdf1979-on.coalAgg.csv` = pre-processed version of the above so that coalition votes appear properly aggregated.
    -   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.
-   `data/dfdf2012-onCandidates.csv` = names of all federal deputy candidates in districts and party lists since 2012.
-   `data/seedcandidates2018.csv` = names of all senatorial candidates in states and party lists in 2018.
-   `data/goed1961-on.csv` = updated to 2010
    -   **Citation for this dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.
-   `data/prdf2006-on.csv`
    -   **Citation for this dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.
-   <del>`datosBrutos/` = large directory containing primary sources</del> (dropped from repo due to large size&#x2026; [mail me](mailto:emagar@itam.mx) if you need this).


<a id="org8287abd"></a>

# Codebook

Most variables are included in every file, some appear in selected files only.  

-   *edon* = state number 1:32.
-   *edo* =
-   *disn* = district number.
-   *emm* = municipal indentifying code (*edo*-electionCycle./munn/).
-   *mun* = municipality.
-   *munn*, *inegi*, *ife* = municipal identifier, reporting the number and the codes used by INEGI and IFE, respectively.
-   *yr*, *mo*, *dy* = year, month, day of the election.
-   *cab* = cabecera, district's administrative center.
-   *circ* = PR district (circunscripcion electoral, 2nd tier).
-   *v01*, *v02*, &#x2026; = raw vote for candidate 1, 2, etc.
-   *l01*, *l02*, &#x2026; = label of candidate 1's, 2's, &#x2026; party or coalition.
-   *c01*, *c02*, &#x2026; = candidate 1's, 2's, &#x2026; name.
-   *s01*, *s02*, &#x2026; = suplente (substitute) for candidate 1, 2, etc.
-   *efec* = effective votes, equal the total raw votes minus votes for write-in candidates and invalid ballots.
-   *nr* = votes for write-in candidates.
-   *nul* = invalid ballots.
-   *tot* = total raw votes.
-   *lisnom* = eligible voters (*lista nominal*).
-   *nota* = notes.
-   *fuente* = source.
-   *ncand* = number of candidates running.
-   *dcoal* = dummy equal 1 if at least one major party candidate ran on a multi-party pre-electoral coalition, 0 otherwise.
-   *coalpan*, *coalpri*, *coalprd* = members of major-party coalitions ('no' indidates no coalition).
-   *imputacion*, *distpan*, *distpri*, *distprd* = when some parties coelesced in such way that only their pooled vote was reported, an attempt is made to infer how many votes each coalition member contributed to team. Variable *imputacion* lists what earlier election was used for this purpose ('no' if none carried); *dist* variables report the share of the coalition total attributable to PAN, PRI, and PRD, respectively. See [this](https://github.com/emagar/replicationMaterial/blob/master/gubCoat/onlineAppendix.pdf) for details.
-   *seyr*, *semo* = year of the previous/concurrent senatorial election.
-   *sepan*, *sepri*, *seprd* = votes won by major parties in previous/concurrent senatorial election.
-   *seefec* = effective votes in previous/concurrent senatorial election.
-   *fake* = indicates fake data for hegemonic era elections, made up of best guesses about what happened in the state's race for the purpose of computing vote lags. Will normally be dropped from analysis.
-   *win* = winner's party or coalition.
-   *incumbent* = winning candidate's name.
-   *race.after* = incumbent's status in the subsequent race. See [this](#orgf6b80f0) for categories and coding procedure ([aquí](#orgf862a5f) la versión en español del procedimiento codificador).


<a id="org0589965"></a>

# Coding procedure for the incumbent's status<a id="orgf6b80f0"></a>

In file `data/aymu1985-present.incumbents.csv`, variable *race.after* equals one of the following categories: 

1.  'Beaten' if the incumbent re-ran and lost;
2.  'Reelected' if the incumbent re-ran and won;
3.  'Renom-killed' if the incumbent re-ran and was killed in the campaign;
4.  'Hi-office' if the incumbent ran for higher office;
5.  'Out' if the incumbent withdrew or was not renominated;
6.  'Term-limited' if the incumbent was ineligible for reelection due to a term limit;
7.  A year indicates that it is too early to know the incumbent's status (and the year of the next race).

In categories other than the first two above, a suffix may be present. 

-   Suffix '-p-lost' indicates that the party lost the subsequent race (or, in case of incumbents elected by a multi-party coalition, that none of them won or was part of the winning coalition).
-   Suffix '-p-won' indicates that the party won the subsequent race (or, in case of incumbents elected by a multi-party coalition, that one of them won or at least one of them was in the winning coalition).


<a id="org2c02832"></a>

# Procedimiento para codificar el estatus del ocupante<a id="orgf862a5f"></a>

En el archivo `data/aymu1985-present.incumbents.csv`, la variable *race.after* indica el estatus del ocupante en la elección subsecuente. El estatus puede ser una de las categorías siguientes: 

1.  'Beaten' si el ocupante volvió a contender y perdió;
2.  'Reelected' si el ocupante volvió a contender y ganó;
3.  'Renom-killed' si el ocupante volvió a contender y fue asesinado en la campaña;
4.  'Hi-office' si el ocupante contendió por otro cargo de elección (p.ej. gobernador o senador);
5.  'Out' si el ocupante se retiró o no fue repostulado por el partido;
6.  'Term-limited' si el ocupante estaba constitucionalmente impedido para aspirar a reelegirse;
7.  Un año indica que aún es temprano para conocer el estatus (y el año de la próxima elección).

En las categorías 3 en adelante, un sufijo puede estar presente. 

-   El sufijo '-p-lost' indica que el partido perdió la elección subsecuente (o, para ocupantes electos por una coalición multi-partidista, que ninguno de esos partidos ganó o fue parte de la coalición ganadora).
-   El sufijo '-p-won' indica que el partido ganó la elección subsecuente (o, para ocupantes electos por una coalición multi-partidista, que uno de esos partidos ganó o que por lo menos uno fue parte de la coalición ganadora).


<a id="org3d213ae"></a>

# Sources

Work in progress&#x2026;

-   *Fuente* = iee indicates data obtined from the primary source, the state's election board's web site.
-   *Fuente* = tesis Melissa
-   *Fuente* = Mexico Electoral Banamex
-   *Fuente* = prep
-   *Fuente* = Toledo Patiño paper
-   *Fuente* = UAM Iztapalapa
-   *Fuente* = voz y voto

