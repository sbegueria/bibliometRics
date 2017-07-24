# bibliometRics [![DOI](https://zenodo.org/badge/57246514.svg)](https://zenodo.org/badge/latestdoi/57246514)
Santiago Beguería  
26 April 2016
    
`bibliometRics` is an R package for bibliometric analysis of scientific production.
It can be used for analysing the production of a single author, a working team, department, institute, etcetera.

This document describes the main functionalities in the package, and how to do a bibliometric analysis with `bibliometRics`, including producing automatic pdf reports via `knitr`.

First of all, make sure you installed the package by sourcing it (note that this is a working project, so no 'oficial' package has been created yet).


```r
source('bibliometRics.R')
```


## Getting some bibliometric data

So far the unique source of blibiometric information accepted by `bibliometRics` is the Web of Science (WoS) by Thomson Reuters, but other sources such as Scopus or Google Scholar can be added in the future.
Publications can be selected for a given author (easiest if you know its author ID) or a group (such a research group or a department, for instance).
The WoS allows to generate a citation report, stating the number of citations received by each item, every year after its publication.
So once you are good with the selection of publications you want to analyze, you can click on the 'create citation report' button and then select the 'save to text file' option.
This will generate a text file and download it to your computer.
You may want to edit the AUTHOR field on the first line of this text file, although this is not strictly necessary for doing the analysis.

An example citation report file from the WoS is the file [sbegueria.txt](sbegueria.txt), which contains data of papers I have authored, as of January 2017.

The core function for reading WoS citation report data is, not surprisingly, `read.wos`. 
Its only argument is the name of the data file you want to read:


```r
bib <- read.wos('sbegueria.txt')
str(bib)
```

```r
## List of 3
##  $ author   : chr "BEGUERIA,  SANTIAGO"
##  $ reference: chr "sbegueria"
##  $ pubs     :'data.frame':	88 obs. of  39 variables:
##   ..$ Title            : chr [1:88] "Estimating erosion rates using Cs-137 measurements and WATEM/SEDEM in a Mediterranean cultivated field" "Recent changes and drivers of the atmospheric evaporative demand in the Canary Islands" "Mid and late Holocene forest fires and deforestation in the subalpine belt of the Iberian range, northern Spain" "Use of disdrometer data to evaluate the relationship of rainfall kinetic energy and intensity (KE-I)" ...
##   ..$ Authors          : chr [1:88] "Quijano, Laura; Begueria, Santiago; Gaspar, Leticia; Navas, Ana" "Vicente-Serrano, Sergio M.; Azorin-Molina, Cesar; Sanchez-Lorenzo, Arturo; El Kenawy, Ahmed; Martin-Hernandez, Natalia; Pena-Ga"| __truncated__ "Garcia-Ruiz, Jose M.; Sanjuan, Yasmina; Gil-Romera, Graciela; Gonzalez-Samperiz, Penelope; Begueria, Santiago; Arnaez, Jose; Co"| __truncated__ "Angulo-Martinez, M.; Begueria, S.; Kysely, J." ...
##   ..$ Corporate Authors: logi [1:88] NA NA NA NA NA NA ...
##   ..$ Editors          : logi [1:88] NA NA NA NA NA NA ...
##   ..$ Book Editors     : chr [1:88] "" "" "" "" ...
##   ..$ Source Title     : chr [1:88] "CATENA" "HYDROLOGY AND EARTH SYSTEM SCIENCES" "JOURNAL OF MOUNTAIN SCIENCE" "SCIENCE OF THE TOTAL ENVIRONMENT" ...
##   ..$ Publication Date : chr [1:88] "MAR 2016" "AUG 23 2016" "OCT 2016" "OCT 15 2016" ...
##   ..$ Publication Year : int [1:88] 2016 2016 2016 2016 2016 2016 2016 2015 2015 2015 ...
##   ..$ Volume           : chr [1:88] "138" "20" "13" "568" ...
##   ..$ Issue            : chr [1:88] "" "8" "10" "" ...
##   ..$ Part Number      : chr [1:88] "" "" "" "" ...
##   ..$ Supplement       : logi [1:88] NA NA NA NA NA NA ...
##   ..$ Special Issue    : chr [1:88] "" "" "" "" ...
##   ..$ Beginning Page   : int [1:88] 38 3393 1760 83 2120 3413 NA 429 853 773 ...
##   ..$ Ending Page      : chr [1:88] "51" "3410" "1772" "94" ...
##   ..$ Article Number   : chr [1:88] "" "" "" "" ...
##   ..$ DOI              : chr [1:88] "10.1016/j.catena.2015.11.009" "10.5194/hess-20-3393-2016" "10.1007/s11629-015-3763-8" "10.1016/j.scitotenv.2016.05.223" ...
##   ..$ Conference Title : chr [1:88] "" "" "" "" ...
##   ..$ Conference Date  : chr [1:88] "" "" "" "" ...
##   ..$ Total Citations  : int [1:88] 0 0 0 0 2 2 4 0 4 4 ...
##   ..$ Average per Year : num [1:88] 0 0 0 0 1 1 2 0 1.33 1.33 ...
##   ..$ 2000             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2001             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2002             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2003             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2004             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2005             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2006             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2007             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2008             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2009             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2010             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2011             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2012             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2013             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2014             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2015             : int [1:88] 0 0 0 0 0 0 0 0 0 1 ...
##   ..$ 2016             : int [1:88] 0 0 0 0 2 2 4 0 4 3 ...
##   ..$ 2017             : int [1:88] 0 0 0 0 0 0 0 0 0 0 ...
```

The result is a list with the following three elements:
* `author`, a character object with the name of the author being analysed;
* `reference`, a character object; and
* `pubs`, a data frame with the publications in rows, and the data referring to each publications in columns, including the number of citations received, year by year.


## Analyzing bibliometric data

The main function for analyizing these data is `bibliometric`.
It takes an object with the bibliometric data resulting from a call to `read.wos` and returns a data.frame with a number of bibliometric indices.

In order to compute some metrics which are based on the number of citations received by each publication, data on the percentile baselines for each scientific discipline is requireed.
This data can be found on another Thomson Reuters product, the 'Essential Science Indicators' database.
You need to navigate to the 'Field Baselines' tab, and then select 'Percentiles' to get the field percentile baselines (FPB).
You can then download the FPB table in csv format.

An example FPB table as of January 2017 can be found in file [BaselinePercentiles.csv](BaselinePercentiles.csv).
This table can be read with the function 'read.baselines':

```r
base <- read.baselines('BaselinePercentiles.csv')
```

The resulting object is a list with as many items as disciplines.
For each discipline, a data.frame is stored containing the threshold number of citations corresponding to each percentile, depending on the publication year.
For instance, this is are the baseline thresholds for the discipline 'Geosciences':

```r
base$GEOSCIENCES
```

We can now use the function `bibliometric`, especifying the, the field baselines, and the discipline (the table for 'all fields' will be used as a default if no discipline is specified).

```r
bibliometric(bib, base, 'GEOSCIENCES')
```

```r
##                   name  ini years pubs lead pubs_year hin hin_year
##  1 BEGUERIA,  SANTIAGO 2000    16   88   17       5.5  33     2.06
##    gin gin_year cit_tot cit_year cit_art ifact2 ifact5 i10 i25 i50
##  1  57     3.56    3472      217   39.45  15.43  14.13  65  48  16
##    cit_max pubs09 pubs09_lead pubs099 iscore iscore_lead
##  1     616     32           7      11   4145         187
```

These are the following:

 Label         | Meaning
---------------| ----------------------------------------------------
 name          | Name of the author, group, department, etc.
 ini           | Initial year of the publication record
 span          | Time span (years) of the publication record
 pubs          | Total number of publications
 lead          | Total number of publications, as the lead (first) author
 pubs_year     | Mean number of publications per year
 hin           | Hirsch's h-index
 hin_year      | h-index per year
 gin           | Egghe's g-index
 gin_year      | g-index per year
 cit_tot       | Total number of citations
 cit_year      | Mean number of citations per year 
 cit_art       | Mean number of citations per article
 ifact2        | Impact factor, computed over the last two years
 ifact5        | Impact factor, computed over the last five years
 i10           | Number of publications with 10 or more citations
 i25           | Number of publications with 25 or more citations
 i50           | Number of publications with 50 or more citations
 cit_max       | Number of citations of the most citated publications
 pubs09        | Number of publications over the 90th percentile in its discipline
 pubs09_lead   | Number of publications over the 90th percentile in its discipline, as lead author
 pubs099       | Number of publications over the 99th percentile in its discipline
 iscore        | i-score
 iscore_lead   | i-score, as lead author

There are functions for computing some of the indices, such as the Hirsch and the Egghe indices.
These can be computed for the whole period analyized, or up to a given year.


```r
hirsch(bib)
```

```r
##  [1] 33
```

```r
egghe(bib)
```

```r
##  [1] 57
```

```r
hirsch(bib, 2010)
```

```r
##  [1] 13
```

```r
egghe(bib, 2010)
```

```r
##  [1] 45
```

There is also a function for ranking the publications in quantiles:

```r
rank(bib, q=base$GEOSCIENCES)
```

```r
##   [1] >q0     >q0     >q0     >q0     >q0.9   >q0.9   >q0.99  >q0    
##   [9] >q0.8   >q0.8   >q0.9   >q0.9   >q0.99  >q0.99  >q0.99  >q0.999
##  [17] >q0     >q0.99  >q0.99  >q0.5   >q0.9   >q0.9   >q0.999 >q0    
##  [25] >q0     >q0.5   >q0.5   >q0.8   >q0.8   >q0.9   >q0.9   >q0.9  
##  [33] >q0     >q0     >q0.5   >q0.5   >q0.8   >q0.8   >q0.8   >q0.8  
##  [41] >q0.9   >q0.9   >q0.99  >q0     >q0     >q0.5   >q0.8   >q0.9  
##  [49] >q0.9   >q0.9   >q0.9   >q0.9   >q0.99  >q0.999 >q0.5   >q0.5  
##  [57] >q0.8   >q0.8   >q0.8   >q0.8   >q0.8   >q0.9   >q0.9   >q0    
##  [65] >q0.5   >q0.8   >q0.8   >q0.8   >q0.8   >q0.5   >q0.5   >q0.8  
##  [73] >q0.8   >q0.9   >q0.9   >q0.9   <NA>    <NA>    <NA>    <NA>   
##  [81] <NA>    <NA>    <NA>    <NA>    <NA>    <NA>    <NA>    <NA>   
##  Levels: >q0.9999 >q0.999 >q0.99 >q0.9 >q0.8 >q0.5 >q0
```

```r
table(rank(bib, q=base$GEOSCIENCES))
```
```r
##  >q0.9999  >q0.999   >q0.99    >q0.9    >q0.8    >q0.5      >q0 
##         0        3        8       21       20       11       13 
```


A specific plotting function makes it easy to resume most of this information in graphic form.
The plots also inform about the temporal evolution of the bibliometric indices, which may be useful for evaluating the scientific career of the evaluated.


```r
biblioplot(bib, q=base$GEOSCIENCES)
```

<div class="figure" style="text-align: center">
<img src="./biblioplot_example.pdf" alt="A graphic bibliometric analysis" />
<p class="caption">A graphic bibliometric analysis</p>
</div>

The first plot reflects the productivity (quantity) of the author, as well as its impact (citations received). It shows the cumulative number of publications, with distinction between the publications as lead author (black bars) and those as co-author (white bars). The plot also showcases the number of citations for all the publications (white circles) and for those as lead author (black circles). There is a fixed ratio of 1:10 between the left (publications) and the right (citations) axis, allowing for easy comparison across authors, groups, etc. This ratio implies assuming a mean citation rate of 10 citations per article publishes (a rate that is, of course, arbitrary).

The second plot focuses on the impact of the publications. It shows the annual evolution of the Hirsch's h-index (white circles) and the Egghe's g-index (black circles), with a fixed ratio of 1:2 between them. Evolution of the h-index is compared with an 1:1 evolution (dashed line), since it is usually assumed that the h-index grows, as an average, at a rate of 1 per year.

The third plot attempts at evaluating the excelence of the publications. It shows the number of publications classified by quantiles, according to the ISI-WoK Scientific Indicators per discipline. For each quantile, the total number of publications is shown (white bars), as well as the publications as lead author (black bars).

A formatted list of all the publications is also produced.

```r
format_pub(cbind(bib$pubs, rank(bib,q=base$GEOSCIENCES))[1,], au=bib$au)
```

```r
##  [1] "\\item Quijano, Laura;\\textbf{ Begueria, Santiago}; Gaspar, Leticia; Navas, Ana. Estimating Erosion Rates using Cs-137 Measurements and Watem/Sedem in a Mediterranean Cultivated Field. \\textit{CATENA} 138: 38--51. 2016. (cit: 0; $7$)\n"
```


## Automated bibliometric reports

The package also contains a template `bibliometRics.Rtex` file, useful for creating automated reports.
You'll need to load the package `knitr` in order to produce the report.

```r
require(knitr)
```


```r
infile <- 'sbegueria.txt'
outfile <- 'sbegueria.Rtex'

# Create custom .Rtex file from the template and knit it
x <- readLines('bibliometRics.Rtex')
x <- gsub('FILENAME',infile,x)
write(x,outfile)
knit(outfile)

# Compile the resulting .tex file and create a .pdf from it
system(paste('/Library/TeX/texbin/pdflatex ',
	gsub('.txt','',infile),'.tex',sep=''))

# Remove unnecesary intermediate files
kk <- list.files('.',paste(gsub('.txt','', infile)))
file.remove(kk[-c(grep('.pdf',kk),grep('.txt',kk))])
```

An example report generated by the above code chunk can be found in the file [sbegueria.pdf]('./sbegueria.pdf').
