# bibliometRics
Santiago Beguería  
26 April 2016  
    
`bibliometRics` is an R package for bibliometric analysis of scientific production.
It can be used for analysing the production of a single author, a working team, department, institute, etcetera.

This document describes the main functionalities in the package, and how to do a bibliometric analysis with `bibliometRics`, including producing automatic pdf reports via `knitr`.

First of all, make sure you installed the package by sourcing it (note that this is a working project, so no 'oficial' package has been created yet).
You can download the source code from this [link]('bibliometRics.R').


```r
source('bibliometRics.R')
```


# Getting some bibliometric data

So far the unique source of blibiometric information accepted by `bibliometRics` is the ISI Web of Knowledge, but other sources can be added.
It's easy to select the publications of a given author on the ISI-WoK if you know its author ID.
You can also make a selection of articles (for example, for all the members of a research group) and use the 'add to marked list' option to create a custom publication list.
Once you are good with your selection, click on the 'create citation report' button, then select the 'save to text file' option and specify the full range of records.
This will generate a text file and download it to your computer.
You may want to edit the AUTHOR field on the text file, although this is not strictly necessary for doing the analysis.

One example of bibliometric information that can be downloaded from ISI-WoK is the file [sbegueria.txt](sbegueria.txt), which contains data of papers I have authored, as of January 2015.

The core function for reading ISI-WoK data is, not surprisingly, `read.isiwok`. 
Its only argument is the name of the data file to read:


```r
bib <- read.isiwok('sbegueria.txt')
str(bib)
```

```
## List of 3
##  $ author   : chr "BEGUERIA,  SANTIAGO"
##  $ reference: chr "sbegueria"
##  $ pubs     :'data.frame':	80 obs. of  38 variables:
##   ..$ Title            : chr [1:80] "Climate Change Impact for Spatial Landslide Susceptibility" "Detachment of soil organic carbon by rainfall splash: Experimental assessment on three agricultural soils of Spain" "Reply to the comment on Rainfall erosivity in Europe by Auerswald et al." "Drought Variability and Land Degradation in Semiarid Regions: Assessment Using Remote Sensing Data and Drought Indices (1982-20"| __truncated__ ...
##   ..$ Authors          : chr [1:80] "Gassner, Christine; Promper, Catrin; Begueria, Santiago; Glade, Thomas" "Begueria, Santiago; Angulo-Martinez, Marta; Gaspar, Leticia; Navas, Ana" "Panagos, Panos; Meusburger, Katrin; Ballabio, Cristiano; Borrelli, Pasquale; Begueria, Santiago; Klik, Andreas; Rymszewicz, Ann"| __truncated__ "Vicente-Serrano, Sergio M.; Cabello, Daniel; Tomas-Burguera, Miquel; Martin-Hernandez, Natalia; Begueria, Santiago; Azorin-Moli"| __truncated__ ...
##   ..$ Corporate Authors: logi [1:80] NA NA NA NA NA NA ...
##   ..$ Editors          : logi [1:80] NA NA NA NA NA NA ...
##   ..$ Book Editors     : chr [1:80] "Lollino, G; Manconi, A; Clague, J; Shan, W; Chiarle, M" "" "" "" ...
##   ..$ Source Title     : chr [1:80] "ENGINEERING GEOLOGY FOR SOCIETY AND TERRITORY, VOL 1: CLIMATE CHANGE AND ENGINEERING GEOLOGY" "GEODERMA" "SCIENCE OF THE TOTAL ENVIRONMENT" "REMOTE SENSING" ...
##   ..$ Publication Date : chr [1:80] "2015" "MAY 2015" "NOV 1 2015" "APR 2015" ...
##   ..$ Publication Year : int [1:80] 2015 2015 2015 2015 2015 2015 2015 2015 2014 2014 ...
##   ..$ Volume           : chr [1:80] "" "245" "532" "7" ...
##   ..$ Issue            : chr [1:80] "" "" "" "4" ...
##   ..$ Part Number      : logi [1:80] NA NA NA NA NA NA ...
##   ..$ Supplement       : logi [1:80] NA NA NA NA NA NA ...
##   ..$ Special Issue    : chr [1:80] "" "" "" "" ...
##   ..$ Beginning Page   : int [1:80] 429 21 853 4391 160 42 249 801 NA 3001 ...
##   ..$ Ending Page      : int [1:80] 433 30 857 4423 173 54 262 814 NA 3023 ...
##   ..$ Article Number   : chr [1:80] "" "" "" "" ...
##   ..$ DOI              : chr [1:80] "10.1007/978-3-319-09300-0_82" "10.1016/j.geoderma.2015.01.010" "10.1016/j.scitotenv.2015.05.020" "10.3390/rs70404391" ...
##   ..$ Conference Title : chr [1:80] "12th International IAEG Congress" "" "" "" ...
##   ..$ Conference Date  : chr [1:80] "SEP 15-19, 2014" "" "" "" ...
##   ..$ Total Citations  : int [1:80] 0 1 1 2 2 2 5 15 0 15 ...
##   ..$ Average per Year : num [1:80] 0 0.5 0.5 1 1 1 2.5 7.5 0 5 ...
##   ..$ 2000             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2001             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2002             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2003             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2004             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2005             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2006             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2007             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2008             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2009             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2010             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2011             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2012             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2013             : int [1:80] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ 2014             : int [1:80] 0 0 0 0 0 0 2 0 0 1 ...
##   ..$ 2015             : int [1:80] 0 0 0 2 2 2 2 11 0 11 ...
##   ..$ 2016             : int [1:80] 0 1 1 0 0 0 1 4 0 3 ...
```

The result is a list with three elements: `author`, `reference`, and `pubs`.
The latter is a data.frame with the publications in rows, and the data referring to each publications in columns, including the number of citations received, year by year.


# Analyzing bibliometric data

The main function for analyizing these data is `bibliometric`.
It takes an object with the bibliometric data resulting from a call to `read.isiwok` and returns a data.frame with a number of bibliometric indices.


```r
bibliometric(bib)
```

```
##                  name  ini years pubs lead pubs_year hin hin_year gin
## 1 BEGUERIA,  SANTIAGO 2000    15   80   16      5.33  28     1.87  48
##   gin_year cit_tot cit_year cit_art ifact2 ifact5 i10 i25 i50 cit_max
## 1      3.2    2587   172.47   32.34   5.23   9.26  59  37  13     376
##   pubs09 pubs09_lead pubs099 iscore iscore_lead
## 1     32           8       8   5631        1089
```

These are:

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

```
## [1] 28
```

```r
egghe(bib)
```

```
## [1] 48
```

```r
hirsch(bib, 2010)
```

```
## [1] 13
```

```r
egghe(bib, 2010)
```

```
## [1] 44
```

There is also a function for ranking the publications in quantiles according to the ISI-WoK Science Indicators tables.
These need to be loaded as a table named [quantiles.csv]('./quantiles.csv'), located in the same directory as the data.


```r
rank(bib)
```

```
##  [1] <NA>    <NA>    <NA>    <NA>    <NA>    <NA>    <NA>    <NA>   
##  [9] >q0     >q0.999 >q0.999 >q0     >q0.9   >q0.99  >q0.999 >q0    
## [17] >q0     >q0     >q0.5   >q0.5   >q0.9   >q0.9   >q0.9   >q0.99 
## [25] >q0     >q0     >q0.5   >q0.5   >q0.9   >q0.9   >q0.9   >q0.9  
## [33] >q0.9   >q0.9   >q0.999 >q0     >q0.5   >q0.8   >q0.9   >q0.9  
## [41] >q0.9   >q0.9   >q0.9   >q0.99  >q0.999 >q0.5   >q0.8   >q0.8  
## [49] >q0.8   >q0.8   >q0.8   >q0.9   >q0.9   >q0.9   >q0     >q0.5  
## [57] >q0.8   >q0.8   >q0.8   >q0.8   >q0.5   >q0.5   >q0.8   >q0.9  
## [65] >q0.9   >q0.9   >q0.9   >q0     >q0.8   >q0.9   >q0     >q0.5  
## [73] >q0.9   <NA>    <NA>    <NA>    <NA>    <NA>    <NA>    <NA>   
## Levels: >q0.9999 >q0.999 >q0.99 >q0.9 >q0.8 >q0.5 >q0
```

```r
table(rank(bib))
```

```
## 
## >q0.9999  >q0.999   >q0.99    >q0.9    >q0.8    >q0.5      >q0 
##        0        5        3       24       12       10       11
```

A specific plotting function makes it easy to resume most of this information in graphic form.
The plots also inform about the temporal evolution of the bibliometric indices, which may be useful for evaluating the scientific career of the evaluated.


```r
biblioplot(bib)
```

<div class="figure" style="text-align: center">
<img src="analysis_files/figure-html/unnamed-chunk-6-1.png" alt="A graphic bibliometric analysis"  />
<p class="caption">A graphic bibliometric analysis</p>
</div>

The first plot reflects the productivity (quantity) of the author, as well as its impact (citations received). It shows the cumulative number of publications, with distinction between the publications as lead author (black bars) and those as co-author (white bars). The plot also showcases the number of citations for all the publications (white circles) and for those as lead author (black circles). There is a fixed ratio of 1/10 between the left (publications) and the right (citations) axis, allowing for easy comparison between different authors, groups, etc.

The second plot focuses on the impact of the publications. It shows the annual evolution of the Hirsch's h-index (black circles) and the Egghe's g-index (black circles), with a fixed ratio of 1/2 between them. Evolution of the h-index is compared with an 1:1 evolution (dashed line), since it is usually assumed that the h-index grows, as an average, at a rate of 1 per year.

The third plot attempts at evaluating the excelence of the publications. It shows the number of publications classified by quantiles, according to the ISI-WoK Scientific Indicators per discipline. For each quantile, the total number of publications is shown (white bars), as well as the publications as lead author (black bars).


```r
#format_pub(bib$pubs[1], au=bib$au)
apply(cbind(bib$pubs,rank(bib$pubs)),1,format_pub,au=bib$au)
```


# Automated bibliometric reports

The package also contains a template Rtex file, useful for creating automated reports.


```r
require(knitr)

infile <- 'sbegueria.txt'
outfile <- 'sbegueria.Rtex'

# Create custom .Rtex file from the template and knit it
x <- readLines('template.Rtex')
x <- gsub('FILENAME',infile,x)
write(x,outfile)
knit(outfile)

# Compile the resulting .tex file and create a .pdf from it
system(paste('/Library/TeX/texbin/pdflatex ',
	gsub('.txt','',f),'.tex',sep=''))

# Remove all intermediate files
kk <- list.files('.',paste(gsub('.txt','',f)))
file.remove(kk[-c(grep('.pdf',kk),grep('.txt',kk))])
```

