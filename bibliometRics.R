# Functions to read and process ISI WOK data

read.isiwok <- function(infile) {

	# author
	au <- read.table(infile, sep=',', nrows=1, stringsAsFactors=FALSE)
	au <- paste(gsub('AUTHOR: \\(','',au[1]),gsub('\\)','',au[2]),sep=', ')
	au <- toupper(au)

	# reference
	re <- gsub('.txt','',rev(strsplit(infile,'/')[[1]])[1])

	# read publications
	nskip <- ifelse (read.table(infile,sep=',',nrows=1,skip=1)=='null', 5, 4)
	dat <- read.table(infile, sep=',', stringsAsFactors=FALSE, skip=nskip)
	colnames(dat) <- read.table(infile,sep=',',stringsAsFactors=FALSE,skip=nskip-1,nrows=1)
	o <- rev(order(dat[,'Publication Year']))
	dat <- dat[o,]
	start <- min(dat[,'Publication Year'])
	w <- which(colnames(dat)==start)
	dat <- dat[,c(1:21,w:ncol(dat))]

	return(list(author=au,reference=re,pubs=dat))
}
#bib <- read.isiwok('isiwok_MAM.txt')


# read.scopus <- function(file) {

	# # author
	# au <- read.table(file,sep=',',nrows=1,stringsAsFactors=FALSE)[,2]
	# au <- substr(au,9,nchar(au))

	# # read publications
	# dat <- read.table(file,sep=',',stringsAsFactors=FALSE,skip=7)[,c(1:7,9,12)]
	# colnames(dat) <- c('year','title','authors','issn','journal','volume','issue','cit','cittotal')

	# return()
# }
# bib <- read.scopus('scopus_SBP_win.csv')

# Hirsch's h-index
hirsch <- function(x,y=NULL) {
    x <- x$pubs
    if (is.null(y)) {
        #y <- max(x[,'Publication Year'])
        y <- rev(colnames(x))[1]
    }
    w <- which(x[,'Publication Year']<=y)
    ww <- which(colnames(x)<=y)
    if (length(ww)>1) {
        cit <- rowSums(x[w,ww])
    } else {
        cit <- x[w,ww]
    }
    #cit <- x[,'Total Citations']
	hin <- cbind(cit[order(cit,decreasing=TRUE)],1:length(cit))
	w <- (hin[,1]-hin[,2])>0
	if (sum(w)==0) return(0)
	return(max(hin[w,2]))
}
#hirsch(bib)
#hirsch(bib, 2010)


# Egghe's g-index
egghe <- function(x,y=NULL) {
    x <- x$pubs
    if (is.null(y)) {
		y <- max(x[,'Publication Year'])
	}
	w <- x[,'Publication Year']<=y
	cit <- x[w,'Total Citations']
	gin <- cbind(
		1:length(cit),
		cumsum(cit[order(cit,decreasing=TRUE)]),
		(1:length(cit))^2)
	w <- (gin[,2]-gin[,3])>0
	return(max(gin[w,1]))
}
#egghe(bib)

# citation ranking (quantiles)
rank <- function(x, w=NULL, q=quant) {
    x <- x$pubs
    if (!is.null(w)) {
        x <- x[w,]
    }
    r <- data.frame(a=x[,'Publication Year'],b=x[,'Total Citations'],c='')
	r$c <- factor(r$c,levels=c('>q0.9999','>q0.999','>q0.99','>q0.9','>q0.8',
		'>q0.5','>q0'))
	for (i in 1:nrow(r)) {
		w <- which(r[i,1]==q$year)
		if (length(w)==0) next()
		ww <- (r[i,2]-q[w,-1])>0
		if (sum(ww)==0) {
			r[i,3] <- '>q0'
		} else {
			r[i,3] <- paste('>',colnames(q[,-1])[ww][1],sep='')
		}
	}
	return(r[,3])
}
#rank(bib, c(9,10))
#rank(bib)

# impact factor
ifactor <- function(x, y=NULL, n=2) {
	year <- x[,'Publication Year']
	if (is.null(y)) {
		y <- max(year)-1
	}
	w <- which(year>={y-n} & year<y)
	ww <- which(colnames(x)==y)
	sum(x[w,ww]) / length(w)
}
#ifactor(dat) # 2-year impact factor
#ifactor(dat,2013) # 2-year impact factor, year 2013
#ifactor(dat,n=5)  # 5-year impact factor

bibliometric <- function(bib, quant) {

	au <- bib$author
	dat <- bib$pubs

	# publications
	pubs <- nrow(dat)

	# publications as lead author
	leads <- as.character(lapply(dat[,'Authors'],function(x) {strsplit(x,';')[[1]][1]}))
	auu <- gsub('[ñÑ]','N',strsplit(au,',')[[1]][1])
	pubs_lead <- grep(auu,leads,ignore.case=TRUE)
#	pubs_lead <- length(grep(auu,leads,ignore.case=TRUE))

	# impact factor
	ifact2 <- round(ifactor(dat, n=2), 2)
	ifact5 <- round(ifactor(dat, n=5), 2)

	# citations
	cit_tot <- sum(dat[,'Total Citations'])
	cit_max <- max(dat[,'Total Citations'])
	cit_art <- round(mean(dat[,'Total Citations']),2)
	ini <- min(dat[,'Publication Year'])
	span <- max(dat[,'Publication Year'])-ini
	
	# number of pubs with n citations
	icit <- function(x,n=10) {
		length(which(x[,'Total Citations']>=n))
	}
	i10 <- icit(dat)
	i25 <- icit(dat,25)
	i50 <- icit(dat,50)
	
	# h-index
	hin <- hirsch(bib)

	# g-index
	gin <- egghe(bib)

	# pubs >0.9, >0.99
	r <- rank(bib, q=quant)
	p09 <- sum(as.numeric(r)<=4,na.rm=TRUE)
	p099 <- sum(as.numeric(r)<=3,na.rm=TRUE)

	# pubs >0.9, as lead author
	r_lead <- rank(bib, pubs_lead, quant)
	p09_lead <- sum(as.numeric(r_lead)<=4,na.rm=TRUE)

	# i-score
	scores <- c(1/(1-0.9999),1/(1-0.999),1/(1-0.99),1/(1-0.9),1/(1-0.8),1/(1-0.5),1/1)
	iscore <- sum(scores[as.numeric(r)],na.rm=TRUE)
	
	# i-score, as lead author
	isc_lead <- sum(scores[as.numeric(r_lead)],na.rm=TRUE)

	# output
	out <- as.data.frame(t(c(name=au,
		ini,span,pubs,length(pubs_lead),round(pubs/span,2),
		hin,round(hin/span,2),gin,round(gin/span,2),
		cit_tot,round(cit_tot/span,2),cit_art,ifact2,ifact5,
		i10,i25,i50,cit_max,p09,p09_lead,p099,round(iscore,2),round(isc_lead,2))))
	colnames(out) <- c('name','ini','years','pubs','lead','pubs_year','hin','hin_year',
		'gin','gin_year','cit_tot','cit_year','cit_art','ifact2','ifact5','i10','i25',
		'i50','cit_max','pubs09','pubs09_lead','pubs099','iscore','iscore_lead')
	return(out)
}
#bibliometric(bib)

biblioplot <- function(bib) {

	par(mfrow=c(1,3))

	au <- bib$author
	dat <- bib$pubs

	# Publication range
	pubrange <- range(dat[,'Publication Year'])

	# Leading authors
	leads <- as.character(lapply(dat[,'Authors'],function(x) {strsplit(x,';')[[1]][1]}))
	w_lead <- grep(strsplit(gsub('Ñ','N',au),',')[[1]][1], leads, ignore.case=TRUE)

	# Publications and citations
	# pubs per year
	years <- dat[,'Publication Year']
	years <- factor(years,levels=as.character(min(years):max(years)))
	years_nolead <- years[-w_lead]
	years_lead <- years[w_lead]
	pubs <- rbind(table(years_lead),table(years_nolead))
	rownames(pubs) <- c('pubs_lead','pubs_nolead')
	# cumulative pubs
	pubs_cum <- t(apply(pubs,1,cumsum))
	# citations per year
	cits_lead <- t(as.matrix(colSums(bib$pubs[w_lead,22:ncol(bib$pubs)])))
	cits_nolead <- t(as.matrix(colSums(bib$pubs[-w_lead,22:ncol(bib$pubs)])))
	cits <- rbind(cits_lead,cits_nolead)
	rownames(cits) <- c('cits_lead','cits_nolead')
	w <- colnames(cits)>=pubrange[1] & colnames(cits)<=pubrange[2]
	cits <- cits[,w]
	# cumulative citations
	cits_cum <- t(apply(cits,1,cumsum))
	# plot
	par(mar=c(5, 4, 4, 4)+0.1)
	ylabs <- pretty(c(0,max(max(colSums(pubs_cum)),ceiling(max(colSums(cits_cum))/10))))
	ylims <- c(min(ylabs),max(ylabs))
	pubbar <- barplot(pubs_cum,ylim=ylims,ylab='',yaxt='n')
	abline(h=ylabs,col='lightgray',lty='dotted')
	par(new=TRUE)
	pubbar <- barplot(pubs_cum,ylim=ylims,ylab='',yaxt='n')
	lines(x=pubbar,y=cits_cum['cits_lead',]/10)
	points(x=pubbar,y=cits_cum['cits_lead',]/10,pch=19)
	lines(x=pubbar,y=colSums(cits_cum)/10)
	points(x=pubbar,y=colSums(cits_cum)/10,pch=19,col='lightgray')
	points(x=pubbar,y=colSums(cits_cum)/10,pch=21)
	axis(2)
	axis(4,at=pretty(ylims),labels=pretty(ylims)*10)
	mtext('Pubs',2,line=2.5,cex=0.75)
	mtext('Citations',4,line=2.5,cex=0.75)

	# h- and g-index
	years <- c(min(dat[,'Publication Year']):max(dat[,'Publication Year']))
	hh <- NULL
	gg <- NULL
	for (y in years) {
		hh <- c(hh,hirsch(bib,y))
		gg <- c(gg,egghe(bib,y))
	}
	# plot
	ylabs <- pretty(c(0,max(max(hh),ceiling(max(gg)/2))))
	ylims <- c(min(ylabs),max(ylabs))
	plot(x=years,y=hh,type='n',ylab='',xlab='',ylim=ylims)
	abline(h=pretty(hh),col='lightgray',lty='dotted')
	lines(years,0:{length(years)-1},lty='dashed')
	lines(x=years,y=hh)
	points(x=years,y=hh,pch=19)
	mtext('h-index',2,line=2.5,cex=0.75)
	par(new=TRUE)
	lines(x=years,y=gg/2)
	points(x=years,y=gg/2,pch=19,col='lightgray')
	points(x=years,y=gg/2,pch=21)
	axis(4,at=pretty(ylims),labels=pretty(ylims)*2)
	mtext('g-index',4,line=2.5,cex=0.75)

	# Excelence
	exc <- rbind(rev(table(rank(bib, w_lead))),rev(table(rank(bib, -w_lead))))[,-7]
	rownames(exc) <- c('pubs_lead','pubs_nolead')
	# plot
	barplot(exc)
	abline(h=pretty(colSums(exc)),col='lightgray',lty='dotted')
	par(new=TRUE)
	barplot(exc)
	mtext('Counts',2,line=2.5,cex=0.75)
}
#biblioplot(bib)

biblioplot2 <- function(bib) {

	par(mfrow=c(1,2))

	au <- bib$author
	dat <- bib$pubs

	# Publication range
	pubrange <- range(dat[,'Publication Year'])

	# Leading authors
	leads <- as.character(lapply(dat[,'Authors'],function(x) {strsplit(x,';')[[1]][1]}))

	# Publications and citations
	# pubs per year
	years <- dat[,'Publication Year']
	years <- factor(years,levels=as.character(min(years):max(years)))
	pubs <- table(years)
	# cumulative pubs
	pubs_cum <- cumsum(pubs)
	# citations per year
	cits <- t(as.matrix(colSums(bib$pubs[,22:ncol(bib$pubs)])))
	rownames(cits) <- c('cits')
	w <- colnames(cits)>=pubrange[1] & colnames(cits)<=pubrange[2]
	cits <- cits[,w]
	# cumulative citations
	cits_cum <- cumsum(cits)
	# plot
	par(mar=c(5, 4, 4, 4)+0.1)
	ylabs <- pretty(c(0,max(max(pubs_cum),ceiling(max(cits_cum)/10))))
	ylims <- c(min(ylabs),max(ylabs))
	pubbar <- barplot(pubs_cum,ylim=ylims,ylab='',yaxt='n')
	abline(h=ylabs,col='lightgray',lty='dotted')
	par(new=TRUE)
	pubbar <- barplot(pubs_cum,ylim=ylims,ylab='',yaxt='n')
	lines(x=pubbar,y=cits_cum/10)
	points(x=pubbar,y=cits_cum/10,pch=19)
	axis(2)
	axis(4,at=pretty(ylims),labels=pretty(ylims)*10)
	mtext('Pubs',2,line=2.5,cex=0.75)
	mtext('Citations',4,line=2.5,cex=0.75)
	title('Publications and citations')
	
	# h-index
	years <- c(min(dat[,'Publication Year']):max(dat[,'Publication Year']))
	hh <- NULL
	for (y in years) {
	    hh <- c(hh,hirsch(bib,y))
	    gg <- c(gg,egghe(bib,y))
	}
	# plot
	ylabs <- pretty(c(0,max(max(hh))))
	ylims <- c(min(ylabs),max(ylabs))
	plot(x=years,y=hh,type='n',ylab='',xlab='',ylim=ylims)
	abline(h=pretty(hh),col='lightgray',lty='dotted')
	lines(years,0:{length(years)-1},lty='dashed')
	lines(x=years,y=hh)
	points(x=years,y=hh,pch=19)
	mtext('h-index',2,line=2.5,cex=0.75)
	title('h-index')
}


format_pub <- function(x,au) {
	# format authors
	if (substr(x['Authors'],nchar(x['Authors']),nchar(x['Authors']))!='.') {
		x['Authors'] <- paste(x['Authors'],'.',sep='')
	}
	auths <- strsplit(x['Authors'],';')[[1]]
	w <- grep(strsplit(gsub('Ñ','N',au),',')[[1]][1],auths,ignore.case=TRUE)
	auths[w] <- paste('\\textbf{',auths[w],'}',sep='')
	auths <- paste0(auths,sep=';',collapse='')
	auths <- substr(auths,1,nchar(auths)-1)
	# format citations rank
	if (is.na(x['rank(bib, q = quant)']) | x['rank(bib, q = quant)']=='') {
		rank <- ''
	} else {
		rank <- paste('; $',x['rank(bib, q = quant)'],'$',sep='')
	}
	# format title
	title <- gsub('&','\\\\&',x['Title'])
	# format source
	source <- gsub('&','\\\\&',x['Source Title'])
	# format volume and pages
	if (is.na(x['Volume'])) {
		vol <- ''
	} else {
		vol <- paste(' ',x['Volume'],sep='')
	}
	if (is.na(x['Issue']) | x['Issue']=='') {
		issue <- ''
	} else {
		issue <- paste('(',x['Issue'],')',sep='')
	}
	if (is.na(x['Beginning Page']) | x['Beginning Page']=='' |
		is.na(x['Ending Page']) | x['Ending Page']=='') {
		if (is.na(x['Article Number']) | x['Article Number']=='') {
			pages <- ''
		} else {
			pages <- paste(': ', x['Article Number'],sep='')
		}
	} else {
		pages <- paste(': ',x['Beginning Page'],'--',x['Ending Page'],sep='')
	}
	# put everything together
	paste('\\item ',auths,' ',title,'. ',
		'\\textit{',source,'}',vol,issue,pages,'. ',
		x['Publication Year'],'. ',
		'(cit: ',x['Total Citations'],rank,')\n', sep='')
}

