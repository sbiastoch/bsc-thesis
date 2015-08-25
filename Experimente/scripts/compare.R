library(stringr)
library(plotrix)

data_folder = '/home/sbiastoch/Schreibtisch/thesis/Experimente/csv/classifications/'
#data_folder = '~/Schreibtisch/thesis/Experimente/csv/classifications/'
folder = '/home/sbiastoch/Schreibtisch/thesis/Experimente/evaluation/'
#folder = '/home/sbiastoch/Schreibtisch/thesis/Experimente/evaluation/'

# Plottet die beste totalperformance je featureset in abhängigkeit zur durchschnittlichen anzahl an binären/numerischen attributen des feautesets
#xs=list(praefix=341.3, suffix=332.5, affix=673.8, phoncat=136.3, sonority=10, weight=23.5, phon=169.8, sylstruct=70, meta=19.6, sparse=130.5, numeric=33.5, all=936.6)
#ys=c(80.3, 79.2, 92.2, 83.2, 75.3, 74.7, 88, 85.3, 71.9, 91.4, 88.8, 96.2)
#data=rbind(xs,ys)
#sorted = data[,order(unlist(data['xs',]))]
#lines(sorted['xs',],sorted['ys',])
#plot(sorted['xs',],sorted['ys',])


pretty_name <- function(str) {
	shorter = gsub('_error', '', str)
	gsub('_', '/', shorter)
}

# Liefert die besten Modelle mit ihren Ergebnissen je Silbe sowie das gewichtete Gesamtergebnis aller Top-Models
performance <- function(table) {
	# liefert beste ergebnisse je silbenanzahl
	best_percent = apply(table[,3:ncol(table)],1, max)

	# liefert modelle, die die besten ergebnisse je silbe erreicht haben
	best_models = apply(table[,3:ncol(table)], 1, function(table) names(which.max(table)))
	
	# liefert die gewichtete gesamtperformance der besten modelle je silbenzahl
	total_top_performance = round(sum(best_percent * table['n'])/sum(table['n']),2)

	tbl = rbind(cbind(best_models,best_percent),c('',total_top_performance))
	matrix(tbl,,2, dimnames=list(c(2:(nrow(table)+1),'weighted'),c('best_model','result')))
}
#perf = performance(main_stats[c(1,2,grep('_',names(main_stats)))])
#barplot(as.numeric(perf[,'result']), names=c(perf[1:6,'best_model'],'weighted'), las=1,horiz=TRUE)

addLabels <- function(x, stat) {
	stat = as.numeric(stat)
	lbl <- paste(round(stat,1),'%',sep='')
	text(x, stat+1, labels=lbl, col="black", cex=0.7)
}

plot_bestmodels_stat <- function(models, main_stats) {

	filename = paste(folder,'bagging/bag_of_best_algorithms_for_featuresets.png',sep='')
	png(filename = filename, width = 1080, height = 500, units = "px", bg = "white", pointsize = 20)

	# liefert je featureset bestes kummuliertes ergebnis
	cummulativ_best_stat = matrix(unlist(lapply(models, function(model) {
		p = performance(main_stats[c(1,2,grep(model,names(main_stats)))])
		c(model,unlist(p['weighted','result']))
	}), recursive=FALSE),2)
	zeroR = sum(main_stats['n'] * main_stats['zeroR']) / sum(main_stats['n'])
	par(las=1,
	#	xpd=TRUE,
		cex.lab=0.9,
		cex.axis=0.9,
		cex.main=1,
		mar=c(5,3,3,4)
	)
	stat = matrix(cummulativ_best_stat[2,], 1, dimnames=list('',cummulativ_best_stat[1,]))
	x=barplot(stat, las=2, ylim=c(60,100), xpd=FALSE)
	par(xpd=TRUE)
	abline(h=zeroR,lty=2)
	text(max(x)+1.5,zeroR+1, paste('ZeroR (',round(zeroR,1),'%)',sep=''), cex=.8)
	addLabels(x, stat)
	title('Akkumulierte Erkennungsraten der besten Algorithmen je Featureset (%)')
	dev.off()
}

# 
plot_overview_stat <- function(main_stats, plot_title, colors, fileName) {

	filename = paste(folder,'featuresets/',fileName,sep='')
	png(filename = filename, width = 1080, height = 500, units = "px", bg = "white", pointsize = 20)

	par(#mfrow=c(3,1),
		las=1,
		xpd=TRUE,
		cex.lab=.7,
		cex.axis=.85,
		cex.main=1,
		mar=c(2,2,2,1),
		oma=c(4,1,0,0)
	)
	stats = main_stats[,3:ncol(main_stats)]
	lbls = pretty_name(names(stats))
	boxplot(stats,las=2, col=colors, names=lbls, ylim=c(30,100))
	title(plot_title)
	dev.off()
}
getStatSubset <- function(rexp) {
	main_stats[c(1,2,grep(rexp,names(main_stats)),length(main_stats))]
}

getNumberOfRules <- function(fileName) {
#	f = readChar(fileName, file.info(fileName)$size)
#	phrase = 'Number of Rules : '
#	NoR = gregexpr(pattern=phrase, f)[[1]][1] + nchar(phrase)
#	as.numeric(strsplit(substr(f,NoR,NoR+10),'\n')[[1]][1])
	length(getRules(fileName))+1
}


# ohne die letzte Regel!
getRules <- function(fileName) {
	f = readChar(fileName, file.info(fileName)$size)
	phrase = 'JRIP rules:\n===========\n'
	start = gregexpr(pattern=phrase, f)[[1]][1] + nchar(phrase)
	start_to_end = substr(f,start,nchar(f))
	end = gregexpr(pattern='\n =>', start_to_end)[[1]][1]
	rules_string = str_trim(substr(f,start,start+end))
	if(nchar(rules_string) == 0) {
		return(NULL)
	}
	rules = strsplit(rules_string,'\n')[[1]]
	rules
}

getTree <- function(fileName) {
	f = readChar(fileName, file.info(fileName)$size)
	phrase = 'J48 pruned tree\n------------------\n'
	start = gregexpr(pattern=phrase, f)[[1]][1] + nchar(phrase)
	start_to_end = substr(f,start,nchar(f))
	end = gregexpr(pattern='\nNumber of Leaves', start_to_end)[[1]][1] - 2
	rules_string = str_trim(substr(f,start,start+end))
	if(nchar(rules_string) == 0) {
		return(NULL)
	}
	rules = strsplit(rules_string,'\n')[[1]]
	rules
}

# Liefert die Tiefe jeder Regel im Entscheidungsbaum
getTreeDepths <- function(fileName) {
	tree = getTree(fileName)
	unlist(lapply(tree, function(branch) str_count(branch,'\\|')+1))
}

getMaxTreeDepth <- function(fileName) {
	depths = getTreeDepths(fileName)
	max(depths)
}

getTreeSize <- function(fileName) {
	depths = getTreeDepths(fileName)
	length(depths)
}

# ohne die letzte Regel!
getRulesLength <- function(fileName) {
	rules = getRules(fileName)
	unlist(lapply(rules, function(rule) {
		str_count(rule,' and ')+1
	}))
}

####
# boxplot für anzahl der regeln bei jrip
####


plot_analyze_j48 <- function(models, main_stats) {
	filename = paste(folder,'algorithms/J48.png',sep='')
	png(filename = filename, width = 640, height = 1080, units = "px", bg = "white", pointsize = 20)

	# boxplot der tiefe aller regeln von J48
	treedepth_stat = lapply(2:7, function(num_syls) {
		lapply(models, function(model) {
			fileName = paste(folder,'../trained_models/',num_syls,'syl/models-',model,'.txt',sep='')
			getTreeDepths(fileName)
		})
	})
	# boxplot der tiefe aller regeln von J48
	treesize_stat = lapply(2:7, function(num_syls) {
		lapply(models, function(model) {
			fileName = paste(folder,'../trained_models/',num_syls,'syl/models-',model,'.txt',sep='')
			getTreeSize(fileName)
		})
	})
	par(mfrow=c(3,1),
		las=1,
		xpd=TRUE,
		cex.lab=1,
		cex.axis=1.2,
		cex.main=1.2,
		mar=c(0,3,3,3),
		oma=c(7.5,1,1,1)
	)

	boxplot(lapply(1:12, function(mi) unlist(lapply(1:6, function(i) treedepth_stat[[i]][mi]))),xaxt='n')
	title('Tiefe der Blätter des J48-Decision-Trees')

	leaves_stat = lapply(1:12, function(mi) unlist(lapply(1:6, function(i) treesize_stat[[i]][mi])))
	boxplot(leaves_stat,xaxt='n')
	title('Anzahl der Blätter des J48-Decision-Trees')


	j48_stat = main_stats[c(1,2,grep('48',names(main_stats)),length(main_stats))]
	j48_cum_corr = apply(j48_stat, 2, function(col) sum(col*j48_stat[['n']])/sum(j48_stat[['n']]))
	stat = j48_cum_corr[4:length(j48_cum_corr)-1]
	names(stat) = pretty_name(names(stat))
	x = barplot(stat, ylim=c(84,96), las=2,xpd=FALSE)
	abline(h=j48_cum_corr['zeroR'],lty=2)
	text(max(x)+1.5,j48_cum_corr['zeroR']+.3, paste('ZeroR (',round(j48_cum_corr['zeroR'],1),'%)',sep=''), cex=.7)
	title('Erkennungsraten von J48 (%)')
	#addLabels(j48_cum_corr[3:13],x)

	dev.off()
}




# get the most signifcant rules
#rules = getRules('/home/sbiastoch/Schreibtisch/thesis/Experimente/trained_models/2syl/models-all.txt')
#lapply(rules, function(rule) {
#	start = gregexpr(pattern='a (', rule)[[1]][1]
#	substr(f,start,start+end)
#})

plot_analyze_jrip <- function(models, main_stats) {
	filename = paste(folder,'algorithms/JRip.png',sep='')
	png(filename = filename, width = 640, height = 1080, units = "px", bg = "white", pointsize = 20)

	syls = 2:7
	ruleslen_stat=lapply(syls, function(num_syls) {
		lapply(models, function(model) {
			fileName = paste(folder,'../trained_models/',num_syls,'syl/models-',model,'.txt',sep='')
			getRulesLength(fileName)
		})
	})

	par(mfrow=c(3,1),
		las=1,
		xpd=TRUE,
		cex.lab=1,
		cex.axis=1.2,
		cex.main=1.2,
		mar=c(0,3,3,3),
		oma=c(7.5,1,1,1)
	)
	colors = c('chartreuse1','brown1','chartreuse2','brown2','chartreuse3','brown3','chartreuse4','brown4')
	#library(vioplot)
	#lapply(seq_along(ruleslen_stat), function(stat_idx) {
	#	filename = paste(folder,'total/',num_syls,'syl-basicstats.png',sep='')
	#	png(filename = filename, width = 1920, height = 1080, units = "px", pointsize = 24, bg = "white")
	#	boxplot(ruleslen_stat[[stat_idx]],horiz=FALSE, col=rainbow(length(ruleslen_stat)), main=paste(stat_idx+1,'Silben'))
	#	dev.off()
	#})

	boxplot(lapply(1:12, function(mi) unlist(lapply(1:5, function(i) ruleslen_stat[[i]][mi]))),xaxt='n')
	title('Länge der von JRip generierten Regeln')


	boxplot(t(matrix(unlist(lapply(1:5,function(i) unlist(lapply(ruleslen_stat[[i]],length)))),12)),xaxt='n')
	title('Anzahl der von JRip generierten Regeln')

	jrip_stat = main_stats[c(1,2,grep('JRip',names(main_stats)),length(main_stats))]
	jrip_cum_corr = apply(jrip_stat, 2, function(col) sum(col*jrip_stat[['n']])/sum(jrip_stat[['n']]))
	stat = jrip_cum_corr[4:length(jrip_cum_corr)-1]
	names(stat) = pretty_name(names(stat))
	x=barplot(stat, ylim=c(84,96), las=2)
	abline(h=jrip_cum_corr['zeroR'],lty=2)
	text(max(x)+1.5,jrip_cum_corr['zeroR']+.3, paste('ZeroR (',round(jrip_cum_corr['zeroR'],1),'%)',sep=''), cex=.7)
	title('Erkennungsraten von JRip (%)')
	#addLabels(jrip_cum_corr[3:length(jrip_cum_corr)],x)

	dev.off()
}

main_stats = read.csv(paste(folder,'stats.csv',sep=''))

#models = c(
#	#'',
#	'NN','J48','JRip',
#	'all','sparse','numeric','phon_','affix',
#	'praefix','suffix','phoncat','sylstruct','weight','sonority','meta'
#	)

models = c(
	'all','sparse','numeric','phon','affix',
	'praefix','suffix','phoncat','sylstruct','weight','sonority','meta'
)

plot_bestmodels_stat(models, main_stats)

colors = c(replicate(6,c(replicate(3,'gray65'),replicate(3,'gray90'))),'gray100')
plot_overview_stat(main_stats, 'Erkennungsraten aller Featuresets (%)', colors, 'all.png')

colors = c(replicate(3,'gray50'),replicate(2, 'gray90'),'gray50',replicate(3,'gray90'),'gray50',replicate(2,'gray90'),'gray100')
plot_overview_stat(getStatSubset('NN'), 'Erkennungsraten aller NN (%)', colors, 'NN.png')
plot_overview_stat(getStatSubset('JRip'), 'Erkennungsraten aller JRip (%)', colors, 'JRip.png')
plot_overview_stat(getStatSubset('48'), 'Erkennungsraten aller J48 (%)', colors, 'J48.png')


plot_analyze_jrip(models, main_stats)
plot_analyze_j48(models, main_stats)






function() {
	syls = 2:7
	rules_stat=t(matrix(unlist(lapply(syls, function(num_syls) {
		matrix(unlist(lapply(jrip_rules_models, function(model) {
			fileName = paste(folder,'../trained_models/',num_syls,'syl/models-',model,'.txt',sep='')
			getNumberOfRules(fileName)
		})),length(jrip_rules_models))
	}),recursive=FALSE),,6,dimnames=list(jrip_rules_models,2:7)))
	write.csv(rules_stat, paste(folder,'number_of_rules.csv',sep=''))

	# liefert die einflussreichsten rules
	r=unlist(unlist(lapply(jrip_rules_models, function(model) { lapply(syls, function(syl) {
		fileName = paste(folder,'../trained_models/',syl,'syl/models-',model,'.txt',sep='')
		rules = getRules(fileName)
		lapply(rules, function(rule) {
			start_phrase = 'a \\('
			start = gregexpr(pattern=start_phrase, rule)[[1]][1] + nchar(start_phrase)-1
			start_to_end = substr(rule,start,nchar(rule)-1)
			ns = as.numeric(strsplit(start_to_end,'/')[[1]])
			list(corr=ns[1], err=ns[2], rule=rule)
		})
	}) }),recursive=FALSE), recursive=FALSE)
}

#models=c("all", "sparse", "numeric", "phon", "affix", "praefix", "suffix", "phoncat", "sylstruct", "weight", "sonority", "meta")
#par(mfrow=c(3,4))
folder = '/home/sbiastoch/Schreibtisch/thesis/Experimente/evaluation/'
models=c("praefix", "suffix", "phoncat", "sylstruct", "weight", "sonority")
par(mfrow=c(2,3),
	las=1,
	xpd=TRUE,
	cex.lab=1,
	cex.axis=1.2,
	cex.main=1.2,
	mar=c(0,3,3,3),
	oma=c(1,1,1,1))
lapply(models, function(model) {
	csv = read.csv(paste(folder,'../scripts/',model,'01',sep=''),sep='\t',skip=2, col.names=c('n','attr')) # jrip
#	csv = read.csv(paste(folder,'../scripts/',model,'00',sep=''),sep='\t',skip=2, col.names=c('n','attr')) # j48
	x=subset(csv,csv$n>0)
	pie(x[,1],labels=x[,2], main=model, radius=1.3)
	title('Verwendungshäufigkeit der Features bei JRip je Featureset', outer=TRUE)
	csv
})