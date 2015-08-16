#######
###### Barplot für Fehler und richtige Klassifizierungen, jeweils segmentiert in Unique errors und common errors, gleiches für korrekte klassifizierungen
#
#							sylstruct_JRip 		sylstruct_J48 	...
#		unique_errors	
#		common_errors
#		other_errors
#		unique_correct
#		other_correct
#		common_correct

match_row <- function(rows, filter) {
	sum(apply(rows, 1, function(row) all(row == filter)))
}

# sucht in einer matrix nach zeilen die bestimmte werte enthalten
# filters = c('yes','yes') liefert alle Zeilen die mit y/y beginnen
# filter = c(label42='yes') liefert alle Zeilen mit label42=yes
find_row <- function(rows, filters) {
	result = rows
	lapply(1:length(filters), function(filter_i) {
		if(is.null(names(filters[filter_i])))
					result <<- subset(result, result[filter_i] == filters[filter_i])
				else
					result <<- subset(result, result[names(filters[filter_i])] == filters[filter_i])
	})
	return(result)
}

# neg=FALSE liefert n-1 mal "no" und 1 mal "yes"
shifted_vector <- function(n, neg=FALSE) {
	lapply(1:n, function(i) {
		x=replicate(n, if(neg) 'yes' else 'no')
		x[i]=if(neg) 'no' else 'yes'
		x
	})
}

uniques <- function(rows, correct=FALSE) {
	x = shifted_vector(ncol(rows), correct)
	ret = matrix(lapply(x, function(filter) {
		sum(apply(rows,1,function(row) all(row==filter)))
	}),,ncol(rows))
	dimnames(ret) = list(if(correct) 'unique_correct' else 'unique_error',colnames(rows))
	ret
}

# Anzahl der richtigen Klassifizierungen eines Models, das auch von der Mehrheit richtig klassifiziert worden ist
# FALSE FALSE liefert Zeilen mit korrekter Klassifizierung, die auch Mehrheit richtig klassifiziert hat
# FALSE TURE liefert Zeilen mit korrekter Klassifizierung, die die Mehrheit nicht richtig klassifiziert hat
# TRUE FALSE liefert Zeilen mit falscher Klassifizierung, die die Mehrheit jedoch richtig klassifiziert hat
# TRUE TRUE liefert Zeilen mit falscher Klassifizierung, die die Mehrheit jedoch auch falsch gemacht hat
majority <- function(results, error=FALSE, minority=FALSE) {
	apply(results, 2, function(col) {
		target_value = if(error) 'yes' else 'no'
		correct_rows = subset(results, col == target_value)
		correct_per_row = apply(correct_rows == target_value, 1, sum)
		pivot = length(results) / 2
		length(correct_per_row[if(minority) correct_per_row < pivot else correct_per_row >= pivot])
	})
}

compare <- function(results, num_syls, folder) {
	unique_errors = uniques(results)							# anzahl der zeilen mit genau einem NO in der Spalte von MODEL
	unique_correct = uniques(results, correct=TRUE)				# anzahl der zeilen mit genau einem YES in der Spalte von MODEL
	common_errors_value = match_row(results, replicate(ncol(results), 'yes'))		# anzahl der zeilen mit NO, bei der alle anderen MODELs ein YES haben
	common_errors = matrix(replicate(length(unique_errors),common_errors_value),1, dimnames=list('common_errors',colnames(results)))
	common_correct_value = match_row(results, replicate(ncol(results), 'no'))		# anzahl der zeilen mit YES, bei der alle anderen MODELs ein NO haben
	common_correct = matrix(replicate(length(unique_correct),common_correct_value),1, dimnames=list('common_correct',colnames(results)))

	errors_per_row = apply(results, 2, function(col) {
		unlist(
			apply(subset(results,col == 'yes'), 1, function(row) {
				sum(row == 'yes')
			}))
	})
	correct_per_row = apply(results, 2, function(col) {
		unlist(
			apply(subset(results,col == 'no'), 1, function(row) {
				sum(row == 'no')
			}))
	})

	other_errors = lapply(errors_per_row, function(ers) {
		length(ers[ers > 1 & ers < length(errors_per_row)])
	})
	other_correct = lapply(correct_per_row, function(cors) {
		length(cors[cors > 1 & cors < length(correct_per_row)])
	})
	stat = rbind(
		unique_correct, 	common_correct,		other_correct,
		unique_errors,		common_errors,		other_errors
	)
	olddimnames = dimnames(stat)
	stat = 100*apply(stat, 2, as.numeric)/nrow(results)
	dimnames(stat) = olddimnames
	return(stat)

}

plot_majorites <- function(results, num_syls, folder) {
	majority_correct = majority(results, error=FALSE, minority=FALSE)
	minority_correct = majority(results, error=FALSE, minority=TRUE)
	majority_error = majority(results, error=TRUE, minority=FALSE)
	minority_error = majority(results, error=TRUE, minority=TRUE)
	stat = rbind(
		100*majority_correct/nrow(results), 	100*minority_correct/nrow(results),
		100*majority_error/nrow(results), 	100*minority_error/nrow(results)
	)

	return(stat)
}


compare_jrips <- function (results, num_syls, folder) {
	jrips = (jrips = results[grep("JRip", names(results))])[grep("error", names(jrips))]

	unique_errors = t(unlist(lapply(seq_along(jrips), function(i) {
		nrow( jrips[  jrips$error_all_JRip		==	(if(i == 1) 'yes' else 'no')
					& jrips$error_phon_JRip		==	(if(i == 2) 'yes' else 'no')
					& jrips$error_affix_JRip	==	(if(i == 3) 'yes' else 'no')
					& jrips$error_sylstruct_JRip==	(if(i == 4) 'yes' else 'no')
				   ,]
		)
	})))
	dimnames(unique_errors)=list('unique errors',names(jrips))

	unique_correct = t(unlist(lapply(seq_along(jrips), function(i) {
		nrow( jrips[  jrips$error_all_JRip		==	(if(i == 1) 'no' else 'yes')
					& jrips$error_phon_JRip		==	(if(i == 2) 'no' else 'yes')
					& jrips$error_affix_JRip	==	(if(i == 3) 'no' else 'yes')
					& jrips$error_sylstruct_JRip==	(if(i == 4) 'no' else 'yes')
				   ,]
		)
	})))
	dimnames(unique_correct)=list('unique correct',names(jrips))
	
	common_errors = t(unlist(lapply(seq_along(jrips), function(i) {
		nrow( jrips[  jrips$error_all_JRip		==	'yes'
					& jrips$error_phon_JRip		==	'yes'
					& jrips$error_affix_JRip	==	'yes'
					& jrips$error_sylstruct_JRip==	'yes'
				   ,]
		)
	})))
	dimnames(common_errors)=list('common errors',names(jrips))
	
	common_correct = t(unlist(lapply(seq_along(jrips), function(i) {
		nrow( jrips[  jrips$error_all_JRip		==	'no'
					& jrips$error_phon_JRip		==	'no'
					& jrips$error_affix_JRip	==	'no'
					& jrips$error_sylstruct_JRip==	'no'
				   ,]
		)
	})))
	dimnames(common_correct)=list('common errors',names(jrips))

	total_errors = t(unlist(t(lapply(jrips, function(x) sum(x=='yes')))))
	dimnames(total_errors) = list('total errors',names(jrips))

	total_correct = t(unlist(t(lapply(jrips, function(x) sum(x=='no')))))
	dimnames(total_correct) = list('total correct',names(jrips))

	merged_errors = rbind(total_correct - unique_correct - common_correct, unique_correct, common_correct, total_errors - unique_errors - common_errors, common_errors, unique_errors)
	dimnames(merged_errors) = list(c('other correct', 'unique_correct','common correct','other errors', 'common errors', 'unique_errors'),names(jrips))


#	merged_errors = rbind(total_correct, total_errors)
#	dimnames(merged_errors) = list(c('other correct','other errors'),names(jrips))

	par(las=1) # make label text perpendicular to axis
	par(mar=c(4,7,2,9)) # increase y-axis margin.
	par(xpd=TRUE)

	colors = c('chartreuse1','chartreuse3','chartreuse4','brown1','brown3','brown4')
	barplot(
		merged_errors, 
		horiz=TRUE,
		cex.names=0.8,legend.text = TRUE, args.legend = list(x = 'topright', bty = "n", inset = c(-0.45,0)), col=colors, main=paste(num_syls,' Silben'))
	savePlot(paste(folder,num_syls,'-syl-jrips.png',sep=''))

}

unique_errors <- function (results, num_syls, folder) {
	jrips = results[grep("error", names(results))]

	unique_errors = t(unlist(lapply(seq_along(jrips), function(i) {
		nrow( jrips[  jrips$error_all_J48		==	(if(i == 1) 'yes' else 'no')
					& jrips$error_all_JRip		==	(if(i == 2) 'yes' else 'no')
					& jrips$error_all_NN		==	(if(i == 3) 'yes' else 'no')
					& jrips$error_phon_J48		==	(if(i == 4) 'yes' else 'no')
					& jrips$error_phon_JRip		==	(if(i == 5) 'yes' else 'no')
					& jrips$error_phon_NN		==	(if(i == 6) 'yes' else 'no')
					& jrips$error_affix_J48		==	(if(i == 7) 'yes' else 'no')
					& jrips$error_affix_JRip	==	(if(i == 8) 'yes' else 'no')
					& jrips$error_affix_NN		==	(if(i == 9) 'yes' else 'no')
					& jrips$error_sylstruct_J48	==	(if(i == 10) 'yes' else 'no')
					& jrips$error_sylstruct_JRip==	(if(i == 11) 'yes' else 'no')
					& jrips$error_sylstruct_NN	==	(if(i == 12) 'yes' else 'no')
				   ,]
		) / nrow(jrips)
	})))
	dimnames(unique_errors)=list('unique errors',names(jrips))
#	names = c('error_phon_J48','error_sylstruct_J48','error_affix_J48','error_phon_JRip','error_sylstruct_JRip','error_affix_JRip','error_phon_NN','error_sylstruct_NN','error_affix_NN')
#	dimnames(unique_errors) = list(c('unique errors'),names)
	par(las=2) # make label text perpendicular to axis
	par(mar=c(10,3,2,2)) # increase y-axis margin.
	barplot(unique_errors)
	savePlot(paste(folder,'unique-errors/',num_syls,'syl.png',sep=''))


	unique_correct = t(unlist(lapply(seq_along(jrips), function(i) {
		nrow( jrips[  jrips$error_all_J48		==	(if(i == 1) 'no' else 'yes')
					& jrips$error_all_JRip		==	(if(i == 2) 'no' else 'yes')
					& jrips$error_all_NN		==	(if(i == 3) 'no' else 'yes')
					& jrips$error_phon_J48		==	(if(i == 4) 'no' else 'yes')
					& jrips$error_phon_JRip		==	(if(i == 5) 'no' else 'yes')
					& jrips$error_phon_NN		==	(if(i == 6) 'no' else 'yes')
					& jrips$error_affix_J48		==	(if(i == 7) 'no' else 'yes')
					& jrips$error_affix_JRip	==	(if(i == 8) 'no' else 'yes')
					& jrips$error_affix_NN		==	(if(i == 9) 'no' else 'yes')
					& jrips$error_sylstruct_J48	==	(if(i == 10) 'no' else 'yes')
					& jrips$error_sylstruct_JRip==	(if(i == 11) 'no' else 'yes')
					& jrips$error_sylstruct_NN	==	(if(i == 12) 'no' else 'yes')
				   ,]
		) / nrow(jrips)
	})))	
	dimnames(unique_correct)=list('unique correct',names(jrips))
#	names = c('error_phon_J48','error_sylstruct_J48','error_affix_J48','error_phon_JRip','error_sylstruct_JRip','error_affix_JRip','error_phon_NN','error_sylstruct_NN','error_affix_NN')
#	dimnames(unique_correct) = list(c('unique correct'),names)
	par(las=2) # make label text perpendicular to axis
	par(mar=c(10,3,2,2)) # increase y-axis margin.
	barplot(unique_correct,legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
	savePlot(paste(folder,'unique-correct/',num_syls,'syl.png',sep=''))

	total_errors = t(unlist(t(lapply(jrips, function(x) sum(x=='yes')/nrow(jrips)))))
#	dimnames(other_errors) = list(c('other errors'),names)

	total_correct = t(unlist(t(lapply(jrips, function(x) sum(x=='no')/nrow(jrips)))))
#	dimnames(correct) = list(c('correct'),names)

	merged_errors = rbind(total_correct - unique_correct, unique_correct, total_errors - unique_errors, unique_errors)
	dimnames(merged_errors) = list(c('correct', 'unique correct', 'error', 'unique error'),names(jrips))
#	merged_errors = rbind(unique_errors, unique_correct, other_errors - unique_errors)
#	names = c('error_phon_J48','error_sylstruct_J48','error_affix_J48','error_phon_JRip','error_sylstruct_JRip','error_affix_JRip','error_phon_NN','error_sylstruct_NN','error_affix_NN')
#	dimnames(merged_errors) = list(c('unique errors','unique_correct', 'other errors'),names)
#	dimnames(merged_errors) = list(c('unique errors', 'other errors', 'correct', 'unique_correct'),names)

	par(las=1) # make label text perpendicular to axis
	par(mar=c(4,7,2,8)) # increase y-axis margin.

	barplot(merged_errors,horiz=TRUE,cex.names=0.8, legend.text = TRUE, args.legend = list(x = 1.4, bty = "n"), col=c('green', 'blue','yellow', 'red'))
	savePlot(paste(folder,num_syls,'syl-aggregated.png',sep=''))

}


addLabels <- function(y, stat) {
	x <- 0
	one_percent = sum(unlist(stat[,1]))/100
	up <- FALSE
	apply(stat, 1, function(row) {
		row = unlist(row)
		lbl <- paste(round(row,1),'%',sep='')
		lbl <- unlist(lapply(1:length(row), function(i) if((row[i]/one_percent) < 2) '' else lbl[i]))
		text(x+(.5*row), y, labels=lbl, col="black", cex=0.62)
		x <<- x + row
	})
}

basic_stats <- function(results, num_syls, folder) {
	x = apply(results,2,table)
	return(x)
}

plot_basic_stats <- function(results, zeroR, num_syls, folder) {
	filename = paste(folder,'total/',num_syls,'syl-basicstats.png',sep='')
	png(filename = filename, width = 1920, height = 1080, units = "px", pointsize = 24, bg = "white")

	stat = basic_stats(results, num_syls, folder)
	stat = 100*stat/nrow(results)
#	total_rows = sum(unlist(errors[1]))

	par(las=1,
		xpd=TRUE,
		cex.lab=1,
		cex.axis=.9,
		cex.main=1.2,
		mar=c(3,10,3,2),
		oma=c(1,1,2.5,1)
	)


	colors = c('chartreuse2','brown2')
	y <- barplot(stat,horiz=TRUE, col=colors, main=paste(num_syls, 'Silben: Fehler je Modell / n =',nrow(results)))
	#	legend.text = TRUE, args.legend = c('error','corr')
	addLabels(y, stat)
	abline(v=zeroR,lty=2)
	text(zeroR-4, max(y)+2, paste('ZeroR (',round(zeroR,1),'%)',sep=''), cex=.7)

	#barplot(t(stat/total_rows),horiz=TRUE,cex.names=0.8,legend.text = TRUE, args.legend = list(x = "topright", bty = "n"), main='Errors by classifier')
	#savePlot(paste(folder,'total/',num_syls,'syl.png',sep=''))
	dev.off()
}

stats <- function(results) {
	apply(results, 2, function(col) c(sum(col == 'no')/length(col), sum(col == 'yes')/length(col)))
}

voting <- function(list_of_results) {#, num_syls, folder) {
	matrix(unlist(lapply(list_of_results, function(results) {
		correct_voted = 100*sum(apply(results == 'no', 1, sum) >= ncol(results)/2) / nrow(results)
		error_voted = 100*sum(apply(results == 'no', 1, sum) < ncol(results)/2) / nrow(results)
		stat = rbind(correct_voted, error_voted)
		stat
	})),2,dimnames=list(c('errors','correct'),names(list_of_results)))
}

find_best_complementary <- function(results, col_indices) {
	all_error_rows = subset(results, results[col] == 'yes')
	corrects = apply(all_error_rows, 2, function(col) sum(col=='no'))
	best_model = names(which.max(corrects))
	best_model_corect = max(corrects)
	base_model = names(results[col_i])
	base_model_errors = nrow(all_error_rows)
	remaining_errors = base_model_errors - best_model_corect
	matrix(c(base_model_errors, best_model_corect, remaining_errors),3, dimnames=list(c(base_model,best_model,'remaining_errors'),c('hits')))
}
#find_best_complementary(results, c(1,2))
#unlist(lapply(1:2, function(i) find_best_complementary(results, c(i))), recursive=FALSE)


compute_votes <- function(rows) {
	apply(rows,1,function(x) names(sort(table(x), decreasing=TRUE)[1]))
}
votes <- function(rows, correct_stress_class) {
	votes = compute_votes(rows)
	correct = 100*sum(votes == correct_stress_class)/nrow(rows)
	error = 100*sum(votes != correct_stress_class)/nrow(rows)
	rbind(correct,error)
}

evaluate_voting_models <- function(results, correct_class, models, folder, num_syls) {
	spacing = NULL
	x = lapply(models, function(model) {
		res = results[grep(model, names(results))]
		spacing <<- c(spacing, c(1.1,0.4,replicate(ncol(res)-1,0)))
		models_correct = 100*apply(res,2,function(x) sum(x==correct_class))/nrow(results)
		models_error = 100*apply(res,2,function(x) sum(x!=correct_class))/nrow(results)

		models_results = rbind(models_correct,models_error,deparse.level = 2)
		vote_results = votes(res, correct_class)
		colnames(vote_results) = list('voting')
		cbind(vote_results, models_results,deparse.level = 2)

	})
	ret = matrix(unlist(x),2, dimnames=list(c('correct','error'),unlist(lapply(1:length(x),function(i) colnames(x[[i]])))))
	par(las=1)
	par(mar=c(5,10,4,6))
	barplot(ret,horiz=TRUE,cex.names=0.6,legend.text = TRUE, args.legend = list(x = 1.25, bty = "n"), main=paste(num_syls, 'Syllables: Voting models'),
		space = spacing)
	savePlot(paste(folder,'votings/',num_syls,'syl.png',sep=''))

}

#error_freq <- function(model_sets, errors) {
#	lapply(model_sets, function(results) {
#		matrix(
#			unlist(lapply(colnames(results), function(colname) {
#				arg = list(); arg[colname] = 'yes'
#				table(apply(find_row(results, arg) == 'yes',1,sum))
#			}), recursive=FALSE),
#		length(results))
#	})
#}
#x=error_freq(model_sets,list(error_all_J48='yes'))

plot_compare <- function(model_sets, num_syls, folder) {
	filename = paste(folder,'compare/',num_syls,'syl.png',sep='')
	png(filename = filename, width = 1920, height = 1080, units = "px", pointsize = 24, bg = "white")

	par(mfrow=c(4,2),
		las=1,
		xpd=TRUE,
		cex.lab=1,
		cex.axis=1.2,
		cex.main=1.2,
		mar=c(3,10,3,2),
		oma=c(1,1,2.5,1)
	)
	colors = c('chartreuse1','chartreuse3','chartreuse4','brown1','brown3','brown4')
	
	modelset_names = names(model_sets)
	lapply(modelset_names, function(dataset_name) {
		stat = compare(model_sets[[dataset_name]], num_syls, folder)
		pretty_dataset_name = gsub('\\|','-/',dataset_name)
		x = barplot(stat, horiz=TRUE, col=colors, main=paste('Vergleich der ',pretty_dataset_name,'-Modelle',sep=''))
		addLabels(x, stat)
	})

	plot.new()
	labels = c('Als einziges Modell korrekt', 'Von allen Modellen korrekt', 'Sonstige richtige Klassifizierungen',
		'Als einziges Modell falsch','Von allen Modellen falsch','Sonstige Fehler')
	legend_title = 'Fehlerverteilungen'
	legend('topleft', title=legend_title, bty = "n", ncol=2, cex=1.2, fill=colors, legend=labels)

	title(paste('Häufigkeiten der falschen/richtigen Klassifizierungen (Wörter mit ',num_syls, ' Silben)', sep=''), outer=TRUE)
	dev.off()
}

do_plot_majorites <- function(model_sets, num_syls, folder) {
	filename = paste(folder,'majorities/',num_syls,'syl.png',sep='')
	png(filename = filename, width = 1920, height = 1080, units = "px", pointsize = 24, bg = "white")

	par(mfrow=c(4,2),
		las=1,
		xpd=TRUE,
		cex.lab=1,
		cex.axis=1.2,
		cex.main=1.2,
		mar=c(3,10,3,2),
		oma=c(1,1,2.5,1)
	)
	modelset_names = names(model_sets)
	colors = c('chartreuse3','chartreuse1','brown4','brown1')
	modelset_names = names(model_sets)
	lapply(modelset_names, function(dataset_name) {
		stat = plot_majorites(model_sets[[dataset_name]], num_syls, folder)
		pretty_dataset_name = gsub('\\|','-/',dataset_name)
		x=barplot(stat, horiz=TRUE, col=colors, main=paste('Vergleich der ',pretty_dataset_name,'-Modelle',sep=''))
		addLabels(x, stat)
	})

	plot.new()
	labels = c('Korrekt; Mehrheit ebenfalls richtig', 'Korrekt; Mehrheit jedoch falsch',
		'Falsch; Mehrheit ebenfalls falsch','Falsch; Mehrheit jedoch richtig')
	legend_title = 'Legende'
	legend('topleft', title=legend_title, bty = "n", ncol=2, cex=1.2, fill=colors, legend=labels)

	title(paste('Wie unterscheidet sich ein Model vom Durchschnitt? Klassifizierungen nach Mehrheitsmeinung (',num_syls, ' Silben)', sep=''), outer=TRUE)
	dev.off()
}
pretty_name <- function(str) {
	shorter = gsub('error_', '', str)
	nicer = gsub('_', '/', shorter)
	parts = unlist(strsplit(nicer, '/'))
	camel = paste(parts[2],'@',parts[1],sep='')
	unlist(camel)
}

data_folder = '/home/sbiastoch/Schreibtisch/thesis/Experimente/csv/classifications/'
#data_folder = '~/Schreibtisch/thesis/Experimente/csv/classifications/'
folder = '/home/sbiastoch/Schreibtisch/thesis/Experimente/evaluation/'
#folder = '/home/sbiastoch/Schreibtisch/thesis/Experimente/evaluation/'

lapply(2:7, function(num_syls) {
	csv = read.csv(paste(data_folder,num_syls,'syl-classifications.csv',sep=''), header=TRUE)

	results_errors_with_strs_class = apply(csv,2,function(col) col != csv[,1])
	results_errors_with_strs_class[results_errors_with_strs_class==TRUE] <- 'yes'
	results_errors_with_strs_class[results_errors_with_strs_class==FALSE] <- 'no'
	results_errors = results_errors_with_strs_class[,2:ncol(results_errors_with_strs_class)]
	colnames(results_errors) = gsub('classification','error',colnames(results_errors))


#	colnames(results) = lapply(colnames(results), pretty_name) ### verursacht data too long

	zeroR = round(100*max(table(csv[,1]))/nrow(csv),2)
	r = matrix(
		c(nrow(results_errors),c(round(100*stats(results_errors)[1,],2),zeroR)),
		1, dimnames=list(num_syls,c('n',c(colnames(results_errors),'zeroR'))))
	print(r)
	if(num_syls==2) {
		write.csv(r, paste(folder,'stats.csv',sep=''))
	} else {
		write.table(r, paste(folder,'stats.csv',sep=''),sep=",",col.names=FALSE,append=TRUE)
	}

	results_class = csv[grep("classification", names(csv))]


	# Bauen der zu vergleichenden Modelle
	#models = list('praefix','suffix','affix','sonority','weight','phoncat','phon','sylstruct','meta','numeric','sparse','all')
	#models = list('J48', 'JRip', 'NN')
	models = list('J48', 'JRip', 'NN', 'phon', 'affix', 'sylstruct', 'meta')
	model_sets = lapply(models, function(modelname) {
		results_errors[,grep(modelname, colnames(results_errors))]
	})
	names(model_sets) = models

	# Fehler je Einzelmodell
	plot_basic_stats(results_errors, zeroR, num_syls, folder)

	#
#	unique_errors(results_errors, num_syls, folder) depracted?

	# Vergleich der error/correct: unique, common, other
	plot_compare(model_sets, num_syls, folder)

	# Aufschlüsselung der error/correct in Mehrheits/Minderheits corrects/errors
	do_plot_majorites(model_sets, num_syls, folder)

	# ???????????????
#	evaluate_voting_models(results_class, csv$stress_class, methodsodels, folder, num_syls)

	# Histogramm der Fehlerhäufigkeiten der Fehler eines Models 
#	error_freq_histogram = rbind(
#		all=error_freq(model_sets, errors=list(error_all_J48=TRUE)),
#		phon=error_freq(model_sets, errors=list(error_phon_J48=TRUE)),
#		affix=error_freq(model_sets, errors=list(error_affix_J48=TRUE)),
#		sylstruct=error_freq(model_sets, errors=list(error_sylstruct_J48=TRUE))
#	)
#	barplot(error_freq_histogram, legend=TRUE)


	# ????????????????????
#	votes = voting(model_sets)
#	colnames(votes) = c(models)

#	stat = cbind(votes, stats(results_errors))
 
#	par(las=1)
#	par(mar=c(5,8,4,6)) 
#	barplot(stat,
#		horiz=TRUE,
#		cex.names=0.8,
#		legend.text = TRUE, 
#		args.legend = list(x = "topright", bty = "n"), #
#		main=paste(num_syls, 'Syllables: Errors by classifier'))

})

performance <- function(table) {
	best_percent = apply(table[,3:ncol(table)],1, max)
	best_models = apply(table[,3:ncol(table)], 1, function(table) names(which.max(table)))
	total_performance = round(sum((apply(table[,3:ncol(table)], 1, max)*table[,2]))/sum(table[,2]),2)
	tbl = rbind(cbind(best_models,best_percent),c('',total_performance))
	matrix(tbl,,2, dimnames=list(c(2:(nrow(table)+1),'weighted'),c('best_model','result')))
}

main_stats = read.csv(paste(folder,'stats.csv',sep=''))

models = c(
	'',
	'NN','J48','JRip',
	'all','sparse','numeric','phon_','affix',
	'praefix','suffix','phoncat','sylstruct','weight','sonority','meta'
	)

# liefert je featureset bestes kummuliertes ergebnis
best_cum = matrix(unlist(lapply(models, function(model) {
	p = performance(main_stats[c(1,2,grep(model,names(main_stats)))])
	c(model,unlist(p['weighted','result']))
}), recursive=FALSE),2)
par(las=1,
#	xpd=TRUE,
	cex.lab=1,
	cex.axis=1.2,
	cex.main=1.2,
	mar=c(5,3,3,3)
)
barplot(matrix(best_cum[2,], 1, dimnames=list('',best_cum[1,])), las=2, ylim=c(70,100), xpd=FALSE)
title('Erkennungsraten der besten Modelle je Featureset')

####
# boxplot für anzahl der regeln bei jrip
####


jrip_rules_models = c(
	'all','sparse','numeric','phon','affix',
	'praefix','suffix','phoncat','sylstruct','weight','sonority','meta'
	)

getNumberOfRules <- function(fileName) {
#	f = readChar(fileName, file.info(fileName)$size)
#	phrase = 'Number of Rules : '
#	NoR = gregexpr(pattern=phrase, f)[[1]][1] + nchar(phrase)
#	as.numeric(strsplit(substr(f,NoR,NoR+10),'\n')[[1]][1])
	length(getRules(fileName))+1
}

library(stringr)

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
# boxplot der tiefe aller regeln von J48
treedepth_stat = lapply(2:7, function(num_syls) {
	lapply(jrip_rules_models, function(model) {
		fileName = paste(folder,'../trained_models/',num_syls,'syl/models-',model,'.txt',sep='')
		getTreeDepths(fileName)
	})
})
# boxplot der tiefe aller regeln von J48
treesize_stat = lapply(2:7, function(num_syls) {
	lapply(jrip_rules_models, function(model) {
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
	oma=c(10,1,1,1)
)
boxplot(lapply(1:12, function(mi) unlist(lapply(1:6, function(i) treedepth_stat[[i]][mi]))),xaxt='n')


boxplot(lapply(1:12, function(mi) unlist(lapply(1:6, function(i) treesize_stat[[i]][mi]))),xaxt='n')


table = main_stats[c(1,2,grep('48',names(main_stats)),length(main_stats))]
cum_corr = apply(table, 2, function(col) sum(col*table[['n']])/sum(table[['n']]))
x=barplot(cum_corr[4:length(cum_corr)-1], ylim=c(60,92), las=2,xpd=FALSE)
abline(h=cum_corr['zeroR'],lty=2)
text(max(x)+1,cum_corr['zeroR']+1, paste('ZeroR (',round(cum_corr['zeroR'],1),'%)',sep=''), cex=.7)
title('Erkennungsraten von J48')
#addLabels(cum_corr[3:13],x)




# get the most signifcant rules
#rules = getRules('/home/sbiastoch/Schreibtisch/thesis/Experimente/trained_models/2syl/models-all.txt')
#lapply(rules, function(rule) {
#	start = gregexpr(pattern='a (', rule)[[1]][1]
#	substr(f,start,start+end)
#})

# ohne die letzte Regel!
getRulesLength <- function(fileName) {
	rules = getRules(fileName)
	unlist(lapply(rules, function(rule) {
		str_count(rule,' and ')+1
	}))
}

syls = 2:6
ruleslen_stat=lapply(syls, function(num_syls) {
	lapply(jrip_rules_models, function(model) {
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
	oma=c(10,1,1,1)
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

table = main_stats[c(1,2,grep('JRip',names(main_stats)))]
cum_corr = apply(table, 2, function(col) sum(col*table[['n']])/sum(table[['n']]))
x=barplot(cum_corr[3:length(cum_corr)], ylim=c(65,90), las=2)
title('Erkennungsraten von JRip')
#addLabels(cum_corr[3:13],x)


#plot.new()
#title('Länge der von JRip generierten Regeln', outer=TRUE, cex.main=1.7)
#legend('topright', title='Modelle/Featuresets', bty = "n", ncol=2,fill=rainbow(length(ruleslen_stat)), legend=jrip_rules_models)


#write.table(ruleslen_stat, paste(folder,'length_of_rules.csv',sep=''))



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