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
	common_errors = match_row(results, replicate(ncol(results), 'yes'))		# anzahl der zeilen mit NO, bei der alle anderen MODELs ein YES haben
	common_correct = match_row(results, replicate(ncol(results), 'no'))		# anzahl der zeilen mit YES, bei der alle anderen MODELs ein NO haben

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
	return(stat)

}

plot_majorites <- function(results, num_syls, folder) {
	majority_correct = majority(results, error=FALSE, minority=FALSE)
	minority_correct = majority(results, error=FALSE, minority=TRUE)
	majority_error = majority(results, error=TRUE, minority=FALSE)
	minority_error = majority(results, error=TRUE, minority=TRUE)
	stat = rbind(
		majority_correct, 	minority_correct,
		majority_error, 	minority_error
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
	savePlot(paste(folder,'stats/plots/',num_syls,'syl-jrips.png',sep=''))

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
	savePlot(paste(folder,'stats/plots/unique-errors/',num_syls,'syl.png',sep=''))


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
	savePlot(paste(folder,'stats/plots/unique-correct/',num_syls,'syl.png',sep=''))

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
	savePlot(paste(folder,'stats/plots/',num_syls,'syl-aggregated.png',sep=''))

}

basic_stats <- function(results, num_syls, folder) {
	# sum up all corrects and errors per model
	errors_data = results[grep("error", names(results))]
	errors = lapply(errors_data, table)

	y = matrix(unlist(errors),2,byrow=FALSE)
	dimnames(y) <- list(c('yes','no'),names(errors_data))
	return(y)
}

plot_basic_stats <- function(results, num_syls, folder) {
	filename = paste(folder,'stats/plots/total/',num_syls,'syl-basicstats.png',sep='')
	png(filename = filename, width = 1920, height = 1080, units = "px", pointsize = 24, bg = "white")

	stat = basic_stats(results, num_syls, folder)
#	total_rows = sum(unlist(errors[1]))

	par(las=1,
		xpd=TRUE,
		cex.lab=1.2,
		cex.axis=1.2,
		cex.main=1.2,
		mar=c(3,10,3,2),
		oma=c(1,1,2.5,1)
	)


	colors = c('chartreuse2','brown2')
	barplot(stat,horiz=TRUE, col=colors, main=paste(num_syls, 'Silben: Fehler je Modell'))
	#barplot(t(stat/total_rows),horiz=TRUE,cex.names=0.8,legend.text = TRUE, args.legend = list(x = "topright", bty = "n"), main='Errors by classifier')
	#savePlot(paste(folder,'stats/plots/total/',num_syls,'syl.png',sep=''))
	dev.off()
}

stats <- function(results) {
	apply(results, 2, function(col) c(sum(col == 'no')/length(col), sum(col == 'yes')/length(col)))
}

voting <- function(list_of_results) {#, num_syls, folder) {
	matrix(unlist(lapply(list_of_results, function(results) {
		correct_voted = sum(apply(results == 'no', 1, sum) >= ncol(results)/2) / nrow(results)
		error_voted = sum(apply(results == 'no', 1, sum) < ncol(results)/2) / nrow(results)
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
	correct = sum(votes == correct_stress_class)/nrow(rows)
	error = sum(votes != correct_stress_class)/nrow(rows)
	rbind(correct,error)
}

evaluate_voting_models <- function(results, correct_class, models, folder, num_syls) {
	spacing = NULL
	x = lapply(models, function(model) {
		res = results[grep(model, names(results))]
		spacing <<- c(spacing, c(1.1,0.4,replicate(ncol(res)-1,0)))
		models_correct = apply(res,2,function(x) sum(x==correct_class))/nrow(results)
		models_error = apply(res,2,function(x) sum(x!=correct_class))/nrow(results)

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
	savePlot(paste(folder,'stats/plots/votings/',num_syls,'syl.png',sep=''))

}

error_freq <- function(results, errors) {
	table(apply(find_row(results,errors) == 'yes',1,sum))
}

plot_compare <- function(model_sets, num_syls, folder) {
	filename = paste(folder,'stats/plots/compare/',num_syls,'syl.png',sep='')
	png(filename = filename, width = 1920, height = 1080, units = "px", pointsize = 24, bg = "white")

	par(mfrow=c(4,2),
		las=1,
		xpd=TRUE,
		cex.lab=1.2,
		cex.axis=1.2,
		cex.main=1.2,
		mar=c(3,10,3,2),
		oma=c(1,1,2.5,1)
	)
	modelset_names = names(model_sets)
	colors = c('chartreuse1','chartreuse3','chartreuse4','brown1','brown3','brown4')
	lapply(modelset_names, function(dataset_name) {
		stat = compare(model_sets[[dataset_name]], num_syls, folder)
		pretty_dataset_name = gsub('\\|','-/',dataset_name)
		barplot(stat, horiz=TRUE, col=colors, main=paste('Vergleich der ',pretty_dataset_name,'-Modelle',sep=''))
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
	filename = paste(folder,'stats/plots/majorities/',num_syls,'syl.png',sep='')
	png(filename = filename, width = 1920, height = 1080, units = "px", pointsize = 24, bg = "white")

	par(mfrow=c(4,2),
		las=1,
		xpd=TRUE,
		cex.lab=1.2,
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
		barplot(stat, horiz=TRUE, col=colors, main=paste('Vergleich der ',pretty_dataset_name,'-Modelle',sep=''))
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

lapply(2:8, function(num_syls) {
	folder = '~/Schreibtisch/results/'
	csv = read.csv(paste(folder,num_syls,'syl-results.csv',sep=''), header=TRUE)

	results = csv[grep("error", names(csv))]
#	colnames(results) = lapply(colnames(results), pretty_name) ### verursacht data too long

	results_class = csv[grep("classification", names(csv))]


	# Bauen der zu vergleichenden Modelle
	models = list('J48', 'JRip', 'NN', 'phon', 'affix', 'sylstruct', 'phon_NN|affix_J48')
	model_sets = lapply(models, function(modelname) {
		results[grep(modelname, names(results))]
	})
	names(model_sets) = models

	# Fehler je Einzelmodell
	plot_basic_stats(results, num_syls, folder)

	#
#	unique_errors(results, num_syls, folder) depracted?

	# Vergleich der error/correct: unique, common, other
	plot_compare(model_sets, num_syls, folder)

	# Aufschlüsselung der error/correct in Mehrheits/Minderheits corrects/errors
	do_plot_majorites(model_sets, num_syls, folder)

	# ???????????????
#	evaluate_voting_models(results_class, csv$stress_class, methodsodels, folder, num_syls)

	# Histogramm der Fehlerhäufigkeiten der Fehler eines Models 
#	error_freq_histogram = rbind(
#		all=error_freq(model_sets, errors=list(error_all_J48='yes')),
#		phon=error_freq(model_sets, errors=list(error_phon_J48='yes')),
#		affix=error_freq(model_sets, errors=list(error_affix_J48='yes')),
#		sylstruct=error_freq(model_sets, errors=list(error_sylstruct_J48='yes'))
#	)
#	barplot(error_freq_histogram, legend=TRUE)


	# ????????????????????
#	votes = voting(model_sets)
#	colnames(votes) = c(models)

#	stat = cbind(votes, stats(results))
 
#	par(las=1)
#	par(mar=c(5,8,4,6)) 
#	barplot(stat,
#		horiz=TRUE,
#		cex.names=0.8,
#		legend.text = TRUE, 
#		args.legend = list(x = "topright", bty = "n"), #
#		main=paste(num_syls, 'Syllables: Errors by classifier'))

})