#!/bin/bash

# Trennt die von count_feature_occurences.sh Dreierausgabe in Einzelne Textdateien.

script="/home/sbiastoch/Schreibtisch/thesis/Experimente/scripts/count_feature_occurences.sh"
savepath="/home/sbiastoch/Schreibtisch/thesis/Experimente/evaluation/features/"
for featureset in "praefix" "suffix" "affix" "sylstruct" "weight" "phoncat" "sonority" "phon" "meta" "sparse" "numeric" "all"
do
	echo "$($script "*" "$featureset" | csplit - '/JRip/' '{*}' -f $featureset)"
done


