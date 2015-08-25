#!/bin/bash
if [ $# -eq 1 ]
then
	syl="$1syl"
	files="*"
elif [[ $# -eq 2 ]]; then
	syl="$1syl"
	files="$2"
else
	syl="*"
	files="*"
fi
basepath="/home/sbiastoch/Schreibtisch/thesis/Experimente/trained_models"
path="$basepath/$syl/models-$files.txt"
result_j48=""
result_jrip=""
result_counts=""
#for feature in "signi_suffix " "suffix. " "suffix_phoncat. " "suff_class " "signi_praefix " "praefix. " "praefix_phoncat. " "prae_class " "sonority. " "sonority_ratio. " "sonority_dir " "syl_weight. " "syl_open. " "syl_phoncat. " "onset_phoncat. " "koda_phoncat. " "nucleus_phoncat. " "syl_len. " "koda_len. " "onset_len. " "nucleus_len. " "syl_cv. " "cv_ratio. " "pos " "comp_struct " "nomen " "comp_len "
for feature in "signi_suffix" "suffix1" "suffix2" "suffix3" "suffix4" "suffix5" "suffix_phoncat1" "suffix_phoncat2" "suffix_phoncat3" "suffix_phoncat4" "suffix_phoncat5" "suff_class" "signi_praefix" "praefix1" "praefix2" "praefix3" "praefix4" "praefix5" "praefix_phoncat1" "praefix_phoncat2" "praefix_phoncat3" "praefix_phoncat4" "praefix_phoncat5" "prae_class" "sonority0" "sonority1" "sonority2" "sonority3" "sonority4" "sonority5" "sonority6" "sonority_ratio0" "sonority_ratio1" "sonority_ratio2" "sonority_ratio3" "sonority_ratio4" "sonority_ratio5" "sonority_ratio6" "sonority_dir" "syl_weight0" "syl_weight1" "syl_weight2" "syl_weight3" "syl_weight4" "syl_weight5" "syl_weight6" "syl_open0" "syl_open1" "syl_open2" "syl_open3" "syl_open4" "syl_open5" "syl_open6" "syl_phoncat0" "syl_phoncat1" "syl_phoncat2" "syl_phoncat3" "syl_phoncat4" "syl_phoncat5" "syl_phoncat6" "onset_phoncat0" "onset_phoncat1" "onset_phoncat2" "onset_phoncat3" "onset_phoncat4" "onset_phoncat5" "onset_phoncat6" "koda_phoncat0" "koda_phoncat1" "koda_phoncat2" "koda_phoncat3" "koda_phoncat4" "koda_phoncat5" "koda_phoncat6" "nucleus_phoncat0" "nucleus_phoncat1" "nucleus_phoncat2" "nucleus_phoncat3" "nucleus_phoncat4" "nucleus_phoncat5" "nucleus_phoncat6" "syl_len0" "syl_len1" "syl_len2" "syl_len3" "syl_len4" "syl_len5" "syl_len6" "koda_len0" "koda_len1" "koda_len2" "koda_len3" "koda_len4" "koda_len5" "koda_len6" "onset_len0" "onset_len1" "onset_len2" "onset_len3" "onset_len4" "onset_len5" "onset_len6" "nucleus_len0" "nucleus_len1" "nucleus_len2" "nucleus_len3" "nucleus_len4" "nucleus_len5" "nucleus_len6" "syl_cv0" "syl_cv1" "syl_cv2" "syl_cv3" "syl_cv4" "syl_cv5" "syl_cv6" "cv_ratio0" "cv_ratio1" "cv_ratio2" "cv_ratio3" "cv_ratio4" "cv_ratio5" "cv_ratio6" "pos" "comp_struct" "nomen" "comp_len"
do
	result_j48="$result_j48\n$(awk "/[^\(]$feature/" $path | wc -l)\t$feature"
	result_jrip="$result_jrip\n$(awk "/\($feature/" $path | wc -l)\t$feature"
	result_counts="$counts\n$(grep "($feature " $path | awk -F'/|\\(|)|:' '{print $(NF-2) "\t" $(NF-1) "\t" 1-($(NF-1)/$(NF-2)) "\t" $8 "\t" $9 "\t" $NL}')" 
done

echo -e "Nennungen der Features in J48:\n"
echo -e "$result_j48"  | sort -nr
echo -e "Nennungen der Features in JRip:\n"
echo -e "$result_jrip" | sort -nr
echo -e "Wichtigste JRip-Regeln:\n"
rep=""
echo -e "${result_counts//$basepath/$rep}" | sort -nr
#echo -e "$result_counts" | sort -nr