awk '/^@attribute /' 2syl-all_training.csv.arff | awk -F' ' 'BEGIN{ print "attribute,count" }{ print $2 "," gsub(/,/,"")+1}' > 2syl_attr_counts
awk '/^@attribute /' 3syl-all_training.csv.arff | awk -F' ' 'BEGIN{ print "attribute,count" }{ print $2 "," gsub(/,/,"")+1}' > 3syl_attr_counts
awk '/^@attribute /' 4syl-all_training.csv.arff | awk -F' ' 'BEGIN{ print "attribute,count" }{ print $2 "," gsub(/,/,"")+1}' > 4syl_attr_counts
awk '/^@attribute /' 5syl-all_training.csv.arff | awk -F' ' 'BEGIN{ print "attribute,count" }{ print $2 "," gsub(/,/,"")+1}' > 5syl_attr_counts
awk '/^@attribute /' 6syl-all_training.csv.arff | awk -F' ' 'BEGIN{ print "attribute,count" }{ print $2 "," gsub(/,/,"")+1}' > 6syl_attr_counts
awk '/^@attribute /' 7syl-all_training.csv.arff | awk -F' ' 'BEGIN{ print "attribute,count" }{ print $2 "," gsub(/,/,"")+1}' > 7syl_attr_counts