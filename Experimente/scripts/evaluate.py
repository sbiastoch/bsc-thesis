#!/usr/bin/python
# -*- coding: utf-8 -*-
import sys

import weka.core.jvm as jvm
from weka.core.converters import Loader, Saver
from weka.classifiers import Classifier, Evaluation
from weka.core.classes import Random
from weka.filters import Filter
import weka.core.serialization as serialization
from weka.classifiers import Classifier
from weka.classifiers import FilteredClassifier
from weka.core.dataset import Instances
from weka.filters import Filter
from collections import Counter

class Experiment:
	data = None
	class_index = -1
	classifier = None
	attrs = []

	def __init__(self):
#		jvm.start(max_heap_size="2500M")
		pass

	def out(self, x):
		print x.__str__().encode('ascii', 'ignore')

	def loadCSV(self, filename, path='/home/sbiastoch/Schreibtisch/csv_files/'):
		weka_loader = Loader(classname="weka.core.converters.CSVLoader")
		self.data = weka_loader.load_file(path+filename)

	def setClassIndex(self, index):
		if index < 0:
			self.data.class_index = self.data.num_attributes + index
		else:
			self.data.class_index = index

	def train_J48(self, min_per_rule=20):
		params = [
			'-C','0.3',
			'-M',str(min_per_rule),
	#		'-N',str(folds),
	#		'-R',
		]
		self.base_classifier = Classifier(classname='weka.classifiers.trees.J48', options=params)
		self._train()

	def train_JRip(self, min_per_rule=20, optimizations=2, folds=3, seed=42):
		params = [
			'-F', str(folds), # folds
			'-N', str(min_per_rule), # min elements per rule
			'-O', str(optimizations), # optimizations
			'-S', str(seed) #seed
		] 
		self.base_classifier = Classifier(classname='weka.classifiers.rules.JRip', options=params)
		self._train()

	def _train(self):
		params = [
			'-F','weka.filters.unsupervised.attribute.RemoveByName -E ^('+'|'.join(self.attrs)+')$ -V',
			'-W', self.base_classifier.classname, '--',
			]
		params.extend(self.base_classifier.options)


#		self.classifier = Classifier(classname='weka.classifiers.meta.FilteredClassifier', options=params)
		self.classifier = FilteredClassifier(options=params)
	#	self.classifier.filter(Filter(classname="weka.filters.unsupervised.attribute.RemoveByName", options=['-E','^('+'|'.join(self.attrs)+')$','-V']))
		self.classifier.build_classifier(self.data)
		self.out(self.classifier.__str__().encode('ascii', 'ignore').split("\n")[-2])

	def test(self, folds = 10):
		evaluation = Evaluation(self.data)                     # initialize with priors
		evaluation.crossvalidate_model(self.classifier, self.data, folds, Random(42))  # 10-fold CV
		print('Total number of instances: '+str(evaluation.num_instances)+'.')
		print(str(round(evaluation.percent_correct,2))+'% / '+str(round(evaluation.correct, 2))+' correct.')
		print(str(round(evaluation.percent_incorrect,2))+'% / '+str(round(evaluation.incorrect, 2))+' incorrect.')
		
	def saveCSV(self, filename, path='/home/sbiastoch/Schreibtisch/csv_files/'):
		saver = Saver(classname="weka.core.converters.CSVSaver")
		saver.save_file(self.data, path+filename)

	def loadClassifier(self, filename, path='/home/sbiastoch/Schreibtisch/classifiers/'):
		objects = serialization.read_all(path+filename)
		self.classifier = Classifier(jobject=objects[0])
		#self.data = Instances(jobject=objects[1])

	def saveClassifier(self, filename, path='/home/sbiastoch/Schreibtisch/classifiers/'):
		serialization.write_all(path+filename, [self.classifier, Instances.template_instances(self.data)])


	def remove_correct_classified(self, invert = False):
		options=[
			'-W', self.classifier.to_commandline(), 
			'-C', str(self.class_index), #classindex
	#		'-F','0', # folds
	#		'-T','0.1', #threshold by numeric classes
			'-I','0', # max iterations
			'-V' if not invert else '' 
		] # invert
		classname = "weka.filters.unsupervised.instance.RemoveMisclassified"
		remove = Filter(classname=classname, options=options)
		remove.inputformat(self.data)
		self.data = remove.filter(self.data)

	def remove_incorrect_classified(self):
		self.remove_correct_classified(True)

	def set_attributes(self, attrs):
		self.attrs = attrs

	def select_missclassified(self):
		remove = Filter(classname="weka.filters.supervised.attribute.AddClassification", options=['-classification' ,'-error' ,'-W' ,self.base_classifier.to_commandline()])
		remove.inputformat(self.data)
		self.data = remove.filter(self.data)

		remove = Filter(classname="weka.filters.unsupervised.instance.RemoveWithValues", options=['-S','0.0','-C','last','-L','last','-V'])
		remove.inputformat(self.data)

		remove = Filter(classname="weka.filters.unsupervised.attribute.Remove", options=['-R',str(self.data.num_attributes-2)+',last'])
		remove.inputformat(self.data)
		self.data = remove.filter(self.data)

	def merge_nominal_attributes(self, significance=0.01):
		remove = Filter(classname="weka.filters.supervised.attribute.MergeNominalValues", options=['-L',str(significance),'-R','first-last'])
		remove.inputformat(self.data)
		self.data = remove.filter(self.data)




def run(type, dataset, classindex, attrs, name='undef'):
	E = Experiment()

	#loading data
	E.loadCSV(dataset+'.csv')
	E.setClassIndex(classindex)
	E.set_attributes(attrs)

	getattr(E,'train_'+type)(20)
	E.saveClassifier(dataset+'-'+type+'-'+name+'.model')
	E.test()
	E.select_missclassified()
	E.saveCSV(dataset+'-'+type+'-'+name+'-err.csv')

#	E.kill()

def test(model, dataset, classindex):
	E = Experiment()

	E.loadCSV(dataset+'.csv')
	E.setClassIndex(classindex)
	E.set_attributes(attrs)
	E.loadClassifier((dataset if dataset[-4:] != '-err' else dataset[:-4])+'.model')
	E.test()

#	E.kill()

#####################################
#####################################
#####################################

feature_sets = {
	'all': [
		'SylCnt',
		'syl_len.',
		'rhyme.',
		'suffix',
		'praefix',
		'praefix_class',
		'suffix_class',
		'is_nomen',
		'cv_ratio',
		'koda_len',
		'onset_len',
		'nucleus_len',
		'sonority',
		'sonority_ratio',
		'syl_class',
		'onset_class',
		'koda_class',
		'nucleus_class',

		'part_of_speech',
		'composita_struct',
		'sa_struct',
		'comp_len',
	],
	'sylcnt': [
		'SylCnt',
	],
	'prae_suffixes': [
		'SylCnt',
		'praefix_class',
		'praefix',
		'suffix_class',
		'suffix',
	],
	'sylstruct': [
		'SylCnt',
		'koda_len',
		'onset_len',
		'nucleus_len',
		'syl_class',
		'onset_class',
		'nucleus_class',
		'koda_class',
		SylCnt, onset_class, nucleus_class, koda_class, praefixes_classes, suffixes_classes, syl_weights
		
	],
	'phonological': [
		'SylCnt',
		'syl_len.',
		'sonority',
		'sonority_ratio',
		'cv_ratio',
	]
}
classifiers = [
	'J48',
	'JRip',
]
dataset = '000040-3syls-5mann'
classindex = -1

jvm.start(max_heap_size="2500M")

if 0 == 0:
	for name, feature_set in feature_sets.items():
		for classifier in classifiers:
			print '################################'
			print classifier+'/'+name+': '+', '.join(feature_set)
			run(classifier, dataset, classindex, feature_set, name)
			print '################################'


all_results = {}
firstrun = True
for feature_set in feature_sets.keys(): # alle featuresets
	for classifier in classifiers: # alle classifier
		E = Experiment()
		E.loadCSV(dataset+'.csv')
		E.setClassIndex(classindex)
		E.loadClassifier(dataset + '-' + classifier + '-' + feature_set + '.model')
		for index, inst in enumerate(E.data): # alle instanzen
			data = inst.__str__().split(',')
			word = data[-2]
			actual_class = data[classindex]
			pred_class_idx = E.classifier.classify_instance(inst)
			pred_class = E.data.class_attribute.value(pred_class_idx)
			if firstrun:
				all_results.update({str(index)+word: [actual_class, pred_class]})
			else:
				all_results[str(index)+word].append(pred_class)

		firstrun = False
		break
correct = 0
incorrect = 0
for word, results in all_results.items():
	actual_class = results[0]

	majority = Counter(results[1:]).most_common()[0][0]

	if majority == actual_class:
		correct += 1
	else:
		incorrect += 1
	
print str(round(100*float(correct)/float(incorrect+correct),2))+'% correct by majority-voting'


"""
results = {}
data = inst.__str__().split(',')
word = data[-2]
actual_class = data[classindex]
for index, inst in enumerate(E.data): # alle instanzen
	for model in feature_sets.keys(): # alle featuresets
		for classifier in classifiers: # alle classifier
			pred_class_idx = E.classifier.classify_instance(inst)
			pred_class = E.data.class_attribute.value(pred_class_idx)
			#dist = E.classifier.distribution_for_instance(inst)
			#print(str(index+1) + ": label index=" + str(pred) + ", class distribution=" + str(dist))
			if actual_class == pred_class:
				print word + ' richtig als ' + pred_class + ' erkannt.'
			else:
				print word + '('+actual_class+') falsch als ' + pred_class + 'klassifiziert.'
"""
jvm.stop()

#test('J48', dataset+'-JRip-err', classindex)
#test('JRip', dataset+'-J48-err', classindex)

"""
E = Experiment()

#loading data
E.loadCSV(dataset+'.csv')
E.setClassIndex(classindex)
E.set_attributes(attrs)
E.merge_nominal_attributes()

E.train_J48(20)
#E.saveClassifier(dataset+'-J48.model')
E.test()
#E.select_missclassified()
#E.saveCSV(dataset+'-J48-err.csv')

E.kill()
"""
################


"""
Über Projection Plot classifikation+error hinzufügen, speichern

Algo:
Klassifizierungen durchühren, nach fehlerwahrscheinlichkeit gewichtete stimme bestimmt silbe

"""