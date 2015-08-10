#!/usr/bin/python
# -*- coding: utf-8 -*-

import MySQLdb
import MySQLdb.cursors
import sys
import re
import collections
from affix_handler import AffixHandler
from phon_handler import PhonHandler
from sylstruct_handler import SylstructHandler
from meta_handler import MetaHandler
orderedDict = collections.OrderedDict()
from collections import OrderedDict

class Query(AffixHandler, PhonHandler, SylstructHandler, MetaHandler):


	# pretty names for ugly database fields
	attributes = {
		'no_of_syllables': 'MySylCnt',
		'stress_pattern': 'StrsPat',
		'stress_class': 'StrsClass',
		'all_part_of_speech': 'class',
		'composita_struct_all': 'REPLACE(ImmClass, \'x\', \'\')',
		'comp_len': 'LENGTH(REPLACE(ImmClass, \'x\', \'\'))',
	}

	# virutal attributes and their prerequisits
	augments = {
		'syl_lens': ['MySylCnt', 'PhonCV', 'HeadSyl', 'Word'],
		'syllables': ['MySylCnt', 'HeadSyl', 'Word'],
		'syl_suffix': ['MySylCnt', 'HeadSyl', 'Word'],
		'syl_praefix': ['Flat'],
		'signi_praefix': ['Flat'],
		'signi_suffix': ['Flat'],
		'pos': ['class'],
		'comp_struct': ['REPLACE(ImmClass, \'x\', \'\')'],
		'sa_struct': ['ImmSA'],
		'prae_class': ['Flat'],
		'praefixes': ['Word'],
		'suffixes': ['Word'],
		'praefixes_phoncat': ['PhonStrsDISC'],
		'suffixes_phoncat': ['PhonStrsDISC'],
		'suff_class': ['Flat'],
		'rhymes': ['MySylCnt', 'PhonCV'],
		'syl_open': ['MySylCnt', 'PhonCV'],
		'syl_cv': ['MySylCnt', 'PhonCV'],
		'cv_ratio': ['MySylCnt', 'PhonCV'],
		'koda_len': ['MySylCnt', 'PhonCV'],
		'onset_len': ['MySylCnt', 'PhonCV'],
		'nucleus_len': ['MySylCnt', 'PhonCV'],
		'comp_len': ['LENGTH(REPLACE(ImmClass, \'x\', \'\'))'],
		'sonority': ['PhonStrsDISC', 'MySylCnt'],
		'sonority_ratio': ['PhonStrsDISC', 'MySylCnt'],
		'sonority_dir': ['PhonStrsDISC', 'MySylCnt'],
		'syl_phoncat': ['PhonStrsDISC', 'MySylCnt'],
		'onset_phoncat': ['PhonStrsDISC', 'MySylCnt'],
		'koda_phoncat': ['PhonStrsDISC', 'MySylCnt'],
		'nucleus_phoncat': ['PhonStrsDISC', 'MySylCnt'],
		'syl_weights': ['PhonCV', 'PhonStrsDISC', 'MySylCnt'],
		'nomen': ['Word']
	}


#############################################################

############ Featuresets ##################
suffix = 'syl_suffix, signi_suffix, suffixes, suffixes_phoncat, suff_class'
praefix = 'syl_praefix, signi_praefix, praefixes, praefixes_phoncat, prae_class'
affix = suffix +', '+ praefix

sonority = 'sonority, sonority_ratio, sonority_dir'
weight = 'syl_weights, syl_open'
phoncat = 'syl_phoncat, onset_phoncat, koda_phoncat, nucleus_phoncat'

lens =  'syl_lens, koda_len, onset_len, nucleus_len'
cv = 'syl_cv, cv_ratio'
sylstruct = lens+', '+cv

meta = 'pos, comp_struct, nomen, comp_len'

all = suffix+', '+praefix+', '+sonority+', '+weight+', '+phoncat+', '+lens+', '+cv+', '+meta
numeric = ''
sparse = ''

featureset = {
	'suffix': 	suffix,
	'praefix': 	praefix,
	'affix':	suffix+', '+praefix,
	'sonority':	sonority,
	'weights':	weight,
	'phoncat': 	phoncat,
	'phon': 	sonority+', '+weight+', '+phoncat,
	'lens':		lens,
	'cv':		cv,
	'meta':		meta,

}

############ Filters #################

only_proper_classes = '(StrsClass != "multa" and StrsClass != "nulla" and StrsClass != "undef")'
only_improper_classes = '(StrsClass = "multa" or StrsClass = "nulla" or StrsClass = "undef")'
only_nonkomposita = 'LENGTH(REPLACE(ImmClass, \'x\', \'\'))<=1'
only_komposita = 'LENGTH(REPLACE(ImmClass, \'x\', \'\'))>1'

###################################

#q = Query('lemmas2',1000000)
#q.maxs.update({'MySylCnt':9})
#q.alter_by_stress_class()
#q.alter_by_multiple_stress_class()
#q.fix_umlauts()
#q.alter_by_mysylcnt()
#q.setFields('Word, stress_class').augment(all).where(only_proper_classes)
#res = q.execute()
#res.show(2)
#res.csv('test.csv')

# alle mit vernünftigen klassen
q = Query('lemmas2',1000000)
q.setFields('SylCnt, Word, stress_class').augment(all).where(only_proper_classes)
res = q.execute()
res.removeInfrequent()
res.csv('all.csv')
del q, res

for syls in list('8765432'):
	# alle mit vernünftigen klassen
	q = Query('lemmas2',1000000)
	q.setFields('Word, stress_class').augment(all).where('MySylCnt='+str(syls)).where(only_proper_classes)
	res = q.execute()
	res.removeInfrequent()
	res.csv(str(syls)+'syl-all.csv')
	del q, res

	# alle wörter, von denen wir erwarten vernünftige regeln zu finden, da keine komposita und keine komischen multa-klassen
	q2 = Query('lemmas2',1000000)
	q2.setFields('Word, stress_class').augment(all).where('MySylCnt='+str(syls)).where(only_proper_classes).where(only_nonkomposita)
	res2 = q2.execute()
	res2.removeInfrequent()
	res2.csv(str(syls)+'syl-nocomp.csv')
	del q2, res2

	# Interessant zu schauen, was die Modelle dann auf "komischen" Daten tun...vllt kommt ja was sinnvolles bei raus?
	q1 = Query('lemmas2',1000000)
	q1.setFields('Word, stress_class').augment(all).where('MySylCnt='+str(syls)).where(only_improper_classes)
	res1 = q1.execute()
	res1.csv(str(syls)+'syl-onlynoise.csv')
	del q1, res1

	# interessant für evtl komposita-only studien?
	q3 = Query('lemmas2',1000000)
	q3.setFields('Word, stress_class').augment(all).where('MySylCnt='+str(syls)).where(only_komposita)
	res3 = q3.execute()
	res3.csv(str(syls)+'syl-onlycomp.csv')
	del q3, res3

"""

Suffixe von Silben entkoppeln (-or in Motor), Eisenberg S.23
3 Parameter: Silbenposition, -gewicht, -betonbarkeit
Silbengewicht inkl. Schwa?
Die 7 Phonetisch Distinktive Merkmale
3 Betrachtungsebenen für Silben: CV, Phon, Morph
Sonantische Konsonanten + Schwa (@,m=,n=,N=)
Suffixe ohne Onset betrachten!

c 	x

pf 	+
ts 	=
tS 	J
dZ 	_

i: 	i
a: 	#
O: 	$
u: 	u


a 	&

\"	'
. 	-

9 	/
9~	^
A~	q
E~	0
O~	~


sa_struct keinen mehrwert im vergleich zu composita_struct
was zeichnet betonte präfixe aus? Machine learning zur Feature bestimmung. 3 Klassen: acc, noacc, var
Heuristiken
 - Default: MySylCnt
 - Silbenstruktur: C/V-Ratio, syl_len, koda_len, sonorität, ...
 - Prä-/Suffixe:
 - Sonority, CV-Ratio, Comp-Len, sa_struct, basic_pos

 Reimstruktur klassifizieren:
CCC	V		C
CCC	V		CC
CCC	VV
CCC	VV		C
CC	V
CC	V		C
CC	V		CC
CC	V		CCC
CC	VV
CC	VV		C
CC	VV		CC
C	V
C	V		C
C	V		CC
C	V		CCC
C	V		CCCC
C	VV
C	VV		C
C	VV		CC
C	VV		CCC
	V	
	V		C
	V		CC
	V		CCC
	V		CCCC
	VV	
	VV		C
	VV		CC
	VV		CCC
'balance','blutarm','heroin','kredit','modern','hinterbringen','hintergehen','service','tailleur','tuerkis','widerstreben','gegenueber','imperativ','kopfstehen','hinweg','misstrauen','nebeneinander','perfekt'

POS vereinfachung auf N/A/ø

Prä/Suffixe => 80% (MLP, 2000 samples) => Kombiniere mit Stammsilbendetektion!

Silbenlänge, Gewichte, Komplexitäten als ein Wert darstellen: Balance? 

Silbengewicht (Mengel, S.38)
Sonorität (Mengel, S.42) => Silbierung via Maximum Onset Principle
Offene / Geschlossene Silben
Komplexe / Einfache Silbe

Akzentlose 	Präfixe (be-, ge-, ver-, ...)
			Suffixe (-ig, -bar, -keit, -heit, -ung)
Akzenthafte	Präfixe (ab-, auf- ein-, weg-)

VC-Muster
Silbenlaenge
Indikatorvokale?
Morphklasse: Freies/Gebundenes Präfix P/p, Lex L, (Nicht/)Natives Derivationssuffix S/s, Flexionsmorph f
V zu C Verhaeltnis pro Silbe

Silben alleine binär klassifizieren, zusätzliche Features
 - Position im Wort
 - ...

http://cornelia.siteware.ch/phonetik/arbeitsblphonet/betonungwoe.pdf

 """