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
	maxs = {}

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
	'part_of_speech': ['class'],
	'composita_struct': ['REPLACE(ImmClass, \'x\', \'\')'],
	'sa_struct': ['ImmSA'],
	'praefix_class': ['Flat'],
	'praefixes': ['Word'],
	'suffixes': ['Word'],
	'praefixes_phoncat': ['PhonStrsDISC'],
	'suffixes_phoncat': ['PhonStrsDISC'],
	'suffix_class': ['Flat'],
	'rhymes': ['MySylCnt', 'PhonCV'],
	'cv_ratio': ['MySylCnt', 'PhonCV'],
	'koda_len': ['MySylCnt', 'PhonCV'],
	'onset_len': ['MySylCnt', 'PhonCV'],
	'nucleus_len': ['MySylCnt', 'PhonCV'],
	'comp_len': ['LENGTH(REPLACE(ImmClass, \'x\', \'\'))'],
	'sonority': ['PhonStrsDISC', 'MySylCnt'],
	'sonority_ratio': ['PhonStrsDISC', 'MySylCnt'],
	'sonority_dir': ['PhonStrsDISC', 'MySylCnt'],
	'syl_class': ['PhonStrsDISC', 'MySylCnt'],
	'onset_class': ['PhonStrsDISC', 'MySylCnt'],
	'koda_class': ['PhonStrsDISC', 'MySylCnt'],
	'nucleus_class': ['PhonStrsDISC', 'MySylCnt'],
	'syl_weights': ['PhonCV', 'PhonStrsDISC'],
	'is_nomen': ['Word']
	}

#############################################################

q = Query('lemmas2',100)
#q.maxs.update({'MySylCnt':9})
q.alter_by_stress_class()
q.alter_by_multiple_stress_class()
#q.fix_umlauts()
q.alter_by_mysylcnt()
"""q.setFields('sylcnt,stress_class').augment('\
	koda_len,\
	koda_class,\
	nucleus_len,\
	nucleus_class,\
	onset_len,\
	onset_class,\
	syl_lens,\
	syl_suffix,\
	syl_praefix,\
	praefixes,\
	praefix_class,\
	praefixes_classes,\
	suffixes,\
	suffixes_classes,\
	suffix_class,\
	rhymes,\
	cv_ratio,\
	comp_len,\
	sonority,\
	sonority_ratio,\
	sonority_dir,\
	syl_class,\
	syl_weights,\
	is_nomen\
	')\
"""


############ Featuresets ##################
suffix = 'syl_suffix, signi_suffix, suffixes, suffixes_phoncat'
praefix = 'syl_praefix, signi_praefix, praefixes, praefixes_phoncat'
affixe = suffix+', '+praefix

sonority = 'sonority, sonority_ratio, sonority_dir'
weight = 'syl_weights'
sonority_weight = sonority+', '+weight

sylstruct = ''

meta = ''


numeric = ''

sparse = ''

all = affixe +', '+ sonority_weight +', '+ sylstruct +', '+ meta

############ Filters #################
only_proper_classes = 'StrsClass != "multa" and StrsClass != "nulla" and StrsClass != "undef"'
only_nonkomposita = 'LENGTH(REPLACE(ImmClass, \'x\', \'\'))<=1'

###################################

q.setFields('Word, stress_class').augment(sonority_weight).where('MySylCnt=3').where(only_proper_classes)
res = q.execute()
res.show(4)
#res.csv('000047-everything-10syl.csv')

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