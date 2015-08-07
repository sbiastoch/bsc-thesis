#!/usr/bin/python
# -*- coding: utf-8 -*-

import MySQLdb
import MySQLdb.cursors
import sys
import re
import collections
from celex_query import CelexQuery
orderedDict = collections.OrderedDict()
from collections import OrderedDict

"""
	Field 			Function 					Description
	pos 			compute_pos 		 		Grundlegendes POS-Tagging
	comp_struct		compute_comp_struct			Struktur des Kompositums
	nomen			compute_nomen 				Ist das Wort ein Nomen?
	comp_len		compute_comp_len 			Anzahl der Komponenten

	Future Work:
	 - Verlgeichen welche Wortarten (POS) auf die gleiche Weise auf den Akzent wirken
"""
class MetaHandler(CelexQuery):
	signi_classes = ['N',	#30k
					 'A',	#9.8k
					 'V', 	#9.4k
					 'ADV']	#1.2k	Rest: NUM:133, PRON:116, PREP: 108, C:78, I:37, ART:2
					 
	main_compound_strucs = ['N', #11k
							'V', #10k
							'NN',#8.6k
							'A', #3.9k
							'R', #2.1k
							'VN',#1.9k
							'AN',#1.1k
							'F', #1.1k
							'NA',#1.0k
							'BV',#0.7k
							'PV',#0.7k
							'NV',#0.7k
							'PN',#0.45k
							'AV',#0.38k
							'NF',#0.33k
							'AA',#0.27k
							'B'] #0.26k

	def compute_pos(self, row):
		return {'pos': row['class'] if row['class'] in self.signi_classes else 'X'}

	def compute_comp_struct(self, row):
		return {'comp_struct': row['REPLACE(ImmClass, \'x\', \'\')'] if row['REPLACE(ImmClass, \'x\', \'\')'] in self.main_compound_strucs else 'ø'}

	def compute_nomen(self, row):
		return {'nomen': 'T' if row['Word'][0].istitle() else 'F'}

	def compute_comp_len(self, row):		
		l = row['LENGTH(REPLACE(ImmClass, \'x\', \'\'))']
		return {'comp_len': 1 if l < 2 else l}