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
						]#	'PN',#0.45k
						#	'AV',#0.38k
						#	'NF',#0.33k
						#	'AA',#0.27k
						#	'B'] #0.26k

	main_ImmSAs = ['SA','SS','S','AS','SAS','SSA','SAA'] # s=stem, a=affix

	def compute_part_of_speech(self, row):
		return {'basic_pos': row['class'] if row['class'] in self.signi_classes else 'ø'}

	def compute_composita_struct(self, row):
		return {'comp_struct': row['REPLACE(ImmClass, \'x\', \'\')'] if row['REPLACE(ImmClass, \'x\', \'\')'] in self.main_compound_strucs else 'ø'}

	def compute_is_nomen(self, row):
		return {'nomen': 'T' if row['Word'][0].istitle() else 'F'}

	def compute_sa_struct(self, row):
		return {'sa_struct': row['ImmSA'] if row['ImmSA'] in self.main_ImmSAs else 'ø'}

	def compute_comp_len(self, row):		
		l = row['LENGTH(REPLACE(ImmClass, \'x\', \'\'))']
		return {'comp_len': 1 if l < 2 else l}