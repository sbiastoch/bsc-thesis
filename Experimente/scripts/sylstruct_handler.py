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
	Field 				Function 		Description
	syl_len[i]		compute_syl_lens 	Länge der Silben
	syl[i]			compute_syllables 	Liefert die Silben   ### Unbenutzt, da overfitting wahrschienlich
	syl_cv[i]		compute_syl_cv 		Liefert die Silben in CV-Darstellung
	cv_ratio[i]		compute_cv_ratio 	Verhältnis von C (Konsonanten) zu V (Vokalen)
	koda_len[i]		compute_koda_len 	Länge der Koda der Silben
	onset_len[i]	compute_onset_len 	Länge des Onsets der Silben
	nucleus_len[i]	compute_nucleus_len	Länge des Nucleus der Silben

	Feature Work:
	 - 
"""
class SylstructHandler(CelexQuery):

	def compute_syl_lens(self, row):
		ret = OrderedDict()
		syls = self._get_syllables(row)
		i = 0
		for syl in syls:
			ret.update({'syl_len'+str(i): len(syl)})
			i += 1

		return self._fill(ret, 'MySylCnt', 'syl_len', '0')

	def compute_syllables(self, row):
		ret = OrderedDict()
		syls = self._get_syllables(row)

		i = 0
		for syl in syls:
			ret.update({'syl'+str(i): syl})
			i += 1

		return self._fill(ret, 'MySylCnt', 'syl')

	def compute_syl_cv(self, row):
		
		ret = OrderedDict()
		i = 0
		for r in row['PhonCV'].split('-'):
			ret.update({'syl_cv'+str(i): r})
			i += 1
		return self._fill(ret, 'MySylCnt', 'syl_cv')


	def compute_cv_ratio(self, row):
		
		ret = OrderedDict()
		i = 0
		for r in row['PhonCV'].split('-'):
			ret.update({'cv_ratio'+str(i): r.count('C')/r.count('V') if r.count('V') > 0 else 10})
			i += 1
		return self._fill(ret, 'MySylCnt', 'cv_ratio', '0')

	def compute_koda_len(self, row):
		#fixme
		
		ret = OrderedDict()
		i = 0
		for r in row['PhonCV'].split('-'):
			pos = r.find('VC')
			ret.update({'koda_len'+str(i): len(r[pos+1:]) if int(pos) > -1 else '0'})
			i += 1
		return self._fill(ret, 'MySylCnt', 'koda_len', '0')

	def compute_onset_len(self, row):
		
		#fixme
		ret = OrderedDict()
		i = 0
		for r in row['PhonCV'].split('-'):
			pos = r.find('CV')
			ret.update({'onset_len'+str(i): len(r[:pos+1]) if int(pos) > -1 else '0'})
			i += 1
		return self._fill(ret, 'MySylCnt', 'onset_len', '0')

	def compute_nucleus_len(self, row):
		
		#fixme
		ret = OrderedDict()
		i = 0
		for r in row['PhonCV'].split('-'):
			ret.update({'nucleus_len'+str(i): str(r.count('V'))})
			i += 1
		return self._fill(ret, 'MySylCnt', 'nucleus_len', '0')
