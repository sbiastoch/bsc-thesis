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

	def compute_rhymes(self, row):
		
		ret = OrderedDict()
		i = 0
		for r in row['PhonCV'].split('-'):
			ret.update({'rhyme'+str(i): r})
			i += 1
		return self._fill(ret, 'MySylCnt', 'rhyme')


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


	def compute_syl_open(self, row):
		
		ret = OrderedDict()
		i = 0
		for syl in row['PhonCV'].split('-'):
			ret.update({'syl_open'+str(i): '1' if syl[-1]=='V' else '0'})
			i += 1
		return self._fill(ret, 'MySylCnt', 'syl_open', '-1')
