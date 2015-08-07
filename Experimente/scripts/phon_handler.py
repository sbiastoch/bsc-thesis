#!/usr/bin/python
# -*- coding: utf-8 -*-

import MySQLdb
import MySQLdb.cursors
import sys
import re
import math
import collections
from celex_query import CelexQuery
orderedDict = collections.OrderedDict()
from collections import OrderedDict

"""
	Field 				Function 					Description
	sonority[i] 		compute_sonority 			Berechnet die Sonorität der Silben
	sonority_ratio[i]	compute_sonority_ratio 		Berechnet das Verhältnis von Sonorität pro Phonem je Silbe
	sonority_dir		compute_sonority_dir 	 	Berechnet per Regression den Verlauf (=Steigung) der Sonorität.
	syl_weight[i]		compute_syl_weights 		Liefert das Silbengewich
	syl_phoncat[i]		compute_syl_phoncat 		Liefert die Phonklassen der Phoneme der Silben
	onset_phoncat[i]	compute_onset_phoncat 		Liefert die Phonklassen der Phoneme der Onsets der Silben
	koda_phoncat[i]		compute_koda_phoncat 		Liefert die Phonklassen der Phoneme der Kodas der Silben
	nucleus_phoncat[i]	compute_nucleus_phoncat 	Liefert die Phonklassen der Phoneme der Nulceus der Silben
	syl_open[i]			compute_syl_open 			Ist die Silbe offen (=keine Koda)?


	Feature Work:
	 - Verschiedene Sonoritätsskalen evaluieren.
	 - Verschiedene Phonem-Kategorien evaluieren.
"""
class PhonHandler(CelexQuery):
	
	####  grob nach celex_gug.pdf
	disc_alphabet = { 
		'plosive':				['p', 'b', 't', 'd', 'k', 'g','G'], # stimmt G?
		'frikative':			['f', 'v', 's', 'z', 'S', 'Z', 'x', 'h','D'],
		'affrikate':			['=', '+','J','_'],
		'nasale':				['m', 'n', 'N', 'C','F','H'],
		'liquide':				['l', 'R', 'r'],
		'approximanten':		['E', '/', '&', 'O', '@', 'j', 'w', '#','$','3',')','e','|','o','<'],
		'geschlossene_vokale':	['i', 'u', 'y', 'I', 'Y', 'U'],
		'offene_vokale':		['a', '&', '{', 'A', 'V'],
		'diphtonge': 			['1','2','4','6','W','B','X'],
		'komische_diphtonge':	['^', 'c', 'q', '0', '~'],
	}

	# Source?
	sonority_scale = {
		1: disc_alphabet['plosive']+disc_alphabet['frikative']+disc_alphabet['affrikate'],	# obstruente
		3: disc_alphabet['nasale'],
		4: disc_alphabet['liquide'],
		6: disc_alphabet['approximanten'],
		8: disc_alphabet['geschlossene_vokale']+disc_alphabet['komische_diphtonge'],
		10: disc_alphabet['offene_vokale']+disc_alphabet['diphtonge'],
	}

	# used for phoncat
	phon_cats = {
	'C':				list('pbtdkgNmnlrfvTDszSjxGhwZ'), 	#consonanten
	'A':				list('+=J_'), 						#affrikate
	'S': 				list('CFHPR'), 						#syllabische_consonanten
	'N':				list('^cq0~'), 						#nasalvokale
	'K':				list('IYE/{&AQVOU}@'), 				#kurzvokale
	'D':				list('12456789KLMWBX'), 			#diphtonge
	'L':				list('i!#a$u3y()*<e|o'), 			#langvokale
	}

	"""
	phon_cats = {
		'O': disc_alphabet['plosive']+disc_alphabet['frikative']+disc_alphabet['affrikate'],
		'N': disc_alphabet['nasale'],
		'L': disc_alphabet['liquide'],
		'V': disc_alphabet['approximanten']
			+disc_alphabet['geschlossene_vokale']
			+disc_alphabet['komische_diphtonge']
			+disc_alphabet['offene_vokale']
			+disc_alphabet['diphtonge'],
	}"""

	possible_nucleuses = list('SLDKN')



	def compute_sonority(self, row):
		
		disc = row['PhonStrsDISC'].replace("'",'')
		syls = disc.split('-')
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = 0
			for c in syl:
				missing = c
				for weight, chars in self.sonority_scale.items():
					if c in chars:
						son += int(weight)
						missing = False
						break
				if missing != False:
					print 'Missing DISC-Symbol in Alphabet: ' + missing

#			ret.update({'sonority'+str(i): round(math.log10(son),1)})
			ret.update({'sonority'+str(i): son})
			i += 1

		return self._fill(ret, 'MySylCnt', 'sonority', '0')


	def compute_sonority_ratio(self, row):
		
		disc = row['PhonStrsDISC'].replace("'",'')
		syls = disc.split('-')
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = 0
			for c in syl:
				missing = c
				for weight, chars in self.sonority_scale.items():
					if c in chars:
						son += weight
						missing = False
						break
				if missing != False:
					print 'Missing DISC-Symbol in Alphabet: ' + missing

			ret.update({'sonority_ratio'+str(i): son/len(syl)})
			i += 1

		return self._fill(ret, 'MySylCnt', 'sonority_ratio', '0')

	# Berechnet die Richtung der Sonorität
	def compute_sonority_dir(self, row):
		
		disc = row['PhonStrsDISC'].replace("'",'')
		syls = disc.split('-')
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = 0
			for c in syl:
				missing = c
				for weight, chars in self.sonority_scale.items():
					if c in chars:
						son += int(weight)
						missing = False
						break
				if missing != False:
					print 'Missing DISC-Symbol in Alphabet: ' + missing

			ret.update({'sonority'+str(i): son})
			i += 1

		a,b = self._basic_linear_regression(xrange(0, len(syls)), ret.values())

		return {'sonority_dir': a}
	#	return {'sonority_dir': 1 if a > 0 else 0 if a==0 else -1}
	#	return {'sonority_dir': round(math.log10(a+1) if a >= 1 else (-math.log10(-a+1) if a<=-1 else 0), 1)}

	def compute_syl_weights(self, row):
		ret = OrderedDict()
		cv_syls = row['PhonCV'].split('-')
		phon_syls = self._get_phon_syls(row)
		i = 0
		for syl in cv_syls:
			weight = "heavy" # 2=schwer

			if phon_syls[i].find('@') > -1:
				weight = "schwa" # schwasilbe
			elif len(syl) >= 2 and syl[-2:] == 'VC' or syl[-2:] == 'VV':
				weight = "light" # leicht

			ret.update({'syl_weight'+str(i): weight})
			i += 1
		return self._fill(ret, 'MySylCnt', 'syl_weight', 'ø')


	def compute_syl_phoncat(self, row):
		
		syls = self._get_phon_syls(row)
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = ""
			for c in syl:
				son += self._classify_morph(c)
			ret.update({'syl_phoncat'+str(i): son})
			i += 1

		return self._fill(ret, 'MySylCnt', 'syl_phoncat', 'ø')

	def compute_onset_phoncat(self, row):
		syls = self._get_phon_syls(row)
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = ""
			for c in syl:
				son += self._classify_morph(c)
			n = 0

			while n < len(son) and son[n] not in self.possible_nucleuses:
				n += 1

			ret.update({'onset_phoncat'+str(i): son[0:n]})
			i += 1

		return self._fill(ret, 'MySylCnt', 'onset_phoncat', 'ø')

	def compute_koda_phoncat(self, row):
		syls = self._get_phon_syls(row)
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = ""
			for c in syl:
				son += self._classify_morph(c)

			n = len(son)-1
			while n >= 0 and son[n] not in self.possible_nucleuses:
				n -= 1

			ret.update({'koda_phoncat'+str(i): son[n+1:]})
			i += 1

		return self._fill(ret, 'MySylCnt', 'koda_phoncat', 'ø')

	def compute_nucleus_phoncat(self, row):
		syls = self._get_phon_syls(row)
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = ""
			for c in syl:
				son += self._classify_morph(c)
			start = re.search('['+''.join(self.possible_nucleuses)+']',son).start()
			n = start
			while n < len(son) and son[n] in self.possible_nucleuses:
				n += 1

			ret.update({'nucleus_phoncat'+str(i): son[start:n]})
			i += 1
			if len(son[start:n]) < 1:
				print 'Fehler: Silbe hat kein Nukleus!',row

		return self._fill(ret, 'MySylCnt', 'nucleus_phoncat', 'ø')


	def compute_syl_open(self, row):
		
		ret = OrderedDict()
		i = 0
		for syl in row['PhonCV'].split('-'):
			ret.update({'syl_open'+str(i): 'o' if syl[-1]=='V' else 'c'})
			i += 1
		return self._fill(ret, 'MySylCnt', 'syl_open', 'ø')


	def _get_phon_syls(self, row):
		disc = row['PhonStrsDISC'].replace("'",'')
		syls = disc.split('-')
		return syls


	# Übersetzt ein Graphem in ein Phonem
	def _classify_morph(self, char):
		for mclass, chars in self.phon_cats.items():
			if char in chars:
				return str(mclass)

		print 'Missing DISC-Symbol in Alphabet: ' + char
		return False
		
	def _basic_linear_regression(self, x, y):
	# From: http://jmduke.com/posts/basic-linear-regressions-in-python/

	    # Basic computations to save a little time.
	    length = len(x)
	    sum_x = sum(x)
	    sum_y = sum(y)

	    # Σx^2, and Σxy respectively.
	    sum_x_squared = sum(map(lambda a: a * a, x))
	    sum_of_products = sum([x[i] * y[i] for i in range(length)])

	    try:
		    # Magic formulae!  
		    a = (sum_of_products - (sum_x * sum_y) / length) / (sum_x_squared - ((sum_x ** 2) / length))
		    b = (sum_y - a * sum_x) / length
	    except Exception, e:
	    	return 0,0
	    return a, b
