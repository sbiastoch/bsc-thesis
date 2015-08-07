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
	Field					Function 					Description
	suffix_class			compute_suffix_class: 		Liefert die Klasse des Präfixes: ø/Betont/Unbetont
	praefix_class			compute_praefix_class: 		Liefert die Klasse des Suffixes: ø/Betont/Unbetont
	signi_praefix			compute_signi_praefix: 		Liefert das Präfix, sofern es ein häufig auftretendes ist
	signi_suffix			compute_signi_suffix: 		Liefert das Suffix, sofern es ein häufig auftretendes ist
	praefix_phoncat[1-5]	compute_praefixes_phoncat: 	Liefert die Phonetische Klasse (O/S/A/...) der ersten 1-5 Phoneme
	suffix_phoncat[1-5]		compute_suffixes_phoncat: 	Liefert die Phonetische Klasse (O/S/A/...) der letzten 1-5 Phoneme
	suffix[1-5]				compute_suffixes: 			Liefert die letzten 1-5 Buchstbaen eines Wortes
	praefix[1-5]			compute_praefixes: 			Liefert die ersten 1-5 Buchstaben eines Wortes

	Feature work:
	 - 
"""
class AffixHandler(CelexQuery):

# Codierung Umlaute / ss+ß
#'1',	 '2', 	'3', 	'9', 	'8', 	'7', 	'$',	'$'
#'ae', 	'ue', 	'oe',	'Ae', 	'Ue', 	'Oe',	'ss',	'ß'])

#	signifikante_praefixe = ['ab','an','auf','aus','be','bei','ein','ent','er','fern', 'fort','hin','hoch','mit','nach','2ber','um','unter','ur','ver','vor','weg','zer','zu','durch','gegen','hinter'] # 27 aus 83
#	substantivierungssuffixe = ['ei','er','ette','graph','graphie','ik','ismus','ist','ion','heit','keit','ler','ling','loge','logie','mut','nis','oid','oide','rich','schaft','tum','ung','zid']
#	adjektivierungssuffixe = ['bar', 'esk', 'id', 'ig', 'isch', 'lich', 'sam', 'oid', 'zid']
#	andere_suffixe = ['end', 'lei']

	#siehe "Zusammenfassung Suffixe bei Nonkomposita"
	signifikante_suffixe = ['ig','ung','er','ler','lich','isch','ling','ik','nis', 'ei','ist','ismus','bar','sam','keit','heit','tum','schaft'] # 18 aus 35
	suff_unbetont = ['ig','ik','ismus','lich','ler','ling','nis','tum']
	suff_betont = ['ist', 'ei']
	eigene_suffixe = signifikante_suffixe + suff_unbetont + suff_betont

	#siehe "Zusammenfassung Präfixe bei Nonkomposita"
	prae_unbetont = ['be','ent','er','ver','zer'] # 2. silbe wird betont
	prae_betont = ['ab','an','auf','aus','bei','durch','ein','fern','fort','gegen','hinter','hin','hoch','mit','nach','2ber','um','unter','ur','vor','weg','zu']
	eigene_praefixe = prae_unbetont + prae_betont

	# S.28: Giegerich: metrical phonology and phonological structure
	akzenttragende_suffixe = ['abel','age','al','ial','and','ant','anz','ar','1r','at','eil','ement','end','ei','ent','enz','esk','euse','lade','ibel','ie','ier','ine','ion','ist','it1t','iv','os','3s','ual','uell','ur'] 
	native_suffixe = ['chen','ler','heit','keit','igkeit','isch','lein','ling','los','bar','m1$ig','nis','sam','schaft','ung','tum','sel','er','icht']
	nonnative_unstressed_suffixe = ['ian','ien','ier','is','iter','us','a','um','o','i','or','ik']
	giegerich_suffixe = akzenttragende_suffixe + native_suffixe + nonnative_unstressed_suffixe


	def compute_suffix_class(self, row):
		parts = row['Flat'].split('+')
		suffix = parts[-1]

		if len(parts) < 2:
			return {'suff_class': 'ø'}
		elif suffix in self.suff_unbetont:
			return {'suff_class': 'noacc'}
		elif suffix in self.suff_betont:
			return {'suff_class': 'acc'}
	#	elif suffix in self.substantivierungssuffixe + self.adjektivierungssuffixe + self.andere_suffixe:
	#		return {'suff_class': suffix}
		else:
			return {'suff_class': 'ø'}

	# klassifizierung der 1. silbe in betont/unbetont/gemischt
	def compute_praefix_class(self, row):
		parts = row['Flat'].split('+')
		praefix = parts[0]

		if len(parts) < 2:
			return {'prae_class': 'ø'}
		elif praefix in self.prae_unbetont:
			return {'prae_class': 'noacc'}
		elif praefix in self.prae_betont:
			return {'prae_class': 'acc'}
	#	elif praefix in self.verbalpraefixe + self.wiki_praefixes:
	#		return {'prae_class': praefix}
		else:
			return {'prae_class': 'ø'}
		

	# erste silbe, falls häufiges präfix
	def compute_signi_praefix(self, row):
#		praefixes = self.signifikante_praefixe
		praefixes = self.eigene_praefixe
		praefix = ''.join(self._get_word_struct(row)[0])
		return {'signi_praefix': praefix if praefix in praefixes else 'ø'}

	def compute_signi_suffix(self, row):
	#	suffixes = self.signifikante_suffixe
		suffixes = self.giegerich_suffixe
		suffix = ''.join(self._get_word_struct(row)[-1])
		return {'signi_suffix': suffix if suffix in suffixes else 'ø'}

	def compute_syl_suffix(self, row):
		return {'syl_suffix': ''.join(self._get_word_struct(row)[-1])}

	def compute_syl_praefix(self, row):
		return {'syl_praefix': ''.join(self._get_word_struct(row)[0])}




	def compute_suffixes_phoncat(self,row):
		phons = ''.join(self._get_phon_syls(row))
		return OrderedDict((
			('suffix_phoncat1', self._classify_morph(phons[-1:])),
			('suffix_phoncat2', ''.join(map(lambda x: self._classify_morph(x), list(phons[-2:]))) if len(phons) >= 2 else 'ø'),
			('suffix_phoncat3', ''.join(map(lambda x: self._classify_morph(x), list(phons[-3:]))) if len(phons) >= 3 else 'ø'),
			('suffix_phoncat4', ''.join(map(lambda x: self._classify_morph(x), list(phons[-4:]))) if len(phons) >= 4 else 'ø'),
			('suffix_phoncat5', ''.join(map(lambda x: self._classify_morph(x), list(phons[-5:]))) if len(phons) >= 5 else 'ø'),
		))

	# Phonetische Klasse der ersten 1-5 Phone 
	def compute_praefixes_phoncat(self,row):
		phons = ''.join(self._get_phon_syls(row))
		return OrderedDict((
			('praefix_phoncat1', self._classify_morph(phons[:1])),
			('praefix_phoncat2', ''.join(map(lambda x: self._classify_morph(x), list(phons[:2]))) if len(phons) >= 2 else 'ø'),
			('praefix_phoncat3', ''.join(map(lambda x: self._classify_morph(x), list(phons[:3]))) if len(phons) >= 3 else 'ø'),
			('praefix_phoncat4', ''.join(map(lambda x: self._classify_morph(x), list(phons[:4]))) if len(phons) >= 4 else 'ø'),
			('praefix_phoncat5', ''.join(map(lambda x: self._classify_morph(x), list(phons[:5]))) if len(phons) >= 5 else 'ø'),
		))



	# Letzte 1-5 Buchstaben eines Wortes
	def compute_suffixes(self,row):
		return OrderedDict((
			('suffix1', row['Word'][-1:]),
			('suffix2', row['Word'][-2:] if len(row['Word']) >= 2 else 'ø'),
			('suffix3', row['Word'][-3:] if len(row['Word']) >= 3 else 'ø'),
			('suffix4', row['Word'][-4:] if len(row['Word']) >= 4 else 'ø'),
			('suffix5', row['Word'][-5:] if len(row['Word']) >= 5 else 'ø'),
		))

	# Erste 1-5 Buchstaben eines Wortes
	def compute_praefixes(self,row):
		return OrderedDict((
			('praefix1', row['Word'][:1]),
			('praefix2', row['Word'][:2] if len(row['Word']) >= 2 else 'ø'),
			('praefix3', row['Word'][:3] if len(row['Word']) >= 3 else 'ø'),
			('praefix4', row['Word'][:4] if len(row['Word']) >= 4 else 'ø'),
			('praefix5', row['Word'][:5] if len(row['Word']) >= 5 else 'ø'),
		))
