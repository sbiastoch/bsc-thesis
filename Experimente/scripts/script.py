#!/usr/bin/python
# -*- coding: utf-8 -*-

import MySQLdb
import MySQLdb.cursors
import sys
import re
import collections
orderedDict = collections.OrderedDict()
from collections import OrderedDict

class Query:
	fields = []
	filters = []
	table = 'lemmas'
	limit = 1000
	results = []
	augmented = []
	query = ""
	connection = None

	# http://de.wiktionary.org/wiki/Kategorie:Pr%C3%A4fix_(Deutsch)
	# http://de.wiktionary.org/wiki/Vorlage:Suffixe_%28Deutsch%29
	verbalpraefixe = ['ab', 'an', 'auf', 'aus', 'auseinander', 'be', 'bei', 'da', 'dafuer', 'dagegen',
	'daher', 'dahin', 'dahinter', 'daneben', 'dar', 'darein', 'darueber', 'darunter', 
	'davon', 'davor', 'dazu', 'dazwischen', 'durch', 'ein', 'er', 'ent', 'entgegen', 
	'entlang', 'entzwei', 'fern', 'fort', 'fuer', 'gegen', 'gegenueber', 'her', 'herab',
	'heran', 'herauf', 'heraus', 'herbei', 'herein', 'hernieder', 'herueber', 'herum',
	'herunter', 'hervor', 'herzu', 'hin', 'hinab', 'hinan', 'hinauf', 'hinaus', 'hinein', 
	'hinter', 'hinueber', 'hinunter', 'hinweg', 'hinzu', 'hoch', 'mit', 'nach', 'neben', 
	'nieder', 'ober', 'um', 'unter', 'ur', 'ueber', 'ver', 'veraus', 'verun', 'vor',
	'vorab', 'voran', 'vorauf', 'voraus', 'vorbei', 'vorueber', 'weg', 'wider', 'wieder', 'zer', 'zu']

	wiki_praefixes = ['a','ab','abs','ac','ad','af','ag','ak','al','aller','allo','amb','ambi','amphi',
	'an','ana','ant','ante','anti','ap','aph','apo','ar','arsch','as','at','auf','aus',
	'auseinander','auto','azo','be','bei','bi','bin','cis','Cyber','da','dagegen','daher',
	'dahin','daneben','dar','davon','de','der','des','di','dia','dif','dis','durch','e',
	'ef','ein','ek','ent','entgegen','entlang','entzwei','er','Er','erz','Erz','eu','Eu',
	'ex','exo','extra','fern','fort','für','ge','gegen','haupt','heim','hemi','Hemi','her',
	'herab','heran','heraus','herunter','hie','hin','homo','hyper','Hyper','hypo','il','im',
	'in','infra','Ingen','inter','intra','intro','ir','iso','juxta','kata','ko','kol','kom',
	'kon','kontra','kor','meta','miss','mon','mono','nach','neo','nieder','non','ob','oc',
	'of','ok','oligo','omni','op','ortho','pan','Pan','para','per','peri','plagio','poly',
	'post','pro','proto','prä','präter','re','red','semi','stock','sub','suf','sug','suk',
	'sup','super','supra','sur','syl','sym','syn','tele','Tele','tra','tran','trans','tri',
	'ultra','um','un','unter','ur','ver','vor','weg','wider','wieder','zer','zirkum','zis',
	'zu','äqui','über']

	substantivierungssuffixe = ['ei','er','ette','graph','graphie','ik','ismus','ist','ion','heit','keit','ler',
	'ling','loge','logie','mut','nis','oid','oide','rich','schaft','tum','ung','zid']

	adjektivierungssuffixe = ['bar', 'esk', 'id', 'ig', 'isch', 'lich', 'sam', 'oid', 'zid']

	andere_suffixe = ['end', 'lei']

	signifikante_praefixe = ['ab','an','auf','aus','be','bei','ein','ent','er','fern', # 27 aus 83
	'fort','hin','hoch','mit','nach','ueber','um','unter','ur','ver','vor','weg','zer','zu','durch','gegen','hinter']

	signifikante_suffixe = ['ig','ung','er','ler','lich','isch','ling','ik','nis', 
	'ei','ist','ismus','bar','sam','keit','heit','tum','schaft'] # 18 aus 35

	#siehe "Zusammenfassung Präfixe bei Nonkomposita"
	prae_unbetont = ['be','ent','er','ver','zer'] # 2. silbe wird betont
	prae_betont = ['ab','an','auf','aus','bei','durch','ein','fern','fort','gegen','hinter','hin','hoch','mit','nach','ueber','um','unter','ur','vor','weg','zu']

	suff_unbetont = ['ig','ik','ismus','lich','ler','ling','nis','tum']
	suff_betont = ['ist', 'ei']

	akzenttragende_suffixe = ['abel','age','al','ial','and','ant','anz','ar','är','at','eil','ement','end','ei','ent','enz','esk','euse','lade','ibel','ie','ier','ine','ion','ist','ität','iv','os','ös','ual','uell','ur'] # eisenberg nach giegerih

	def __alter_table(self, table, col):
		alter = 'ALTER TABLE  `'+table+'` ADD  `'+col+'` VARCHAR(50);'
		try:
			if self.init_connection().execute(alter):
				self.connection.commit()
				sys.stdout.write('Altering table '+table+' by column '+col+'...',)
				return True
		except Exception, e:
			return False

	# Klassifiziert das Betonungsmuster in Prima (1.), Sekunda (2.), Ultima (-1.), Paenultima (-2.), Antepaenultima (-3.), 
	#	Nulla (unbetont) und undef (sonstiges).
	#	Bei Wörtern mit drei oder weniger Silben wird von hinten gezählt.
	def alter_by_stress_class(self, force=False):
		if self.__alter_table(self.table, 'StrsClass') or force:
			classify_sql = ["UPDATE "+self.table+" SET StrsClass =  '';",
			'UPDATE '+self.table+' SET StrsClass =  "prima" WHERE StrsPat LIKE "1%%" AND StrsPat NOT LIKE  "1%%1%%";',
			'UPDATE '+self.table+' SET StrsClass =  "sekunda" WHERE StrsPat LIKE "01%%" AND StrsPat NOT LIKE  "01%%1%%";',
			'UPDATE '+self.table+' SET StrsClass =  "ultima" WHERE StrsPat LIKE "%%1" AND StrsPat NOT LIKE  "%%1%%1";',
			'UPDATE '+self.table+' SET StrsClass =  "paenultima" WHERE StrsPat LIKE "%%10" AND StrsPat NOT LIKE  "%%1%%10";',
			'UPDATE '+self.table+' SET StrsClass =  "antepaenultima" WHERE StrsPat LIKE "%%100" AND StrsPat NOT LIKE  "%%1%%100";',
			'UPDATE '+self.table+' SET StrsClass =  "multa" WHERE StrsPat LIKE "%%1%%1%%";',
			'UPDATE '+self.table+' SET StrsClass =  "nulla" WHERE StrsPat not LIKE "%%1%%";',
			'UPDATE '+self.table+' SET StrsClass =  "undef" WHERE StrsClass="";']
			
			self.make_update(classify_sql)
			print "altering done\n"

	def make_update(self, sqls):
		conn =  self.init_connection()
		for sql in sqls:
			conn.execute(sql)
		return self.connection.commit()

	def alter_by_multiple_stress_class(self, force=False):
		if self.__alter_table(self.table, 'MultipleStrsClass') or force:
			sqls = [
			"UPDATE "+self.table+" SET MultipleStrsClass='0'", 
			"UPDATE "+self.table+" SET MultipleStrsClass='1' WHERE \
						HeadSyl like 'unter-%' or \
						HeadSyl like 'um-%' or \
						HeadSyl like 'ueber-%' or \
						HeadSyl like 'durch-%' or \
						Word in ('balance','blutarm','heroin','kredit','modern','hinterbringen','hintergehen',\
								 'service','tailleur','tuerkis','widerstreben','gegenueber','imperativ','kopfstehen',\
								 'hinweg','misstrauen','nebeneinander','perfekt')"]
			
			self.make_update(sqls)
			print "altering done\n"

	def alter_by_mysylcnt(self, force=False):
		print "Computing and altering MySylCnt..."
		if self.__alter_table(self.table, 'MySylCnt') or force:
			q = Query(self.table,55000).setFields('IdNum').augment('syllables').execute()
			sqls = ["UPDATE "+self.table+" SET MySylCnt=''"]
			for row in q.results:
				vals = row.values()
				sqls.append('UPDATE '+self.table+' SET MySylCnt='+str(sum([x != 'ø' for x in vals[:-1]]))+' WHERE IdNum='+vals[-1]+';')
			print "Computing finished, making updates..."
			
			self.make_update(sqls)
			print "altering done\n"

	def fix_umlauts(self): # untested
		sqls = [
		"UPDATE "+self.table+" SET Word = REPLACE(Word,'ae','ä') WHERE Word Like '%ae%';",
		"UPDATE "+self.table+" SET Word = REPLACE(Word,'oe','ä') WHERE Word Like '%oe%';",
		"UPDATE "+self.table+" SET Word = REPLACE(Word,'ue','ä') WHERE Word Like '%ue%';",
		]
		self.make_update(sqls)
		print "Umlaute gefixt\n"

	def __init__(self, table='lemmas', limit=1000):
		self.table = table
		self.limit = limit

	def table(self, table):
		self.table = table
		return self

	def limit(self, limit, limit2=0):
		if limit2 > 0:
			self.limit = str(limit)+', '+str(limit2)
		else:
			self.limit = str(limit)

		return self

	def where(self, filter):
		for pretty, ugly in self.attributes.items():
			if filter.find(pretty) > -1:
				print filter, pretty, ugly
				filter.replace(pretty, ugly)
		self.filters.append(filter)
		return self
	
	def setFields(self, field):
		self.fields.extend(map(str.strip, field.split(',')))
		return self

	def init_connection(self):
		self.connection = MySQLdb.connect(	host="localhost", # your host, usually localhost
								user="root", # your username
								passwd="***REMOVED***", # your password
								charset='utf8',
								use_unicode=True,
								db="celex", cursorclass=MySQLdb.cursors.DictCursor ) # name of the data base
		return self.connection.cursor() 

	def _build_query(self, actual_fields):
		sql = 'SELECT ' + ', '.join(map(lambda x: self.ugly_name(x), actual_fields)) + ' FROM ' + self.table
		
		if len(self.filters)>0:
			sql += ' WHERE ' + (' AND '.join(self.filters)) 
		
	#	sql += ' ORDER by RAND()'		# breaks the max(MySylCnt)
		
		sql += ' LIMIT '+ str(self.limit)
		
		print 'Query built: "' + sql + '"'
		return sql

	def _actual_fields(self):
		self.out('Computing all required fields...')
		actual_fields = list(self.fields)

		#add fields required for augments
		for var in self.augmented:
			if var in self.augments:
				actual_fields.extend(self.augments[var])
			else:
				print 'Can\'t augment unknown variable "' + var + '"!'

		if len(actual_fields) == 0:
			actual_fields = ['*']

		actual_fields = list(set(actual_fields))
		return actual_fields

	def _compute_augments(self, row):
		
		result_row = OrderedDict()

		for var in self.augmented:
			value = getattr(self, 'compute_'+var)(row)
			for l,v in value.items():
				result_row.update({l: v})

		return result_row

	def _build_resultset(self, cur, actual_fields):
		self.out('Building resultset...')
		rows = cur.fetchall()

		i=0
		step = int(len(rows)/(len(rows)/100.0))
		for row in rows:
			result_row = self._compute_augments(row)
			if step > 0 and i % step == 0:
				self.out(str(i)+'/'+str(len(rows))+' processed')
			i += 1
			
			# collect desired columns
			for field, value in row.items():
				pretty_label = self.pretty_name(field)
				if pretty_label in self.fields + self.augments.keys():
					result_row.update({pretty_label: str(value)})

			self.results.append(result_row)

		if step > 0:
			self.out(str(len(rows)-1)+'/'+str(len(rows)-1)+' processed\nProcessing finished.')

	def out(self, msg):
		print(msg)

	def execute(self):
		self.out('Execution started...')
		actual_fields = self._actual_fields()
		sql = self._build_query(actual_fields)

		cur = self.init_connection()
		
		cur.execute(sql)
		self.query = sql
		self._build_resultset(cur, actual_fields)
		self.postprocess()
		
		return self

	def postprocess(self):
		removed = []
		print "Postprocessing started..."
		# determine the actual col-count
		cols = 0
		for row in self.results[:10]:
			cols += len(row)
		cols = int(cols/10)

		i = 0
		total = len(self.results)
		while i < total:
			if len(self.results[i]) != cols:
				print "Removed the "+str(i)+"th row ("+str(cols)+" cols expected, found "+str(len(self.results[i]))+")"
				removed.append(self.results[i])
				self.results.remove(self.results[i])
				total -= 1
				i -= 1
			i += 1

		print
		print str(len(removed))+" were rows removed:",
		self.show(2, removed)
		print "Postprocessing finished!"

	def pretty_name(self, ugly_name):
		int_attrs = dict(map(reversed, self.attributes.items()))

		if ugly_name in int_attrs:
			pretty_name = int_attrs[ugly_name]
		else:
			pretty_name = ugly_name

		return pretty_name

	def ugly_name(self, pretty_name):
		if pretty_name in self.attributes:
			return self.attributes[pretty_name]
		else:
			return pretty_name

	def clear(self):
		self.results = []
		return self

	def count(self):
		return len(self.results)

	def csv(self, file=None):
		self.out('Writing to CSV-file...')
		if file == None:
			file = sys.stdout
		else:
			file = open('/home/sbiastoch/Schreibtisch/csv_files/'+file, 'w')
		last = self.results[0].keys()[-1]
		for label, v in self.results[0].items():
			if last != label:
				file.write(self.pretty_name(label)+',')
			else:
				file.write(self.pretty_name(label))
		file.write("\n")

		for row in self.results:
			for label, value in row.items():
				if last != label:
					file.write(str(value)+',')
				else:
					file.write(str(value))
			file.write("\n")

		file.close()
		self.out('Writing finished! Tutti completti!')
		

	def count(self):
		if self.results == None or self.results == []:
			return False
		return len(self.results)

	def show(self, spacing=5, results = None):
		if results == None:
			results = self.results

		if len(results) < 1:
			print "No elements to show."
			return self
		print
		for label, v in results[0].items():
			print self.pretty_name(label) + (spacing-len(self.pretty_name(label))/4) * "\t",
		print

		for row in results:
			for label, value in row.items():
				print (str(value) if len(str(value))>0 else 'Ø') + (spacing-len(str(value))/4)*"\t",
			print

	def compute_syl_lens(self, row):
		
		ret = OrderedDict()
		syls = self._get_syllables(row)
		i = 0
		for syl in syls:
			ret.update({'syl_len'+str(i): len(syl)})
			i += 1

		return self._fill(ret, 'MySylCnt', 'syl_len', '0')

	def _get_syllables(self, row):

		# Ausgehend von der silbifizierung des Hauptwortes, also des Lemmas
		head = row['HeadSyl']

		# Der Duden sagt, es gibt keine Silben mit nur einem Buchstaben, jedoch gäbe es zB
		# in Abend dann zwei Sonoritätsberge, was linguistisch sehr Problematisch ist
		head = head.replace('=','-')
		
		# Anhand der Silbenmarkierungen aufsplitten
		syls = head.split('-')

		# Um Umlaute und ausgelassene f wie in Schifffahrt zu erkennen, muss auch "word" berücksichtigt werden
		# Silbifiziere word mithilfe von syls
		word = row['Word']

		# Umlaute in word durch einzelne zeichen ersetzen
		for x, uml in dict(zip(['1', '2', '3', '9', '8', '7'],['ae', 'ue', 'oe','Ae', 'Ue', 'Oe'])).items():
			if word.count(uml) > 0:
				word = word.replace(uml,x)


		a,b = 0,len(syls[0])
		i = 0

		for syl in syls:
			word_syl = word[a:b]

			if syl != word_syl:
				# In HeadSyl ist jedes ss als $ notiert (bis auf 6 Ausnahmen), wie zwischen ss und ß unterscheiden?! Es gibt 3210 Wörter mit ss
				# generisches $ vielleicht gar nicht so doof, ß-Regeln nämlich recht schwierig...
				# Trotzdem muss die Silbenlänge in word angepasst werden, sofern ein $ auftritt und ein ss in word ist
				if word[a:b+1 if b < len(word) else 0].find('ss') > -1 and syl.find('$') > -1:
					b += 1
					word_syl = word[a:b]

				# Neue Rechtschreibung: Bettuch => Betttuch, Schiffahrt => Schifffahrt, da in silbifizierung das fff/ttt/..? enthalten ist
				# ist in HeadSyl bereits richtig enthalten

				# Bek-ken => Becken
				if syl[-1] == 'k' and word_syl[-1] == 'c':
					if i+1 == len(syls):
						print word_syl, word, head, syls
					syls[i+1] = 'c'+syls[i+1]
					syl = syl[:-1]
					b -= 1
					word_syl = word[a:b]
					
				# Umlaute einfügen
				for uml in ['1', '2', '3', '9', '8', '7']:
					pos = word_syl.find(uml)
					if pos > -1:
						syl = syl[:pos] + uml + syl[pos+1:]
			
			syls[i] = syl
			i += 1
			a,b = b,b+len(syls[i]) if i < len(syls) else 0

		return syls

	def _get_word_struct(self, row):
		syls = self._get_syllables(row)
		struct = []
		sonants = list('aeiouAEIOU123987')
		for syl in syls:		
			i = 0
			for i in xrange(0,len(syl)):
				if syl[i] in sonants:
					break
			j = i
			while j < len(syl) and syl[j] in sonants:
				j += 1

			onset = syl[:i]
			nucleus = syl[i:j] if j < len(syl) else syl[i:]
			koda = syl[j:] if j < len(syl) else ''

			struct.append([onset, nucleus, koda])

		return struct

	def _get_nucleus(self,row):
		ret = []
		syls = self._get_word_struct(row)
		for syl in syls:
			ret.append(syl[1])
		return ret

	def _get_koda(self,row):
		ret = []
		syls = self._get_word_struct(row)
		for syl in syls:
			ret.append(syl[2])
		return ret

	def _get_onset(self,row):
		ret = []
		syls = self._get_word_struct(row)
		for syl in syls:
			ret.append(syl[0])
		return ret

	def compute_syllables(self, row):
		ret = OrderedDict()
		syls = self._get_syllables(row)

		i = 0
		for syl in syls:
			ret.update({'syl'+str(i): syl})
			i += 1


		return self._fill(ret, 'MySylCnt', 'syl')

	maxs = {}

	def _fill(self, ret, field, label, empty = 'ø'):
		if not field in self.maxs.keys():
			sql = "SELECT max("+field+") from ("+self.query+") a"
			cur = self.connection.cursor()
			cur.execute(sql)
			max = int(cur.fetchone()['max(MySylCnt)'])
			self.maxs.update({field: max})

		while len(ret) < self.maxs[field]:
			ret.update({label+str(len(ret)): 0 if empty == '0' else empty})
		return ret

	def compute_suffix_class(self, row):
		
		#alle_suffixe = self.substantivierungssuffixe + self.adjektivierungssuffixe + self.andere_suffixe
		#alle_suffixe = self.signifikante_suffixe
		#suffix = row['Flat'].split('+')[-1]
		#return {'suffix': suffix if suffix in alle_suffixe else 'ø'}

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
		
		#classes = {
		#	'bound': ['be', 'emp', 'ent', 'er', 'ge', 'ver', 'zer'],
		#	'bound_acc': ['miss', 'un'],
		#	'bound_noacc': ['kon','per','re'],
		#	'free_acc': ['ab','an','auf','aus','bei','ein','fort','hin','her','los','mit','nach','vor','weg','zu'],
		#}
		#praefix = row['Flat'].split('+')[-1]
		#for pclass, preas in classes.items():
		#	if praefix in preas:
		#		return {'prae_class': pclass}

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
	def compute_praefix(self, row):
		
		#praefixes = self.verbalpraefixe
		praefixes = self.signifikante_praefixe
		praefix = row['Flat'].split('+')[0]
		return {'praefix': praefix if praefix in praefixes else 'ø'}

	# suffix bestehend aus den letzte zwei buchstaben
	def compute_nk_suffix(self, row):
		return {'nk_suffix': ''.join(self._get_word_struct(row)[-1][-2:])}

	signi_classes = ['N',	#30k
					 'A',	#9.8k
					 'V', 	#9.4k
					 'ADV']	#1.2k	Rest: NUM:133, PRON:116, PREP: 108, C:78, I:37, ART:2

	def compute_part_of_speech(self, row):
		
		return {'basic_pos': row['class'] if row['class'] in self.signi_classes else 'ø'}

	def compute_composita_struct(self, row):
		
		return {'comp_struct': row['REPLACE(ImmClass, \'x\', \'\')'] if row['REPLACE(ImmClass, \'x\', \'\')'] in self.main_compound_strucs else 'ø'}

	def compute_rhymes(self, row):
		
		ret = OrderedDict()
		i = 0
		for r in row['PhonCV'].split('-'):
			ret.update({'rhyme'+str(i): r})
			i += 1
		return self._fill(ret, 'MySylCnt', 'rhyme')

	def augment(self, var):
		self.augmented.extend(map(str.strip, var.split(',')))
		return self

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

	def compute_comp_len(self, row):
		
		l = row['LENGTH(REPLACE(ImmClass, \'x\', \'\'))']
		return {'comp_len': 1 if l < 2 else l}

	def compute_syl_open(self, row):
		
		ret = OrderedDict()
		i = 0
		for syl in row['PhonCV'].split('-'):
			ret.update({'syl_open'+str(i): '1' if syl[-1]=='V' else '0'})
			i += 1
		return self._fill(ret, 'MySylCnt', 'syl_open', '-1')

	disc_alphabet = { #  celex_gug
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
#w A: O: 3: |: / { A V
	disc_alphabet2 = { # http://coral.lili.uni-bielefeld.de/Classes/Winter95/Grundkurs/grundkur/node22.html &
	'plosive':				['p', 'b', 't', 'd', 'k', 'g','G'], # gleich /// stimmt G?
	'affrikate':			['+','=','J','_'], # gleich
	'frikative':			['r','f','v','s','z','S','Z','x','h','D'], #gleich
	'sonoranten':			['N','m','n','j','C','F','H'],
	'ungespannte_vokale':	['I','Y','E','&','O','U','@'], # @ ist schwa?
	'gespannte_vokale_und_diphtonge': ['a',')','2','4','6','i','u','y','e','o','1','W','B','X'],
	}
	celex_disc_alphabet = {
	'consonanten':				list('pbtdkgNmnlrfvTDszSjxGhwZ'),
	'affrikate':				list('+=J_'),
	'syllabische_consonanten': 	list('CFHPR'),
	'langvokale':				list('i!#a$u3y()*<e|o'),
	'diphtonge':				list('12456789KLMWBX'),
	'kurzvokale':				list('IYE/{&AQVOU}@'),
	'nasalvokale':				list('^cq0~'),
	}

	hierarchy = {
		1: disc_alphabet['plosive']+disc_alphabet['frikative']+disc_alphabet['affrikate'],	# obstruente
		2: disc_alphabet['nasale'],
		3: disc_alphabet['liquide'],
		4: disc_alphabet['approximanten'],
		5: disc_alphabet['geschlossene_vokale']+disc_alphabet['komische_diphtonge'],
		6: disc_alphabet['offene_vokale']+disc_alphabet['diphtonge'],
	}

	hierarchy2 = {
		'O': disc_alphabet['plosive']+disc_alphabet['frikative']+disc_alphabet['affrikate'],
		'N': disc_alphabet['nasale'],
		'L': disc_alphabet['liquide'],
		'V': disc_alphabet['approximanten']
			+disc_alphabet['geschlossene_vokale']
			+disc_alphabet['komische_diphtonge']
			+disc_alphabet['offene_vokale']
			+disc_alphabet['diphtonge'],
	}

	hierarchy3 = {
		'C': 	celex_disc_alphabet['consonanten'],
		'A': 	celex_disc_alphabet['affrikate'],
		'S': 	celex_disc_alphabet['syllabische_consonanten'],
		'L': 	celex_disc_alphabet['langvokale'],
		'D': 	celex_disc_alphabet['diphtonge'],
		'K': 	celex_disc_alphabet['kurzvokale'],
		'N': 	celex_disc_alphabet['nasalvokale'],
	}

#		disc_map = {1: 'O',2: 'N',3: 'L',4: 'A',5: 'G',6: 'V',}
	disc_map = {
	'plosive':'P',
	'frikative':'F',
	'affrikate':'a',
	'nasale':'N',
	'liquide':'L',
	'approximanten':'A',
	'geschlossene_vokale':'v',
	'offene_vokale':'V',
	'diphtonge':'D',
	'komische_diphtonge':'d',
	}

	def _classify_morph(self, char):
		for mclass, chars in self.hierarchy3.items():
			if char in chars:
				return str(mclass)

		print 'Missing DISC-Symbol in Alphabet: ' + char
		return False

	def _get_phon_syls(self, row):
		disc = row['PhonStrsDISC'].replace("'",'')
		syls = disc.split('-')
		return syls

	def compute_syl_class(self, row):
		
		syls = self._get_phon_syls(row)
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = ""
			for c in syl:
				son += self._classify_morph(c)
			ret.update({'syl_class'+str(i): son})
			i += 1

		return self._fill(ret, 'MySylCnt', 'syl_class', 'ø')

	def compute_onset_class(self, row):
		nucleus_sign = 'SLDKN'

		syls = self._get_phon_syls(row)
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = ""
			for c in syl:
				son += self._classify_morph(c)
			n = 0

			while n < len(son) and son[n] not in list(nucleus_sign):
				n += 1

			ret.update({'onset_class'+str(i): son[0:n]})
			i += 1

		return self._fill(ret, 'MySylCnt', 'onset_class', 'ø')

	def compute_koda_class(self, row):
		nucleus_sign = 'SLDKN'
		syls = self._get_phon_syls(row)
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = ""
			for c in syl:
				son += self._classify_morph(c)

			n = len(son)-1
			while n >= 0 and son[n] not in list(nucleus_sign):
				n -= 1

			ret.update({'koda_class'+str(i): son[n+1:]})
			i += 1

		return self._fill(ret, 'MySylCnt', 'koda_class', 'ø')

	def compute_nucleus_class(self, row):
		nucleus_sign = 'SLDKN'
		
		syls = self._get_phon_syls(row)
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = ""
			for c in syl:
				son += self._classify_morph(c)

			start = re.search('['+nucleus_sign+']',son).start()
			n = start
			while n < len(son) and son[n] in list(nucleus_sign):
				n += 1

			ret.update({'nucleus_class'+str(i): son[start:n]})
			i += 1
			if len(son[start:n]) < 1:
				print 'Fehler: Silbe hat kein Nukleus!',row

		return self._fill(ret, 'MySylCnt', 'nucleus_class', 'ø')

	def compute_sonority_dir(self, row):
		
		disc = row['PhonStrsDISC'].replace("'",'')
		syls = disc.split('-')
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = 0
			for c in syl:
				missing = c
				for weight, chars in dict(zip(['1','10','100','1000'], self.hierarchy2.values())).items():
					if c in chars:
						son += int(weight)
						missing = False
						break
				if missing != False:
					print 'Missing DISC-Symbol in Alphabet: ' + missing

			ret.update({'sonority'+str(i): son})
			i += 1

		a,b = self.basic_linear_regression(xrange(0, len(syls)), ret.values())
	#	turn = OrderedDict()
	#	turn.update({'sonority_dir_a': a})
	#	turn.update({'sonority_dir_b': b})
	#	return turn
		return {'sonority_dir': 1 if a > 0 else 0 if a==0 else -1}


	def compute_sonority_ratio(self, row):
		
		disc = row['PhonStrsDISC'].replace("'",'')
		syls = disc.split('-')
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = 0
			for c in syl:
				missing = c
				for weight, chars in self.hierarchy.items():
					if c in chars:
						son += weight
						missing = False
						break
				if missing != False:
					print 'Missing DISC-Symbol in Alphabet: ' + missing

			ret.update({'sonority_ratio'+str(i): son/len(syl)})
			i += 1

		return self._fill(ret, 'MySylCnt', 'sonority_ratio', '0')

	def compute_sonority(self, row):
		
		disc = row['PhonStrsDISC'].replace("'",'')
		syls = disc.split('-')
		i = 0
		ret = OrderedDict()

		for syl in syls:
			son = 0
			for c in syl:
				missing = c
				for weight, chars in dict(zip(['1','10','100','1000'], self.hierarchy2.values())).items():
					if c in chars:
						son += int(weight)
						missing = False
						break
				if missing != False:
					print 'Missing DISC-Symbol in Alphabet: ' + missing

			ret.update({'sonority'+str(i): son})
			i += 1

		return self._fill(ret, 'MySylCnt', 'sonority', '0')

	def compute_is_nomen(self, row):
		return {'nomen': 'T' if row['Word'][0].istitle() else 'F'}

	def compute_syl_weights(self, row):
		ret = OrderedDict()
		cv_syls = row['PhonCV'].split('-')
		phon_syls = self._get_phon_syls(row)
		i = 0
		for syl in cv_syls:
			weight = "2" # 2=schwer

			if phon_syls[i].find('@') > -1:
				weight = "0" # schwasilbe
			elif len(syl) >= 2 and syl[-2:] == 'VC' or syl[-2:] == 'VV':
				weight = "1" # leicht

			ret.update({'syl_weight'+str(i): weight})
			i += 1
		return self._fill(ret, 'MySylCnt', 'syl_weight', '-2')

	def compute_suffixes_classes(self,row):
		phons = ''.join(self._get_phon_syls(row))
		return OrderedDict((
			('suffix1', self._classify_morph(phons[-1:])),
			('suffix2', ''.join(map(lambda x: self._classify_morph(x), list(phons[-2:]))) if len(phons) >= 2 else 'ø'),
			('suffix3', ''.join(map(lambda x: self._classify_morph(x), list(phons[-3:]))) if len(phons) >= 3 else 'ø'),
			('suffix4', ''.join(map(lambda x: self._classify_morph(x), list(phons[-4:]))) if len(phons) >= 4 else 'ø'),
			('suffix5', ''.join(map(lambda x: self._classify_morph(x), list(phons[-5:]))) if len(phons) >= 5 else 'ø'),
		))

	# Phonetische Klasse der ersten 1-5 Phone 
	def compute_praefixes_classes(self,row):
		phons = ''.join(self._get_phon_syls(row))
		return OrderedDict((
			('praefix1', self._classify_morph(phons[:1])),
			('praefix2', ''.join(map(lambda x: self._classify_morph(x), list(phons[:2]))) if len(phons) >= 2 else 'ø'),
			('praefix3', ''.join(map(lambda x: self._classify_morph(x), list(phons[:3]))) if len(phons) >= 3 else 'ø'),
			('praefix4', ''.join(map(lambda x: self._classify_morph(x), list(phons[:4]))) if len(phons) >= 4 else 'ø'),
			('praefix5', ''.join(map(lambda x: self._classify_morph(x), list(phons[:5]))) if len(phons) >= 5 else 'ø'),
		))

	# Erste 1-5 Buchstaben eines Wortes
	def compute_praefixes(self,row):
		return OrderedDict((
			('praefix1', row['Word'][-1:]),
			('praefix2', row['Word'][-2:] if len(row['Word']) >= 2 else 'ø'),
			('praefix3', row['Word'][-3:] if len(row['Word']) >= 3 else 'ø'),
			('praefix4', row['Word'][-4:] if len(row['Word']) >= 4 else 'ø'),
			('praefix5', row['Word'][-5:] if len(row['Word']) >= 5 else 'ø'),
		))

	def compute_suffixes(self,row):
		return OrderedDict((
			('suffix1', row['Word'][:1]),
			('suffix2', row['Word'][:2] if len(row['Word']) >= 2 else 'ø'),
			('suffix3', row['Word'][:3] if len(row['Word']) >= 3 else 'ø'),
			('suffix4', row['Word'][:4] if len(row['Word']) >= 4 else 'ø'),
			('suffix5', row['Word'][:5] if len(row['Word']) >= 5 else 'ø'),
		))

	def basic_linear_regression(self, x, y):
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

	def compute_sa_struct(self, row):
		
		return {'sa_struct': row['ImmSA'] if row['ImmSA'] in self.main_ImmSAs else 'ø'}

	attributes = {
	'no_of_syllables': 'MySylCnt',
	'stress_pattern': 'StrsPat',
	'stress_class': 'StrsClass',
	'all_part_of_speech': 'class',
	'composita_struct_all': 'REPLACE(ImmClass, \'x\', \'\')',
	'comp_len': 'LENGTH(REPLACE(ImmClass, \'x\', \'\'))',
	}

	augments = {
	'syl_lens': ['MySylCnt', 'PhonCV', 'HeadSyl', 'Word'],
	'syllables': ['MySylCnt', 'HeadSyl', 'Word'],
	'nk_suffix': ['MySylCnt', 'HeadSyl', 'Word'],
	'praefix': ['Flat'],
	'part_of_speech': ['class'],
	'composita_struct': ['REPLACE(ImmClass, \'x\', \'\')'],
	'sa_struct': ['ImmSA'],
	'praefix_class': ['Flat'],
	'praefixes': ['Word'],
	'suffixes': ['Word'],
	'praefixes_classes': ['PhonStrsDISC'],
	'suffixes_classes': ['PhonStrsDISC'],
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

q = Query('lemmas2',1000000)
#q.maxs.update({'MySylCnt':9})
q.alter_by_stress_class()
q.alter_by_multiple_stress_class()
#q.fix_umlauts()
q.alter_by_mysylcnt()

q.setFields('sylcnt,stress_class')\
.augment('\
	koda_len,\
	koda_class,\
	nucleus_len,\
	nucleus_class,\
	onset_len,\
	onset_class,\
	syl_lens,\
	nk_suffix,\
	praefix,\
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
.where('MySylCnt=10')
#.where('MultipleStrsClass = 0 and StrsClass != "multa" and StrsClass != "nulla" and StrsClass != "undef"')\
#.where('LENGTH(REPLACE(ImmClass, \'x\', \'\'))<=1').where('Mann>20')#.where('MySylCnt=3')
res = q.execute()
#res.show(2)
res.csv('000047-everything-10syl.csv')

#Query('lemmas2',50).setFields('Word, StrsPat, HeadSyl').augment('syllables, syl_lens, cv_ratio').where('word like "%ss%" and headsyl like "%$%"').execute().show(2)
"""

Suffixe von Silben entkoppeln (-or in Motor), EIsenberg S.23
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