#!/usr/bin/python
# -*- coding: utf-8 -*-

import MySQLdb
import MySQLdb.cursors
import sys
import re
import collections
orderedDict = collections.OrderedDict()
from collections import OrderedDict

class CelexQuery:

	fields = []
	filters = []
	table = 'lemmas'
	limit = 1000
	results = []
	augmented = []
	query = ""
	connection = None

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

	def augment(self, var):
		self.augmented.extend(map(str.strip, var.split(',')))
		return self



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

	# unused
	def _get_nucleus(self,row):
		ret = []
		syls = self._get_word_struct(row)
		for syl in syls:
			ret.append(syl[1])
		return ret

	#unused
	def _get_koda(self,row):
		ret = []
		syls = self._get_word_struct(row)
		for syl in syls:
			ret.append(syl[2])
		return ret

	# unused
	def _get_onset(self,row):
		ret = []
		syls = self._get_word_struct(row)
		for syl in syls:
			ret.append(syl[0])
		return ret
