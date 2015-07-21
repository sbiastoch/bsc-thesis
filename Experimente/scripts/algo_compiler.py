#!/usr/bin/python
# -*- coding: utf-8 -*-


class AlgoCompiler:
	file = None
	type = 'unknown'
	start_tags = {
		'JRip': 'JRIP rules:',
		'J48': 'J48 pruned tree',
	}
	end_tags = {
		'JRip': 'Number of Rules',
		'J48': 'Number of Leaves',
	}
	start_offset = {
		'JRip': 3,
		'J48': 3,
	}
	end_offset = {
		'JRip': 1,
		'J48': 1,
	}
	raw_rules = []

	def __init__(self, path):
		f = open(path, 'r')
		self.file = [line.rstrip() for line in f]
		self.detectType()
		self.compile()

	def detectType(self):
		for line in self.file:
			s = "Scheme:       "
			if line.find(s) > -1:
				if line.find('JRip') > -1:
					self.type = 'JRip'
				elif line.find('J48') > -1:
					self.type = 'J48'
				else:
					print "Can't detect line from following line: "+line
					self.type = 'unknown'

	def compile(self):
		if self.type != 'unknown':
			self.get_rules()
			getattr(self,'compile_'+self.type)()
		else:
			print "Can't compile: Unknown type:"+self.type

	def get_rules(self):
		start = 0
		for line in self.file:
			if line.find(self.start_tags[self.type]) > -1:
				start += self.start_offset[self.type]
				break
			else:
				start += 1

		end = start
		for line in self.file[start:]:
			if line.find(self.end_tags[self.type]) > -1:
				end -= self.end_offset[self.type]
				break
			else:
				end += 1

		self.raw_rules = self.file[start:end]

	def out(self, str):
		print str

	def compile_JRip(self):
		for raw_rule in self.raw_rules:
			comps = raw_rule.split(' and ')
			comps = comps[:-1] + comps[-1].split(' => ')

			begin_class = comps[-1].find('=')+1
			end_class = comps[-1].find(' (')

			comps[-1] = comps[-1][begin_class:end_class]

			i = 0
			for comp in comps[:-1]:
				var = comp.split('= ')[-1][:-1]
				numeric = False
				try:
					int(var)
					numeric = True
				except Exception, e:
					pass
				comps[i] = comp = comp.replace(' = ', ' == ')
				if not numeric:
					comps[i] = comp = comp.replace('= ', '= "')
					comps[i] = comp = comp.replace(')', '")')
				i += 1

			self.out('if' + ' and '.join(comps[:-1])+':')
			self.out("\treturn "+comps[-1])

			print 

	def compile_J48(self):
		pass

AlgoCompiler('/home/sbiastoch/Schreibtisch/Bachelorarbeit/Skripte/jrip_test.txt')
#AlgoCompiler('/home/sbiastoch/Schreibtisch/Bachelorarbeit/Skripte/j48_test.txt')