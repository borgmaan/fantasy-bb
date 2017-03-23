#!/usr/bin/env python
# andrew
# fix espn files b/c they have weird encoding

with open('../projections/espn-projections.tsv') as infile:
	with open('../projections/clean/espn-hitters.tsv', 'w') as outfile:

		outfile.write('RNK\tPLAYER, TEAM POS\tTYPE\tACTION\tR\tHR\tRBI\tSB\tOBP\tSLG\tfirst_name\tlast_name\tteam\tpos_str\n')

		for line in infile:

			if not line.startswith('RNK'):

				print line
				spl = line.replace('\n', '').split('\t')

				print spl

				name_spl = spl[1].replace('DTD', '').strip().split('\xc2\xa0')
				zz = [x.split(', ') for x in name_spl]
				flat = [item for sublist in zz for item in sublist]
				first_name = flat[0].split(' ')[0]
				last_name = ' '.join(flat[0].split(' ')[1:])
				team = flat[1]
				pos = ";".join([x for x in flat[2:] if x != ''])

				outfile.write('\t'.join(['\t'.join(spl), first_name, last_name, team, pos]) + '\n')
				print '++++++++++++++++++++'


with open('../projections/espn-pitchers.tsv') as infile:
	with open('../projections/clean/espn-pitchers.tsv', 'w') as outfile:

		outfile.write('RNK\tPLAYER, TEAM POS\tTYPE\tACTION\tK\tQS\tSV\tERA\tWHIP\tK/9\tfirst_name\tlast_name\tteam\tpos_str\n')
		

		for line in infile:

			if not line.startswith('RNK'):

				print line
				spl = line.replace('\n', '').split('\t')

				print spl

				name_spl = spl[1].replace('DTD', '').strip().split('\xc2\xa0')
				zz = [x.split(', ') for x in name_spl]
				flat = [item for sublist in zz for item in sublist]
				first_name = flat[0].split(' ')[0]
				last_name = ' '.join(flat[0].split(' ')[1:])
				team = flat[1]
				pos = ";".join([x for x in flat[2:] if x != ''])

				outfile.write('\t'.join(['\t'.join(spl), first_name, last_name, team, pos]) + '\n')
				print '++++++++++++++++++++'
			