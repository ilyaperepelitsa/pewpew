import re
import shelve
import pandas as pd

towork = shelve.open("/Users/ilyaperepelitsa/variables/child_file")
par = re.compile("""\n
	\s+						# new line followed by whitespace
	(Mr\.|
	Ms\.|
	Mrs\.|
	Senator|
	Chairman|
	Secretary|
	Dr\.|
	Major\sGeneral|
	Major|
	General|
	Governor|
	Amb\.)						# title

	\s 							# whitespace
	(\w+)						# name
	(\.)						# period
	(.+?)(?=\n\s+
	
	(Mr\.|
	Ms\.|
	Mrs\.|
	Senator|
	Chairman|
	Secretary|
	Dr\.|
	Major\sGeneral|
	Major|
	General|
	Governor|
	Amb\.)						# title

	\s(\w+)\.) 					# character or whitespace
		""", re.VERBOSE|re.DOTALL|re.MULTILINE)



title = re.compile("\<title\>(.+)\<\/title\>")
meta = re.compile("\[.+?\]", re.MULTILINE)
upperwords = re.compile("[A-Z]+")
laughdetect = re.compile("\slaughter\s", re.IGNORECASE)

# statement = re.compile("""(.+?)\sstatement\sof\s(.+?)\n
# 	""", re.VERBOSE|re.DOTALL|re.MULTILINE|re.IGNORECASE)

date = re.compile("""(\w+?\s\d{1,2},\s\d{4})
	""", re.VERBOSE|re.DOTALL|re.MULTILINE|re.IGNORECASE)


namepar = re.compile("""\n\s+						# new line followed by whitespace
					(Mr\.|Ms\.|Mrs\.|Senator)	# title
					\s (\w+)\.""", re.VERBOSE|re.DOTALL|re.MULTILINE)

punctuation = re.compile('[\[\]\<\>\(\@\#\&\*\-\_\=\+\$\%\)\!\?\|]',re.MULTILINE)
upperletters = re.compile("[A-Z]", re.MULTILINE)
lowerletters = re.compile("[a-z]", re.MULTILINE)
whitespace = re.compile("\s", re.MULTILINE)

documentcount = 0 

sessionvector = []
chambervector = []
committeevector = []
linkvector = []
pathvector = []
docnumber = []
datevector = []
heartit = []
linevector = []
titlevector = []
lastnamevector = []
paragraphvector = []
count_laugh_list = []
meta_list = []
count_punct_list = []
count_upper_list = []
count_lower_list = []
count_white_list = []
discard_list = []

discard_counter = 0

for i in towork.keys():


	session = towork[i][0]
	chamber = towork[i][1]
	committee = towork[i][3]

	link = towork[i][5]
	path = towork[i][6]


	
	try: 
		skip_file_read = open(path, "rb")
	except FileNotFoundError:
		continue

	skip_file = str(skip_file_read.read())

	try:
		docdate = date.search(skip_file).group(1)
	except AttributeError:
		# print("Error")
		docdate = "NA"


	try:
		hearingtitle = title.search(skip_file).group(1)
	except AttributeError:
		# print("Error")
		hearingtitle = "NA"

	line = 0
	documentcount += 1
	total_laugh = 0
		
	for i in par.findall(skip_file):
		line += 1
		try:
			title = i[0]
			lastname = i[1]
			paragraph = i[3]
			paragraph = paragraph.replace("\\n", "")
			paragraph = paragraph.replace("\n", "")
			paragraph = paragraph.replace("\\", "")
			paragraph = paragraph.replace("  ", " ")


			count_laugh = 0
			count_punct = 0
			count_upper = 0
			count_lower = 0
			count_white = 0


			try:
				for laughter_case in laughdetect.findall(paragraph):
					count_laugh += 1
					total_laugh += 1

			except AttributeError:
				pass

		
			meta_cases = []

			try:
				for metabracket in meta.findall(paragraph):
					meta_cases.append(metabracket)
			except AttributeError:
				pass



			try:
				for metabracket in meta.findall(paragraph):
					paragraph = paragraph.replace(metabracket, "")
			except AttributeError:
				pass




			try:
				for punctuation_case in punctuation.findall(paragraph):
					count_punct += 1
			except AttributeError:
				pass


			try:
				for upper_case in upperletters.findall(paragraph):
					count_upper += 1
			except AttributeError:
				pass


			try:
				for lower_case in lowerletters.findall(paragraph):
					count_lower += 1
			except AttributeError:
				pass



			try:
				for white_case in whitespace.findall(paragraph):
					count_white += 1
			except AttributeError:
				pass

			print("\n\n\n\n\n\n")
			print(title)
			print("\n\n")
			print(lastname)
			print("\n\n")
			print(paragraph)
			print("\n\n")


			if discard_counter < 1000:
				discard_counter += 1
				some_response = input("What do we do with that paragraph?: ")
				if some_response == "":
					response = "keep"
				else:
					response = "discard"
			else:
				response = ""



			
			# print(count_laugh)

			# print("\n\n\n\n")
			# counter += 1
			print(documentcount)
		except AttributeError:
			# print("Error")
			continue

		sessionvector.append(session)
		chambervector.append(chamber)
		committeevector.append(committee)
		linkvector.append(link)
		pathvector.append(path)
		docnumber.append(documentcount)
		datevector.append(docdate)
		heartit.append(hearingtitle)
		linevector.append(line)
		titlevector.append(title)
		lastnamevector.append(lastname)
		paragraphvector.append(paragraph)
		meta_list.append(meta_cases)
		count_laugh_list.append(count_laugh)
		count_punct_list.append(count_punct)
		count_upper_list.append(count_upper)
		count_lower_list.append(count_lower)
		count_white_list.append(count_white)
		discard_list.append(response)


		# print(total_laugh)
		


columns  = ["session", "chamber", "committee",
			"link", "path", "document", "date",
			"hearing_title", "line", "speaker_title",
			"speaker_lastname", "paragraph", "meta", "laughter",
			"punctuation", "upper", "lower", "whitespace", "discard"]

df = pd.DataFrame.from_items([(columns[0], sessionvector), 
                              (columns[1], chambervector), 
                              (columns[2], committeevector), 
                              (columns[3], linkvector), 
                              (columns[4], pathvector), 
                              (columns[5], docnumber), 
                              (columns[6], datevector), 
                              (columns[7], heartit), 
                              (columns[8], linevector), 
                              (columns[9], titlevector), 
                              (columns[10], lastnamevector), 
                              (columns[11], paragraphvector),
							  (columns[12], meta_list),
							  (columns[13], count_laugh_list),
							  (columns[14], count_punct_list),
							  (columns[15], count_upper_list),
							  (columns[16], count_lower_list),
							  (columns[17], count_white_list),
							  (columns[18], discard_list)])

df.to_csv("/Users/ilyaperepelitsa/variables/5000_hearings.csv", index=False)



