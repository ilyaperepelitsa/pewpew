import re


punctuation = re.compile('[\[\]\<\>\(\@\#\&\*\-\_\=\+\$\%\)\!\?\|]')
upperletters = re.compile("[A-Z]+")
lowerletters = re.compile("[a-z]", re.MULTILINE)
whitespace = re.compile("\s", re.MULTILINE)
meta = re.compile("\[.+?\]", re.MULTILINE)




stttt = "SOME VERY IMPORTANT TEWXT"

# for i in punctuation.findall(stttt):
# 	print(i)

# print("_________________________")	

# for i in upperletters.findall(stttt):
# 	print(i)

# print("_________________________")	

for i in upperletters.findall(stttt):
	print(i)


# print("_________________________")	

# count = 0
# for i in whitespace.findall(stttt):
# 	count += 1
# 	print(count)



# response = input("Please enter your name: ")
# if response != "":
# 	response = 12345

# print(response)


