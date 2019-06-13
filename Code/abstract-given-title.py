


def chunks(l, n):
    """Yield successive n-sized chunks from l."""
    for i in range(0, len(l), n):
        yield l[i:i + n]

NUM_ABSTRACTS = 12078
with open('../Abstracts/aaai.txt','r') as f:	
	titles_and_abstracts = []
	
	line = f.readline()
	i = 0
	while line:
		while(len(line.split(";")) < 6):
			line = line + f.readline()
		aux = line.split(";")

		title 		= aux[2].replace("#","")
		abstract 	= aux[5].replace("#","")

		titles_and_abstracts.append((title,abstract))

		line = f.readline()
		print( "{:.2f}%       ".format( 100*((i+1)/NUM_ABSTRACTS) ), end = "\r" )
		i+=1
	print()

stop_words = []
#with open('stop-words.txt','r') as f:
with open('empty.txt','r') as f:
	stop_words = f.read().split()

classical_keywords 	= []
ethics_keywords 	= []
trending_keywords 	= []
keywords 			= []
with open('../DBLP/classical_keywords.txt') as f:
	classical_keywords += f.read().split()
with open('../DBLP/ethics_keywords.txt') as f:
	ethics_keywords += f.read().split()
with open('../DBLP/trending_keywords.txt') as f:
	trending_keywords += f.read().split()

keywords = classical_keywords + trending_keywords + ethics_keywords

count1,count2 = 0,0
# For each (title,abstract) pair:
for i, title_and_abstract in enumerate( titles_and_abstracts ):
	title, abstract = title_and_abstract
	title_words 	= map(lambda x: x.lower(), title.split())
	abstract_words 	= map(lambda x: x.lower(), abstract.split())
	# For each word in the title:
	for word in abstract_words:
		if (True or not word in stop_words) and (word in keywords ) : #ethics_keywords ) :
			count1 += 1
			if (word in title_words):
				count2 += 1
	print( "{:.2f}%       ".format( 100*((i+1)/NUM_ABSTRACTS) ), end = "\r" )
print()

print(count2/count1)
