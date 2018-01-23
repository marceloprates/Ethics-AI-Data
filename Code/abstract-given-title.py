
def chunks(l, n):
    """Yield successive n-sized chunks from l."""
    for i in range(0, len(l), n):
        yield l[i:i + n]

with open('../Abstracts/aaai.txt','r') as f:
	
	titles_and_abstracts = []
	
	line = f.readline()
	while line:
		
		while(len(line.split(";")) < 6):
			line = line + f.readline()
		aux = line.split(";")

		title 		= aux[2].replace("#","")
		abstract 	= aux[5].replace("#","")

		titles_and_abstracts.append((title,abstract))

		line = f.readline()

stop_words = []
with open('stop-words.txt','r') as f:
	stop_words = f.read().split()

our_keywords = []
with open('../DBLP/classical_keywords.txt') as f:
	our_keywords += f.read().split()
with open('../DBLP/ethics_keywords.txt') as f:
	our_keywords += f.read().split()
with open('../DBLP/trending_keywords.txt') as f:
	our_keywords += f.read().split()

count1,count2 = 0,0
# For each (title,abstract) pair:
for (title,abstract) in titles_and_abstracts:
	title_words 	= map(lambda x: x.lower(), title.split())
	abstract_words 	= map(lambda x: x.lower(), abstract.split())
	# For each word in the title:
	for word in abstract_words:
		if (True or not word in stop_words) and (word in our_keywords) :
			count1 += 1
			if (word in title_words):
				count2 += 1

print(count2/count1)