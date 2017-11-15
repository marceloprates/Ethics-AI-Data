import sys
from lxml import etree

with open( "venues.txt", "r", encoding = "utf-8" ) as f:
	VENUES = f.read().splitlines()
#end with
with open( "venuesb.txt", "r", encoding = "utf-8" ) as f:
	BROAD_VENUES = f.read().splitlines()
#end with
with open( "keywords.txt", "r", encoding = "utf-8" ) as f:
	KEYWORDS = f.read().splitlines()
#end with

CSV_SEP = ';'
PATH_TO_XML = "./dblp.xml"
CATEGORIES = set(
	["article", "inproceedings", "proceedings", "book", "incollection", "phdthesis", "mastersthesis", "www"]
)
SKIP_CATEGORIES = set(["phdthesis", "mastersthesis", "www"])
DATA_ITEMS = ["title", "booktitle", "year", "journal", "ee"]#, "volume"]

def clear_element(element):
	element.clear()
	while element.getprevious() is not None:
		del element.getparent()[0]
	#end while
#end clearn_element

def extract_paper_elements( iter_parser, log_file = sys.stderr ):
	try:
		for event, element in iter_parser:
			if element.tag in CATEGORIES:
				yield element
				clear_element( element )
			#end if
		#end for
	except etree.XMLSyntaxError as e:
		print( e )
	#end try
#end extract_paper_elements

def write_header(csv_file,end='\n'):
	csv_file.write( "dblpkey{CS}tag{CS}mdate".format( CS = CSV_SEP ) )
	for data_item in DATA_ITEMS:
		csv_file.write( "{CS}{}".format( data_item, CS = CSV_SEP ) )
	csv_file.write( end )
#end write_header

def write_entry(paper,csv_file,end='\n'):
	csv_file.write( "{dblpkey}{CS}{tag}{CS}{mdate}".format(
		dblpkey = paper["dblpkey"],
		tag = paper["element"],
		mdate = paper["mdate"],
		CS = CSV_SEP)
	)
	for data_item in DATA_ITEMS:
		if type( paper[ data_item ] ) is str:
			item = "{CS}\"{}\"".format( ''.join( c for c in paper[ data_item ] if c not in ['"', CSV_SEP] ), CS = CSV_SEP )
		else:
			item = "{CS}{}".format( paper[ data_item ], CS = CSV_SEP )
		#end if
		csv_file.write( item )
	#end for
	csv_file.write( end )
#end write_entry

def check_text(text,keywords):
	for keyword in keywords:
		if keyword is not None and text is not None and keyword in text:
			return True
		#end if
	#end for
	return False
#end check_text

def read_from_write_to( iter_parser, csv_file, log_file = sys.stderr ):
	for venue in VENUES:
		if venue is None:
			continue
		#end if
		fname = ''.join( c for c in venue if c.isalnum() )
		with open( "venues/{venue}.txt".format( venue=fname ), "w", encoding = "utf-8" ) as f:
			write_header( f, end='' )
			f.write( "{FS}contains\n".format( FS = CSV_SEP ) )
		#end with
	#end for
	for venue in BROAD_VENUES:
		if venue is None:
			continue
		#end if
		fname = ''.join( c for c in venue if c.isalnum() )
		with open( "bvenues/{venue}.txt".format( venue=fname ), "w", encoding = "utf-8" ) as f:
			write_header( f, end='' )
			f.write( "{FS}contains\n".format( FS = CSV_SEP ) )
		#end with
	#end for
	#found_confs = set()
	#found_books = set()
	#end for
	for paperCounter, element in enumerate( extract_paper_elements( iter_parser ) ):
		authors = [ author.text for author in element.findall( "author" ) ]
		if element.get("key") is None or element.tag is None:
			continue
		#end if
		paper = {
			"element" : element.tag,
			"mdate" : element.get("mdate"),
			"dblpkey" : element.get("key")
		}
		for data_item in DATA_ITEMS:
			data = element.find(data_item)
			if data is not None:
				try:
					paper[ data_item ] = data.text
				except AttributeError:
					paper[ data_item ] = data
				#end try
				#if data_item == "journal":
				#	found_confs.add( paper[ data_item ] )
				#elif data_item == "booktitle":
				#	found_books.add( paper[ data_item ] )
				#end if
			else:
				paper[ data_item ] = ""
			#end if
		#end for
		if paper[ "element" ] not in SKIP_CATEGORIES:
			for venue in VENUES:
				if venue is None:
					continue
				#end if
				fname = ''.join( c for c in venue if c.isalnum() )
				with open( "venues/{venue}.txt".format( venue=fname ), "a", encoding = "utf-8" ) as f:
					v = venue
					bt = '' if paper["booktitle"] is None else paper["booktitle"]
					j = '' if paper["journal"] is None else paper["journal"]
					if v == bt or v == j:
						contains = check_text( paper["title"], KEYWORDS )
						write_entry( paper, f, end='')
						f.write( "{CS}{}\n".format( 1 if contains else 0, CS = CSV_SEP ) )
					#end if
			#end for
			for venue in BROAD_VENUES:
				if venue is None:
					continue
				#end if
				fname = ''.join( c for c in venue if c.isalnum() )
				with open( "bvenues/{venue}.txt".format( venue=fname ), "a", encoding = "utf-8" ) as f:
					v = venue
					bt = '' if paper["booktitle"] is None else paper["booktitle"]
					j = '' if paper["journal"] is None else paper["journal"]
					if v in bt or v in j:
						contains = check_text( paper["title"], KEYWORDS )
						write_entry( paper, f, end='')
						f.write( "{CS}{}\n".format( 1 if contains else 0, CS = CSV_SEP ) )
					#end if
				#end with
			#end for
		#end if
		print( "{counter}".format( counter = paperCounter ), file=sys.stdout )
	#end for
	#with open( "found_confs.txt", "w", encoding = "utf-8" ) as f:
	#	for conf in found_confs:
	#		f.write( "{conf}\n".format( conf = conf ) )
	#	#end for
	#end with
	#with open( "found_books.txt", "w", encoding = "utf-8" ) as f:
	#	for book in found_books:
	#		f.write( "{book}\n".format( book = book ) )
	#	#end for
	#end with
#end read_from_write_to

def main():
	with open( "dblp.csv", mode = "w", encoding = "utf-8" ) as csv_file:
		iter_parser = etree.iterparse(PATH_TO_XML, dtd_validation=True, events=("start", "end"))
		read_from_write_to(iter_parser, csv_file)
	#end with
#end main

if __name__ == "__main__":
	main()
#end main
