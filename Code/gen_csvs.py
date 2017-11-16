import sys
from lxml import etree

with open( "venuesb.txt", "r", encoding = "utf-8" ) as f:
	BROAD_VENUES = f.read().splitlines()
#end with

CSV_SEP = '\t'
PATH_TO_XML = "./dblp.xml"
CATEGORIES = set(
	["article", "inproceedings", "proceedings", "book", "incollection", "phdthesis", "mastersthesis", "www"]
)
DATA_ITEMS = ["title", "booktitle", "journal", "volume", "year", "ee"]
TDATA_ITEMS = ["key", "tag", "title", "booktitle", "journal", "volume", "year", "ee"]
JDATA_ITEMS = ["key", "title", "journal", "volume", "year", "ee"]
CDATA_ITEMS = ["key", "title", "booktitle", "year", "ee"]
DBLP_SIZE = 11792843

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

def write_header(csv_file,data_items,end='\n'):
	for data_item in data_items[:-1]:
		csv_file.write( "{}{CS}".format( data_item, CS = CSV_SEP ) )
	csv_file.write( "{}".format( data_items[-1], CS = CSV_SEP ) )
	csv_file.write( end )
#end write_header

def write_entry(paper,csv_file,data_items,end='\n'):
	for data_item in data_items[:-1]:
		if type( paper[ data_item ] ) is str:
			item = "\"{}\"{CS}".format( paper[ data_item ].replace('"','').replace(CSV_SEP,''), CS = CSV_SEP )
		else:
			item = "{}{CS}".format( paper[ data_item ], CS = CSV_SEP )
		#end if
		csv_file.write( item )
	#end for
	if type( paper[ data_items[-1] ] ) is str:
		item = "\"{}\"{CS}".format( paper[ data_items[-1] ].replace('"','').replace(CSV_SEP,''), CS = CSV_SEP )
	else:
		item = "{}{CS}".format( paper[ data_items[-1] ], CS = CSV_SEP )
	#end if
	csv_file.write( item )
	csv_file.write( end )
#end write_entry

def read_from_write_to( iter_parser, journal_csv_file, conference_csv_file, trash_csv_file, log_file = sys.stderr ):
	write_header( journal_csv_file, JDATA_ITEMS )
	write_header( conference_csv_file, CDATA_ITEMS )
	write_header( trash_csv_file, TDATA_ITEMS )
	for paperCounter, element in enumerate( extract_paper_elements( iter_parser ) ):
		if element.get("key") is None or element.tag is None:
			continue
		#end if
		paper = {
			"tag" : element.tag,
			"mdate" : element.get("mdate"),
			"key" : element.get("key")
		}
		for data_item in DATA_ITEMS:
			data = element.find(data_item)
			if data is not None:
				try:
					paper[ data_item ] = '' if data.text is None else data.text
				except AttributeError:
					paper[ data_item ] = data
			else:
				paper[ data_item ] = ""
			#end if
		#end for
		for venue in BROAD_VENUES:
			bt = '' if paper["booktitle"] is None else paper["booktitle"]
			j = '' if paper["journal"] is None else paper["journal"]
			if venue in bt or venue in j:
				if paper["tag"] == "inproceedings":
					write_entry( paper, conference_csv_file, CDATA_ITEMS )
				elif paper["tag"] == "article":
					write_entry( paper, journal_csv_file, JDATA_ITEMS )
				else:
					write_entry( paper, trash_csv_file, TDATA_ITEMS )
				#end if
				break
			#end if
		#end for
		if paperCounter % 1000 == 0:
			print(
				"{counter}\t{percentage:.2f}%".format(
					counter = paperCounter,
					percentage = 100 * paperCounter / DBLP_SIZE),
				file=sys.stdout
			)
		#end if
	#end for
#end read_from_write_to

def main():
	with open( "jdblp.csv", mode = "w", encoding = "utf-8" ) as journal_csv_file:
		with open( "cdblp.csv", mode = "w", encoding = "utf-8" ) as conference_csv_file:
			with open( "trash.csv", mode = "w", encoding = "utf-8" ) as trash_csv_file:
				iter_parser = etree.iterparse(PATH_TO_XML, dtd_validation=True, events=("start", "end"))
				read_from_write_to(iter_parser, journal_csv_file, conference_csv_file, trash_csv_file)
			#end with
		#end with
	#end with
#end main

if __name__ == "__main__":
	main()
#end main
