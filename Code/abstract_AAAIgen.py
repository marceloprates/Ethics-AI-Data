from lxml import html, etree
import requests
import os.path
import sys
from util import make_fname, find_between, find_after, get_page, csv_clean_string
from util import AAAI_is_paper as is_paper

FILENAME = "aaai" #.txt

# These characters will be removed from any text inside to keep CSV file consistent
TEXT_DELIMITER = '#' # Text delimiter for csv file
CSV_SEPARATOR = ';' # Item delimiter for csv file
CSV_END_LINE = '\n' # End of entry for csv file

SCRAPE_DELAY_BEFORE = 5.0 # Seconds
SCRAPE_DELAY_RETRY = None # Sets to be equal to SCRAPE_DELAY_BEFORE

aaai_confs_years = [2017,2016,2015,2014,2013,2012,2011,2010,2008,2007,2006,2005,2004,2002,2000,1999,1998,1997,1996,1994,1993,1992,1991,1990,1988,1987,1986,1984,1983,1982,1980]

with open( "{fname}.txt".format( fname = FILENAME ), mode = 'w', encoding = 'utf-8' ) as conf_file:
    for conf_year in aaai_confs_years:
        conf_paper_id = 1
        print( "\n\n\n{}".format( conf_year ) )
        conf_id = str( conf_year )[-2:]
        conf_url = "https://www.aaai.org/Library/AAAI/aaai{conf_id}contents.php".format( conf_id = conf_id )
        # Scrape nicely
        conf_page = get_page( conf_url, SCRAPE_DELAY_BEFORE, SCRAPE_DELAY_RETRY )
        conf_tree = html.fromstring( conf_page.content )
        conf_page.close()
        conf_tree.make_links_absolute( "https://www.aaai.org/Library/AAAI/" )
        conf_papers = conf_tree.xpath( '//div[@id="box6"]/div[@class="content"]/p[@class="left"]' )
        l = [ ( ( '' if p.xpath( 'a' )[0].text is None else p.xpath( 'a' )[0].text) + ''.join( filter( None, [ subtext.text for subtext in p.xpath( 'a' )[0] ] ) ), p.xpath( 'i' )[0].text, p.xpath('a')[0].attrib['href'] ) for p in conf_papers if is_paper( p ) ]
        # Create file
        for paper, author, paper_url in l:
            paper_id = None
            abstract = ''
            try:
                if "/paper/view/" in paper_url:
                    paper_url = paper_url.replace( 'paper/view', 'paper/viewPaper' )
                    if "https://" not in paper_url:
                        paper_url = paper_url.replace( 'http://', 'https://' )
                    #end if
                    paper_id = "{conf}{year}_{paper_id}".format(
                        conf = find_between( paper_url, 'index.php/', '/' ),
                        year = conf_id,
                        paper_id = find_after( paper_url, '/paper/view/' )
                    )
                    paper_page = get_page( paper_url, SCRAPE_DELAY_BEFORE, SCRAPE_DELAY_RETRY )
                    paper_tree = html.fromstring( paper_page.content )
                    abstract = "{text}".format( text = paper_tree.xpath( '//div[@id="abstract"]/div' )[0].text )
                    paper_page.close()
                elif ".org/Library/" in paper_url:
                    if "https://" not in paper_url:
                        paper_url = paper_url.replace( 'http://', 'https://' )
                    #end if
                    paper_id = find_between( paper_url, '/{year}/'.format( year = conf_year ), ".php" )
                    paper_page = get_page( paper_url, SCRAPE_DELAY_BEFORE, SCRAPE_DELAY_RETRY )
                    paper_tree = html.fromstring( paper_page.content )
                    abstract = "{text}".format( text = paper_tree.xpath( '//div[@id="abstract"]/p' )[1].text )
                    paper_page.close()
                else:
                    pass
                #end if
            except (etree.XMLSyntaxError, IndexError, AttributeError) as e:
                print( "Failed to get abstract for paper. Id={id}{sep}Title={td}{title}{td}{sep}Year={year}{sep}Error={error}".format(
                    id    = paper_id,
                    title = paper,
                    year  = conf_year,
                    error = e,
                    sep   = CSV_SEPARATOR,
                    td    = TEXT_DELIMITER),
                    file = sys.stderr
                )
            #end try
            paper_id = make_fname( "{:02d}-".format( conf_paper_id ) + paper ) if paper_id is None else paper_id
            # Dump to file
            print( "{year}{sep}{td}{id}{td}{sep}{td}{title}{td}{sep}{td}{authors}{td}{sep}{td}{url}{td}{sep}{tda}{abstract}{tda}{endl}".format(
                year     = str(conf_year)[:20],
                id       = str(paper_id)[:20],
                title    = str(paper)[:20],
                authors  = str(author)[:20],
                url      = str(paper_url)[11:],
                abstract = str(abstract)[:20],
                td   = '',
                tda   = '"',
                sep  = '\t',
                endl = '\n')
            )
            conf_file.write( "{year}{sep}{td}{id}{td}{sep}{td}{title}{td}{sep}{td}{authors}{td}{sep}{td}{url}{td}{sep}{td}{abstract}{td}{endl}".format(
                year     = csv_clean_string( str( conf_year ), TEXT_DELIMITER, CSV_SEPARATOR, CSV_END_LINE ),
                id       = csv_clean_string( str( paper_id ), TEXT_DELIMITER, CSV_SEPARATOR, CSV_END_LINE ),
                title    = csv_clean_string( str( paper ), TEXT_DELIMITER, CSV_SEPARATOR, CSV_END_LINE ),
                authors  = csv_clean_string( str( author ), TEXT_DELIMITER, CSV_SEPARATOR, CSV_END_LINE ),
                url      = csv_clean_string( str( paper_url ), TEXT_DELIMITER, CSV_SEPARATOR, CSV_END_LINE ),
                abstract = csv_clean_string( str( abstract ), TEXT_DELIMITER, CSV_SEPARATOR, CSV_END_LINE ),
                td   = TEXT_DELIMITER,
                sep  = CSV_SEPARATOR,
                endl = CSV_END_LINE)
            )
            conf_paper_id += 1
        #end for
    #end for
# end open file
