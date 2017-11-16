from lxml import html, etree
import requests
import os.path
import sys
from util import make_fname, find_between, find_after, get_page
from util import NIPS_is_paper as is_paper, NIPS_has_pdf as has_pdf
#import PyPDF2

nips_books = list( reversed( [ ( 1988+ed, "https://papers.nips.cc/book/advances-in-neural-information-processing-systems-{ed}-{year}".format( ed = ed+1, year = 1988+ed) ) for ed in range(29) ] ) )
nips_books.append( (1987, "https://papers.nips.cc/book/neural-information-processing-systems-1987") )

for conf_year, conf_url in nips_books:
    conf_paper_id = 1
    print( "\n\n\n{}".format( conf_year ) )
    conf_id = str( conf_year )
    # Scrape nicely
    conf_page = get_page( conf_url )
    conf_tree = html.fromstring( conf_page.content )
    conf_page.close()
    conf_tree.make_links_absolute( "https://papers.nips.cc/" )
    conf_papers = conf_tree.xpath( '//div[@class="main wrapper clearfix"]/ul/li' )
    l = [ ( p.xpath( 'a' )[0].text, ",".join( filter( None, [ author.text for author in p.xpath( 'a[@class="author"]' ) ] ) ), p.xpath('a')[0].attrib['href'] ) for p in conf_papers if is_paper( p ) ]
    # Create file
    with open( "{year}.txt".format( year = conf_year ), mode = 'w', encoding = 'utf-8' ) as conf_file:
        for paper, author, paper_url in l:
            paper_id = find_between( paper_url, '/paper/', '-' )
            pdf_url = paper_url + '.pdf'
            
            print( paper, author, paper_url, end='\t' )
            
            # Extract PDF
            if pdf_url is not None:
                pdf_fname = "{year}-{fname}.pdf".format( year = conf_year, fname = find_between( pdf_url, '/paper/', '.pdf' )[:100] )
                print( pdf_fname, end='' )
                if not os.path.exists( pdf_fname ):
                    # Scrape nicely
                    pdf = get_page( pdf_url )
                    print( "\n" + pdf.content[:160].decode('ascii', errors='ignore') )
                    with open( pdf_fname, mode = 'wb' ) as pdf_file:
                        pdf_file.write( pdf.content )
                    pdf.close()
                #end if
            #end if
            print( "" )
            # Dump to file
            conf_file.write( "{y};{i};{p};{a};{u}\n".format( y=conf_year, i=paper_id, p=paper, a=author, u=paper_url ) )
            conf_paper_id += 1
        #end for
    # end open file
#end for
