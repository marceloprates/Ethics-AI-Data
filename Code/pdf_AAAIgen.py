from lxml import html, etree
import requests
import os.path
import sys
from util import make_fname, find_between, find_after, get_page
from util import AAAI_is_paper as is_paper, AAAI_has_pdf as has_pdf
#import PyPDF2

aaai_confs_years = [2017,2016,2015,2014,2013,2012,2011,2010,2008,2007,2006,2005,2004,2002,2000,1999,1998,1997,1996,1994,1993,1992,1991,1990,1988,1987,1986,1984,1983,1982,1980]

for conf_year in aaai_confs_years:
    conf_paper_id = 1
    print( "\n\n\n{}".format( conf_year ) )
    conf_id = str( conf_year )[-2:]
    conf_url = "https://www.aaai.org/Library/AAAI/aaai{conf_id}contents.php".format( conf_id = conf_id )
    # Scrape nicely
    conf_page = get_page( conf_url )
    conf_tree = html.fromstring( conf_page.content )
    conf_page.close()
    conf_tree.make_links_absolute( "https://www.aaai.org/Library/AAAI/" )
    conf_papers = conf_tree.xpath( '//div[@id="box6"]/div[@class="content"]/p[@class="left"]' )
    l = [ ( ( '' if p.xpath( 'a' )[0].text is None else p.xpath( 'a' )[0].text) + ''.join( filter( None, [ subtext.text for subtext in p.xpath( 'a' )[0] ] ) ), p.xpath( 'i' )[0].text, p.xpath('a')[0].attrib['href'], p.getchildren()[1].text if has_pdf( p ) else None ) for p in conf_papers if is_paper( p ) ]
    # Create file
    with open( "{year}.txt".format( year = conf_year ), mode = 'w', encoding = 'utf-8' ) as conf_file:
        for paper, author, paper_url, commented_url in l:
            pdf_url = None
            paper_id = None
            try:
                if conf_year < 2010:
                    if paper_url.endswith( "pdf" ):
                        pdf_url = paper_url
                    else:
    		                print( paper_url )
    		                paper_page = get_page( paper_url )
    		                paper_tree = html.fromstring( paper_page.content )
    		                paper_tree.make_links_absolute( "https://www.aaai.org/Library/AAAI/{year}".format( year = conf_year ) )
    		                pdf_url = paper_tree.xpath( '//div/h1/a' )[0].attrib['href']
    		                paper_page.close()
                    #end if
                else:
                    paper_url = paper_url.replace( 'paper/view', 'paper/viewPaper' )
                    if commented_url is not None:
                        id_url = find_between( commented_url, 'href="', '"' )
                        paper_id = find_between( id_url, 'paper/view/', '/' )
                        file_id = find_after( id_url, 'paper/view/' + paper_id + '/' )
                        pdf_url = "https://aaai.org/ocs/index.php/AAAI/AAAI{conf_id}/paper/viewFile/{paper_id}/{file_id}.pdf".format( conf_id = conf_id, paper_id = paper_id, file_id = file_id )
                    #end if
                #end if
            except etree.ParserError as e:
                print( "Error, unable to get pdf link: {}".format( e ) )
            except IndexError as e:
                print( "Error, unable to get pdf link: {}".format( e ) )
            #end try
            
            print( paper, author, paper_url, end='\t' )
            
            paper_id = make_fname( "{:02d}-".format( conf_paper_id ) + paper ) if paper_id is None else paper_id
            # Extract PDF
            if pdf_url is not None:
                print( pdf_url, end='' )
                pdf_fname = "{year}-{paper_id}.pdf".format( year = conf_year, paper_id = paper_id )
                if not os.path.exists( pdf_fname ):
                    # Scrape nicely
                    pdf = get_page( pdf_url )
                    print( "\n" + pdf.content[:160].decode('ascii', errors='ignore') )
                    with open( pdf_fname, mode = 'wb' ) as pdf_file:
                        pdf_file.write( pdf.content )
                    #end with pdf_file
                    # TODO extract text from pdf for analysis
                    #with open( "{year}-{paper_id}.pdf".format( year = conf_year, paper_id = paper_id ), mode = 'rb' ) as pdf_file:
                    #    with open( "{year}-{paper_id}.txt".format( year = conf_year, paper_id = paper_id ), mode = 'w', encoding = 'utf-8' ) as txt_file:
                    #        pdfReader = PyPDF2.PdfFileReader( pdf_file )
                    #        for page_number in range( pdfReader.numPages ):
                    #            txt_file.write( "{}\n\n\n".format( pdfReader.getPage( page_number ).extractText() ) )
                    #        #end for
                    #    #end with txt_file
                    #end with pdf_file
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
