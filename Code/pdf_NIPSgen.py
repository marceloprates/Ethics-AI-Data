from lxml import html, etree
import requests
import time
import os.path
import sys
#import PyPDF2

nips_books = list( reversed( [ ( 1988+ed, "https://papers.nips.cc/book/advances-in-neural-information-processing-systems-{ed}-{year}".format( ed = ed+1, year = 1988+ed) ) for ed in range(29) ] ) )
nips_books.append( (1987, "https://papers.nips.cc/book/neural-information-processing-systems-1987") )

def make_fname( s ):
    r = [ '_' if c.isspace() else c for c in s if c.isalnum() or c.isspace() or c == '-' ]
    return ( ''.join( r ) )[:40]
#end make_fname

def find_between(s, first, last):
    try:
        start = s.index( first ) + len( first )
        end = s.index( last, start )
        return s[start:end]
    except ValueError:
        return ""
#end find_between

def find_after(s, ss):
    try:
        start = s.index( ss ) + len( ss )
        return s[start:]
    except ValueError:
        return ""
#end find_between

def is_paper( p ):
    try:
        _,_,_ = p.xpath( 'a' )[0].text, ",".join( filter( None, [ author.text for author in p.xpath( 'a[@class="author"]' ) ] ) ), p.xpath('a')[0].attrib['href']
    except IndexError:
        return False
    return True
#end is_paper

def has_pdf( p ):
    try:
        c = p.getchildren()[1]
        return isinstance( c, html.HtmlComment ) and "pdf" in c.text.rstrip().lower()
    except IndexError:
        return False
    return False
#end is_paper

def get_page( url, delay_before=5.0, delay_retry=None ):
    page = None
    if delay_retry is None:
        delay_retry = delay_before
    #end if
    time.sleep( delay_before )
    while page is None:
        try:
            page = requests.get( url )
        except requests.exceptions.RequestException as e:
            print( "Scraping unsucessful {}".format( e ), file=sys.stderr )
            time.sleep( delay_retry )
        #end try
    #end while
    return page
#end is_paper


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
