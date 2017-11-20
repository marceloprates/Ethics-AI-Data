import requests
import time
from lxml import etree, html
# Utility functions for the scraping scripts

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
#end find_after

def get_page( url, delay_before=5.0, delay_retry=None ):
    page = None
    if delay_retry is None:
        delay_retry = delay_before
    #end if
    time.sleep( delay_before )
    while page is None:
        try:
            page = requests.get( url ) #,  headers={'User-Agent':'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:56.0) Gecko/20100101 Firefox/56.0'} )
        except requests.exceptions.RequestException as e:
            print( "Scraping unsucessful {}".format( e ), file=sys.stderr )
            time.sleep( delay_retry )
        #end try
    #end while
    return page
#end get_page

def AAAI_is_paper( p ):
    try:
        _,_,_ = p.xpath( 'a' )[0].text, p.xpath( 'i' )[0].text, p.xpath('a')[0].attrib['href']
    except IndexError:
        return False
    return True
#end AAAI_is_paper

def NIPS_is_paper( p ):
    try:
        _,_,_ = p.xpath( 'a' )[0].text, ",".join( filter( None, [ author.text for author in p.xpath( 'a[@class="author"]' ) ] ) ), p.xpath('a')[0].attrib['href']
    except IndexError:
        return False
    return True
#end NIPS_is_paper

def csv_clean_string( s, text_delimiter, csv_separator, csv_end_line ):
    return s.replace( text_delimiter, '' ).replace( csv_separator, '' ).replace( csv_end_line, ' ' )
#end csv_clean_string
