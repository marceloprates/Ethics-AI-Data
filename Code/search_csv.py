import csv, sys, re

csvfname = sys.argv[1]
newcsvfname = sys.argv[2]
kwfname = sys.argv[3]
ukwfname = sys.argv[4]

def parse_keywords( kwfile ):
    kw = []
    for line in kwfile.read().splitlines():
        words = line.split('\t')
        kw.append( words )
    #end
    return kw
#end parse_keywords

def check( text, kws ):
    qty = 0
    for kw in kws:
        has = True
        for k in kw:
            if k not in text:
                has = False
                break
            #end if
        #end for
        if has:
            qty += 1
        #end if
    return qty
#end check

with open( kwfname, mode="r", encoding="utf-8" ) as kwf:
    kw = parse_keywords( kwf )
#end with

with open( ukwfname, mode="r", encoding="utf-8" ) as ukwf:
    ukw = parse_keywords( ukwf )
#end with

with open( csvfname, mode="r", encoding="utf-8", newline='' ) as csvf:
    with open( newcsvfname, mode="w", encoding="utf-8", newline='' ) as ncsvf:
        reader = csv.reader( csvf, delimiter='\t', quotechar='"')
        writer = csv.writer( ncsvf, delimiter='\t', quotechar='"', quoting=csv.QUOTE_NONNUMERIC )
        for i, row in enumerate( reader ):
            title = row[1]
#            print( re.split( "[\W\d]", title.lower() ) )
            if check( re.split( "[\W\d]", title.lower() ), kw ) > check( re.split( "[\W\d]", title.lower() ), ukw ):
                writer.writerow( row )
            #end if
            if i % 1000 == 0:
                print( "{i}: {title}".format( i=i, title=title[:60] ) )
            #end if
        #end for
    #end with
#end with