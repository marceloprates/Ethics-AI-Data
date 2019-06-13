
import pandas as pd
from matplotlib import pyplot as plt
import re
import math
import os

def get_matches(text, keywords):
    pattern = f'|'.join([keyword.lower() for keyword in keywords])
    return re.findall(pattern, text.lower())
#end

# Get classical, trending, ethics keywords
classical_keywords  = [ line.strip() for line in open('../DBLP/classical_keywords.txt').readlines() ]
trending_keywords   = [ line.strip() for line in open('../DBLP/trending_keywords.txt').readlines() ]
ethics_keywords     = [ line.strip() for line in open('../DBLP/ethics_keywords.txt').readlines() ]

# Get DBLP data for conferences and journals
conf_data = pd.read_csv('../DBLP/cdblp.csv', sep='\t', index_col=False)
journal_data = pd.read_csv('../DBLP/jdblp.csv', sep='\t', index_col=False)

# Compute statistics for conferences
if not os.path.isfile('../Stats/conf_matches.csv'):
    conf_matches = pd.DataFrame(columns=['year','venue','classical','trending','ethics'])
    for i,row in conf_data.iterrows():
        # Get year, venue, title
        year = row['year']
        venue = row['booktitle']
        title = row['title']
        # Skip blanks
        if str(year)=='nan' or str(venue)=='nan' or str(title)=='nan': continue;
        # Add row
        conf_matches.loc[i] = [
            int(year),
            venue,
            1 if len(get_matches(title,classical_keywords)) > 0 else 0,
            1 if len(get_matches(title,trending_keywords)) > 0 else 0,
            1 if len(get_matches(title,ethics_keywords)) > 0 else 0
        ]
    #end
    conf_matches.to_csv('../Stats/conf_matches.csv',index=False)
else:
    conf_matches = pd.read_csv('../Stats/conf_matches.csv', index_col=False)
#end

# Compute statistics for journals
if not os.path.isfile('../Stats/journal_matches.csv'):
    journal_matches = pd.DataFrame(columns=['year','venue','classical','trending','ethics'])
    for i,row in journal_data.iterrows():
        # Get year, venue, title
        year = row['year']
        venue = row['journal']
        title = row['title']
        # Skip blanks
        if str(year)=='nan' or str(venue)=='nan' or str(title)=='nan': continue;
        # Add row
        journal_matches.loc[i] = [
            int(year),
            venue,
            1 if len(get_matches(title,classical_keywords)) > 0 else 0,
            1 if len(get_matches(title,trending_keywords)) > 0 else 0,
            1 if len(get_matches(title,ethics_keywords)) > 0 else 0
        ]
    #end
    journal_matches.to_csv(f'../Stats/journal_matches.csv',index=False)
else:
    journal_matches = pd.read_csv('../Stats/journal_matches.csv')
#end

# Aggregate conference and journal matches by venue and year
conf_matches['ethics'].groupby([conf_matches['venue'],conf_matches['year']]).mean().to_csv('../Stats/conf_matches_aggregated.csv')
journal_matches['ethics'].groupby([conf_matches['venue'],conf_matches['year']]).mean().to_csv('../Stats/journal_matches_aggregated.csv')