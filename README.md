# Ethics-AI-Data

This repository is related to the GCAI-2018 submission "Towards Understanding and Quantifying the Role of Ethics in AI Research".
It contains three folders: Abstracts, Code and DBLP.

## Abstracts

This folder contains the collected abstracts from the AAAI and NIPS conferences. To generate this data, you can use the scripts provided in the "Code" folder.

To generate the data in "aaai.txt" run:
python abstract_AAAIgen.py

To generate the data in "nips.txt" run:
python abstract_NIPSgen.py

The data is a CSV file with ";" as the cell separator, "#" as the text delimiter and a line break as entry separator.
The data is in the format:
year;id;title;author_list;url;abstract

Where *year* is an integer representing the year, *id* is a unique string that represents the publication inside the file, *title* is a string with the title of the paper, *author_list* is a string that contains the comma-separated author list and *abstract* is the abstract scraped from the website. Sometimes the abstract may contain a filler string or be empty. Note that all linebreaks and the ";" and "#" characters are removed from the strings before they are saved.

## Code

This folder contains the scripts used to generate the datasets and run the analysis on them.

The python files require python 3 with the requests (version 2.18.1) and lxml (version 4.0.0.0) packages.

The python files are mostly configured with constants in the code.

## DBLP

This folder contains the data generated from parsing the DBLP xml file, available at: [http://dblp.uni-trier.de/xml/](http://dblp.uni-trier.de/xml/)
The version we used was downloaded 12/06/2019.
