import glob

# warning: package to be installed is pdfminer.six, not pdfminer or pdfminer3k
# pip install pdfminer.six

from io import StringIO
from pdfminer.converter import TextConverter
from pdfminer.layout import LAParams
from pdfminer.pdfdocument import PDFDocument
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfparser import PDFParser

import regex as re
import numpy as np
import pandas as pd

files = glob.glob('pdf/*.pdf')

results = pd.DataFrame(index = pd.MultiIndex.from_tuples([], names=['county', 'age_category']))

for file in files:
    
    # this chuck is for extracting content as plain text from pdf
    output_string = StringIO()
    f = open(file, 'rb')
    parser = PDFParser(f)
    doc = PDFDocument(parser)
    rsrcmgr = PDFResourceManager()
    device = TextConverter(rsrcmgr, output_string, laparams=LAParams())
    interpreter = PDFPageInterpreter(rsrcmgr, device)
    
    pageNumber = 1
    for page in PDFPage.create_pages(doc):
        if pageNumber == 2:
            interpreter.process_page(page)
        else:
            pageNumber += 1
    
    text = output_string.getvalue()
    
    f.close()
    device.close()
    output_string.close()
    
    
    # this chuck is to convert the plain text into pandas dataframe
    
    # remove disclaimer
    text = text[:text.find('Information provided in this report is maintained by Child Care Aware')+1]
    
    # extract county name
    county_extractor = re.compile(r'County:\s([A-Z\s]+)Cost')
    
    try:
        county = county_extractor.search(text).group(1)
    except Exception as e:
        print(file + " is considered of county ROANOKE CITY by manual inspection")
        county = 'ROANOKE CITY'
                
    text = text[text.find('Cost'):]
    
    # extract table entry values
    entry_value_extractor = re.compile(r"X|\$[0-9.]+")
    values = np.array(entry_value_extractor.findall(text))
    
    def convert_to_numeric(string):
        if string == 'X':
            return None
        else:
            return float(string.replace('$', ''))
    converter = np.vectorize(convert_to_numeric)
    values = converter(values)
    
    # organizer into dataframes with appropriate names
    try:
        values = values.reshape([5, 12])
    except Exception as e:
        print(file)
        raise e
    age_cate = pd.Series(['Infant (Birth - 16months)', 'Toddler (17 - 23 months)',
                          'Preschool (2-3 years)', 
                          'Preschool (4-5 years and notin school)',
                          'School Age (5-12 years)'], name='age_category')
    df = pd.DataFrame(values, index=age_cate)
    
    def column_names(column_number):
        column_number = int(column_number)
        care_cate = ['Center', 'Family Child Care', 'Preschools', 'School Age']
        stat_type = ['Min', 'Avg', 'Max']
        return "_".join([care_cate[column_number//3], stat_type[column_number%3]])
    
    df.rename(mapper=column_names, axis=1, inplace=True)
    
    results = pd.concat([results, df.assign(county=county).set_index(
        'county', append=True).swaplevel(0,1)])

results.to_csv('va_childcare_cost.csv')