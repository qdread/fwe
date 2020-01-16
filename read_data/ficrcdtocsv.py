# -*- coding: utf-8 -*-
"""
Created on Wed Mar 27 10:43:50 2019

@author: qread
"""

# Convert FICRCD sas7bdat files to CSV files
from sas7bdat import SAS7BDAT

with SAS7BDAT('Q:/raw_data/FICRCD/ficrcd_2007_08.sas7bdat') as f:        
        df = f.to_data_frame()
        
df.to_csv('Q:/raw_data/FICRCD/ficrcd_2007_2008.csv')

with SAS7BDAT('Q:/raw_data/FICRCD/ficrcd_2005_06.sas7bdat') as f:        
        df = f.to_data_frame()
        
df.to_csv('Q:/raw_data/FICRCD/ficrcd_2005_2006.csv')

with SAS7BDAT('Q:/raw_data/FICRCD/ficrcd_2003_04.sas7bdat') as f:        
        df = f.to_data_frame()
        
df.to_csv('Q:/raw_data/FICRCD/ficrcd_2003_2004.csv')