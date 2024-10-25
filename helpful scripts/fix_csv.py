import pandas as pd

# Define the file path
file_path = '/Users/ritalavi/Desktop/vowel_pilot/results.csv'

# Try loading with different delimiters (comma, semicolon, or tab)
df = pd.read_csv(file_path, delimiter=',', encoding='utf-16')  

import re 
def clean_filename(name):
    # Remove all numbers
    name = re.sub(r'\d+', '', name)
    # Remove everything after '-' including '-'
    name = re.split(r'-', name)[0]
    return name.strip()

# Apply the function to the 'Filename' column
df['Filename'] = df['Filename'].apply(clean_filename)

#check to see if it worked
print(df)





