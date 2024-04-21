'''
    Using the format of statements from easyweb, this script
    collates all statements into a single file.
'''
import os

def get_trans(file):
    with open(file, 'r') as f:
        lines = f.readlines()
    return lines

trans = [get_trans(file) for file in os.listdir('.') 
                         if file.startswith('acc')]
trans = [item for sublist in trans for item in sublist]
trans = list(set(trans))

with open('transactions.csv', 'w') as f:
    f.write(''.join(trans))