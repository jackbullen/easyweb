'''
    Assumes that collate.py has been run.
    This script reads transactions.csv and 
    creates daily and monthly basket of purchases.
'''
with open('transactions.csv', 'r') as f:
    transactions = f.readlines()

from collections import defaultdict
daily = defaultdict(list)
monthly = defaultdict(list)

for trans in transactions:
    # Overlimit transactions will fail here (extra ,)
    date, location, x, y, z = trans.split(',')
    #               x, y, z = credit, debit, balance
    daily[date].append((location, x, y, z))
    month = date[:2]
    monthly[month].append((location, x, y, z))

days = []
for date, purchases in daily.items():
    purchases = list(set(list(map(lambda x: x[0], purchases))))
    days.append(purchases)

months = []
for month, purchases in monthly.items():
    purchases = list(set(list(map(lambda x: x[0], purchases))))
    months.append(purchases)

with open('daily.csv', 'w') as f:
    for day in days:
        f.write(','.join(day)+'\n')

with open('monthly.csv', 'w') as f:
    for month in months:
        f.write(','.join(month)+'\n')