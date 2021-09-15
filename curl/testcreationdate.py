#!/bin/python3

import json
import subprocess


date = '2021-09-12'
process = subprocess.run(['./posts_created.sh', date], capture_output=True, text=True)
print ('getting posts with ' + date + ' creation date')
response = json.loads(process.stdout)
for i in range(len(response)):
    t = response[i]['creationDate']
    print('Creation date is ' + t)
    print(t == date)

process = subprocess.run(['./posts_created_earlier.sh', date], capture_output=True, text=True)
print ('getting posts created then or earlier than ' + date)
response = json.loads(process.stdout)
for i in range(len(response)):
    t = response[i]['creationDate']
    print('Creation date is ' + t)
    print(t <= date)

process = subprocess.run(['./posts_created_later.sh', date], capture_output=True, text=True)
print ('getting posts created then or later than ' + date)
response = json.loads(process.stdout)
for i in range(len(response)):
    t = response[i]['creationDate']
    print('Creation date is ' + t)
    print(t >= date)

