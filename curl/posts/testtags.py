#!/bin/python3

import sys
import json
import subprocess

def tagstruct_to_tagid(x):
    return x['tagId']

def tostring(lst):
    res = '[' + lst[0]
    for i in range(len(lst)-1):
        res = res + ',' + lst[i+1]
    return res + ']'

date = '2021-09-12'
#taglist = ['4', '7']
if len(sys.argv) < 2:
    taglist = ['4', '7']
else:
    taglist = sys.argv[1:]

# print(tostring(taglist))
taglist_nums = list(map(int, taglist))
tagset = set(taglist_nums)
process = subprocess.run(['curl', '-G', '-d', 'tags__in='
    + tostring(taglist), 'localhost:5555/posts/get' ], capture_output=True, text=True)
print ('getting posts with tags intersecting with ' + tostring(taglist))
response = json.loads(process.stdout)
for i in range(len(response)):
    tags = list(map(tagstruct_to_tagid, response[i]['tags']))
    intersection_ = set(tags).intersection(tagset)
    print(tags)
    print(len(intersection_) > 0)


process = subprocess.run(['curl', '-G', '-d', 'tags__all='
    + tostring(taglist), 'localhost:5555/posts/get' ], capture_output=True, text=True)

print ('getting posts with all of tags: ' + tostring(taglist))
response = json.loads(process.stdout)
for i in range(len(response)):
    tags = list(map(tagstruct_to_tagid, response[i]['tags']))
    print(tags)
    print(set(tagset).issubset(tags))




