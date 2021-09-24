#!/bin/python3

import json
import subprocess
import sys


basicArgs = ['curl', '-G', '-d']
host = 'localhost:5555/posts/get'

tagparam = "tag="
tagsinparam = "tags__in="
tagsallparam = "tags__all="

def tagone(tag_):
    s = tagparam + str(tag_)
    return s

def tagsin(tags_):
    s = tagsinparam + lstparam1(lst)
    return s    

def tagsall(tags_):
    s = tagsallparam + lstparam1(lst)
    return s    

createdparam = "created_at="
createdearlierparam = "created_at__lt="
createdlaterparam = "created_at__gt="

def created(date_):
    s = createdparam + date_
    return s

def createdearlier(date_):
    s = createdearlerparam + date_
    return s

def createdlater(date_):
    s = createdlaterparam + date_
    return s


def searchp(patt_):
    s =  "search=" + patt_
    return s

def sort(sort_):
    s = "sort=" + sort_
    return s


def run(args):
    print('REQUEST -- ' + str(args))
    process = subprocess.run(args, capture_output=True, text=True)
    #print(process.stdout)
    response = json.loads(process.stdout)
    return response
def get(tags_, created_, sortopts_, search_):
    res = run(basicArgs + [tags_+'&'+created_+'&'+sortopts_+'&'+search_
                                , host])
    return res

def lstparam1(lst):
    s = str(lst)
    s1 = s.replace(' ', '+')
    return s1.replace('\'', '"')


if True:

    tagslist_ = [7, [6,7], [2,4,7]]
    created_ = '2021-09-08'
    

    print('getting posts')
    res = get(tagone(tagslist_[0]), createdlater(created_), sort('dd'), searchp('%'))
    print(res) 





