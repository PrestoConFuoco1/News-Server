#!/bin/python3

import json
import subprocess
import sys
import string
import random

basicArgs = ['curl', '-G', '-d']
host = 'localhost:5555/posts/get'

tagparam = "tag="
tagsinparam = "tags__in="
tagsallparam = "tags__all="

def tagone(tag_):
    s = tagparam + str(tag_)
    return s

def tagsin(tags_):
    s = tagsinparam + lstparam1(tags_)
    return s    

def tagsall(tags_):
    s = tagsallparam + lstparam1(tags_)
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

    
    
def check_tagsin(tags1_, tagsreceived_):
    intersection_ = set(tags1_).intersection(set(tagsreceived_))
    print('tags: received: ', tagsreceived_, ', should overlap with ', tags1_, '  ', len(intersection_) > 0)

def check_tagsall(tags1_, tagsreceived_):
    print('tags: received: ', tagsreceived_, ', should include ', tags1_, '  ', set(tags1_).issubset(tagsreceived_))


def checkforall(func_, posts_, what):
    for i in range(len(posts_)):
        func_(posts_[i], what)

def assertposttagsin(post_, tagsrecv_):
    check_tagsin(tagsrecv_, gettagids(post_['tags']))

def assertposttagsall(post_, tagsrecv_):
    check_tagsall(tagsrecv_, gettagids(post_['tags']))



def gettagids(whatis):
    a = []
    for i in range(len(whatis)):
        a = a + [int(whatis[i]['tagId'])]
    return a

def gettagnames(tags_):
    b = ''
    print(tags_)
    tmp = b.join(i['tagName'] for i in tags_)
    return tmp




def assertpostcreatedlater(post_, date_):
    print('creation date: received: ', post_['creationDate'], ' should be greater than ', date_, post_['creationDate'] >= date_)

def assertpostsearch(post_, search_):
    catName_ = post_['category']['description']
 #   tagNames_ = lstparam1(gettagids(post_['tags']))
    tagNames_ = gettagnames(post_['tags'])
    content_ = post_['content']
    title_ = post_['title']
    print('one of next statements should be true')
    print('title: received: ', title_, 'should have a substring ', search_)
    print('content: received: ', content_, 'should have a substring ', search_)
    print('category: received: ', catName_, 'should have a substring ', search_)
    print('tags: received: ', tagNames_, 'should have a substring ', search_)
    print(issubstr_(title_, search_) | issubstr_(content_, search_) |
        issubstr_(catName_, search_) | issubstr_(tagNames_, search_))


def assertsort(func_, posts_):
    a = True
    for i in range(len(posts_)-1):
        a = a & (func_(posts_[i], posts_[i+1]))
    print(a)

def dategreater(post1_, post2_):
    s = post1_['creationDate'] >= post2_['creationDate']
    return s

def datelower(post1_, post2_):
    print(post1_['creationDate'], post2_['creationDate'])
    s = post1_['creationDate'] <= post2_['creationDate']
    return s


def issubstr_(str_, patt_):
    b = (str_.lower()).find(patt_.lower()) >= 0
    return b


def randomstring(length):
   letters = string.ascii_lowercase
   return ''.join(random.choice(letters) for i in range(length))


def randomtags(n):
    lengthlist = random.randint(1,n)
    a = []
    for i in range(lengthlist):
        a = a + [random.randint(1,7)]
    return list(set(a))



#if True:
def mainfunc():

    tagslist_ = [7, [6,7], [2,4,7]]
    created_ = '2021-09-12'


#    tagsin_ = tagslist_[1]
    tagsin_ = randomtags(3)
    search_ = randomstring(2)
 #   search_ = 'ng'
    print('getting posts')
    res = get(tagsin(tagsin_), createdlater(created_), sort('dd'), searchp(search_))
#    print(res) 

    posts_ = res['result']
    checkforall( assertposttagsin, posts_, tagsin_)
    checkforall( assertpostcreatedlater, posts_, created_)
    checkforall( assertpostsearch, posts_, search_)
    assertsort( dategreater, posts_)

    created_ = '2021-08-12'
    tagsall_ = randomtags(2)
    res = get(tagsall(tagsall_), createdlater(created_), sort('da'), searchp(search_))
    posts_ = res['result']
    checkforall( assertposttagsall, posts_, tagsall_)
    checkforall( assertpostcreatedlater, posts_, created_)
    checkforall( assertpostsearch, posts_, search_)
    assertsort( datelower, posts_)



if True:
#    for i in range(60):
    if True:
        mainfunc()


