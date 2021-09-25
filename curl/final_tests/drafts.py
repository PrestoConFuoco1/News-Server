#!/bin/python3

import json
import subprocess
import sys
import random
import string

basicArgs = ['curl', '-G', '-d']
token = 'token='
draft_id = 'draft_id='
title = 'title='
tags = 'tags='
category_id = 'category_id='
content = 'content='
main_photo = 'main_photo='
extra_photos = 'extra_photos='


host = 'localhost:5555/drafts/'
hostpub = 'localhost:5555/publish'


def run(args):
    print('REQUEST -- ' + str(args))
    process = subprocess.run(args, capture_output=True, text=True)
#    print(process.stdout)
    response = json.loads(process.stdout)
    return response
def get(token_):
    res = run(basicArgs + [token + token_
                                , host + 'get'])
    return res
def create(token_, title_, tags_, category_id_, content_, main_photo_, extra_photos_):
    res = run(basicArgs + [token + token_ + '&' + title + title_ + '&' + tags + tags_ + '&' +
                           category_id + category_id_ + '&' +
                           content + content_ + '&' +  main_photo + main_photo_ + '&' +
                            extra_photos + extra_photos_, host+'create'])
    return res
def edit(token_, draft_id_, title_, tags_, category_id_, content_, main_photo_, extra_photos_):
    res = run(basicArgs + [
                            token + token_ + '&' +
                            draft_id + draft_id_ + '&' +
                           title + title_ + '&' + tags + tags_ + '&' +
                           category_id + category_id_ + '&' +
                           content + content_ + '&' +  main_photo + main_photo_ + '&' +
                            extra_photos + extra_photos_, host+'edit'])
 
    return res
def delete(token_, draft_id_):
    res = run(basicArgs + [token + token_ + '&' + draft_id + draft_id_, host+'delete'])
    return res

def publish(token_, draft_id_):
    res = run(basicArgs + [token + token_ + '&' + draft_id + draft_id_, hostpub])
    return res

def getPosts():

    process = subprocess.run(['curl', 'localhost:5555/posts/get' ], capture_output=True, text=True)
    #print(process.stdout)
    response = json.loads(process.stdout)
   
    return response

def getdraftbyid(drafts_, did_):
    i = 0
    while (drafts_[i]['draftId'] != did_):
        i = i + 1
    return drafts_[i]

def getpostbyid(posts_, did_):
    i = 0
    # здесь было -1 почему-то
    while (posts_[i]['postId'] != did_):
        i = i + 1
    return posts_[i]

def compareDraftPost(post_, draft_):
    i = 0


def printTags(tags):
    a = '['
    j = -1
    for i in range(len(tags)-1):
        a = a + str(tags[i]['tagId']) + ','
        j = i
    if j >= 0:
        a = a + str(tags[j+1]['tagId'])
    a = a + ']'
    return a

def assert1(name, whatshouldbe, whatis):
    print(name, " is ", whatis, ", should be: ", whatshouldbe, end=' ')
    print(whatis == whatshouldbe)

def gettagids(whatis):
    a = []
    for i in range(len(whatis)):
        a = a + [int(whatis[i]['tagId'])]
    return a

def getcatid(category_):
    return int(category_['categoryId'])

def assertdraft(draft_, title_, tags_, category_, content_, main_photo_, extra_photos_):
    assert1("title", title_, draft_['title'])
    assert1("tags", set(tags_), set(gettagids(draft_['tags'])))
    assert1("category", category_, getcatid(draft_['category']))
    assert1("content", content_, draft_['content'])
    assert1("main photo", main_photo_, draft_['mainPhoto'])
    assert1("extra photos", set(extra_photos_), set(draft_['extraPhotos']))
    
def ep(eph):
    a = []
    for i in range(len(eph)):
        a = a + [eph[i].replace('\'', '"')]
    print(a)
    return a

def disp(res, sh):
    q = res['message']
    print('message: ', q, end=' ')
    ok_ = res['_ok']
    print(ok_ == sh)
    if(ok_) :
        print(res['result'])
    print('')


def dispwithoutresults(res, sh):
    q = res['message']
    print('message: ', q, end=' ')
    ok_ = res['_ok']
    print(ok_ == sh)
    print('')



def lstparam(lst):
    b = ''
    tmp = b.join(str(i)+',' for i in lst)
    b = '[' + tmp[:-1] + ']'
    return b

def lstparam1(lst):
    s = str(lst)
    s1 = s.replace(' ', '+')
    return s1.replace('\'', '"')

def testedit(token_, draft_id_, title_, tags_, category_id_, content_, main_photo_, extra_photos_):
    res = edit(token_, str(draft_id_), title_, lstparam1(tags_), str(category_id_), content_, main_photo_, lstparam1(extra_photos_))
    disp(res, True)

    print('getting drafts to assert that there is exactly what we asked to edit')
    res = get(token_)
    disp(res, True)
    d = getdraftbyid(res['result'], draft_id_)
    assertdraft(d, title_, tags_, category_id_, content_, main_photo_, extra_photos_)
 #   print('tags: ', gettagids(d['tags']))
 #   print('category: ', getcatid(d['category']))

def randomtags():
    lengthlist = random.randint(3,5)
    a = []
    for i in range(lengthlist):
        a = a + [random.randint(1,7)]
    return list(set(a))

def randomstring(length):
   letters = string.ascii_lowercase
   return ''.join(random.choice(letters) for i in range(length))

#if len(sys.argv) < 2:

def mainfunc():
# def create(token_, title_, tags_, category_id_, content_, main_photo_, extra_photos_):
    print ('trying to create draft being unauthorized')
    #res = create('asasda', 'bora-bora1', '[2,3,4,5]', '2', 'poshla zhara :D', 'tipa url1', '["tipaurl1", "tipaurl1_2"]')
#    tags1__ = [4,5]
    tags1__ = randomtags()
    tags1_ = '[4,5]'
   # tags1_ = spacestoplus(str(tags1__))
    cat1__ = random.randint(1,7)
    cat1_ = '2'
    extraphotos1_ =  ['tipaurl1','tipaurl1_2']
    res = create('asasda', 'bora-bora1', lstparam1(tags1__), cat1_, 'poshla+zhara+:D', 'tipaurl1', lstparam1(extraphotos1_))
    disp(res, False)


    tags2_ = randomtags()
    cat2_ = random.randint(1,7)
#    title2_ = 'bora-bora2'
    title2_ = randomstring(10)
    #content2_ = 'poshlazhara'
    content2_ = randomstring(10)
    #photo2_ = 'tipaurl2'
    photo2_ = randomstring(10)
 #   extraphotos2_ =  ['tipaurl2','tipaurl2_2']
    extraphotos2_ = [randomstring(10), randomstring(10)]
    print('creating draft with author token')
    res = create('push', title2_, lstparam1(tags2_), str(cat2_), content2_,  photo2_, lstparam1(extraphotos2_))
    draftid = res['result']
    disp(res, True)

    print('getting drafts to assert that there is exactly what we asked to create')
    res = get('push')
    disp(res, True)
    d = getdraftbyid(res['result'], draftid)
    assertdraft(d, title2_, tags2_, cat2_, content2_, photo2_, extraphotos2_)
    print('tags: ', gettagids(d['tags']))
    print('category: ', getcatid(d['category']))


#def edit(token_, draft_id_, title_, tags_, category_id_, content_, main_photo_, extra_photos_):
    print('editing draft with author token')
    #title_ = 'Messi3'
    title_ = randomstring(10)
    #content_ = 'PSG'
    content_ = randomstring(10)
    #photo_ = 'tipaurl3'
    photo_ = randomstring(10)
    #tags_ = [1,7]
    tags_ = randomtags()
    cat_ = random.randint(1,7)
    
    extraphotos_ = ['tipaurl3', 'tipaurl3_2']
    testedit('push', draftid, title_, tags_, cat_, content_, photo_, extraphotos_)

    print('editing draft with author token with empty tags')
    #title_ = 'Messi4'
    title_ = randomstring(10)
    #content_ = 'PSG4'
    content_ = randomstring(10)
    #photo_ = 'tipaurl4'
    photo_ = randomstring(10)
    tags_ = []
    cat_ = 8
    
    extraphotos_ = ['tipaurl5', 'tipaurl3_2', 'blllfsds']
    testedit('push', draftid, title_, tags_, cat_, content_, photo_, extraphotos_)

    print('editing draft with author token with empty extra photos')
    #title_ = 'Messi4'
    title_ = randomstring(10)
    #content_ = 'PSG4'
    content_ = randomstring(10)
    #main_photo_ = 'tipaurl4'
    main_photo_ = randomstring(10)
    tags_ = [3]
    cat_ = 8
    
    extra_photos_ = []
    testedit('push', draftid, title_, tags_, cat_, content_, main_photo_, extra_photos_)

    draft_id_ = draftid
    title_ = 'xD'
    tags_ = [1,5]
    category_id_ = 4
    content_ = 'content289'
    main_photo_ = 'sdad'
    extra_photos_ = ['haha']
    testedit('push', draft_id_, title_, tags_, category_id_, content_, main_photo_, extra_photos_)

    print('publish draft')
    res = publish('push', str(draftid))
    disp(res, True)
    post_id_ = res['result']


    print('getting posts to assert that there is exactly what we asked to edit')
    res = getPosts()
    dispwithoutresults(res, True)
    p = getpostbyid(res['result'], post_id_)
    assertdraft(p, title_, tags_, category_id_, content_, main_photo_, extra_photos_)


    print('editing one more time, then we wil publish again')
    draft_id_ = draftid
    title_ = randomstring(10)
    tags_ = randomtags()
    category_id_ = random.randint(1,7)
    content_ = randomstring(10)
    main_photo_ = randomstring(10)
    extra_photos_ = [randomstring(10)]
    testedit('push', draft_id_, title_, tags_, category_id_, content_, main_photo_, extra_photos_)
    

    print('publish draft')
    res = publish('push', str(draftid))
    disp(res, True)
    post_id_ = res['result']


    print('getting posts to assert that there is exactly what we asked to edit')
    res = getPosts()
    dispwithoutresults(res, True)
    p = getpostbyid(res['result'], post_id_)
    assertdraft(p, title_, tags_, category_id_, content_, main_photo_, extra_photos_)


    
#def delete(token_, category_id_):
    print('delete draft')
    res = delete('push', str(draft_id_))
    disp(res, True)

if len(sys.argv) == 2:
    tot = int(sys.argv[1])
else:
    tot = 4
if True:
    for i in range(tot):
        mainfunc()


