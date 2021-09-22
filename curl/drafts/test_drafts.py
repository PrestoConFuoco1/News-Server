#!/bin/python3

import json
import subprocess
import sys

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
def delete(token_, category_id_):
    res = run(basicArgs + [token + token_ + '&' + category_id + category_id_, host+'delete'])
    return res

def publish(token_, draft_id_):
    res = run(basicArgs + [token + token_ + '&' + draft_id + draft_id_, hostpub])

def geta(token_):
    host1 = 'localhost:5555/authors/'
    res = run(basicArgs + [token + token_, host1 + 'get'])
    return res

def getdraftbyid(drafts_, did_):
    i = -1
    while (drafts_[i]['draftId'] != did_):
        i = i + 1
    return drafts_[i]

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

#if len(sys.argv) < 2:
if True:

 #   res = geta('admin')
 #   print(res, '\n')
# def create(token_, title_, tags_, category_id_, content_, main_photo_, extra_photos_):
    print ('trying to create draft being unauthorized')
    #res = create('asasda', 'bora-bora1', '[2,3,4,5]', '2', 'poshla zhara :D', 'tipa url1', '["tipaurl1", "tipaurl1_2"]')
    tags1_ = '[2,3,4,5]'
    cat1_ = '2'
    res = create('asasda', 'bora-bora1', tags1_, cat1_, 'poshla+zhara+:D', 'tipaurl1', '["tipaurl1","tipaurl1_2"]')
    print(res, '\n')

    print('creating draft with author token')
    res = create('push', 'bora-bora2', tags1_, cat1_, 'poshla+zhara+:D', 'tipa+url2', '["tipaurl2","tipaurl2_2"]')
    draftid = res['result']
    print(res, '\n')
    print('draft id is ' + str(draftid))
    
    print('getting drafts with author token')
    res = get('push')
    drafts = res['result']
    print(len(drafts), ' drafts fetched\n')

    ourdraft = getdraftbyid(drafts, draftid)
    print('Our draft is ', ourdraft)
    tags1 = ourdraft['tags']
    print('tags are ', printTags(tags1), ', should be ', tags1_)
    cats1 = ourdraft['category']
    print('category is ', cats1, ', should be 2', '\n')
    


#def edit(token_, draft_id_, title_, tags_, category_id_, content_, main_photo_, extra_photos_):

    tags2_ = '[1,3]'
    cat2_ = '3'
    print('trying to edit this post with other user, not an author')
    res = edit('fail', str(draftid), 'bora-bora2', tags2_, cat2_, 'supernova', 'newphoto', '[]')
    print(res, '\n')

    print('editing post')
    res = edit('push', str(draftid), 'bora-bora3', tags2_, cat2_, 'supernova', 'newphoto', '[]')
    print(res, '\n')

    res = get('push')
    drafts = res['result']
    print(len(drafts), ' drafts fetched\n')

    ourdraft = getdraftbyid(drafts, draftid)
    print('Our draft is ', ourdraft)
    tags2 = ourdraft['tags']
    print('tags are ', printTags(tags2), ', should be ' + tags2_)
    cats2 = ourdraft['category']['categoryId']
    print('category is ', cats2, ', should be ' + cat2_)


    tags3_ = '[]'
    cats3_ = '4'
    print('editing post')
    res = edit('push', str(draftid), 'bora-bora4', tags3_, cats3_, 'mozart', 'newphoto', '["photo1","photo2"]')
    print(res, '\n')

    res = get('push')
    drafts = res['result']
    print(len(drafts), ' drafts fetched\n')

    ourdraft = getdraftbyid(drafts, draftid)
    print('Our draft is ', ourdraft)
    tags3 = ourdraft['tags']
    print('tags are ', printTags(tags3), ', should be ' + tags3_)
    cats3 = ourdraft['category']['categoryId']
    print('category is ', cats3, ', should be ' + cats3_)


    print('publish draft')


