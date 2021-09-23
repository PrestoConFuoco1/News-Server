#!/bin/python3

import sys
import json
import subprocess

# for running this test make sure that you have a post with id = 1
# usage: ./test_authors.py | grep False
# (should produce no input)

basicArgs = ['curl', '-G', '-d']
token = 'token='
post_id = 'post_id='
content = 'content='
comment_id = 'comment_id='
host = 'localhost:5555/comments/'
host1 = 'localhost:5555/posts/'

def run(args):
    #print('REQUEST:    ', args)
    process = subprocess.run(args, capture_output=True, text=True)
    response = json.loads(process.stdout)
    #print(response)
    return response

def getAlt(token_, post_id_):
    res = run(basicArgs + [token + token_, host1 + post_id_ + '/comments'])
    return res

def get(token_, post_id_):
    res = run(basicArgs + [token + token_ + '&' + post_id + post_id_, host + 'get'])
    return res

def create(token_, post_id_, content_):
    res = run(basicArgs + [token + token_ + '&' + #author_id + author_id_ + '&' +
                           post_id + post_id_ + '&' + content + content_, host+'create'])
    return res

def delete(token_, comment_id_):
    res = run(basicArgs + [token + token_ + '&' + comment_id + comment_id_ , host+'delete'])
    return res

def disp(res, sh):
    q = res['message']
    print('message: ', q, end=' ')
    ok_ = res['_ok']
    print(ok_ == sh)
#    if(ok_) :
#        print(res['result'])

#def getAlt(token_, post_id_)
#def get(token_, post_id_):
#def create(token_, post_id_, content_):
#def delete(token_, comment_id_):

#def getdraftbyid(drafts_, did_):
#    i = -1
#    while (drafts_[i]['draftId'] != did_):
#        i = i + 1
#    return drafts_[i]


if True:

    ourPost = '1'
    ourUserToken = 'push'

    print('trying to create comment with invalid post_id')
    res = create('push', '', 'comment1')
    disp(res, False)
    print('')

    print('trying to create comment with invalid post_id')
    res = create('push', '6666', 'comment2')
    disp(res, False)
    print('')

    print('creating comment')
    res = create('push', ourPost, 'comment3')
    disp(res, True)
    print('')
    comment1 = str(res['result'])

    print('creating comment')
    res = create('push', ourPost, 'comment4')
    disp(res, True)
    print('')
    comment2 = str(res['result'])

    print('get comments')
    res = get('push', ourPost)
    disp(res, True)

    print(len(res['result'])==2)
    print('')

 
    print('get comments with alternative path')
    res = getAlt('push', ourPost)
#    disp(res, True)


    print(res)
    print(len(res['result'])==2)
    print('')

    print('trying to delete comment with other user')
    res = delete('fail', comment1)
    disp(res, False)
    print('')

    print('deleting a comment with pushkin')
    res = delete('push', comment1)
    disp(res, True)
    print('')
  
    print ("deleting a comment with admin")
    res = delete('admin', comment2)
    disp(res, True)
    print('')

    print('deleting non-existing comment')
    res = delete('push', comment1)
    disp(res, False)
    print('')

#def getAlt(token_, post_id_)
#def get(token_, post_id_):
#def create(token_, post_id_, content_):
#def delete(token_, comment_id_):


