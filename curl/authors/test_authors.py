#!/bin/python3

import sys
import json
import subprocess

# for running this test make sure that you have a user with id = 9
# usage: ./test_authors.py | grep False (should produce no input)

basicArgs = ['curl', '-G', '-d']
token = 'token='
user_id = 'user_id='
author_id = 'author_id='
description = 'description='
host = 'localhost:5555/authors/'

def run(args):
    process = subprocess.run(args, capture_output=True, text=True)
    response = json.loads(process.stdout)
    return response

def get(token_):
    res = run(basicArgs + [token + token_, host + 'get'])
    return res

def create(token_, user_id_, description_):
    res = run(basicArgs + [token + token_ + '&' + #author_id + author_id_ + '&' +
                           user_id + user_id_ + '&' + description + description_, host+'create'])
    return res

def delete(token_, author_id_):
    res = run(basicArgs + [token + token_ + '&' + author_id + author_id_ , host+'delete'])
    return res

def edit(token_, author_id_, description_):
    res = run(basicArgs + [token + token_ + '&' + author_id + author_id_ + '&' + description + description_, host+'edit'])
    return res


def disp(res, sh):
    q = res['message']
    print('message: ', q, end=' ')
    ok_ = res['_ok']
    print(ok_ == sh)
    if(ok_) :
        print(res['result'])


if True:
    uid = '9'
    if(len(sys.argv)==2):
        uid = sys.argv[1]
    

    print ('getting authors not being admin')
    res = get('fail')
    disp(res, False)
    print('')

    print ('getting authors with admin')
    res = get('admin')
    disp(res, True)
    print('')


    print ("trying to create author with wrong token")
    res = create('asdasdad', uid, 'Pushkin1')
    disp(res, False)
    print('')

    print("trying to create author not being admin")
    #./create_author.sh push 2 "Pushkin2"
    res = create('push', uid, 'Pushkin2')
    disp(res, False)
    print('')

    print("trying to create author with admin but with invalid user_id")
    #./create_author.sh admin 666 "Pushkin3"
    res = create('admin', '666', 'Pushkin3')
    disp(res, False)
    print('')

    print("trying to create author with admin but with invalid description")
    #./create_author.sh admin 2 ""
    res = create('admin', uid, '')
    disp(res, False)
    print('')

    print("creating author with admin")
    #./create_author.sh admin 2 "Pushkin4"
    res = create('admin', uid, 'Pushkin4')
    disp(res, True)
    print('')
    ids = str(res['result'])

#---------

    #ids = sys.argv[1]
    
    print ("trying to edit author without token")
    #./edit_author.sh "" $1 "Pushkin5"
    res = edit('', ids, 'Pushkin5')
    disp(res, False)
    print('')
    
    print ("trying to edit author with wrong token")
    res = edit('sdasdasd', ids, 'Pushkin6')
    disp(res, False)
    print('')
    #./edit_author.sh sdadsasda $1 "Pushkin6"
    
    print ("trying to edit author with right token but not being admin")
    res = edit('push', ids, 'Pushkin7')
    disp(res, False)
    print('')
    #./edit_author.sh push $1 "Pushkin7"
    
    #./edit_author.sh admin "" ""
    print ("trying to edit author with all params empty")
    res = edit('admin', ids, '')
    disp(res, False)
    print('')
    
    print ("editing author with admin")
    res = edit('admin', ids, 'Pushkin8')
    disp(res, True)
    print('')
    
    print ("deleting author with admin")
    res = delete('admin', ids)
    disp(res, True)
    print('')

