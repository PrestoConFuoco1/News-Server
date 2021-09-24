#!/bin/python3

import sys
import json
import subprocess

# for running this test make sure that you have a user with id = 9 and it has no author
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

def getauthorbyid(authors_, did_):
    i = 0
#    while (authors_[i]['authorId'] != did_):
#        i = i + 1
#    return authors_[i]
    for i in range(len(authors_)):
        if (authors_[i]['authorId'] == did_):
            return authors_[i]
    return 1


def assertauthor(author_, description_, user_id_):
    assert1("description", description_, author_['description'])
    assert1("user_id", int(user_id_), author_['user']['id'])
 
def assert1(name, whatshouldbe, whatis):
    print(name, " is ", whatis, ", should be: ", whatshouldbe, end=' ')
    print(whatis == whatshouldbe)



def disp(res, sh):
    q = res['message']
    print('message: ', q, end=' ')
    ok_ = res['_ok']
    print(ok_ == sh)
    if(ok_) :
        print(res['result'])
    print('')


if True:
    uid = '9'
    if(len(sys.argv)==2):
        uid = sys.argv[1]
    

    print ('getting authors not being admin')
    res = get('fail')
    disp(res, False)
    

    print ('getting authors with admin')
    res = get('admin')
    disp(res, True)
    


    print ("trying to create author with wrong token")
    res = create('asdasdad', uid, 'Pushkin1')
    disp(res, False)
    

    print("trying to create author not being admin")
    #./create_author.sh push 2 "Pushkin2"
    res = create('push', uid, 'Pushkin2')
    disp(res, False)
    

    print("trying to create author with admin but with invalid user_id")
    #./create_author.sh admin 666 "Pushkin3"
    res = create('admin', '666', 'Pushkin3')
    disp(res, False)
    

    print("trying to create author with admin but with invalid description")
    #./create_author.sh admin 2 ""
    res = create('admin', uid, '')
    disp(res, False)
    

    print("creating author with admin")
    #./create_author.sh admin 2 "Pushkin4"
    description_ = 'Pushkin4'
    res = create('admin', uid, description_)
    disp(res, True)
    
    authorid = res['result']


    print ('getting authors in order to know that there is exactly what we created')
    res = get('admin')
    disp(res, True)
    author_ = getauthorbyid(res['result'], authorid)
    assertauthor(author_, description_, uid)
    

#---------

    #authorid = sys.argv[1]
    
    print ("trying to edit author without token")
    #./edit_author.sh "" $1 "Pushkin5"
    res = edit('', str(authorid), 'Pushkin5')
    disp(res, False)
    
    
    print ("trying to edit author with wrong token")
    res = edit('sdasdasd', str(authorid), 'Pushkin6')
    disp(res, False)
    
    #./edit_author.sh sdadsasda $1 "Pushkin6"
    
    print ("trying to edit author with right token but not being admin")
    res = edit('push', str(authorid), 'Pushkin7')
    disp(res, False)
    
    #./edit_author.sh push $1 "Pushkin7"
    
    #./edit_author.sh admin "" ""
    print ("trying to edit author with all params empty")
    res = edit('admin', str(authorid), '')
    disp(res, False)
    

    description_ = 'Pushkin8'
    print ("editing author with admin")
    res = edit('admin', str(authorid), description_)
    disp(res, True)
 
    
    print ('getting authors in order to know that there is exactly what we edited')
    res = get('admin')
    disp(res, True)
    author_ = getauthorbyid(res['result'], authorid)
    assertauthor(author_, description_, uid)
    
  
    
    print ("deleting author with admin")
    res = delete('admin', str(authorid))
    disp(res, True)
 
    print ('getting authors in order to know that we indeed deleted the author')
    res = get('admin')
    disp(res, True)
    r = getauthorbyid(res['result'], authorid)
    assert1("search result", 1, r)
 




    

