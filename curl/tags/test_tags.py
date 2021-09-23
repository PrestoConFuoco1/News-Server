#!/bin/python3

import json
import subprocess
import sys


# usage: ./test_tags.py <tag name> | grep False
# (should produce no input)



basicArgs = ['curl', '-G', '-d']
token = 'token='
name = 'name='
tag_id = 'tag_id='
host = 'localhost:5555/tags/'

def run(args):
    process = subprocess.run(args, capture_output=True, text=True)
    response = json.loads(process.stdout)
    return response

def get(token_):
    res = run(basicArgs + [token + token_, host + 'get'])
    return res
def create(token_, name_):
    res = run(basicArgs + [token + token_ + '&' +
                           name + name_, host + 'create'])
    return res
def edit(token_, tag_id_, name_):
    res = run(basicArgs + [token + token_ + '&' + tag_id + tag_id_ + '&' +
            name + name_, host+'edit'])
    return res
def delete(token_, tag_id_):
    res = run(basicArgs + [token + token_ + '&' + tag_id + tag_id_, host+'delete'])
    return res

def disp(res, sh):
    q = res['message']
    print('message: ', q, end=' ')
    ok_ = res['_ok']
    print(ok_ == sh)
    if(ok_) :
        print(res['result'])
    print('')


def gettagbyid(tags_, did_):
    i = 0
    for i in range(len(tags_)):
        if (tags_[i]['tagId'] == did_):
            return tags_[i]
    return 1


def asserttag(tag_, name_):
    assert1("name", name_, tag_['tagName'])
 
def assert1(name, whatshouldbe, whatis):
    print(name, " is ", whatis, ", should be: ", whatshouldbe, end=' ')
    print(whatis == whatshouldbe)




if True:
    tname = 'Kafka'
    if len(sys.argv) == 2:
        tname = sys.argv[1]

    print ('getting tags (admin is not required here)')
    res = get('fail')
    disp(res, True)


    print ("trying to create a tag with wrong token")
    res = create('asdasdad', tname+'1')
    disp(res, False)
    


    print("trying to create tag not being admin")
    res = create('push', tname+'2')
    disp(res, False)
    


    print("trying to create a tag but with invalid name (not admin)")
    res = create('fail', '')
    disp(res, False)
    



    print("trying to create a tag with admin but with invalid name")
    res = create('admin', '')
    disp(res, False)
    


    name_ = tname+'3'
    print("creating tag with admin")
    res = create('admin', name_)
    disp(res, True)
    


    #arg = sys.argv[1]
    tagid = res['result']


    print ('getting tags in order to know that there is exactly what we created')
    res = get('push')
    disp(res, True)
    tag_ = gettagbyid(res['result'], tagid)
    print(tagid)
    print(tag_)
    asserttag(tag_, name_)
    




    print("trying to edit tag with wrong token")
    res = edit('sasdads', str(tagid), tname+'4')
    disp(res, False)
    


    print("trying to edit tag with right token but not admin")
    res = edit('push', str(tagid), tname+'5')
    disp(res, False)
    


    print("trying to edit tag with right token (not admin) with wrong parameter")
    res = edit('push', str(tagid), '')
    disp(res, False)
    


  
    print("trying to edit tag with admin token and wrong parameter")
    res = edit('admin', str(tagid), '')
    disp(res, False)
    

    name_ = tname+'6'
    print("editing tag with admin")
    res = edit('admin', str(tagid), name_)
    disp(res, True)
    

    print ('getting tags in order to know that there is exactly what we created')
    res = get('push')
    disp(res, True)
    tag_ = gettagbyid(res['result'], tagid)
    asserttag(tag_, name_)
 

    print("deleting tag with not admin")
    res = delete('push', str(tagid))
    disp(res, False)
    


    print("deleting tag with admin")
    res = delete('admin', str(tagid))
    disp(res, True)
    


    print ('getting tags in order to know that we deleted')
    res = get('push')
    disp(res, True)
    r = gettagbyid(res['result'], tagid)

    assert1("search result", 1, r)
 
