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



if True:
    tname = 'Kafka'
    if len(sys.argv) == 2:
        tname = sys.argv[1]

    print ('getting tags (admin is not required here)')
    res = get('fail')
    disp(res, True)
    print('')


    print ("trying to create a tag with wrong token")
    res = create('asdasdad', tname+'1')
    disp(res, False)
    print('')


    print("trying to create tag not being admin")
    res = create('push', tname+'2')
    disp(res, False)
    print('')


    print("trying to create a tag but with invalid name (not admin)")
    res = create('fail', '')
    disp(res, False)
    print('')



    print("trying to create a tag with admin but with invalid name")
    res = create('admin', '')
    disp(res, False)
    print('')


    print("creating tag with admin")
    res = create('admin', tname+'3')
    disp(res, True)
    print('')


    #arg = sys.argv[1]
    arg = str(res['result'])
    print("trying to edit tag with wrong token")
    res = edit('sasdads', arg, tname+'4')
    disp(res, False)
    print('')


    print("trying to edit tag with right token but not admin")
    res = edit('push', arg, tname+'5')
    disp(res, False)
    print('')


    print("trying to edit tag with right token (not admin) with wrong parameter")
    res = edit('push', arg, '')
    disp(res, False)
    print('')


  
    print("trying to edit tag with admin token and wrong parameter")
    res = edit('admin', arg, '')
    disp(res, False)
    print('')


    print("editing tag with admin")
    res = edit('admin', arg, tname+'6')
    disp(res, True)
    print('')


    print("deleting tag with not admin")
    res = delete('push', arg)
    disp(res, False)
    print('')


    print("deleting tag with admin")
    res = delete('admin', arg)
    disp(res, True)
    print('')


