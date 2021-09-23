#!/bin/python3

import json
import subprocess
import sys

# usage: ./test_categories.py <category_name> | grep False
# (should produce no input)


basicArgs = ['curl', '-G', '-d']
token = 'token='
name = 'name='
parent_id = 'parent_id='
category_id = 'category_id='
host = 'localhost:5555/categories/'

def run(args):
    process = subprocess.run(args, capture_output=True, text=True)
    response = json.loads(process.stdout)
    return response
def get(token_):
    res = run(basicArgs + [token + token_, host + 'get'])
    return res
def create(token_, name_, parent_id_):
    res = run(basicArgs + [token + token_ + '&' +
                           name + name_ + '&' + parent_id + parent_id_, host+'create'])
    return res
def edit(token_, category_id_, parent_id_, name_):
    res = run(basicArgs + [token + token_ + '&' + category_id + category_id_ + '&' +
            parent_id + parent_id_ + '&' + name + name_, host+'edit'])
    return res
def delete(token_, category_id_):
    res = run(basicArgs + [token + token_ + '&' + category_id + category_id_, host+'delete'])
    return res

def disp(res, sh):
    q = res['message']
    print('message: ', q, end=' ')
    ok_ = res['_ok']
    print(ok_ == sh)
    if(ok_) :
        print(res['result'])


if True:
    cname = 'Python'
    if(len(sys.argv) == 2):
        cname = sys.argv[1]

    print ('getting categories (admin is not required here)')
    res = get('fail')
    disp(res, True)
    print('')

    print ("trying to create a category with wrong token")
    res = create('asdasdad', cname+'1', '2')
    disp(res, False)
    print('')


    print("trying to create category not being admin")
#./create_author.sh push 2 "Pushkin2"
    res = create('push', cname+'2', '2')
    disp(res, False)
    print('')


    print("trying to create a category with admin but with invalid parent_id")
#./create_author.sh admin 666 "Pushkin3"
    res = create('push', cname+'3', '666')
    disp(res, False)
    print('')


    print("trying to create a category but with invalid name (not admin)")
    res = create('fail', '', '2')
    disp(res, False)
    print('')



    print("trying to create a category with admin but with invalid name")
#./create_author.sh admin 2 ""
    res = create('admin', '', '2')
    disp(res, False)
    print('')


    print("creating category with admin")
#./create_author.sh admin 2 "Pushkin4"
    res = create('admin', cname+'4', '2')
    disp(res, True)
    print('')

    arg = str(res['result'])

   # arg = sys.argv[1]
    print("trying to edit category with wrong token")
    res = edit('sasdads', arg, '1', cname+'4')
    disp(res, False)
    print('')


    print("trying to edit category with right token but not admin")
    res = edit('push', arg, '1', cname+'5')
    disp(res, False)
    print('')


    print("trying to edit category with right token (not admin) with wrong parameter")
    res = edit('push', arg, '1', '')
    disp(res, False)
    print('')


  
    print("trying to edit category with admin token and wrong parameter")
    res = edit('admin', arg, '1', '')
    disp(res, False)
    print('')


    print("editing category with admin")
    res = edit('admin', arg, '1', cname+'6')
    disp(res, True)
    print('')


    print("deleting category with not admin")
    res = delete('push', arg)
    disp(res, False)
    print('')


    print("deleting category with not admin")
    res = delete('admin', arg)
    disp(res, True)
    print('')


