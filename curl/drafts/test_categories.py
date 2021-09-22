#!/bin/python3

import json
import subprocess
import sys

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


if len(sys.argv) < 2:

    print ('getting categories (admin is not required here)')
    res = get('fail')
    print(res, '\n')

    print ("trying to create a category with wrong token")
    res = create('asdasdad', 'Python1', '2')
    print(res, '\n')

    print("trying to create category not being admin")
#./create_author.sh push 2 "Pushkin2"
    res = create('push', 'Python2', '2')
    print(res, '\n')

    print("trying to create a category with admin but with invalid parent_id")
#./create_author.sh admin 666 "Pushkin3"
    res = create('push', 'Python3', '666')
    print(res, '\n')

    print("trying to create a category but with invalid name (not admin)")
#./create_author.sh admin 2 ""
    res = create('fail', '', '2')
    print(res, '\n')


    print("trying to create a category with admin but with invalid name")
#./create_author.sh admin 2 ""
    res = create('admin', '', '2')
    print(res, '\n')

    print("creating category with admin")
#./create_author.sh admin 2 "Pushkin4"
    res = create('admin', 'Python4', '2')
    print(res, '\n')

else:
    arg = sys.argv[1]
    print("trying to edit category with wrong token")
    res = edit('sasdads', arg, '1', 'Python4')
    print(res, '\n')

    print("trying to edit category with right token but not admin")
    res = edit('push', arg, '1', 'Python5')
    print(res, '\n')

    print("trying to edit category with right token (not admin) with wrong parameter")
    res = edit('push', arg, '1', '')
    print(res, '\n')

  
    print("trying to edit category with admin token and wrong parameter")
    res = edit('admin', arg, '1', '')
    print(res, '\n')

    print("editing category with admin")
    res = edit('admin', arg, '1', 'Python6')
    print(res, '\n')

    print("deleting category with not admin")
    res = delete('push', arg)
    print(res, '\n')

    print("deleting category with not admin")
    res = delete('admin', arg)
    print(res, '\n')
