#!/bin/python3

import json
import subprocess
import sys

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


if len(sys.argv) < 2:

    print ('getting tags (admin is not required here)')
    res = get('fail')
    print(res, '\n')

    print ("trying to create a tag with wrong token")
    res = create('asdasdad', 'Python1')
    print(res, '\n')

    print("trying to create tag not being admin")
#./create_author.sh push 2 "Pushkin2"
    res = create('push', 'Python2')
    print(res, '\n')

    print("trying to create a tag but with invalid name (not admin)")
#./create_author.sh admin 2 ""
    res = create('fail', '')
    print(res, '\n')


    print("trying to create a tag with admin but with invalid name")
#./create_author.sh admin 2 ""
    res = create('admin', '')
    print(res, '\n')

    print("creating tag with admin")
#./create_author.sh admin 2 "Pushkin4"
    res = create('admin', 'Python3')
    print(res, '\n')

else:
    arg = sys.argv[1]
    print("trying to edit tag with wrong token")
    res = edit('sasdads', arg, 'Python4')
    print(res, '\n')

    print("trying to edit tag with right token but not admin")
    res = edit('push', arg, 'Python5')
    print(res, '\n')

    print("trying to edit tag with right token (not admin) with wrong parameter")
    res = edit('push', arg, '')
    print(res, '\n')

  
    print("trying to edit tag with admin token and wrong parameter")
    res = edit('admin', arg, '')
    print(res, '\n')

    print("editing tag with admin")
    res = edit('admin', arg, 'Python6')
    print(res, '\n')

    print("deleting tag with not admin")
    res = delete('push', arg)
    print(res, '\n')

    print("deleting tag with not admin")
    res = delete('admin', arg)
    print(res, '\n')
