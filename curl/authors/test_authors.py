#!/bin/python3

import json
import subprocess

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

'''
def edit(token_, author_id_, user_id_, description_):
    res = run(basicArgs + [token + token_ + '&' + user_id + user_id_ + '&' + description + description_, host+'create']
    return res
'''

print ('getting authors')
res = get('admin')
print(res)
print ("trying to create author with wrong token")
res = create('asdasdad', '2', 'Pushkin1')
print(res)

print("trying to create author not being admin")
#./create_author.sh push 2 "Pushkin2"
res = create('push', '2', 'Pushkin2')
print(res)

print("trying to create author with admin but with invalid user_id")
#./create_author.sh admin 666 "Pushkin3"
res = create('admin', '666', 'Pushkin3')
print(res)

print("trying to create author with admin but with invalid description")
#./create_author.sh admin 2 ""
res = create('admin', '2', '')
print(res)

print("creating author with admin")
#./create_author.sh admin 2 "Pushkin4"
res = create('admin', '2', 'Pushkin4')
print(res)


