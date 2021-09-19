#!/bin/python3

import json
import subprocess
import sys

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


def delete(token_, author_id_):
    res = run(basicArgs + [token + token_ + '&' + author_id + author_id_ , host+'delete'])
    return res

def edit(token_, author_id_, description_):
    res = run(basicArgs + [token + token_ + '&' + author_id + author_id_ + '&' + description + description_, host+'edit'])
    return res


ids = sys.argv[1]

print ("trying to edit author without token")
#./edit_author.sh "" $1 "Pushkin5"
res = edit('', ids, 'Pushkin5')
print(res)

print ("trying to edit author with wrong token")
res = edit('sdasdasd', ids, 'Pushkin6')
print(res)
#./edit_author.sh sdadsasda $1 "Pushkin6"

print ("trying to edit author with right token but not being admin")
res = edit('push', ids, 'Pushkin7')
print(res)
#./edit_author.sh push $1 "Pushkin7"

#./edit_author.sh admin "" ""
print ("trying to edit author with all params empty")
res = edit('admin', ids, '')
print(res)

print ("editing author with admin")
res = edit('admin', ids, 'Pushkin8')
print(res)
#./edit_author.sh admin $1 "Pushkin8"

print ("deleting author with admin")
res = delete('admin', ids)
print(res)
#./delete_author.sh admin $1

