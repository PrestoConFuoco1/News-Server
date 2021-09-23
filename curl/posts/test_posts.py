#!/bin/python3

import json
import subprocess
import sys


basicArgs = ['curl', '-G', '-d']
token = 'token='
name = 'name='
parent_id = 'parent_id='
category_id = 'category_id='
host = 'localhost:5555/posts/get'






def run(args):
    print('REQUEST -- ' + str(args))
    process = subprocess.run(args, capture_output=True, text=True)
    print(process.stdout)
    response = json.loads(process.stdout)
    return response
def get(token_):
    res = run(basicArgs + [token + token_
                                , host + 'get'])
    return res

