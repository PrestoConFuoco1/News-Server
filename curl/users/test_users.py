#!/bin/python3

import json
import subprocess
import sys

basicArgs = ['curl', '-G', '-d']
login = 'login='
token = 'token='
user_id = 'user_id='
pass_hash = 'pass_hash='
firstname = 'firstname='
lastname = 'lastname='
host = 'localhost:5555/users/'
authhost = 'localhost:5555/auth'

def run(args):
    print(args)
    process = subprocess.run(args, capture_output=True, text=True)
    response = json.loads(process.stdout)
    return response
def get(token_):
    res = run(basicArgs + [token + token_, host + 'profile'])
    return res
def create(login_, pass_hash_, firstname_, lastname_):
    res = run(basicArgs + [login + login_ + '&' +
                           pass_hash + pass_hash_ + '&' +
                           firstname + firstname_ + '&' +
                           lastname + lastname_, host+'create'])
    return res
def delete(token_, user_id_):
    res = run(basicArgs + [token + token_ + '&' + user_id + user_id_, host+'delete'])
    return res

def auth(login_, pass_hash_):
    res = run(basicArgs + [login + login_ + '&' + pass_hash + pass_hash_, authhost])
    return res


login1 = 'jojo666'
firstname1 = 'jojo'
lastname2 = 'bean'
fl = firstname1 + ' ' + lastname2

if (len(sys.argv) == 3):
    print('trying to authenticate with incorrect login')
    res = auth('sdadasdsdasda', '123')
    print(res, '\n')
    



elif len(sys.argv) < 2:
    print ('trying to create user with incorrect login')
    res = create('', '123', firstname1, lastname2)
    print(res, '\n')

    print ('creating user ' + fl)
    res = create(login1, '123', firstname1, lastname2)
    print(res, '\n')

    print('trying to authenticate with empty login')
    res = auth('', '123')
    print(res, '\n')

    print('trying to authenticate with empty password')
    res = auth(login1, '')
    print(res, '\n')



    print('trying to authenticate with incorrect login')
    res = auth('sdadasdsdasda', '123')
    print(res, '\n')

    print('trying to authenticate with incorrect password')
    res = auth(login1, '124')
    print(res, '\n')

    print('authentication with user ' + fl)
    res = auth(login1, '123')
    print(res, '\n')
    token1 = res['result']
    print('token is ' + token1)

    print('trying to get profile with incorrect (empty) token')
    res = get('')
    print(res, '\n')

    print('trying to get profile with incorrect  token')
    res = get('asdasdasdsa')
    print(res, '\n')

    print('getting proper profile')
    res = get(token1)
    print(res, '\n')

else :
    arg = sys.argv[1]

    print('authentication with user ' + fl)
    res = auth(login1, '123')
    print(res, '\n')
    token1 = res['result']
    print('token is ' + token1)


    print('trying to delete user not being admin')
    res = delete('fail', arg)
    print(res, '\n')

    print('trying to delete user not being admin')
    res = delete(token1, arg)
    print(res, '\n')

    print('trying to delete non existing user with admin')
    res = delete('admin', '666666')
    print(res, '\n')

    print('incorrect type of user id')
    res = delete(token1, 'hello')
    print(res, '\n')

    print('deleting user with admin token')
    res = delete('admin', arg)
    print(res, '\n')

    print('trying to delete previously deleted user again with admin token')
    res = delete('admin', arg)
    print(res, '\n')

   



