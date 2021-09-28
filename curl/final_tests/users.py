#!/bin/python3

import json
import subprocess
import sys
import random
import string

# usage: ./test_users.py <optional user login> | grep -v admin | grep False
# (should produce no input)

def randomstring(length):
   letters = string.ascii_lowercase
   return ''.join(random.choice(letters) for i in range(length))



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
    print(process.stdout)
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
   

def disp(res, sh):
    q = res['message']
    print('message: ', q, end=' ')
    ok_ = res['_ok']
    print(ok_ == sh)
    if(ok_) :
        print(res['result'])
    print('')

def assertuser(user_, login_, firstname_, lastname_, user_id_):
    assert1("login", login_, user_['login'])
    assert1("firstName", firstname_, user_['firstname'])
    assert1("lastName", lastname_, user_['lastname'])
    assert1("user_id", user_id_, user_['id'])

 
def assert1(name, whatshouldbe, whatis):
    print(name, " is ", whatis, ", should be: ", whatshouldbe, end=' ')
    print(whatis == whatshouldbe)



if (len(sys.argv) == 3):
    print('trying to authenticate with incorrect login')
    res = auth('sdadasdsdasda', '123')
    print(res, '\n')
 
login1 = randomstring(10)
if (len(sys.argv) == 2):
    login1 = sys.argv[1]

if True:
    print ('trying to create user with incorrect login')
    res = create('', '123', firstname1, lastname2)
    disp(res, False)
    

    password = '123'
    print ('creating user ' + fl)
    res = create(login1, password, firstname1, lastname2)
    disp(res, True)
    
    user_id_ = res['result']


    print('trying to authenticate with empty login')
    res = auth('', '123')
    disp(res, False)
    



    print('trying to authenticate with empty password')
    res = auth(login1, '')
    disp(res, False)
    





    print('trying to authenticate with incorrect login')
    res = auth('sdadasdsdasda', '123')
    disp(res, False)
    



    print('trying to authenticate with incorrect password')
    res = auth(login1, '124')
    disp(res, False)
    



    print('authentication with user ' + fl)
    res = auth(login1, '123')
    disp(res, True)
    


    token1 = res['result']
    print('token is ' + token1)
    disp(res, True)
    


    print('trying to get profile with incorrect (empty) token')
    res = get('')
    disp(res, False)
    



    print('trying to get profile with incorrect  token')
    res = get('asdasdasdsa')
    disp(res, False)
    



    print('getting proper profile')
    res = get(token1)
    disp(res, True)
    


    #res = create(login1, password, firstname1, lastname2)
    assertuser(res['result'], login1, firstname1, lastname2, user_id_)

# else :
#    arg = sys.argv[1]

    print('authentication with user ' + fl)
    res = auth(login1, '123')
    disp(res, True)
    


    token1 = res['result']
    print('token is ' + token1)
    



    print('trying to delete user not being admin')
    res = delete('fail', str(user_id_))
    disp(res, False)
    



    print('trying to delete user not being admin')
    res = delete(token1, str(user_id_))
    disp(res, False)
    



    print('trying to delete non existing user with admin')
    res = delete('admin', '666666')
    disp(res, False)
    



    print('incorrect type of user id')
    res = delete('admin', 'hello')
    disp(res, False)
    



    print('deleting user with admin token')
    res = delete('admin', str(user_id_))
    disp(res, True)
    


   # check

    print('trying to delete previously deleted user again with admin token')
    res = delete('admin', str(user_id_))
    disp(res, False)
    



   



