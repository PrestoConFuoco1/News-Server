#!/bin/python3

import json
import subprocess
import sys
import random
import string


# 
# usage: ./test_categories.py | grep False
# (should produce no input)

def randomstring(length):
   letters = string.ascii_lowercase
   return ''.join(random.choice(letters) for i in range(length))


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
    print('')

def getcategorybyid(categories_, did_):
    i = 0
    for i in range(len(categories_)):
        if (categories_[i]['categoryId'] == did_):
            return categories_[i]
    return 1


def assertcategory(category_, name_, parent_id_):
    print(category_)
    assert1("description", name_, category_['description'])
    if(category_['parentCategory'] == None):
        parent_id_received = None
    else:
        parent_id_received = category_['parentCategory']['categoryId']

    assert1("parent_id", int(parent_id_), parent_id_received)
 
def assert1(name, whatshouldbe, whatis):
    print(name, " is ", whatis, ", should be: ", whatshouldbe, end=' ')
    print(whatis == whatshouldbe)
 
def testgetcategories(category_id_, name_, parent_id_):
    res = get('fail')
    disp(res, True)
    c = getcategorybyid(res['result'], category_id_)
    assertcategory(c, name_, parent_id_)



if True:
#    cname = 'Python'
    cname = randomstring(10)
    if(len(sys.argv) == 2):
        cname = sys.argv[1]

    print ('getting categories (admin is not required here)')
    res = get('fail')
    disp(res, True)

    print ("trying to create a category with wrong token")
    res = create('asdasdad', cname+'1', '2')
    disp(res, False)


    print("trying to create category not being admin")
#./create_author.sh push 2 "Pushkin2"
    res = create('push', cname+'2', '2')
    disp(res, False)


    print("trying to create a category with admin but with invalid parent_id")
#./create_author.sh admin 666 "Pushkin3"
    res = create('push', cname+'3', '666')
    disp(res, False)
    


    print("trying to create a category but with invalid name (not admin)")
    res = create('fail', '', '2')
    disp(res, False)
    



    print("trying to create a category with admin but with invalid name")
#./create_author.sh admin 2 ""
    res = create('admin', '', '2')
    disp(res, False)
    


    print("creating category with admin")
#./create_author.sh admin 2 "Pushkin4"
    # token, name, parent_id
    cname_ = cname+'4'
    parent_id_ = 2
    res = create('admin', cname_, str(2))
    disp(res, True)
    
    category_id_ = res['result']

    print('getting categories to check if everythink is ok')
    testgetcategories(category_id_, cname_, parent_id_)

   # category_id_ = sys.category_id_v[1]
    print("trying to edit category with wrong token")
    res = edit('sasdads', str(category_id_), '1', cname+'4')
    disp(res, False)
    


    print("trying to edit category with right token but not admin")
    res = edit('push', str(category_id_), '1', cname+'5')
    disp(res, False)
    


    print("trying to edit category with right token (not admin) with wrong parameter")
    res = edit('push', str(category_id_), '1', '')
    disp(res, False)
    


  
    print("trying to edit category with admin token and wrong parameter")
    res = edit('admin', str(category_id_), '1', '')
    disp(res, False)
    


    print("editing category with admin")
    cname_ = cname+'6'
    parent_id_ = 2
    res = edit('admin', str(category_id_), str(parent_id_), str(cname_))
    disp(res, True)

    print("check if everything is ok")
    testgetcategories(category_id_, cname_, parent_id_)


    print("deleting category with not admin")
    res = delete('push', str(category_id_))
    disp(res, False)
    


    print("deleting category with admin")
    res = delete('admin', str(category_id_))
    disp(res, True)
    
    print ('getting categories in order to know that we indeed deleted the author')
    res = get('admin')
    disp(res, True)
    r = getcategorybyid(res['result'], category_id_)
    assert1("search result", 1, r)
 



