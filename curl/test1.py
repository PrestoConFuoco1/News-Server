#!/bin/python3

import json
my_json_string = """{
   "article": [
      {
         "id":"01",
         "language": "JSON",
         "edition": "first",
         "author": "Derrick Mwiti"
      },
      {
         "id":"02",
         "language": "Python",
         "edition": "second",
         "author": "Derrick Mwiti"
      }
   ],
   "blog":[
   {
       "name": "Datacamp",
       "URL":"datacamp.com"
   }
   ]
}
"""
to_python = json.loads(my_json_string)
blog = to_python['blog'][0]
print(blog)
name = blog['name']
url = blog['URL']
print(name)
print(url)

