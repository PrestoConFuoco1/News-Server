#!/bin/bash

# to make sure that everything is OK, run two tests:
# 1) ./testall | grep -v admin | grep False
# 2) ./testall | grep Internal

# both commands should produce no input

echo "testing authors"
./authors.py #| grep False
echo "testing categories"
./categories.py #| grep False
echo "testing comments"
./comments.py #| grep False
#echo "testing drafts"
#./drafts.py   #| grep False
echo "testing posts"
./posts.py    #| grep False
echo "testing tags"
./tags.py     #| grep False
echo "testing users"
./users.py    #| grep False
