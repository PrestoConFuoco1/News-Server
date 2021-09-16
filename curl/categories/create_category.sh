#!/bin/bash
curl -G -d "token=$1&name=Scala&parent_id=5" localhost:5555/categories/create
echo ""
