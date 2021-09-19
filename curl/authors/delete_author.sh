#!/bin/bash
curl -v -G -d "token=admin&author_id=$1" localhost:5555/authors/delete
echo ""
