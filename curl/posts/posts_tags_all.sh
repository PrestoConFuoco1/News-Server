#!/bin/bash
curl -G -d "tags__all=[$1,$2]" localhost:5555/posts/get
echo ""
