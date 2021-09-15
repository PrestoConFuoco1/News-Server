#!/bin/bash
curl -G -d "created_at__lt=$1" localhost:5555/posts
echo ""

