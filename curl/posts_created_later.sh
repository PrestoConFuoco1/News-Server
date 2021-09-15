#!/bin/bash
curl -G -d "created_at__gt=$1" localhost:5555/posts
echo ""

