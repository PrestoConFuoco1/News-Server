#!/bin/bash
curl -G -d "created_at__gt=2021-09-10" localhost:5555/posts
echo ""

