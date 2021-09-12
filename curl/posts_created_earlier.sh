#!/bin/bash
curl -G -d "created_at__lt=2021-09-10" localhost:5555/posts
echo ""

