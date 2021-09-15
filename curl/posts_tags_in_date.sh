#!/bin/bash
curl -G -d "tags__in=[4,7]&created_at__lt=2021-09-10" localhost:5555/posts/get
echo ""
