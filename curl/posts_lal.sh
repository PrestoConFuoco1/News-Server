#!/bin/bash
curl -G -d "" localhost:5555/posts
curl -G -d "tag=123" localhost:5555/posts
curl -G -d "tags__in=[123,124,125]" localhost:5555/posts
curl -G -d "tags__all=[123,124,125]" localhost:5555/posts
echo ""
