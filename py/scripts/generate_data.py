#!/usr/bin/env python3
"""
Generate sample JSON data for Haskell Tutorial 2.
Outputs a list of people (name, age) pairs.
"""

import json
import sys

data = [
    {"name": "Ada Lovelace", "age": 36},
    {"name": "Alan Turing", "age": 41},
    {"name": "Grace Hopper", "age": 85},
    {"name": "Edsger Dijkstra", "age": 72},
    {"name": "Hedy Lamarr", "age": 96},
]

# Print as JSON
print(json.dumps(data))
