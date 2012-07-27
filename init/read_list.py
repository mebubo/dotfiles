#!/usr/bin/env python

import sys

if __name__ == '__main__':

    filename = sys.argv[1]
    items = []
    with open(filename) as f:
        for l in f.readlines():
            item = l.split('#')[0].strip()
            if item:
                items.append(item)
    print ' '.join(items)
