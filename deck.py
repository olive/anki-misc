# coding: utf-8

import sqlite3
import sys
import json
import re


def get_kanji(s):
    matches = ""
    # \u3000-\u303f punctuation
    # \u3040-\u309f hiragana
    # \u30a0-\u30ff katakana
    # \uff00-\uff9f fullwidth
    for match in re.findall("[\u4e00-\u9faf\u3400-\u4dbf]", str(s)):
        matches += match
    d = dict()
    res = []
    for c in list(matches):
        if c not in d:
            res.append(c)
        d[c] = 1
    return res

def get_decks(match, c):
    s = None
    for row in c.execute("select decks from col"):
        s = row[0]

    if not s:
        print("failed to get decks")
        exit(1)

    data = json.loads(s)
    names = []
    for d in data:
        name = data[d]['name']
        if match in name:
            #print(d, name)
            names.append(d)
    return names


def get_cards(c, names):
    ss = ",".join(names)

    matches=""
    p = ""
    for row in c.execute("select flds,sfld from cards as c join notes as n on n.id = c.nid where c.did in ("+ss+")"):
        p += str(row)
    return p

def diff(c, names, source):
    p = get_cards(c, names)
    known = get_kanji(p)

    print(str(len(known)) + " known kanji")

    sss = ""
    if source == 'stdin':
        for line in sys.stdin:
            sss += line
    elif "-d=" in source:
        src = source[3:]
        sss = get_cards(c, get_decks(src, c))
    else:
        lines = [ line for line in open(source) ]
        sss = "".join(lines)


    found = get_kanji(sss)

    needed = []
    for k in found:
        if k not in known:
            needed.append(k)
    print("".join(needed))
    return needed


def main():
    dbfile = sys.argv[1]
    kanji = sys.argv[2]
    source = sys.argv[3]
    c = sqlite3.connect(dbfile)
    targets = get_decks(kanji, c)
    diff(c, targets, source)

if __name__ == '__main__':
    main()
