import sys
import codecs
import json
import hashlib


def print_hash(obj):
    data = json.dumps(obj, separators=(',', ':'))
    hasher = hashlib.md5()
    hasher.update(bytes(data, 'utf-8'))
    print(hasher.hexdigest())


def main():
    file_name = 'sample' if len(sys.argv) < 2 else sys.argv[1]
    n = 3 if len(sys.argv) < 3 else int(sys.argv[2])
    with codecs.open(f'{file_name}.json', 'r', 'utf-8-sig') as fp:
        json_str = fp.read()
    obj = json.loads(json_str)
    print_hash(obj)
    array = [json.loads(json_str) for i in range(0, n)]
    print_hash(array)


if __name__ == '__main__':
    main()
