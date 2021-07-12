import sys
import codecs
import json
import hashlib


def main():
    file_name = 'sample' if len(sys.argv) < 2 else sys.argv[1]
    n = 3 if len(sys.argv) < 3 else int(sys.argv[2])
    with codecs.open(f'{file_name}.json', 'r', 'utf-8-sig') as fp:
        json_str = fp.read()
    for i in range(1, n+1):
        obj = json.loads(json_str)
        prettified = json.dumps(obj, indent=i)
        hasher = hashlib.md5()
        hasher.update(bytes(prettified, 'utf-8'))
        print(hasher.hexdigest())


if __name__ == '__main__':
    main()
