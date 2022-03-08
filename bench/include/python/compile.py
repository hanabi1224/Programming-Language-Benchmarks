from py_compile import compile
import os.path


def main():
    for root, dirs, files in os.walk("out"):
        for name in files:
            if name[-3:] == '.py':
                full_path = os.path.join(root, name)
                compile(full_path)

    compile(os.path.join('out', 'app.py'),
            cfile=os.path.join('out', 'app.pyc'))


if __name__ == '__main__':
    main()
