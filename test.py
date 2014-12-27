import os
import subprocess
from itertools import zip_longest

directory = './tests'
parser = './glsl-parser'

def main():
    for root, dirs, files in os.walk(directory if directory.endswith('/') else directory + '/'):
        for file in files:
            if file.endswith('.glsl'):
                name = os.path.splitext(file)[0]
                base = root + name
                if not os.path.isfile(base + '.glsl'):
                    print('failed to find source file for `%s\'' % name)
                    continue
                if not os.path.isfile(base + '.test') and not os.path.islink(base + '.test'):
                    print('failed to find test file for `%s\'' % name)
                    continue
                with open(base + '.test') as test:
                    errors = []
                    process = subprocess.Popen([parser, base + '.glsl'], stdout = subprocess.PIPE)
                    for line1, line2 in zip_longest(test, process.stdout):
                        expect = line1.rstrip().lstrip() if line1 else '<nothing>'
                        got = line2.decode('utf-8').rstrip().lstrip() if line2 else '<nothing>'
                        if expect != got:
                            errors.append("expected `%s' got `%s'" % (expect, got))
                    print('%s: %s' % (name, 'failed' if len(errors) else 'passed'))
                    for error in errors:
                        print('    %s' % error)


if __name__ == "__main__":
    main()
