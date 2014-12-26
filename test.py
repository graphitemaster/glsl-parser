import os
import subprocess

directory = './tests'
parser = './glsl-parser'

def main():
    for root, dirs, files in os.walk(directory if directory.endswith('/') else directory + '/'):
        for file in files:
            if file.endswith('.glsl'):
                name = os.path.splitext(file)[0]
                base = root + name
                if not os.path.isfile(base + '.glsl'):
                    print('failed to find source file for `%s\'' % (name))
                    continue
                if not os.path.isfile(base + '.test') and not os.path.islink(base + '.test'):
                    print('failed to find test file for `%s\'' % (name))
                    continue
                with open(base + '.test') as test:
                    error = False
                    process = subprocess.Popen([parser, base + '.glsl'], stdout = subprocess.PIPE)
                    if len(test.readlines()) == len(process.stdout.readlines()):
                        for line1, line2 in zip(test, process.stdout):
                            expect = line1.rstrip().lstrip()
                            got = line2.decode('utf-8').rstrip().lstrip()
                            if expect != got:
                                error = True
                                print("%s: expected `%s' got `%s'" % (name, expect, got))
                    else:
                        error = True
                    print('%s: %s' % (name, 'failed' if error else 'passed'))

if __name__ == "__main__":
    main()
