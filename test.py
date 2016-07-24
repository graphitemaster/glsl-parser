from glob import glob
import os
import subprocess
from itertools import zip_longest

def main():
    test_dir_name = 'tests'
    repo_dir = os.path.dirname(os.path.realpath(__file__))
    test_dir = os.path.join(repo_dir, test_dir_name)
    parser = os.path.join(repo_dir, 'glsl-parser')

    for name in sorted(glob(os.path.join(test_dir, '*.glsl'))):
        base = os.path.splitext(name)[0]

        if not os.path.isfile(base + '.glsl'):
            print('failed to find source file for `%s\'' % name)
            continue
        if not os.path.isfile(base + '.test') and not os.path.islink(base + '.test'):
            print('failed to find test file for `%s\'' % name)
            continue

        with open(base + '.test') as test:
            errors = []
            # Run the parser from the root directory with a relative
            # input path to the GLSL file to ensure that error output
            # paths match the expected results.
            input_path = os.path.join(test_dir_name, os.path.basename(name))
            process = subprocess.Popen([parser, input_path],
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT,
                                       cwd=repo_dir)

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
