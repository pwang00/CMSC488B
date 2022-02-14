import subprocess
import json
from gradescope_utils.autograder_utils.json_test_runner import JSONTestRunner

TESTS = [ 10, 18, 15, 15, 7 ]
TEST_WEIGHTS = [ 20, 20, 20, 20, 20]

def stack_run_test(json_data):
    cmd = ["stack", "run"]
    res = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    # no stdout, output in stderr
    output = res.stderr.decode("utf-8")

    ex_group = 0
    for line in output.split("\n"):
#        print (line, line.split())
#        print (line, line.startswith('Case'))
        if line.startswith('Case'):
            # Cases: <c> Tried: <t> Errors: <e> Failures: <f>
            words = line.split()
            c = int(words[1])
            t = int(words[3])
            e = int(words[5])
            f = int(words[7])
            print (line, words, c, t, e, f)

            assert (c == TESTS[ex_group])
            success = c - e - f
            json_data["score"] = json_data["score"] + (success / c) * TEST_WEIGHTS[ex_group]
            ex_group = ex_group + 1
            
    json_data["output"] = json_data["output"] + output
                

if __name__ == '__main__':

    json_data = {}
    json_data["tests"] = []
    # json_data["leaderboard"] = []
    json_data["visibility"] = 'visible'
    json_data["stdout_visibility"] = 'visible'

    json_data["score"] = 0
    json_data["output"] = ""

    stack_run_test(json_data)

    with open('/autograder/results/results.json', 'w') as f:
        json.dump(json_data, f, indent = 4)
        f.write('\n')
