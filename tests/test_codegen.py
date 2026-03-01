from colorprint import colorprint as cprint 
import subprocess
import os

def test(file):
    err = False
    with open(file, "r") as f:
        fouine_rewrite = subprocess.run(
            ["./_build/default/bin/fouine.exe", "-showsrc"],
            stdin=f,
            capture_output=True,
            text=True,
            check=True
        )
        with open("tests/temp.ml", "w") as tf:
            tf.write(fouine_rewrite.stdout)
    try:
        with open("tests/temp.ml", "r") as f:
            fouine_rewrite_result = subprocess.run(
                ["./_build/default/bin/fouine.exe"],
                stdin=f,
                capture_output=True,
                text=True,
                check=True
            )
    except subprocess.CalledProcessError as e:
        err = True
    try:
        with open(file, "r") as f:
            fouine_result = subprocess.run(
                ["./_build/default/bin/fouine.exe"],
                stdin=f,
                capture_output=True,
                text=True,
                check=True
            )
    except subprocess.CalledProcessError as e:
        err = True
    # TODO : better handle when error (relaunch fouine in debug mode ?)
    try:
        if err or fouine_result.stderr != fouine_rewrite_result.stderr:
            cprint(f"Error not conform in {file}", "red")
            cprint(f"Showsrc : {fouine_rewrite_result.stderr}", "red")
            cprint(f"Fouine  : {fouine_result.stderr}", "red")
        if err or fouine_result.stdout != fouine_rewrite_result.stdout:
            cprint(f"Output not conform in {file}", "red")
            cprint(f"Showsrc : {fouine_rewrite_result.stdout}", "red")
            cprint(f"Fouine  : {fouine_result.stdout}", "red")
        if not err and fouine_result.stdout == fouine_rewrite_result.stdout and fouine_result.stderr == fouine_rewrite_result.stderr:
            cprint(f"Test {file} passed", "green")
            return True
    except:
        cprint(f"Error in {file}", "red")
    return False

cprint("Building Project", "green")
try:
    with open("tests/temp.ml", "r") as f:
        caml_result = subprocess.run(
            ["dune", "build"],
        )
except subprocess.CalledProcessError as e:
    cprint("Error building project", "red")
cprint("Project successfuly build", "green")
cprint("")

ml_files = [
    os.path.join(root, f)
    for root, dirs, files in os.walk("tests")
    for f in files
    if f.endswith(".ml")
    and f != "temp.ml"
    and "ShouldFail" not in root.split(os.sep)
]

nb_test = len(ml_files)

total = 0
for file in ml_files:
    total += test(file)

print()
if total == nb_test:
    cprint(f"All {total} test passed !!", "green")
else:
    cprint(f"Some tests didnt pass : ({total}/{nb_test} correct).", "red")