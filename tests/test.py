from colorprint import colorprint as cprint 
import subprocess
import os

overhead = "let prInt x = print_int x;print_newline(); x;;"

def test(file):
    with open(f"tests/{file}", "r") as f:
        with open("tests/temp.ml", "w") as tf:
            tf.write(overhead+f.read())
    with open("tests/temp.ml", "r") as f:
        caml_result = subprocess.run(
            ["ocaml", "tests/temp.ml"],
            capture_output=True,
            text=True,
            check=True
        )
    with open(f"tests/{file}", "r") as f:
        fouine_result = subprocess.run(
            ["./_build/default/bin/fouine.exe"],
            stdin=f,
            capture_output=True,
            text=True,
            check=True
        )
    # TODO : better handle when error (relaunch fouine in debug mode ?)
    if fouine_result.stderr != caml_result.stderr:
        cprint(f"Error not conform in {file}, espected {caml_result.stderr}, got {fouine_result.stderr}", "red")
    if fouine_result.stdout != caml_result.stdout:
        cprint(f"Output not conform in {file}, espected {caml_result.stdout}, got {fouine_result.stdout}", "red")
    if fouine_result.stdout == caml_result.stdout and fouine_result.stderr == caml_result.stderr:
        cprint(f"Test {file} passed", "green")

class File:
    def __init__(self, name, expected_output=b'', expected_error=b''):
        self.name = f"tests/{name}.ml"
        self.expected_output  = expected_output
        self.expected_error   = expected_error

ml_files = [
    f for f in os.listdir("tests")
    if f.endswith(".ml") and f != "temp.ml"
]

for file in ml_files:
    test(file)
