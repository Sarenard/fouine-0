from colorprint import colorprint as cprint 
import subprocess

def test(file):
    with open(f"{file.name}", "r") as f:
        result = subprocess.run(
            ["./_build/default/bin/fouine.exe"],
            stdin=f,
            capture_output=True,
            text=True,
            check=True
        )
    (output, error) = (result.stdout, result.stderr)
    if error != file.expected_error:
        cprint(f"Error not conform in {file.name}, espected {file.expected_error}, got {error}", "red")
    if output != file.expected_output:
        cprint(f"Output not conform in {file.name}, espected {file.expected_output}, got {output}", "red")
    if output == file.expected_output and error == file.expected_error:
        cprint(f"Test {file.name} passed", "green")
    return (output, error)

class File:
    def __init__(self, name, expected_output=b'', expected_error=b''):
        self.name = f"tests/{name}.ml"
        self.expected_output  = expected_output
        self.expected_error   = expected_error

# TODO : compare ocaml output and normal output
# TODO : run fouine in debug mode on error
files_to_test = [
    File("if_true", "1\n", ""),
    File("if_false", "0\n", ""),
]

for file in files_to_test:
    test(file)