# done by a LLM for testing purposes
#!/usr/bin/env python3
import argparse
from pathlib import Path

OP = {">": 1, "<": 2, "+": 3, "-": 4, ".": 5, ",": 6, "[": 7, "]": 8}
VALID = set(OP.keys())

RUNBF_PRELUDE = r"""
let fix = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v)) in let pair = fun a -> fun b -> fun k -> k a b in let fst = fun p -> p (fun a -> fun b -> a) in let snd = fun p -> p (fun a -> fun b -> b) in let mk = fun t -> fun p -> fun ip -> pair t (pair p ip) in let tape0 = fun i -> 0 in let get = fun t -> fun p -> t p in let set = fun t -> fun p -> fun v -> fun i -> if i = p then v else t i in let runbf = fun prog -> let seek_fwd = fix (fun self -> fun ip -> fun depth -> let op = prog ip in if op = 0 then ip else if op = 7 then self (ip+1) (depth+1) else if op = 8 then if depth = 0 then ip else self (ip+1) (depth-1) else self (ip+1) depth) in let seek_bwd = fix (fun self -> fun ip -> fun depth -> let op = prog ip in if op = 0 then ip else if op = 8 then self (ip-1) (depth+1) else if op = 7 then if depth = 0 then ip else self (ip-1) (depth-1) else self (ip-1) depth) in let step = fix (fun self -> fun state -> let t = fst state in let p = fst (snd state) in let ip = snd (snd state) in let op = prog ip in if op = 0 then state else if op = 1 then self (mk t (p+1) (ip+1)) else if op = 2 then self (mk t (p-1) (ip+1)) else if op = 3 then let cell = get t p in let t2 = set t p (cell+1) in self (mk t2 p (ip+1)) else if op = 4 then let cell = get t p in let t2 = set t p (cell-1) in self (mk t2 p (ip+1)) else if op = 5 then let cell = get t p in let _ = prInt cell in self (mk t p (ip+1)) else if op = 6 then let t2 = set t p 0 in self (mk t2 p (ip+1)) else if op = 7 then let cell = get t p in if cell = 0 then let j = seek_fwd (ip+1) 0 in self (mk t p (j+1)) else self (mk t p (ip+1)) else if op = 8 then let cell = get t p in if cell = 0 then self (mk t p (ip+1)) else let j = seek_bwd (ip-1) 0 in self (mk t p (j+1)) else self (mk t p (ip+1))) in step (mk tape0 0 0) in   
"""

def compress_one_line(s: str) -> str:
    # ton parser veut une seule ligne (EOL final), donc on écrase les retours ligne
    return " ".join(s.split())

def bf_to_prog_fun(bf_src: str) -> str:
    cmds = [c for c in bf_src if c in VALID]
    parts = []
    for i, c in enumerate(cmds):
        parts.append(f"if i = {i} then {OP[c]} else")
    parts.append("0")
    return f"let prog = fun i -> {' '.join(parts)}"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("bf", nargs="?", help="programme brainfuck en string")
    ap.add_argument("--bf-file", type=Path, help="fichier contenant le brainfuck")
    ap.add_argument("--standalone", action="store_true",
                    help="émet tout: runbf + prog + runbf prog (une seule ligne)")
    ap.add_argument("--out", type=Path, help="écrire dans un fichier (sinon stdout)")
    args = ap.parse_args()

    if args.bf_file:
        bf_src = args.bf_file.read_text(encoding="utf-8")
    elif args.bf is not None:
        bf_src = args.bf
    else:
        raise SystemExit("Donne un brainfuck en argument ou via --bf-file")

    prog_def = bf_to_prog_fun(bf_src)

    if args.standalone:
        full = f"{compress_one_line(RUNBF_PRELUDE)} {prog_def} in runbf prog"
        out = compress_one_line(full) + "\n"
    else:
        # juste la fonction prog (à coller après ton runbf)
        out = prog_def + "\n"

    if args.out:
        args.out.write_text(out, encoding="utf-8")
    else:
        print(out, end="")

if __name__ == "__main__":
    main()
