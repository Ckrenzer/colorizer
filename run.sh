# this R script shows the desired behavior.
#
# it assumes one match per line (the first match)
# it allows the user to supply the text and a regex, and the colorization occurs automatically.
# it has error-handling to ensure the terminal colors are reset should an error occur.
#
# the internals and interface could certainly be improved:
# ideally, the final argument would be a file path or stream rather than
# cat-ed input.
# this implementation stores the entire stream in RAM and also makes a copy
# of it (2 copies of the complete input!). that is obviously bad, but it
# made for easier prototyping.
# it should, but does not, handle lines where a regex match is not found.
cat input.txt | Rscript intended_behavior.R "[a-z]{2}[0-9]{2}-[a-z][0-9]"
printf "\n"
cat input2.txt | Rscript intended_behavior.R "[a-z]{2}[0-9]{2}-[a-z][0-9]"

printf "\n\n"

cat input2.txt | ./colorizer -e "[a-z]{2}[0-9]{2}-[a-z][0-9]"
printf "\n"
./colorizer -e "[a-z]{2}[0-9]{2}-[a-z][0-9]" -f input2.txt



# yikes. perhaps my algo is subpar. it hurts to know that R is doing a better job
# than my beautiful hand-rolled lisp code. I think some fundamental differences
# in implementation cause this discrepancy:
# 1. R reads the whole thing into RAM immediately, making it faster to operate on (at the cost of RAM).
#       lisp, meanwhile, needs to make hash table lookups while R can do indexing.
# 2. lisp is using a lot more tooling than just the base library. It is trying to have a nice CLI, error-handling, etc.
#
# on the bright side, lisp runs much quicker when a line does not contain the input pattern; R will
# process the entire file no matter what (it also currently errors out if a line does not have a pattern...this was
# just a prototype to get the algorithm down so I won't judge it too harshly).
time cat input3.txt | Rscript intended_behavior.R "[a-z]{2}[0-9]{2}-[a-z][0-9]" > /dev/null
time ./colorizer -e "[a-z]{2}[0-9]{2}-[a-z][0-9]" -f input3.txt > /dev/null
time grep --color -e "[a-z]{2}[0-9]{2}-[a-z][0-9]" input3.txt > /dev/null
