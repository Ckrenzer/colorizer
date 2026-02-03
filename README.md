# Colorizer
A grep-like utility that uses ANSI escape sequences to colorize matched text, providing a distinct color for each unique match.

# Building
This requires you to have Docker installed.

Run the following command from the root of the repo, and the dynamically-linked `colorizer` executable will be produced in this directory:
```{sh}
make build
```

If you have SBCL and Quicklisp installed, you may run the following command from the root of the repo to produce the same outcome:
```{sh}
sbcl --load app.lisp
```

# Example Usage
The tool works like `grep` with Perl-compatible regular expressions:
```{sh}
printf 'qxz-35 Good morning!\nqxz-14 The sun is up!\nqxz-35 It sure is!\n' |\
    colorizer -e '^[a-z]+-[0-9]+'
```

And it accepts the `-f` flag to read a file instead piping from stdin.
```{sh}
colorizer -e '[a-z]+' -f input.txt
```

# To-Do
- Create a package and build everything there instead of defining everything in cl-user.
- Use ASDF.
- Allow user input to come from (a) arbitrarily-many files and (b) both stdin and files listed on the command line. The handling of `input-source` in the `colorize` function is also quite awkward.
- Allow the use of positional parameters rather than needing to specify each flag at the command line.
- Is there a better way to handle the terminal colors than your weird math in `determine-appropriate-terminal-color`?
- Can you improve performance? It probably doesn't matter since this is meant for human consumption, but `grep` is an order of magnitude faster! AWK *is* faster, but only some of the time. Testing has shown that AWK is faster for smaller files (since the bootup time is low) and inputs where the regex matches the majority of the time. Lisp takes 10x more time for the executable to start and is about 20% slower than AWK when there are many matches, but it outperforms AWK when there are not many matches (the more lines that skip -> performance more in favor of lisp). AWK is probably better for most real-world scenarios, but it's nice to know that this lisp version in its current state has its niche. The use of ERE in AWK rather than PCRE may be the culprit.
- Can you get a static executable? That might make shipping easier.
