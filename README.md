# Colorizer
A grep-like utility that uses ANSI escape sequences to colorize matched text, providing a distinct color for each unique match.

# Building
This requires you to have Quicklisp installed.

Run the following command from the root of the repo, and the dynamically-linked `colorizer` executable will be produced in this directory:
```{sh}
make build
```

# Example Usage
The tool works like `grep` with Perl-compatible regular expressions:
```{sh}
printf 'qxz-35 Good morning!\nqxz-14 The sun is up!\nqxz-35 It sure is!\n' |\
    colorizer -e '^[a-z]+-[0-9]+'
```
And it accepts the `-f` flag to read a file instead piping from stdin.

# To-Do
- Create a package and build everything there instead of defining everything in cl-user.
- Use ASDF.
- Allow user input to come from (a) arbitrarily-many files and (b) both stdin and files listed on the command line. The handling of `input-source` in the `colorize` function is also quite awkward.
- Allow the use of positional parameters rather than needing to specify each flag at the command line.
- Is there a better way to handle the terminal colors than your weird math in `determine-appropriate-terminal-color`?
- Can you improve performance? It probably doesn't matter since this is meant for human consumption, but `grep` is an order of magnitude faster!
- Set up the tooling to be able to build the executable without needing the user to have things pre-installed. Docker may be helpful here.
- Can you get a static executable? That might make shipping easier.
