#!/usr/bin/awk -f

# Usage: awk -v regexp="pattern" -f script.awk [input_file]

BEGIN {
    num_unique_ids = 0
}

function determine_appropriate_terminal_color(n) {
    # terminal color 15 does not stand out enough--supply a greyscale color
    if (n == 1)
        return 243
    # terminal color 7 similarly does not stand out enough
    if (n == 9)
        return 250
    # exhaust the high intensity colors first, then exhaust the standard colors
    if (n <= 14)
        return 1 + (15 - n)
    # not going to support greyscale, so we are now out of options--reuse
    # colors starting with 1
    if (n >= 232)
        return determine_appropriate_terminal_color(256 - n)
    # choose the 'opposite' color in the table
    if (n % 2 == 1)
        return (231 - n) + 16
    # default: return the number unchanged
    return n
}

{
    line = $0
    if (match(line, regexp)) {
        match_start = RSTART
        match_len = RLENGTH
        matched_text = substr(line, match_start, match_len)

        # determine whether matched_text has been seen before,
        # assigning it an ANSI 8-bit terminal color if not.
        if (!(matched_text in color_assignments)) {
            ++num_unique_ids
            color_assignments[matched_text] = determine_appropriate_terminal_color(num_unique_ids)
        }
        color_id = color_assignments[matched_text]

        printf "%s\033[38;5;%dm%s\033[0m%s\n", substr(line, 1, match_start - 1), color_id, matched_text, substr(line, match_start + match_len)
    }
}
