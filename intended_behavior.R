lines <- readLines("stdin")
regexp <- commandArgs(trailingOnly = TRUE)[[1L]]
get_ids <- function(lines, regexp){
    matches <- regexpr(text = lines, pattern = regexp, perl = TRUE, useBytes = FALSE)
    ids <- substr(lines, start = matches, stop = attr(matches, "match.length"))
    unique(ids)
}
ids <- get_ids(lines, regexp)

# give a color with good contrast after being provided
# an integer between 1 and 256.
determine_appropriate_terminal_color <- function(int) {
    if(int == 1L) {
        # terminal color 15 does not stand out enough--supply a greyscale color
        # instead
        243L
    } else if(int == 9L){
        # terminal color 7 similarly does not stand out enough
        250L
    } else if(int <= 14L){
        # exhaust the high intensity colors first,
        # then exhaust the standard colors
        15L - int + 1L
    } else if(int >= 232L) {
        # not going to support greyscale, so we are now
        # out of options--reuse colors starting with 1
        determine_appropriate_terminal_color(256L - int)
    } else {
        # choose the 'oppposite' color in the table
        231L - int + 16L
    }
}

output_lines <- lines
for(id_num in seq_along(ids)){
    assigned_color <- determine_appropriate_terminal_color(id_num)
    id <- ids[[id_num]]
    # \x1B[38;5; indicates we are changing the terminal foreground
    # the number immediately following this indicates the color
    #   out of the 256 terminal color choices
    # the 'm' ends the escape sequence
    # %s is the actual text
    # \x1b[0m is what resets the state of the terminal colors
    colorized_replacement <- sprintf("\x1b[38;5;%dm%s\x1b[0m",
                                     assigned_color,
                                     id)
    output_lines <- sub(x = output_lines,
                        pattern = id,
                        fixed = TRUE,
                        replacement = colorized_replacement)
}
tryCatch(expr = cat(output_lines, sep = "\n"),
         error = function(cnd){
             cat("\x1b[0m")
             stop(cnd)
         })

# read a line
# extract the pattern
# associate with an ID (eventually, this will correspond to a color)
#   if no ID is found, print the line without highlights
# insert the necessary steps to highlight the word
# print the line with the ID highlighted (this should have error handling)
