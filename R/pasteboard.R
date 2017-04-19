pasteboard = function(delim = '\t', header = T) {
    read.csv(pipe('pbpaste'), sep = delim, header)
}
