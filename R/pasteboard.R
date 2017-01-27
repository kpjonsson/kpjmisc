pasteboard = function(delim = '\t') {
    read.csv(pipe('pbpaste'), sep = delim)
}
