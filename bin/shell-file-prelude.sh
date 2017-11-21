#@/bin/bash

function shell-file-cd {
    cd "$@"
    emacsclient -a '' -e "(with-current-buffer (find-buffer-visiting shell-file-path) (setq default-directory \"$PWD/\"))"
}
