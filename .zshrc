[ -f "/Users/arnaud/.ghcup/env" ] && source "/Users/arnaud/.ghcup/env" # ghcup-env
export PATH=/Users/arnaud/.idris2/bin:/Users/arnaud/.local/bin:/Users/arnaud/Library/Python/3.11/bin:$PATH
export PATH=/Users/arnaud/.radicle/bin:$PATH
export PATH=/Users/arnaud/.cabal/bin:$PATH

export DENO_INSTALL="/Users/arnaud/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH=/Users/arnaud/.aiken/bin:$PATH

# opam configuration
[[ ! -r /Users/arnaud/.opam/opam-init/init.zsh ]] || source /Users/arnaud/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/arnaud/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/arnaud/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/arnaud/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/arnaud/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

# Scaleway CLI autocomplete initialization.
eval "$(scw autocomplete script shell=zsh)"

# use emacs
export EDITOR="emacsclient -nw"

# fast status
function gstatus () {
  dir="."
  if [[ ! -z "$1" ]]; then
    dir=$1
  fi
  echo "Checking git status in repositories under $1"
  for i in $dir/*; do
    pushd $i &> /dev/null
    if [[ ! -z "$(git s --porcelain --untracked-files=no 2> /dev/null)" ]]; then
       echo "\033[31munclean\033[0m: $i"
    else
       echo "\033[32mclean\033[0m: $i"
    fi
    popd &> /dev/null
  done
}
