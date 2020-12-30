export PATH=/Applications/Emacs.app/Contents/MacOS/:/Applications/Emacs.app/Contents/MacOS/bin:${PATH}
export PATH=${HOME}/.local/bin:${PATH}
export PATH=${HOME}/.idris2/bin:${PATH}
export PATH="/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH"
export PATH="/Applications/Racket v7.4/bin:$PATH"
export PATH="$PATH:/Users/arnaud/.dat/releases/dat-13.13.1-macos-x64"
export GOPATH=${HOME}/go
alias ec="emacsclient -c"

# from https://stackoverflow.com/questions/18880024/start-ssh-agent-on-login
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
    echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    #ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"


export PATH="$HOME/.cargo/bin:$PATH"
. ~/.bashrc

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/arnaud/Downloads/google-cloud-sdk/path.bash.inc' ]; then . '/Users/arnaud/Downloads/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/arnaud/Downloads/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/arnaud/Downloads/google-cloud-sdk/completion.bash.inc'; fi
