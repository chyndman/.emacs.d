# Shell Setup

unsetopt LIST_BEEP

autoload -Uz add-zsh-hook
_precmd_title() { print -Pn "\e]0;%1~\a" }
add-zsh-hook precmd _precmd_title

autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats ' (%b)'
setopt PROMPT_SUBST
NEWLINE=$'\n'
PROMPT='%F{green}%n@%m%f %F{yellow}%~%f%F{cyan}${vcs_info_msg_0_}%f${NEWLINE}$ '

# Emacs

export EDITOR="emacsclient -t --alternate-editor="

alias ecn="emacsclient -n"
alias ect="emacsclient -t"
alias enw="emacs -nw"

# tmux

alias tme="tmux new -A -D -s"
alias tml="tmux ls"
tmeh() {
    tme `basename $PWD`
}

# Alacritty

export PATH="$PATH:/Applications/Alacritty.app/Contents/MacOS"
aln() {
    local opts=(--working-directory "$PWD")
    if [[ -n "$1" && -n "$2" ]]; then
        opts+=(-o "window.dimensions={columns=$1,lines=$2}")
    fi
    alacritty msg create-window "${opts[@]}" || alacritty msg create-window "${opts[@]}"
}
alias alnt="aln 120 40"
alias alnm="aln 160 40"
alias alnl="aln 240 60"
alias alnxl="aln 280 70"
alias alrt="alnt && exit"
alias alrm="alnm && exit"
alias alrl="alnl && exit"
alias alrxl="alnxl && exit"
