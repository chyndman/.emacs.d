alias ecn="emacsclient -n"
alias ect="emacsclient -t"
alias enw="emacs -nw"

/usr/bin/which -s emacsclient && export EDITOR="$(which -s emacsclient) -t --alternate-editor="

autoload -Uz add-zsh-hook
_precmd_title() { print -Pn "\e]0;%1~\a" }
add-zsh-hook precmd _precmd_title

autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats ' (%b)'
setopt PROMPT_SUBST
NEWLINE=$'\n'
PROMPT='%F{green}%n@%m%f %F{yellow}%~%f%F{cyan}${vcs_info_msg_0_}%f${NEWLINE}$ '
