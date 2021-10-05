zstyle :compinstall filename '/home/hexular/.zshrc'

autoload -Uz compinit
compinit
autoload -Uz bashcompinit
bashcompinit

setopt interactive_comments

HISTSIZE=500000
HISTFILESIZE=500000
SAVEHIST=500000
HISTFILE=~/.zsh_history
export MANPAGER='nvim +Man!'
export MANWIDTH=80
export EDITOR=nvim
export VISUAL=nvim

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias NR='awk '"'"'{ print NR " " $0 }'"'"
alias ip='ip -color=auto'
alias rs-clippy='cargo clippy --all-targets --all-features'
alias rs-fmt='cargo fmt'
# make macOS and Linux equivalent
if ! command -V pbcopy &>/dev/null; then
    alias pbcopy='xclip -selection c'
    alias pbpaste='xclip -selection c -o'
    alias pbimgpaste='xclip -selection c -t image/png -o'
fi
if ! command -V open &>/dev/null; then
    # on macOS `open` is automatically defined
    alias open='xdg-open'
fi

setopt HIST_IGNORE_SPACE

HOSTNAME="$(hostname)"
case "$HOSTNAME" in
    hexagon)
        HOST_COLOR="red"
        ;;
    hexamac)
        HOST_COLOR="green"
        ;;
esac

if [ ! -z "$HOST_COLOR" ]; then
    PROMPT="<%{%F{$HOST_COLOR}%}%M%{%f%}:%~>%# "
else
    PROMPT="<%M:%~>%# "
fi

bindkey -e
