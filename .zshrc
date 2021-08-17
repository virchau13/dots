# export ZSH="/usr/share/oh-my-zsh"

# plugins=(
#     git
#     adb
#     archlinux
#     cargo
#     colored-man-pages
#     cp
#     python
#     rust
#     rustup
#     yarn
# )

zstyle :compinstall filename '/home/hexular/.zshrc'

autoload -Uz compinit
compinit
autoload -Uz bashcompinit
bashcompinit

setopt interactive_comments

. ~/.env-vars

# ZSH_THEME=gentoo # fishy # essembeh

# source $ZSH/oh-my-zsh.sh

. ~/.aliases

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

HISTFILE=~/.zsh_history
bindkey -e
if [ -e /home/hexular/.nix-profile/etc/profile.d/nix.sh ]; then . /home/hexular/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
