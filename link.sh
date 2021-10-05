# Link dotfiles (for MacOS).

OPT="$1"

link() {
    if [ "$OPT" == "-f" ]; then
        ln -s -f "$@"
    elif [ "$OPT" == "--dry-run" ]; then
        printf '%s => %s\n' "$@"
    else
        ln -s "$@"
    fi
}

cd "$(dirname "$0")"
link "$(pwd)"/zshrc ~/.zshrc
find ./nvim/ -type f -print0 | cut -z -c 3- | while IFS= read -r -d $'\0' file; do
    mkdir -p ~/.config/"$(dirname "$file")"
    link "$(pwd)"/"$file" ~/.config/"$file"
done
