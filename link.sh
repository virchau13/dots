# Link dotfiles (for MacOS).

cd "$(dirname "$0")"
ln -s "$(pwd)"/zshrc ~/.zshrc
cd ./config/
find . -type f -print0 | cut -z -c 3- | while IFS= read -r -d $'\0' file; do
    mkdir -p ~/.config/"$(dirname "$file")"
    ln -s "$(pwd)"/"$file" ~/.config/"$file"
done
