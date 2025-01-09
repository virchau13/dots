let keys = {
    hexagon = "age12zlz6lvcdk6eqaewfylg35w0syh58sm7gh53q5vvn7hd7c6nngyseftjxl";
    hexgoer = "age16ldycczfl4ne520ar8kx56t00u55s8gq69dy0q72hh77jl3qgu7ql9a8wc";
}; in {
    creation_rules = (map (name: {
        path_regex = "hosts/${name}/secrets.yaml$";
        key_groups = [{
            age = [ keys."${name}" ];
        }];
    }) (builtins.attrNames keys)) ++ [{
        path_regex = "hosts/common/secrets.yaml$";
        key_groups = [{
            age = builtins.attrValues keys;
        }];
    }];
}
