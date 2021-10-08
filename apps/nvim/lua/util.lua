function setopt(name, val)
    if val == nil then
        val = true
    end
    vim.o[name] = val
end

function getopt(name)
    return vim.o[name]
end

function catopt(name, val)
    setopt(name, getopt(name) .. val)
end

function setgvar(name, val)
    vim.api.nvim_set_var(name, val)
end

-- Merges ...dictionaries? objects? tables?
function merge(a, b)
    for k, v in pairs(b) do
        a[k] = v
    end
end
