local u = {}

function u.filename(path)
    return path:match("([^/]+)$")
end

return u
