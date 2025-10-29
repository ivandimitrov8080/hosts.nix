local mp = require 'mp'
local utils = require 'mp.utils'
local msg = require 'mp.msg'
local u = require 'util'

local history = {}

local history_path = (os.getenv("HOME") or "") .. "/.config/mpv/dir-player-history.json"
local content = {}

local ok, data = pcall(function()
    local history_file = io.open(history_path, "a+")
    if not history_file then error("Failed to read history file.") end
    local ctnt = history_file:read("*a")
    history_file:close()
    return utils.parse_json(ctnt)
end)
if ok and type(data) == "table" then
    content = data
elseif data == nil then
    msg.info("Empty history file. Continuing...")
else
    error("Error reading history file: {OK: {" .. tostring(ok) .. "} DATA: {" .. data .. "}")
end

local function get_playlist()
    local pl = mp.get_property_native("playlist")
    local res = {}
    for _, entry in ipairs(pl) do
        if entry.filename then
            entry.filename = u.filename(entry.filename)
            table.insert(res, entry)
        end
    end
    return res
end

-- Find the most recently played file in the playlist
local function find_last_played_file()
    local idx = history.get_index()
    return get_playlist()[idx]
end

local function determine_index(filename)
    local playlist = get_playlist()
    for i, entry in ipairs(playlist) do
        if entry.filename == filename then
            return i
        end
    end
    return 0
end

local function determine_filename(index)
    local playlist = get_playlist()
    for i, entry in ipairs(playlist) do
        if i == index then
            return entry.filename
        end
    end
end

function history.update()
    local position = math.floor(mp.get_property_number("time-pos", 0))
    local duration = math.floor(mp.get_property_number("duration", 0))
    local filename = u.filename(mp.get_property("path"))
    local index = determine_index(filename)
    history.current().files[filename] = {
        position = position,
        duration = duration,
        index = index
    }
    if index ~= -1 then
        history.current().index = index
    else
        error("Error finding index for current file " .. filename)
    end
end

function history.current()
    local path = mp.get_property("path")
    local filename = u.filename(path)
    local current_dir = string.sub(path, 1, string.len(path) - string.len(filename))
    local ctnt = content[current_dir]
    if not ctnt then
        content[current_dir] = {}
        content[current_dir].index = 1
        content[current_dir].files = {}
    end
    return content[current_dir]
end

-- Save history to JSON file
function history.save()
    local history_file = io.open(history_path, "w+")
    if not history_file then error("Failed to open history_file") end
    local c = utils.format_json(content)
    history_file:write(c)
    history_file:close()
    msg.info("Saved history {" .. history_path .. "}.")
end

-- Return the last entry in watch history
function history.get_last_entry()
    local filename = find_last_played_file().filename
    return history.current().files[filename]
end

function history.get_index()
    return history.current().index
end

function history.get_entry(id)
    if type(id) == "string" then
        return history.current().files[id]
    end
    return history.current().files[determine_filename(id)]
end

return history
