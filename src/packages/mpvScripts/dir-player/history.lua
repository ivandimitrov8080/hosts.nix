local mp = require 'mp'
local utils = require 'mp.utils'
local msg = require 'mp.msg'
local u = require 'util'
local player = require("src.packages.mpvScripts.dir-player.player")

local history = {}

local history_path = (os.getenv("HOME") or "") .. "/.config/mpv/dir-player-history.json"
local history_file = io.open(history_path, "w+")

if not history_file then
    error("Failed to open history file.")
end
local ok, data = pcall(function() return utils.parse_json(history_file:read("*a")) end)
if ok and type(data) == "table" then
    history.content = data
else
    error("Error reading history file: {OK: " .. ok .. "" .. data .. "}")
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
    return get_playlist()[history.content[player.current_directory()].index]
end

local function get_index(filename)
    local playlist = get_playlist()
    for i, entry in ipairs(playlist) do
        if entry.filename == filename then
            return i
        end
    end
    return -1
end

function history.update()
    local position = math.floor(mp.get_property_number("time-pos", 0))
    local duration = math.floor(mp.get_property_number("duration"))
    local filename = u.filename(mp.get_property("filename"))
    history.content[player.current_directory()][filename] = {
        position = position,
        duration = duration,
        timestamp = os.time()
    }
    history.content[player.current_directory()].index = get_index(filename)
end

-- Save history to JSON file
function history.save()
    local c = utils.format_json(history.content)
    history_file:write(c)
    msg.info("Saved history {" .. history_path .. "}.")
end

-- Return the last entry in watch history
function history.get_last_entry()
    local filename = find_last_played_file().filename
    return history.content[player.current_directory()][filename]
end

function history.get_index()
    return history.content[player.current_directory()].index
end

return history
