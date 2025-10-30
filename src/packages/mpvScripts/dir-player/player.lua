local mp = require 'mp'
local msg = require 'mp.msg'
local utils = require 'mp.utils'
local history = require 'history'
local u = require 'util'

local player = {}

local playlist_filename = "playlist.txt"

local function get_current_dir()
    local path = mp.get_property("path")
    local filename = u.filename(path)
    local current_dir = string.sub(path, 1, string.len(path) - string.len(filename))
    return current_dir
end

local function get_files()
    local files = utils.readdir(get_current_dir(), "files")
    table.sort(files)
    local res = {}
    for _, entry in ipairs(files) do
        local fname = u.filename(entry)
        if fname ~= playlist_filename then
            table.insert(res, fname)
        end
    end
    return res
end

-- Seek to last position of the currently loaded file
local function resume_entry(entry)
    if entry ~= nil and entry.position ~= nil then
        mp.commandv('seek', entry.position, 'absolute', 'exact')
        msg.info("Updated to latest position {" .. entry.position .. "}.")
    end
end

-- Switch to last played file in playlist
local function set_index(index)
    mp.set_property("playlist-pos-1", index)
end

function player.next_from_history()
    local idx = history.get_index();
    local entry = history.get_entry(idx)
    if not entry then return end
    while entry.duration ~= 0 and entry.duration - entry.position < 120 do
        idx = idx + 1
        entry = history.get_entry(idx)
        if not entry then return end
    end
    set_index(idx)
    resume_entry(entry)
end

function player.load_dir()
    local playlist = get_current_dir() .. playlist_filename
    local f = io.open(playlist, "w+")
    if not f then error("Error crating temp file for playlist.") end
    local content = table.concat(get_files(), "\n")
    f:write(content)
    f:flush()
    mp.commandv("loadlist", playlist)
    f:close()
    mp.unregister_event(player.load_dir)
end

return player
