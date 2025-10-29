local mp = require 'mp'
local msg = require 'mp.msg'
local history = require 'history'
local u = require 'util'

local player = {}

-- Seek to last position of the currently loaded file
local function resume_current_file()
    local entry = history.get_last_entry()
    if entry ~= nil and entry.position ~= nil then
        mp.commandv('seek', entry.position, 'absolute', 'exact')
    end
end

-- Switch to last played file in playlist
local function switch_to_last_played_file()
    local index = history.get_index()
    mp.set_property("playlist-pos", index)
    msg.info("Switched to last played file.")
end

function player.next_from_history()
    switch_to_last_played_file()
    resume_current_file()
end

function player.current_directory()
    local p = mp.get_property("path")
    local f = u.filename(mp.get_property("filename"))
    local d = string.gsub(p, f, "")
    return d
end

return player
