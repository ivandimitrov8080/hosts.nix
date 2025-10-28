local mp = require 'mp'
local utils = require 'mp.utils'
local msg = require 'mp.msg'

history_path = (os.getenv("HOME") or "") .. "/.config/mpv/dir-player-history.json"

local function to_filename(path)
    return path:match("([^/]+)$")
end

-- Save history to JSON file
local function save_history(history)
    local file = io.open(history_path, "w")
    if not file then return end
    local content = utils.format_json(history)
    file:write(content)
    file:close()
end

-- Load history from JSON file
local function load_history()
    local file = io.open(history_path, "r")
    if not file then
        save_history()
        file = io.open(history_path, "r")
        if not file then
            return {}
        end
    end
    local content = file:read("*a")
    file:close()
    local ok, data = pcall(function() return utils.parse_json(content) end)
    if ok and type(data) == "table" then return data end
    return {}
end

-- Update history for a file
local function update_history()
    local history = load_history()
    local filename = to_filename(mp.get_property("filename"))
    local position = mp.get_property_number("time-pos", 0)
    history[filename] = {
        position = math.floor(position),
        timestamp = os.time()
    }
    save_history(history)
end

local function get_playlist()
    local pl = mp.get_property_native("playlist")
    local res = {}
    for _, entry in ipairs(pl) do
        if entry.filename then
            entry.filename = to_filename(entry.filename)
            table.insert(res, entry)
        end
    end
    return res
end

-- Find the most recently played file in the playlist
local function find_last_played_file(files, history)
    local latest_file, latest_time = nil, 0
    for _, file in ipairs(files) do
        local entry = history[file.filename]
        if entry and entry.timestamp and entry.timestamp > latest_time then
            latest_file = file.filename
            latest_time = entry.timestamp
        end
    end
    return latest_file
end

-- Seek to last position of the currently loaded file
local function resume_current_file(history)
    local filename = to_filename(mp.get_property("filename"))
    local entry = history[filename]
    if entry ~= nil and entry.position ~= nil then
        mp.commandv('seek', entry.position, 'absolute', 'exact')
        msg.info("Resumed " .. filename .. " at position " .. entry.position)
    end
end

-- Switch to last played file in playlist
local function switch_to_last_played_file(history)
    local playlist = get_playlist()
    local last_file = find_last_played_file(playlist, history)
    if last_file ~= nil then
        for i, v in ipairs(playlist) do
            if v.filename == last_file then
                mp.set_property("playlist-pos", i - 1) -- mpv uses 0-based index
                msg.info("Switched to last played file: " .. last_file)
                return
            end
        end
    end
end

-- On startup, switch to last played file and resume
mp.register_event("file-loaded", function()
    local history = load_history()
    switch_to_last_played_file(history)
    resume_current_file(history)
end)


mp.register_event("on_unload", function()
    update_history()
end)

mp.add_periodic_timer(5, function()
    update_history()
end)

mp.msg.info("dir-player.lua loaded: playlist resume enabled")
