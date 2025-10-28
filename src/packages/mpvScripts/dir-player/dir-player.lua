local mp = require 'mp'
local utils = require 'mp.utils'
local msg = require 'mp.msg'

history_path = (os.getenv("HOME") or "") .. "/.config/mpv/dir-player-history.json"

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
    local filename = mp.get_property("filename")
    local position = mp.get_property_number("time-pos", 0)
    history[filename] = {
        position = math.floor(position),
        timestamp = os.time()
    }
    save_history(history)
end

-- Get playlist file paths
local function get_playlist_files()
    local playlist = mp.get_property_native("playlist")
    local files = {}
    for _, entry in ipairs(playlist) do
        if entry.filename then
            table.insert(files, entry.filename)
        end
    end
    return files
end

-- Find the most recently played file in the playlist
local function find_last_played_file(files, history)
    local latest_file, latest_time = nil, 0
    for _, file in ipairs(files) do
        local entry = history[file]
        if entry and entry.timestamp and entry.timestamp > latest_time then
            latest_file = file
            latest_time = entry.timestamp
        end
    end
    return latest_file
end

-- Seek to last position of the most recently played file
local function resume_last_played(history)
    local files = get_playlist_files()
    local last_file = find_last_played_file(files, history)
    msg.info(last_file)
    if last_file ~= nil then
        local entry = history[last_file]
        if entry ~= nil and entry.position ~= nil then
            -- Find index in playlist
            local playlist = mp.get_property_native("playlist")
            for i, v in ipairs(playlist) do
                if v.filename == last_file then
                    mp.set_property("playlist-pos", i - 1)
                    mp.commandv('seek', entry.position, "absolute+keyframes")
                    msg.info("Resumed " .. last_file .. " at position " .. entry.position)
                    return
                end
            end
        end
    end
end

-- On startup, load history and resume last played file
mp.register_event("file-loaded", function()
    resume_last_played(load_history())
end)

mp.register_event("on_unload", function()
    update_history()
end)

mp.add_periodic_timer(1, function()
    update_history()
end)

mp.msg.info("dir-player.lua loaded: playlist resume enabled")
