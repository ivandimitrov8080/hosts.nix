local mp = require 'mp'
local msg = require 'mp.msg'
local history = require 'history'
local player = require 'player'


local function on_file_loaded()
    player.next_from_history()
end

-- Update history
local function on_timeout()
    history.update()
end

local function on_shutdown()
    history.save()
end

-- On startup, switch to last played file and resume
local function init()
    local interval = 1
    mp.register_event("file-loaded", on_file_loaded)
    msg.info("Switched to last played file from this dir playlist.")

    mp.add_periodic_timer(interval, on_timeout)
    msg.info("Added periodic timer to update in-memory history every {" .. interval .. " seconds}.")

    mp.register_event("shutdown", on_shutdown)
    msg.info("Registered shutdown event handler to save history to file.")
end

init()
