#!/usr/bin/env bash

nix shell nixpkgs#node2nix -c node2nix -i packages.json
