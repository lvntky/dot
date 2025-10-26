#!/usr/bin/env bash
# apt-clean.sh — Safe APT cleanup for Debian-based systems
# Removes unneeded packages, cleans cache, and reports freed space.

set -e

echo "=== 🧩 Updating package lists ==="
sudo apt update -qq

echo "=== 📦 Removing unused dependencies ==="
sudo apt autoremove --purge -y

echo "=== 🧺 Cleaning package cache ==="
sudo apt autoclean -y
sudo apt clean -y

echo "=== 🔍 Removing orphaned libraries ==="
if command -v deborphan >/dev/null; then
    sudo apt purge -y $(deborphan) || true
else
    echo "Skipping deborphan cleanup (install it with: sudo apt install deborphan)"
fi

echo "=== 📊 Disk usage summary ==="
df -h | grep -E '^/dev/'

echo "=== ✅ Done ==="

