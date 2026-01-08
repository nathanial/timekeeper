#!/bin/bash
set -e

# Timekeeper install script
# Builds and installs the timekeeper binary

INSTALL_DIR="${INSTALL_DIR:-$HOME/.local/bin}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Building timekeeper..."
cd "$SCRIPT_DIR"
lake build timekeeper

echo "Installing to $INSTALL_DIR..."
mkdir -p "$INSTALL_DIR"
cp .lake/build/bin/timekeeper "$INSTALL_DIR/timekeeper"
chmod +x "$INSTALL_DIR/timekeeper"
# Remove quarantine/provenance attributes
xattr -cr "$INSTALL_DIR/timekeeper" 2>/dev/null || true
# Re-sign the binary (required on macOS after copying)
codesign -fs - "$INSTALL_DIR/timekeeper" 2>/dev/null || true

echo ""
echo "Installed timekeeper to $INSTALL_DIR/timekeeper"

# Check if install dir is in PATH
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo ""
    echo "NOTE: $INSTALL_DIR is not in your PATH."
    echo "Add it by adding this line to your shell config (~/.bashrc, ~/.zshrc, etc.):"
    echo ""
    echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo ""
fi

echo "Run 'timekeeper' to start tracking time."
