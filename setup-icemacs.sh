#!/bin/bash

# Define the paths
EMACS_DIR="$HOME/.emacs.d"
INIT_ORG="$EMACS_DIR/bwoah.org"
INIT_EL="$EMACS_DIR/init.el"

# Check if init.el exists
if [ ! -f "$INIT_EL" ]; then
  echo "Generating init.el from bwoah.org..."
  # Use Emacs in batch mode to tangle init.org
  emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$INIT_ORG\" \"$INIT_EL\")"
  echo "init.el generated. Starting package installation..."

  # Start Emacs in batch mode to install packages
  emacs --batch -l "$INIT_EL" --eval "(package-refresh-contents)" --eval "(package-install-selected-packages)" --eval "(kill-emacs)"

  echo "Package installation complete. Icemacs is ready!"
else
  echo "init.el already exists."
fi
