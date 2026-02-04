#!/bin/bash

# BVR WQX Uploader Launcher
# Run to launch the Shiny app (auto-updates from GitHub)

REPO="FlowWest/bvr-wqx-uploader"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
APP_DIR="$SCRIPT_DIR/app"
VERSION_FILE="$SCRIPT_DIR/VERSION"

echo "============================================"
echo "  BVR WQX Uploader"
echo "============================================"
echo

# Check for updates
echo "Checking for updates..."

check_for_updates() {
    # Get latest release info from GitHub
    RELEASE_INFO=$(curl -s --connect-timeout 10 "https://api.github.com/repos/$REPO/releases/latest" 2>/dev/null)
    
    if [ $? -ne 0 ] || [ -z "$RELEASE_INFO" ]; then
        echo "Could not check for updates. Continuing with current version..."
        return
    fi
    
    # Extract version tag
    LATEST_VERSION=$(echo "$RELEASE_INFO" | grep -o '"tag_name": *"[^"]*"' | head -1 | cut -d'"' -f4)
    
    if [ -z "$LATEST_VERSION" ]; then
        echo "Could not parse release info. Continuing with current version..."
        return
    fi
    
    # Get local version
    LOCAL_VERSION=""
    if [ -f "$VERSION_FILE" ]; then
        LOCAL_VERSION=$(cat "$VERSION_FILE" | tr -d '[:space:]')
    fi
    
    # Compare versions
    if [ "$LATEST_VERSION" != "$LOCAL_VERSION" ]; then
        echo "Update available: $LOCAL_VERSION -> $LATEST_VERSION"
        echo "Downloading update..."
        
        # Get zipball URL
        ZIPBALL_URL=$(echo "$RELEASE_INFO" | grep -o '"zipball_url": *"[^"]*"' | head -1 | cut -d'"' -f4)
        
        if [ -z "$ZIPBALL_URL" ]; then
            echo "Could not get download URL. Continuing with current version..."
            return
        fi
        
        # Download and extract
        TEMP_DIR=$(mktemp -d)
        TEMP_ZIP="$TEMP_DIR/update.zip"
        
        curl -sL --connect-timeout 120 "$ZIPBALL_URL" -o "$TEMP_ZIP"
        
        if [ $? -ne 0 ] || [ ! -f "$TEMP_ZIP" ]; then
            echo "Download failed. Continuing with current version..."
            rm -rf "$TEMP_DIR"
            return
        fi
        
        # Extract
        unzip -q "$TEMP_ZIP" -d "$TEMP_DIR"
        
        # Find extracted folder (GitHub names it repo-version)
        EXTRACTED_FOLDER=$(find "$TEMP_DIR" -mindepth 1 -maxdepth 1 -type d | head -1)
        
        if [ -d "$EXTRACTED_FOLDER/app" ]; then
            echo "Installing update..."
            rm -rf "$APP_DIR"
            cp -R "$EXTRACTED_FOLDER/app" "$APP_DIR"
        fi
        
        # Save version
        echo -n "$LATEST_VERSION" > "$VERSION_FILE"
        
        # Cleanup
        rm -rf "$TEMP_DIR"
        
        echo "Update complete!"
    else
        echo "You have the latest version ($LOCAL_VERSION)"
    fi
}

check_for_updates
echo

# Find R installation
RSCRIPT=""

# Check common locations
for path in \
    "/usr/local/bin/Rscript" \
    "/usr/bin/Rscript" \
    "/opt/homebrew/bin/Rscript" \
    "/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript" \
    "$HOME/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript"
do
    if [ -x "$path" ]; then
        RSCRIPT="$path"
        break
    fi
done

# Check if R is in PATH
if [ -z "$RSCRIPT" ]; then
    RSCRIPT=$(which Rscript 2>/dev/null)
fi

if [ -z "$RSCRIPT" ] || [ ! -x "$RSCRIPT" ]; then
    echo "ERROR: R installation not found."
    echo "Please install R from https://cran.r-project.org/"
    exit 1
fi

RPATH=$(dirname "$RSCRIPT")
echo "Found R at: $RPATH"
echo

# Change to app directory
cd "$APP_DIR"

# Install dependencies if needed
echo "Checking dependencies..."
"$RSCRIPT" install-deps.R

# Launch the app
echo
echo "Starting WQX Uploader..."
echo "(A browser window will open shortly)"
echo

"$RPATH/R" -e "shiny::runApp('.', launch.browser = TRUE)"
