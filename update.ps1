# BVR WQX Uploader - Update Script
# Called by the launcher to check for and download updates

param(
    [string]$AppDir,
    [string]$VersionFile
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'  # Speeds up Invoke-WebRequest
$Repo = "FlowWest/bvr-wqx-uploader"

try {
    # Force TLS 1.2 for GitHub API
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

    # Get latest release info
    $release = Invoke-RestMethod -Uri "https://api.github.com/repos/$Repo/releases/latest" -TimeoutSec 10
    $latestVersion = $release.tag_name

    # Get local version
    $localVersion = ""
    if (Test-Path $VersionFile) {
        $localVersion = (Get-Content $VersionFile -Raw).Trim()
    }

    # Compare versions
    if ($latestVersion -ne $localVersion) {
        Write-Host "Update available: $localVersion -> $latestVersion"
        Write-Host "Downloading update..."

        $zipUrl = $release.zipball_url
        $tempZip = Join-Path $env:TEMP "bvr-wqx-update.zip"
        $tempExtract = Join-Path $env:TEMP "bvr-wqx-extract"

        # Download
        Invoke-WebRequest -Uri $zipUrl -OutFile $tempZip -TimeoutSec 120

        # Clean up old extract folder
        if (Test-Path $tempExtract) {
            Remove-Item $tempExtract -Recurse -Force
        }

        # Extract
        Expand-Archive -Path $tempZip -DestinationPath $tempExtract -Force

        # Find extracted folder
        $extractedFolder = Get-ChildItem $tempExtract -Directory | Select-Object -First 1
        $sourceApp = Join-Path $extractedFolder.FullName "app"

        if (Test-Path $sourceApp) {
            Write-Host "Installing update..."
            
            # Remove old app folder
            if (Test-Path $AppDir) {
                Remove-Item $AppDir -Recurse -Force
            }
            
            # Copy new app folder
            Copy-Item $sourceApp -Destination $AppDir -Recurse
        }

        # Save version
        $latestVersion | Out-File -FilePath $VersionFile -NoNewline -Encoding ASCII

        # Cleanup temp files
        Remove-Item $tempZip -Force -ErrorAction SilentlyContinue
        Remove-Item $tempExtract -Recurse -Force -ErrorAction SilentlyContinue

        Write-Host "Update complete!"
    }
    else {
        Write-Host "You have the latest version ($localVersion)"
    }
}
catch {
    Write-Host "Could not check for updates: $_"
    Write-Host "Continuing with current version..."
}
