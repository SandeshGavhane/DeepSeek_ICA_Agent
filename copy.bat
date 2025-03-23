@echo off
setlocal enabledelayedexpansion

:: Robocopy Batch File
:: This batch file uses robocopy to perform file and directory copying operations
:: with error handling and logging capabilities

echo ===== Robocopy File Transfer Utility =====
echo.

:: Set source and destination paths
set "source_path=C:\DeepSeek_ICA_Agent\geomates"
set "destination_path=C:\DeepSeek_ICA_Agent_2\geomates"

:: Set log file location
set "log_file=C:\DeepSeek_ICA_Agent\robocopy_log.txt"

:: Check if source directory exists
if not exist "%source_path%" (
    echo ERROR: Source directory does not exist.
    echo Please edit this batch file to set the correct source path.
    goto :EOF
)

:: Create destination directory if it doesn't exist
if not exist "%destination_path%" (
    echo Creating destination directory...
    mkdir "%destination_path%"
)

echo Starting file copy operation...
echo From: %source_path%
echo To: %destination_path%
echo Log: %log_file%
echo.

:: Perform the robocopy operation with the following options:
:: /E - Copy subdirectories, including empty ones
:: /Z - Copy files in restartable mode (can resume if interrupted)
:: /R:3 - Retry 3 times on failed copies
:: /W:5 - Wait 5 seconds between retries
:: /MT:8 - Use 8 threads for multi-threaded copying
:: /NP - No progress percentage displayed
:: /LOG:"%log_file%" - Write log to specified file
:: /TEE - Display output in console and also write to log file
:: /COPY:DAT - Copy Data, Attributes, and Timestamps of files

robocopy "%source_path%" "%destination_path%" /E /Z /R:3 /W:5 /MT:8 /NP /LOG:"%log_file%" /TEE /COPY:DAT

:: Check robocopy exit code
:: 0 = No errors, no copying
:: 1 = Files copied successfully
:: 2 = Extra files/directories detected
:: 3 = (2+1) Some files copied, extra files found
:: 4 = Some mismatches
:: 8 = Some files/directories not copied
:: 16 = Serious error - robocopy did not copy any files

set rc=%errorlevel%
echo.
echo Robocopy finished with exit code %rc%

if %rc% LEQ 1 (
    echo Operation completed successfully.
) else if %rc% LEQ 7 (
    echo Operation completed with warnings. Check the log file for details.
) else (
    echo Operation encountered errors. Check the log file for details.
)

echo.
echo Log file has been saved to: %log_file%
echo.
echo Press any key to exit...
pause > nul