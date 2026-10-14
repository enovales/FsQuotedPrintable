@echo off
setlocal

echo Building FsQuotedPrintable with .NET 8...

if "%1"=="" (
    dotnet restore
    dotnet build FsQuotedPrintable.sln --configuration Release
    dotnet test FsQuotedPrintable.sln --configuration Release --no-build
) else (
    dotnet %*
)
