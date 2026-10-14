#!/usr/bin/env bash

set -eu
set -o pipefail

echo "Building FsQuotedPrintable with .NET 8..."

if [ $# -eq 0 ]; then
    dotnet restore
    dotnet build FsQuotedPrintable.sln --configuration Release
    dotnet test FsQuotedPrintable.sln --configuration Release --no-build
else
    dotnet "$@"
fi
