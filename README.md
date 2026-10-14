# FsQuotedPrintable

This is a small library to handle decoding and encoding data in the [Quoted-Printable](https://en.wikipedia.org/wiki/Quoted-printable) format.

## Usage

```
FsQuotedPrintable.Decode("E=3Dmc2") // "E=mc2"
FsQuotedPrintable.DecodeString("E=3Dmc2", System.Text.Encoding.UTF8)

FsQuotedPrintable.Encode(System.Text.Encoding.UTF8.GetBytes("E=mc2"))
FsQuotedPrintable.EncodeString("E=mc2", System.Text.Encoding.ASCII) // "e=3Dmc2"
```

---

## Builds

macOS/Linux | Windows
--- | ---
[![GitHub Actions Badge](https://github.com/enovales/FsQuotedPrintable/actions/workflows/build.yml/badge.svg)](https://github.com/enovales/FsQuotedPrintable/actions/workflows/build.yml)

## NuGet 

Package | Stable | Prerelease
--- | --- | ---
FsQuotedPrintable | [![NuGet Badge](https://buildstats.info/nuget/FsQuotedPrintable)](https://www.nuget.org/packages/FsQuotedPrintable/) | [![NuGet Badge](https://buildstats.info/nuget/FsQuotedPrintable?includePreReleases=true)](https://www.nuget.org/packages/FsQuotedPrintable/)

---

### Developing

Make sure the following **requirements** are installed on your system:

- [.NET SDK](https://dotnet.microsoft.com/download) 8.0 or higher

or

- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers)

---

### Building

```sh
> build.cmd // on windows
$ ./build.sh  // on unix
```

This will:
1. Restore NuGet packages
2. Build the solution
3. Run all tests

You can also pass custom commands to the dotnet CLI:

```sh
./build.sh build MyProject.fsproj --configuration Debug
```

---

### Releasing

- [Start a git repo with a remote](https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/)

```sh
git add .
git commit -m "Scaffold"
git remote add origin https://github.com/user/MyCoolNewLib.git
git push -u origin master
```

- [Create a GitHub OAuth Token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
  - You can then set the `GITHUB_TOKEN` to upload release notes and artifacts to github

- Then update the `CHANGELOG.md` with an "Unreleased" section containing release notes for this version, in [KeepAChangelog](https://keepachangelog.com/en/1.1.0/) format.

Here's an example of adding an "Unreleased" section to a `CHANGELOG.md` with a `0.1.0` section already released.

```markdown
## [Unreleased]

### Added
- Does cool stuff!

### Fixed
- Fixes that silly oversight

## [0.1.0] - 2017-03-17
First release

### Added
- This release already has lots of features

[Unreleased]: https://github.com/user/MyCoolNewLib.git/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/user/MyCoolNewLib.git/releases/tag/v0.1.0
```

- You can then create a GitHub release and publish to NuGet manually using:

```sh
dotnet pack --configuration Release
```

