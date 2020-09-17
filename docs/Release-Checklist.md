Steps for making a release of Cyclone:

- Sync latest bootstrap code, including cyclone and cyclone-winds
- Prepare release notes
- Update release number in:
  - `common.sld` and rebuild bootstrap source files
  - `Dockerfile`
  - `DEBIAN/control` in cyclone-bootstrap
  - `.github/workflows/Release.yml` job in cyclone-bootstrap
- Update documentation, if applicable
- Tag releases and push to Github
- Upload release notes to `gh-pages` branch
- Update release on AUR
- Update release on Homebrew (automated)
- Update release on Dockerhub (push to bitbucket)
