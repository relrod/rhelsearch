# rhelsearch

See which packages are "officially" packaged in RHEL by using the Fedora
Infrastructure's
[JSON files](https://infrastructure.fedoraproject.org/repo/json/pkg_el7.json).

## How it works

We cache the JSON files in `~/.cache/rhelsearch/`. If the file is over a week
old, we pull the latest files before doing anything else. Otherwise, we use the
cached file. If we're unable to pull the latest files, or if `--offline` is
passed, we simply use the files in cache, if they exist. Otherwise, we give up.

# License

BSD-2
