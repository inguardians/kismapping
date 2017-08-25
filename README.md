# Kismapping

Kismapping is a WiFi heatmapping tool which consumes
[Kismet](https://github.com/kismetwireless/kismet) output and produces a
heatmap. It also serves a google maps web page with the heatmap overlaid on
top.

Originally inspired by work done by Tom Liston ([@tliston](https://twitter.com/tliston)) on a tool named "mykismap".

## Build

If you do not have stack, [install
stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

Then, from the project's root directory, run `stack build`.

## Install

Run `stack install` to install an executable on your system. The default
installation path is $HOME/.local/bin, but stack will tell you its destination
path.

## Run

After building, if you want to run the executable without installing it, run
`stack exec kismapping -- <args>`. Otherwise, see [Install](#install) for
installation details, and use the `kismapping` command.

Currently, the command line interface is not very intuitive. I'm going to
rework it slightly to fix that soon, but for now just go with the example at
the bottom of this section, it should help you do what you want.

If you want to use the web interface (which you almost certainly do), you will
need to get a Google Maps API Key to use from
[https://developers.google.com/maps/documentation/javascript/get-api-key](https://developers.google.com/maps/documentation/javascript/get-api-key)

You'll also need a gpsxml and netxml file from kismet, both in the same
directory. You will need to use at least the following arguments:

- `-i` - Gpsxml file
- `-e` - ESSIDs
- `-k` - Google API Key

For example, the following command would generate a map containing the ESSIDs
`FBI_VAN` and `The Promised LAN`:

    kismapping -e FBI_VAN -e 'The Promised LAN' -i Kismet-data.gpsxml -k ThisWouldBeYourGoogleApiKey

See the command help for further information.

## TODO

- Document code
- Put version constraints on dependencies
- Better command line interface
- Use new kismet output format (we currently use the format from the stable
  release).
