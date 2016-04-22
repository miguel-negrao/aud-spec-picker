aud-spec-picker [![Build Status](https://travis-ci.org/miguel-negrao/aud-spec-picker.svg?branch=master)](https://travis-ci.org/miguel-negrao/aud-spec-picker)
===============

Get the frequencies with highest energy from an exported file from audacity's spectrum analyser

Parses audacity spectrum analysis files, searching for the N frequencies with higher energy.
Will discard frequencies fd which are closer to an already selected frequency f if f/fd < delta

```
args [OPTIONS] INPUTFILE OUTPUTFILE

Common flags:
  -d --delta=NUM     exclude frequencies near selected ones in range of delta
  -n --numfreqs=INT  number of frequencies
  -? --help          Display help message
  -V --version       Print version information
```
