2chainz [![Build Status](https://travis-ci.org/glittershark/2chainz.svg?branch=master)](https://travis-ci.org/glittershark/2chainz)
=======
Keychain management fo' playaz

## Installation

```
git clone https://github.com/glittershark/2chainz.git
cd 2chainz
cabal install
```

## Usage

This is a major WIP, so currently there are only two things you can do:

- `2c set user password` will store the password "password" for "user"
- `2c get user` will retrieve that password and print it

The passwords are stored as AES-encrypted JSON in ~/.keys, but the key they're
encrypted with is hardcoded right now, so you probably shouldn't use this tool.

