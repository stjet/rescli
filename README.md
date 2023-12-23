# Rescli

Rescli is a CLI frontend for [reservoir](https://github.com/jetstream0/reservoir). It is fully compatible with reservoir does everything it can.

## Usage

Brackets indicate optional flags and parens indicate required flags.

 - rescli list [--filter/-f all/title/link/tags] [--sort/-s relevant/newest/oldest] [--query/-q xxx] [--limit/-l n] 
 - rescli new (--title/-t xxx) (--link/-l xxx) [--note/-n xxx] [--tags/-a "x","x","x"] 
 - rescli view [id]
 - rescli init [--overwrite/-o]

## Installing

After downloading the source, and Petite Chez Scheme, copy the bash completions file for uuid autocomplete in the terminal:

```bash
sudo mkdir /etc/bash_completion.d/rescli_utils
sudo cp get_uuids.scm /etc/bash_completion.d/rescli_utils/get_uuids.scm
sudo cp rescli-completion.sh /etc/bash_completion.d/rescli-completion.sh
```

Then put `rescli` and `rescli.scm` onto a directory in PATH so it can be used anywhere (probably `~/.local/bin` to install for user, `/usr/local/bin` for all users`):

```bash
sudo cp rescli ~/.local/bin/rescli
sudo cp rescli.scm ~/.local/bin/rescli.scm
```

Restart the shell and rescli should now be usable!

