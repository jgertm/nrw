# nrw

`nrw` (pronounced *narrow*) is a simple utility that allows interactive
selection of zero or more lines from STDIN.

It furthermore supports *masking* the input to present a clearer overview of the
input lines while still returning the full, unmasked input lines following
selection.

## Installation

Requires `git` and [`stack`](https://docs.haskellstack.org/en/stable/README/).

``` sh
$ git clone https://github.com/jgertm/nrw.git && cd nrw
$ stack install --local-bin-path=$DIRECTORY_ON_YOUR_PATH
...
```

## Usage

- `Up` and `Down` to navigate candidates
- `Tab` to select
- `Enter` to return selections and currently highlighted candidate
- `Esc` to exit

### Open a bunch of papers

Because you'll never get through your reading list one item at a time!

``` sh
$ find ~/papers/ -name *.pdf | nrw --mask='/([^/]+.pdf)$' | while read -r pf; do open $pf; done
```

## FAQ

### Is it any good?

No.

### Why not use [`percol`](https://github.com/mooz/percol), [`peco`](https://github.com/peco/peco), or [`fzf`](https://github.com/junegunn/fzf)?

None of them display an aplomb lambda as the prompt symbol.
