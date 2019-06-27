# declutter - Read html content without clutter from your Emacs

declutter is a small Emacs addon that will help you with reading
online content. It will remove all distractions and present you with
readable html, straight inside your Emacs.

declutter is also able to read articles behind paywall - Finacial
Times, WSJ and so on. Please read `Restriction Hacking` down below,
in case you are not able to read the content.

## Installation

declutter depends on [json.el](https://github.com/thorstadt/json.el)
and
[shr.el](http://bzr.savannah.gnu.org/lh/emacs/trunk/annotate/head:/lisp/net/shr.el). `shr.el`
is part of Emacs since 24.4 version.

To install declutter, just copy `declutter.el` to `$HOME/.emacs.d`
folder or any other location listed in Emacs `load-path` variable.

In your
[Emacs init file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html),
put:

```el
(require 'declutter)
```

Then, in Emacs:

<kbd>M-x declutter [RET]</kbd>

and enter url you'd like to visit.

## Usage

By default, declutter will open `*html*` buffer and render cleaned
content in it. Actually, this is done by `shr.el` so all default
shortcuts for it works here as well.

declutter adds another useful function -
`declutter-under-point`. When you read through the article and you'd
like to open url, you place cursor over link and you have two options:

  * You can use `shr-browse-url` (`v` key) which will open article *as is*
  * Or you can use `declutter-under-point` to load that url and render
    cleaned content in `*html*` buffer.

## Note

declutter is using [outline.com](https://outline.com) to render the
content and sometimes can fail with internal error (received from
`outline.com`). In that case, try url multiple times.

Also regardingy privacy, be aware that `outline.com` **can see** what
you browse. I'm not affiliated with `outline.com` in any way.

## Restriction Hacking

As from May 2019 (at least the period I was able to track),
`outline.com` will report for some sites this:
`We're sorry, but this URL is not supported by Outline`.

To bypass it, use some url shortener (like https://bitly.com) and
short destination url. Pass that shortened url to declutter and it
will be able to render the content again.

## Bug reports & patches

Feel free to report any issues you find or you have suggestions for improvements.
