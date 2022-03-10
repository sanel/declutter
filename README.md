# declutter - Read html content without clutter from your Emacs

declutter is a small Emacs addon that will help you with reading
online content. It will remove all distractions and present you with
readable html, straight inside your Emacs.

declutter can render content using:

 * [lynx](https://en.wikipedia.org/wiki/Lynx_(web_browser))
 * [rdrview](https://github.com/eafer/rdrview)
 * [EWW](https://www.gnu.org/software/emacs/manual/html_mono/eww.html) - Emacs builtin browser
 * [txtify.it](https://txtify.it/)

## Installation

declutter depends on [json.el](https://github.com/thorstadt/json.el)
and [shr.el](http://bzr.savannah.gnu.org/lh/emacs/trunk/annotate/head:/lisp/net/shr.el). `shr.el`
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

By default, declutter will open `*declutter*` buffer and render cleaned
content in it. Actually, this is done by `shr.el` so all default
shortcuts for it works here as well.

declutter adds another useful function -
`declutter-under-point`. When you read through the article and you'd
like to open url, you place cursor over link and you have two options:

  * You can use `shr-browse-url` (`v` key) which will open article *as is*
  * Or you can use `declutter-under-point` to load that url and render
    cleaned content in `*declutter*` buffer.

To change rendering engine (default is outline.com API), use this:

```el
(setq declutter-engine 'lynx)     ; lynx will get and render html
; or
(setq declutter-engine 'rdrview)  ; rdrview will get and render html
; or
(setq declutter-engine 'eww)      ; eww will get and render html
; or
(setq declutter-engine 'txtify)   ; txtify.it will get and render html
```

For `lynx` and `rdrview` engines, you can set a custom path to the
program binary, with `declutter-engine-path`. For example, if you have
`rdrview` installed in `/opt/rdrview/bin/rdrview`, set it with:

```el
(setq declutter-engine-path "/opt/rdrview/bin/rdrview")
```

## Configuration

declutter doesn't have many configuration options, except of
`declutter-engine`, `declutter-engine-path` and `declutter-user-agent`
(where you can override default user-agent string), but if you want to
set default font of rendered content or indentation, check
[shr.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/net/shr.el)
options.

For example, to use default Emacs fonts and add margins, set this:

```el
(setq
 shr-use-fonts nil
 shr-indentation 2)
```

## Note

When declutter is using [txtify.it](https://txtify.it) to render be
aware that `txtify.it` **can see** what you browse. I'm not affiliated
with `txtify.it` in any way.

## Bug reports & patches

Feel free to report any issues you find or you have suggestions for improvements.
