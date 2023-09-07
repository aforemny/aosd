# aosd

A simple on-screen display (OSD) that displays and changes a given value.

Current values (extraneous changes) are accepted on standard input, changes to
the given value are produced on standard output.

## Usage

### Volume control

```console
get_volume() {
  pactl subscribe | grep --line-buffered 'Event '\''change'\'' on sink #0' | while read _; do
    pamixer --get-volume
  done
}

set_volume() {
  while read volume; do
    pamixer --allow-boost --set-volume "$volume"
  done
}

get_volume | aosd --max 153 | set_volume
```

### Brightness control

```console
get_backlight() {
  while true; do
    xbacklight -get
    sleep 1s
  done
}

set_backlight() {
  while read percentage; do
    xbacklight ="$percentage"
  done
}

get_backlight | aosd --position left | set_backlight
```
