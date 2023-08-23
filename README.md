# aosd

## Pre-requisites

- xcompmgr

## Usage

### Volume control

```console
get_volume() {
  while true; do
    pamixer --get-volume
    sleep 1s
  done
}

set_volume() {
  while read volume; do
    pamixer --set-volume "$volume"
  done
}

get_volume | aosd --max 150 | set_volume
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
