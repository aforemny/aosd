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

get_volume | aosd | set_volume
```
