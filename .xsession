#!/bin/sh

xsetroot -cursor_name left_ptr &
xrdb -merge ~/.Xresources  &
xfsettings &
xfce4-volumed &
xfce4-power-manager &

wait

thunar --daemon &
exec xmonad

