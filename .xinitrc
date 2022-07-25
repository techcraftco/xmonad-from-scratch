#!/usr/bin/env sh

# Set fonts etc.
xrdb -merge $HOME/.Xresources

# Set the background
feh --bg-fill --no-fehbg ~/.wallpapers/grad1.jpg

# Fire up the screensaver
xscreensaver -no-splash & 

# Network Applet
nm-applet --sm-disable & 

# Trayer
trayer --edge top --align right \
	--SetDockType true --SetPartialStrut true --expand true \
	--transparent true --alpha 0 --tint 0x44475a \
	--width 6 --height 30 --padding 1 &

exec xmonad

