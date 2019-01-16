source ~/.exports
set -e
cd ~/cm
git pull
~/bin/stack build --fast
linuxdeploy-x86_64.AppImage --appdir=AppDir_slave-exe --executable=.stack-work/install/x86_64-linux/lts-13.2/8.6.3/bin/slave-exe --desktop-file=util/resources/appimage/slave.desktop --output=appimage --icon-file=util/resources/appimage/slave.svg

