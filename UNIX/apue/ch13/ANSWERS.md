Chapter 13 Daemon Processes
===========================
1. If a user process calls chroot before calling openlog then syslog try to open
   /dev/log starting at the root directory of the chroot. This will no doubt not
   be the behaviour one requires.
2. Why is syslogd not a session leader?
3. Non-kernel daemons running on system *Linux eros 4.1.6-1-ARCH x86_64 GNU/Linux*  

* /sbin/init
System startup 

* /usr/lib/systemd/systemd-journald
Collect and store logging data

* /usr/lib/systemd/systemd-udevd
Listen for kernel uevents, executes instructions found in udev rules

* /usr/lib/systemd/systemd-logind
Manage user logins

* /usr/bin/dbus-daemon --system --address=systemd: --nofork --nopidfile --systemd-activation
B-bus message bus deamon. Uses D-Bus library to implement a message bus

* login -- tobin
Used to login to the system

* ssh-agent -s -a /home/tobin/.ssh/envir
holds ssh private keys for re-use

* /usr/lib/systemd/systemd --user
Bring up and maintain user space services

* (sd-pam)
?

* /usr/bin/dbus-daemon --fork --print-pid 4 --print-address 6 --session
As above

* gpg-agent --daemon --enable-ssh-support --write-env-file /home/tobin/.gnupg/gpg-agent.env
hold gpg keys

* /usr/lib/polkit-1/polkitd --no-debug
Authorization manager

* /usr/lib/menu-cache/menu-cached /tmp/.menu-cached-:0-tobin
?

* /usr/bin/pulseaudio --start --log-target=syslog
The Pulse Audio Sound System

* /usr/lib/rtkit/rtkit-daemon
Realtime Policy and Watchdog daemon control

* /usr/lib/upower/upowerd
Provides UPower service on system message bus

* /usr/lib/at-spi2-core/at-spi-bus-launcher
?

* /usr/bin/dbus-daemon --config-file=/etc/at-spi2/accessibility.conf --nofork --print-address 3
see above

* /usr/lib/at-spi2-core/at-spi2-registryd --use-gnome-session
?

* /usr/lib/GConf/gconfd-2
Gnome configuration daemon

* 01 scdaemon --multi-server
Manage smart cards (invoked by gpg-agent)

* 00 /usr/bin/aspell -a -m -B --encoding=utf-8
Interactive spell checker

4. daemon-login.c: view log with
$ journalctl | g 'daemon-login'