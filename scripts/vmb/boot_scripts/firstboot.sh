#!/bin/bash -e

# First boot script: Sets up the system for automated use.
# This will:
# 1) resize the partitions to fill the entire available space.
# 2) Set up /root/.bashrc to execute the daily job with a correct environment.
# 3) Enable automatic login of root, which then in turn launches the /root/daily_job.sh script via /root/.bashrc.
# 4) Restarts serial-getty to trigger automatic login for day zero.

# Wait for network to be up
while ! ping -c1 dns.google; do
  echo "waiting for network"
  sleep 1
done

# Resize /dev/sda1 to be 10G or whatever the image actually is
echo "Growing rootfs..."
DEBIAN_FRONTEND=noninteractive apt-get -y update
DEBIAN_FRONTEND=noninteractive apt-get -y install \
    cloud-utils
growpart /dev/sda 1
resize2fs /dev/sda1
DEBIAN_FRONTEND=noninteractive apt-get -y remove cloud-utils
DEBIAN_FRONTEND=noninteractive apt-get -y autoremove

# Add daily job to root's .bashrc
cat <<EOF >> /root/.bashrc
# Source rupstup environment
if [ -f /root/.cargo/env ]; then
    . "/root/.cargo/env"
fi

# Run daily job.
/root/daily_job.sh
EOF

# Enable autologin for root
mkdir -p /etc/systemd/system/serial-getty@.service.d
cat <<EOF >/etc/systemd/system/serial-getty@.service.d/override.conf
[Service]
ExecStart=
ExecStart=-/sbin/agetty -o '-p -- \\u' --keep-baud 115200,57600,38400,9600 -a root - $TERM
EOF

# Auto-login as root, run first daily script.
systemctl daemon-reload
systemctl restart serial-getty@ttyS0.service
