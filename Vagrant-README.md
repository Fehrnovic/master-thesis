# Getting Started

1. Install `vagrant` and `virtualbox`. 
1. Run `vagrant up` with the provided `Vagrantfile` with the content:
```
Vagrant.configure("2") do |config|
    config.vm.box = "ubuntu/focal64"
    config.ssh.forward_agent = true
end
```
1. `git clone` the `egodsk/otp` repository to the shared folder
1. SSH into the Vagrant machine with `vagrant ssh`
1. Change directory to the `egodsk/otp` directory (e.g. `cd /vagrant/otp`)
1. Run the command:
```
sudo apt-get update && sudo apt-get upgrade && sudo apt-get -y install build-essential autoconf m4 libssl-dev libncurses5-dev libwxgtk3.0-gtk3-dev default-jdk unixodbc-dev xsltproc fop libxml2-utils
```
1. Run ```export ERL_TOP=`pwd`/output```
1. Run ```./configure --prefix=`pwd`/output```
1. Run ```make```
1. Run ```make install```
1. Update the `.bashrc` file with: `export PATH="/vagrant/otp/output/bin:$PATH"`
1. Run `exit` and `vagrant ssh` to have the PATH updated
1. Profit.
