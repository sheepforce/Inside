# INSIDE
A tool to monitor the conditions the "INSIDE" UHV chamber.

## Capabilities
INSIDE monitors three different controllers:
  - [VACOM Coldion CU-100](http://www.vacom-shop.de/epages/VacomShop.sf/en_GB/?ViewObjectID=1088267)
  - [LakeShore 335](https://www.lakeshore.com/products/cryogenic-temperature-controllers/model-335/Pages/Overview.aspx)
  - [Leybold GRAPHIX THREE](https://www.leyboldproducts.uk/products/vacuum-measuring/active-sensors/operating-units-for-active-sensors/operating-units-for-active-sensors/1782/graphix-three) (2 x)

Conditions are periodically updated and warnings are shown, if conditions reach some thresholds. Futhermore all values can be written to a file. Configuration happens via a very simple config file. A website with auto updating plots can be used to follow the conditions in the chamber.

## Installation
The programm is written solely in Haskell and built usind the tool `stack`. To install `stack` visit the [installation guide for stack](https://docs.haskellstack.org/en/stable/README/).

Clone this repository and then build it:

    git clone https://github.com/sheepforce/Inside.git
    cd Inside
    stack install gtk2hs-buildtools
    stack setup
    stack build
    stack install

The commands above may take some time as stack needs to download the compiler, all necessary libraries and compile them.

### Website
To make it easier to follow trends of conditions in the UHV chamber while working or being remote, a website can be used to visualize conditions. The html document and the javascript function for updating the graphics can be found in `web`. You need a document root for the webserver (like Apache), that is given in `WEBPREFIX` in the `Makefile`. Execute

    make

and enable this directory in your web server settings. For apache create a configuration (`/etc/apache/sites-available/inside.conf`) with the following content

     <VirtualHost \*:80>
         ServerAdmin webmaster@localhost

         DocumentRoot /home/user/inside-web # according to whatever you chose in the Makefile

         ErrorLog ${APACHE_LOG_DIR}/error.log
         CustomLog ${APACHE_LOG_DIR}/access.log combined

         <Directory />
            Options FollowSymLinks
            AllowOverride None
        </Directory>

        <Directory /home/user/inside-web>
            Options Indexes FollowSymLinks MultiViews
            AllowOverride None
            Require all granted
        </Directory>
     </VirtualHost>

in `/etc/apache/sites-available` and enable it with

    a2ensite inside.conf
    systemctl apache2.service reload

## Usage
### inside
After you have installed the `inside` executable by executing `stack install`, make sure that the installation directory is available in your `$PATH`. Edit the `device.conf` file, that came with this repository and chnage to your needs. While the syntax should be self explanatory, be aware that the parser is not very flexible and cannot handle changes in the order of lines, nor comments, nor whitespace in names. There are no optional values! if you use `device.conf` everything MUST be set.

- `enabled =` can be `True` or `False`, states if a device is updated or not. If not connected simply set to `False`.
- `port =` is the path to the RS232 interface, if using a USB-RS232 converter it is usually something like `/dev/ttyUSB0`. Check `dmesg` for interfaces
- `warn = ` is the threshold for the devices, above which a warning will be raised. The Coldion takes a single value, the LakeShore two and the two GRAPHIX THREE three values each. Separate them by space.
- `label =` a simple, non breaking string, that is shown in the programm for clarity in front of the reading, takes same amount of values as `warn =`

You can now execute `inside` for executing the programm with internal defaults or execute `inside /path/to/device.conf` to read the configuration file and overwrite the internal defaults.

### insideplotter
