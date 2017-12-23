# INSIDE
A tool to monitor the conditions the "INSIDE" UHV chamber.

## Capabilities
INSIDE monitors three different controllers:
  - [VACOM Coldion CU-100](http://www.vacom-shop.de/epages/VacomShop.sf/en_GB/?ViewObjectID=1088267)
  - [LakeShore 335](https://www.lakeshore.com/products/cryogenic-temperature-controllers/model-335/Pages/Overview.aspx)
  - [Leybold GRAPHIX THREE](https://www.leyboldproducts.uk/products/vacuum-measuring/active-sensors/operating-units-for-active-sensors/operating-units-for-active-sensors/1782/graphix-three) (2 x)

Conditions are periodically updated and warnings are shown, if conditions reach some thresholds. Futhermore all values can be written to a file. Configuration happens via a very simple config file.

## Installation
The programm is written solely in Haskell and built usind the tool `stack`. To install `stack` visit the [installation guide for stack](https://docs.haskellstack.org/en/stable/README/).

Clone this repository and then build it:

    git clone https://github.com/sheepforce/Inside.git
    cd Inside
    stack setup
    stack build
    stack install

The commands above may take some time as stack needs to download the compiler, all necessary libraries and compile them.

## Usage
After you have installed the `inside` executable by executing `stack install`, make sure that the installation directory is available in your `$PATH`. Edit the `device.conf` file, that came with this repository and chnage to your needs. While the syntax should be self explanatory, be aware that the parser is not very flexible and cannot handle changes in the order of lines, nor comments, nor whitespace in names. There are no optional values! if you use `device.conf` everything MUST be set.

- `enabled =` can be `True` or `False`, states if a device is updated or not. If not connected simply set to `False`.
- `port =` is the path to the RS232 interface, if using a USB-RS232 converter it is usually something like `/dev/ttyUSB0`. Check `dmesg` for interfaces
- `warn = ` is the threshold for the devices, above which a warning will be raised. The Coldion takes a single value, the LakeShore two and the two GRAPHIX THREE three values each. Separate them by space.
- `label =` a simple, non breaking string, that is shown in the programm for clarity in front of the reading, takes same amount of values as `warn =`

You can now execute `inside` for executing the programm with internal defaults or execute `inside /path/to/device.conf` to read the configuration file and overwrite the internal defaults.
