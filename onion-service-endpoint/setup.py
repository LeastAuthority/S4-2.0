#!/usr/bin/env python

# Copyright Least Authority TFA GmbH
# See LICENSE for details.

import setuptools

setuptools.setup(
    name="txxonion",
    version="0.1",
    description="A Twisted server string endpoint description parser for Onion services.",
    author="txxonion Developers",
    url="https://leastauthority.com/",
    license="MIT",
    package_dir={"": "src"},
    packages=setuptools.find_packages(where="src") + ["twisted.plugins"],
    include_package_data=True,
    zip_safe=False,
    install_requires=[
        "zope.interface",
        "twisted",
        "txtorcon",
    ],
)
