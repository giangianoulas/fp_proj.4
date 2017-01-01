#!/bin/bash
mkdir buildobj
fpc64 simple1.pas -Fu../../src -FUbuildobj
rm -R buildobj
./simple1

