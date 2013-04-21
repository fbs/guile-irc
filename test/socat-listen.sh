#!/bin/bash

socat openssl-listen:8000,reuseaddr,cert=cert.pem,verify=0 -
